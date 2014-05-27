{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | The type checker checks whether the program is type-consistent.
-- Whether type annotations are already present is irrelevant, but if
-- they are, the type checker will signal an error if they are wrong.
-- The program does not need to have any particular properties for the
-- type checker to function; in particular it does not need unique
-- names.
module Futhark.InternalRep.TypeChecker
  ( checkProg
  , checkProgNoUniqueness
  , checkClosedExp
  , checkOpenExp
  , TypeError
  )
  where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.RWS

import Data.List
import Data.Loc
import Data.Maybe
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

import Futhark.InternalRep
import Futhark.MonadFreshNames
import Futhark.TypeError

-- | Information about an error that occured during type checking.
type TypeError = GenTypeError VName Exp (Several DeclType) (Several Ident)

-- | A tuple of a return type and a list of parameters, possibly
-- named.
type FunBinding = (RetType, [(Maybe VName, DeclType)])

data VarBinding = Bound Type
                | WasConsumed SrcLoc

data Usage = Consumed SrcLoc
           | Observed SrcLoc
             deriving (Eq, Ord, Show)

data Occurence = Occurence { observed :: Names
                           , consumed :: Names
                           , location :: SrcLoc
                           }
             deriving (Eq, Show)

instance Located Occurence where
  locOf = locOf . location

observation :: Names -> SrcLoc -> Occurence
observation = flip Occurence HS.empty

consumption :: Names -> SrcLoc -> Occurence
consumption = Occurence HS.empty

nullOccurence :: Occurence -> Bool
nullOccurence occ = HS.null (observed occ) && HS.null (consumed occ)

type Occurences = [Occurence]

type UsageMap = HM.HashMap VName [Usage]

usageMap :: Occurences -> UsageMap
usageMap = foldl comb HM.empty
  where comb m (Occurence obs cons loc) =
          let m' = HS.foldl' (ins $ Observed loc) m obs
          in HS.foldl' (ins $ Consumed loc) m' cons
        ins v m k = HM.insertWith (++) k [v] m

combineOccurences :: VName -> Usage -> Usage -> Either TypeError Usage
combineOccurences _ (Observed loc) (Observed _) = Right $ Observed loc
combineOccurences name (Consumed wloc) (Observed rloc) =
  Left $ UseAfterConsume name rloc wloc
combineOccurences name (Observed rloc) (Consumed wloc) =
  Left $ UseAfterConsume name rloc wloc
combineOccurences name (Consumed loc1) (Consumed loc2) =
  Left $ UseAfterConsume name (max loc1 loc2) (min loc1 loc2)

checkOccurences :: Occurences -> Either TypeError ()
checkOccurences = void . HM.traverseWithKey comb . usageMap
  where comb _    []     = Right ()
        comb name (u:us) = foldM_ (combineOccurences name) u us

allConsumed :: Occurences -> Names
allConsumed = HS.unions . map consumed

seqOccurences :: Occurences -> Occurences -> Occurences
seqOccurences occurs1 occurs2 =
  filter (not . nullOccurence) $ map filt occurs1 ++ occurs2
  where filt occ =
          occ { observed = observed occ `HS.difference` postcons }
        postcons = allConsumed occurs2

altOccurences :: Occurences -> Occurences -> Occurences
altOccurences occurs1 occurs2 =
  filter (not . nullOccurence) $ map filt occurs1 ++ occurs2
  where filt occ =
          occ { consumed = consumed occ `HS.difference` postcons
              , observed = observed occ `HS.difference` postcons }
        postcons = allConsumed occurs2


-- | The 'VarUsage' data structure is used to keep track of which
-- variables have been referenced inside an expression, as well as
-- which variables the resulting expression may possibly alias.
data Dataflow = Dataflow {
    usageOccurences :: Occurences
  } deriving (Show)

instance Monoid Dataflow where
  mempty = Dataflow mempty
  Dataflow o1 `mappend` Dataflow o2 =
    Dataflow (o1 ++ o2)

-- | A pair of a variable table and a function table.  Type checking
-- happens with access to this environment.  The function table is
-- only initialised at the very beginning, but the variable table will
-- be extended during type-checking when let-expressions are
-- encountered.
data TypeEnv = TypeEnv { envVtable :: HM.HashMap VName VarBinding
                       , envFtable :: HM.HashMap Name FunBinding
                       , envCheckOccurences :: Bool
                       }

-- | The type checker runs in this monad.  The 'Either' monad is used
-- for error handling.
newtype TypeM a = TypeM (RWST
                         TypeEnv            -- Reader
                         Dataflow           -- Writer
                         (NameSource VName) -- State
                         (Either TypeError) -- Inner monad
                         a)
  deriving (Monad, Functor, Applicative,
            MonadReader TypeEnv,
            MonadWriter Dataflow,
            MonadState VNameSource)

runTypeM :: TypeEnv -> NameSource VName -> TypeM a
         -> Either TypeError a
runTypeM env src (TypeM m) = fst <$> evalRWST m env src

bad :: TypeError -> TypeM a
bad = TypeM . lift . Left

instance MonadFreshNames TypeM where
  getNameSource = get
  putNameSource = put

liftEither :: Either TypeError a -> TypeM a
liftEither = either bad return

occur :: Occurences -> TypeM ()
occur occurs = tell Dataflow { usageOccurences = occurs }

-- | Proclaim that we have made read-only use of the given variable.
-- No-op unless the variable is array-typed.
observe :: Ident -> TypeM ()
observe (Ident nm t loc)
  | basicType t = return ()
  | otherwise   = let als = nm `HS.insert` aliases t
                  in occur [observation als loc]

-- | Proclaim that we have written to the given variable.
consume :: SrcLoc -> Names -> TypeM ()
consume loc als = occur [consumption als loc]

collectDataflow :: TypeM a -> TypeM (a, Dataflow)
collectDataflow m = pass $ do
  (x, dataflow) <- listen m
  return ((x, dataflow), const mempty)

noDataflow :: TypeM a -> TypeM a
noDataflow = censor $ const mempty

maybeCheckOccurences :: Occurences -> TypeM ()
maybeCheckOccurences us = do
  check <- asks envCheckOccurences
  when check $ liftEither $ checkOccurences us

alternative :: TypeM a -> TypeM b -> TypeM (a,b)
alternative m1 m2 = pass $ do
  (x, Dataflow occurs1) <- listen m1
  (y, Dataflow occurs2) <- listen m2
  maybeCheckOccurences occurs1
  maybeCheckOccurences occurs2
  let usage = Dataflow $ occurs1 `altOccurences` occurs2
  return ((x, y), const usage)

-- | Make all bindings nonunique.
noUnique :: TypeM a -> TypeM a
noUnique = local (\env -> env { envVtable = HM.map f $ envVtable env})
  where f (Bound t)         = Bound $ t `setUniqueness` Nonunique
        f (WasConsumed loc) = WasConsumed loc

binding :: [Ident] -> TypeM a -> TypeM a
binding bnds = check . local (`bindVars` bnds)
  where bindVars :: TypeEnv -> [Ident] -> TypeEnv
        bindVars = foldl bindVar

        bindVar :: TypeEnv -> Ident -> TypeEnv
        bindVar env (Ident name tp _) =
          let inedges = HS.toList $ aliases tp
              update (Bound tp') =
                Bound $ tp' `changeAliases` HS.insert name
              update b = b
          in env { envVtable = HM.insert name (Bound tp) $
                               adjustSeveral update inedges $
                               envVtable env }

        adjustSeveral f = flip $ foldl $ flip $ HM.adjust f

        -- Check whether the bound variables have been used correctly
        -- within their scope.
        check m = do
          already_bound <- asks envVtable
          case filter ((`HM.member` already_bound) . identName) bnds of
            []  -> return ()
            v:_ -> bad $ TypeError (srclocOf v) $
                         "Variable " ++ textual (identName v) ++ " being redefined."
          (a, usages) <- collectOccurences m
          maybeCheckOccurences usages
          return a

        -- Collect and remove all occurences in @bnds@.  This relies
        -- on the fact that no variables shadow any other.
        collectOccurences m = pass $ do
          (x, usage) <- listen m
          let (relevant, rest) = split $ usageOccurences usage
          return ((x, relevant), const $ usage { usageOccurences = rest })
          where split = unzip .
                        map (\occ ->
                             let (obs1, obs2) = divide $ observed occ
                                 (con1, con2) = divide $ consumed occ
                             in (occ { observed = obs1, consumed = con1 },
                                 occ { observed = obs2, consumed = con2 }))
                names = HS.fromList $ map identName bnds
                divide s = (s `HS.intersection` names, s `HS.difference` names)

lookupVar :: VName -> SrcLoc -> TypeM Type
lookupVar name pos = do
  bnd <- asks $ HM.lookup name . envVtable
  case bnd of
    Nothing -> bad $ UnknownVariableError name pos
    Just (Bound t) -> return t
    Just (WasConsumed wloc) -> bad $ UseAfterConsume name pos wloc

lookupFun :: SrcLoc -> Name -> [SubExp] -> TypeM (RetType, [DeclType])
lookupFun loc fname args = do
  bnd <- asks $ HM.lookup fname . envFtable
  case bnd of
    Nothing -> bad $ UnknownFunctionError fname loc
    Just (ftype, params) -> do
      -- Create arg->param substitution map, then substitute shapes in
      -- type.
      let (paramnames, paramtypes) = unzip params
          sub (Just p, a) = Just (p, a)
          sub _           = Nothing
          substs = HM.fromList $ mapMaybe sub $ zip paramnames args
          substInType t = t `setArrayShape` substInShape (arrayShape t)
          substInShape = ExtShape . map substInDim . extShapeDims
          substInDim (Free (Var v))
            | Just se <- HM.lookup (identName v) substs = Free se
          substInDim dim                                = dim
      return (map substInType ftype, paramtypes)

-- | @t1 `unifyTypes` t2@ attempts to unify @t2@ and @t2@.  If
-- unification cannot happen, 'Nothing' is returned, otherwise a type
-- that combines the aliasing of @t1@ and @t2@ is returned.  The
-- uniqueness of the resulting type will be the least of the
-- uniqueness of @t1@ and @t2@.
unifyTypes :: Type -> Type -> Maybe Type
unifyTypes (Basic t1) (Basic t2) = Basic <$> t1 `unifyBasicTypes` t2
unifyTypes (Array t1 ds1 u1 als1) (Array t2 ds2 u2 als2)
  | shapeRank ds1 == shapeRank ds2 = do
  t <- t1 `unifyBasicTypes` t2
  Just $ Array t ds2 (u1 <> u2) (als1 <> als2)
unifyTypes _ _ = Nothing

-- | As 'unifyTypes', but for element types.
unifyBasicTypes :: BasicType -> BasicType -> Maybe BasicType
unifyBasicTypes t1 t2
  | t1 == t2  = Just t1
  | otherwise = Nothing

-- | Determine if two types are identical, ignoring uniqueness.
-- Causes a '(TypeError vn)' if they fail to match, and otherwise returns
-- one of them.
unifySubExpTypes :: SubExp -> SubExp -> TypeM Type
unifySubExpTypes e1 e2 =
  maybe (bad $ UnifyError (SubExp e1) (justOne $ toDecl t1)
                          (SubExp e2) (justOne $ toDecl t2)) return $
  unifyTypes t1 t2
  where t1 = subExpType e1
        t2 = subExpType e2

-- | @checkAnnotation loc s t1 t2@ returns @t2@ if @t1@ contains no
-- type, and otherwise tries to unify them with 'unifyTypes'.  If
-- this fails, a 'BadAnnotation' is raised.
checkAnnotation :: SrcLoc -> String -> Type -> Type -> TypeM Type
checkAnnotation loc desc t1 t2 =
  case unifyTypes (t1 `setAliases` HS.empty) t2 of
    Nothing -> bad $ BadAnnotation loc desc
                     (justOne $ toDecl t1) (justOne $ toDecl t2)
    Just t  -> return t

-- | @checkResAnnotation loc s rt1 rt2@ tries to unify the types in
-- @rt1@ and @rt2@ with 'unifyTypes'.  If this fails, or @t1s@ and
-- @t2s@ are not the same length, a 'BadAnnotation' is raised.
checkResAnnotation :: SrcLoc -> String -> ResType -> ResType -> TypeM ResType
checkResAnnotation loc desc rt1 rt2
  | rt1 == rt2 = return rt2
  | otherwise = bad $ BadAnnotation loc desc
                (Several $ map toDecl rt1)
                (Several $ map toDecl rt2)

-- | @require ts e@ causes a '(TypeError vn)' if the type of @e@ does
-- not unify with one of the types in @ts@.  Otherwise, simply returns
-- @e@.  This function is very useful in 'checkExp'.
require :: [Type] -> SubExp -> TypeM SubExp
require ts e
  | any (subExpType e `similarTo`) ts = return e
  | otherwise = bad $ UnexpectedType (SubExp e)
                      (justOne $ toDecl $ subExpType e)
                      (map (justOne . toDecl) ts)

-- | Variant of 'require' working on identifiers.
requireI :: [Type] -> Ident -> TypeM Ident
requireI ts ident
  | any (identType ident `similarTo`) ts = return ident
  | otherwise = bad $ UnexpectedType (SubExp $ Var ident)
                      (justOne $ toDecl $ identType ident)
                      (map (justOne . toDecl) ts)

rowTypeM :: SubExp -> TypeM Type
rowTypeM e = maybe wrong return $ peelArray 1 $ subExpType e
  where wrong = bad $ NotAnArray (srclocOf e) (SubExp e) $
                      justOne $ toDecl $ subExpType e

-- | Type check a program containing arbitrary type information,
-- yielding either a type error or a program with complete type
-- information.
checkProg :: Prog -> Either TypeError Prog
checkProg = checkProg' True

-- | As 'checkProg', but don't check whether uniqueness constraints
-- are being upheld.  The uniqueness of types must still be correct.
checkProgNoUniqueness :: Prog -> Either TypeError Prog
checkProgNoUniqueness = checkProg' False

checkProg' :: Bool -> Prog -> Either TypeError Prog
checkProg' checkoccurs prog = do
  ftable <- buildFtable
  let typeenv = TypeEnv { envVtable = HM.empty
                        , envFtable = ftable
                        , envCheckOccurences = checkoccurs
                        }

  liftM Prog $ runTypeM typeenv src $
        mapM (noDataflow . checkFun) $ progFunctions prog
  where
    src = newNameSourceForProg prog
    -- To build the ftable we loop through the list of function
    -- definitions.  In addition to the normal ftable information
    -- (name, return type, argument types), we also keep track of
    -- position information, in order to report both locations of
    -- duplicate function definitions.  The position information is
    -- removed at the end.
    buildFtable = HM.map rmLoc <$>
                  foldM expand (HM.map addLoc initialFtable)
                  (progFunctions prog)
    expand ftable (name,ret,args,_,pos)
      | Just (_,_,pos2) <- HM.lookup name ftable =
        Left $ DupDefinitionError name pos pos2
      | otherwise =
        let argtypes = map (toDecl . identType) args -- Throw away argument names.
            argnames = map (Just . identName) args
        in Right $ HM.insert name (ret,zip argnames argtypes,pos) ftable
    rmLoc (ret,args,_) = (ret,args)
    addLoc (t, ts) = (t, ts, noLoc)

initialFtable :: HM.HashMap Name FunBinding
initialFtable = HM.map addBuiltin builtInFunctions
  where addBuiltin (t, ts) = ([Basic t], zip (repeat Nothing) $ map Basic ts)

checkFun :: FunDec -> TypeM FunDec
checkFun (fname, rettype, params, body, loc) = do
  params' <- checkParams
  body' <- binding (map fromParam params') $ checkBody body

  checkReturnAlias $ bodyType body'

  if map toDecl (bodyType body') `subtypesOf` map toDecl rettype then
    return (fname, rettype, params', body', loc)
  else bad $ ReturnTypeError loc fname
             (Several $ map toDecl rettype)
             (Several $ map toDecl $ bodyType body')

  where checkParams = reverse <$> foldM expand [] params

        expand params' ident@(Ident pname _ _)
          | Just _ <- find ((==identName ident) . identName) params' =
            bad $ DupParamError fname pname loc
          | otherwise =
            return $ ident : params'

        notAliasingParam names =
          forM_ params $ \p ->
            when (not (unique $ identType p) &&
                  identName p `HS.member` names) $
              bad $ ReturnAliased fname (identName p) loc

        -- | Check that unique return values do not alias a
        -- non-consumed parameter.
        checkReturnAlias =
          foldM_ checkReturnAlias' HS.empty . returnAliasing rettype

        checkReturnAlias' seen (Unique, names)
          | any (`HS.member` HS.map snd seen) $ HS.toList names =
            bad $ UniqueReturnAliased fname loc
          | otherwise = do
            notAliasingParam names
            return $ seen `HS.union` tag Unique names
        checkReturnAlias' seen (Nonunique, names)
          | any (`HS.member` seen) $ HS.toList $ tag Unique names =
            bad $ UniqueReturnAliased fname loc
          | otherwise = return $ seen `HS.union` tag Nonunique names

        tag u = HS.map $ \name -> (u, name)

        returnAliasing expected got =
          [ (uniqueness p, aliases t) |
            (p,t) <- zip expected got ]

-- | Type-check a single expression without any calls to non-builtin
-- functions.  Free variables are permitted, as long as they are
-- present in the passed-in vtable.
checkOpenExp :: HM.HashMap VName Type -> Exp ->
                Either TypeError Exp
checkOpenExp bnds e = runTypeM env (newNameSource frees) $ checkExp e
  where env = TypeEnv { envFtable = initialFtable
                      , envCheckOccurences = True
                      , envVtable = vtable
                      }

        frees = freeNamesInExp e

        vtable = HS.foldr tagBnd HM.empty frees
        tagBnd k m =
          case HM.lookup k bnds of
            Nothing -> m
            Just t  -> HM.insert k (Bound t) m

-- | Type-check a single expression without any free variables or
-- calls to non-builtin functions.
checkClosedExp :: Exp -> Either TypeError Exp
checkClosedExp e = runTypeM env src $ checkExp e
  where env = TypeEnv { envFtable = initialFtable
                      , envCheckOccurences = True
                      , envVtable = HM.empty
                      }
        src = newNameSource $ freeNamesInExp e

checkSubExp :: SubExp -> TypeM SubExp
checkSubExp (Constant v loc) =
  return $ Constant v loc
checkSubExp (Var ident) = do
  ident' <- checkIdent ident
  observe ident'
  return $ Var ident'

checkBody :: Body -> TypeM Body

checkBody (Body [] (Result cs es loc)) = do
  cs' <- mapM (requireI [Basic Cert] <=< checkIdent) cs
  es' <- mapM checkSubExp es
  return $ resultBody cs' es' loc

checkBody (Body (Let pat e:bnds) res) = do
  (e', dataflow) <- collectDataflow $ checkExp e
  (scope, pat') <-
    checkBinding (srclocOf e) pat (typeOf e') dataflow
  scope $ do
    Body bnds' res' <- checkBody body
    return $ Body (Let pat' e':bnds') res'
  where body = Body bnds res

checkExp :: Exp -> TypeM Exp

checkExp (SubExp es) =
  SubExp <$> checkSubExp es

checkExp (ArrayLit es t loc) = do
  es' <- mapM checkSubExp es
  -- Find the universal type of the array arguments.
  et <- case es' of
          [] -> return t
          e:es'' ->
            let check elemt eleme
                  | Just elemt' <- elemt `unifyTypes` subExpType eleme =
                    return elemt'
                  | otherwise =
                    bad $ TypeError loc $ ppType (subExpType eleme) ++ " is not of expected type " ++ ppType elemt ++ "."
            in foldM check (subExpType e) es''

  -- Unify that type with the one given for the array literal.
  t' <- checkAnnotation loc "array-element" t et

  return $ ArrayLit es' t' loc

checkExp (BinOp op e1 e2 t pos) = checkBinOp op e1 e2 t pos

checkExp (Not e pos) = do
  e' <- require [Basic Bool] =<< checkSubExp e
  return $ Not e' pos

checkExp (Negate e loc) = do
  e' <- require [Basic Int, Basic Real] =<< checkSubExp e
  return $ Negate e' loc

checkExp (If e1 e2 e3 t pos) = do
  e1' <- require [Basic Bool] =<< checkSubExp e1
  ((e2', e3'), dflow) <- collectDataflow $ checkBody e2 `alternative` checkBody e3
  tell dflow
  let removeConsumed = (`HS.difference` allConsumed (usageOccurences dflow))
  t' <- checkResAnnotation pos "branch result" t $
        map (`changeAliases` removeConsumed) $
        combineResTypes (bodyType e2') (bodyType e3')
  return $ If e1' e2' e3' t' pos

checkExp (Apply fname args t pos)
  | "trace" <- nameToString fname = do
  args' <- mapM (checkSubExp . fst) args
  t'    <- checkResAnnotation pos "return" t $
           closedResult $ map subExpType args'
  return $ Apply fname [(arg, Observe) | arg <- args'] t' pos

checkExp (Apply fname args rettype loc) = do
  (ftype, paramtypes) <- lookupFun loc fname $ map fst args
  (args', argflows) <- unzip <$> mapM (checkArg . fst) args
  let realType = returnType ftype (map diet paramtypes) (map subExpType args')

  rettype' <- checkResAnnotation loc "return" rettype realType

  checkFuncall (Just fname) loc paramtypes (map toDecl rettype') argflows
  return $ Apply fname (zip args' $ map diet paramtypes) rettype' loc

checkExp (Index cs ident idxes pos) = do
  cs' <- mapM (requireI [Basic Cert] <=< checkIdent) cs
  ident' <- checkIdent ident
  observe ident'
  vt <- lookupVar (identName ident') pos
  when (arrayRank vt < length idxes) $
    bad $ IndexingError (identName ident)
          (arrayRank vt) (length idxes) pos
  idxes' <- mapM (require [Basic Int] <=< checkSubExp) idxes
  return $ Index cs' ident' idxes' pos

checkExp (Update cs src idxes ve loc) = do
  cs' <- mapM (requireI [Basic Cert] <=< checkIdent) cs
  src' <- checkIdent src
  idxes' <- mapM (require [Basic Int] <=< checkSubExp) idxes
  ve' <- checkSubExp ve

  unless (unique (identType src') || basicType (identType src')) $
    bad $ TypeError loc $ "Source '" ++ textual (identName src) ++ "' is not unique"

  when (identName src `HS.member` aliases (subExpType ve')) $
    bad $ BadLetWithValue loc

  consume loc $ aliases $ identType src'

  case peelArray (length idxes) (identType src') of
    Nothing -> bad $ IndexingError (identName src)
                     (arrayRank $ identType src') (length idxes) loc
    Just _ -> return $ Update cs' src' idxes' ve' loc

checkExp (Iota e pos) = do
  e' <- require [Basic Int] =<< checkSubExp e
  return $ Iota e' pos

checkExp (Replicate countexp valexp pos) = do
  countexp' <- require [Basic Int] =<< checkSubExp countexp
  valexp' <- checkSubExp valexp
  return $ Replicate countexp' valexp' pos

checkExp (Reshape cs shapeexps arrexp pos) = do
  cs' <- mapM (requireI [Basic Cert] <=< checkIdent) cs
  shapeexps' <- mapM (require [Basic Int] <=< checkSubExp) shapeexps
  arrexp' <- checkSubExp arrexp
  return (Reshape cs' shapeexps' arrexp' pos)

checkExp (Rearrange cs perm arrexp pos) = do
  cs' <- mapM (requireI [Basic Cert] <=< checkIdent) cs
  arrexp' <- checkSubExp arrexp
  let rank = arrayRank $ subExpType arrexp'
  when (length perm /= rank || sort perm /= [0..rank-1]) $
    bad $ PermutationError pos perm rank name
  return $ Rearrange cs' perm arrexp' pos
  where name = case arrexp of Var v -> Just $ identName v
                              _     -> Nothing

checkExp (Rotate cs n arrexp pos) = do
  cs' <- mapM (requireI [Basic Cert] <=< checkIdent) cs
  arrexp' <- checkSubExp arrexp
  return $ Rotate cs' n arrexp' pos

checkExp (Split cs splitexp arrexp secsize pos) = do
  cs' <- mapM (requireI [Basic Cert] <=< checkIdent) cs
  splitexp' <- require [Basic Int] =<< checkSubExp splitexp
  secsize' <- require [Basic Int] =<< checkSubExp secsize
  arrexp' <- checkSubExp arrexp
  _ <- rowTypeM arrexp' -- Just check that it's an array.
  return $ Split cs' splitexp' arrexp' secsize' pos

checkExp (Concat cs arr1exp arr2exp ressize pos) = do
  cs' <- mapM (requireI [Basic Cert] <=< checkIdent) cs
  arr1exp' <- checkSubExp arr1exp
  arr2exp' <- require [subExpType arr1exp'] =<< checkSubExp arr2exp
  ressize' <- require [Basic Int] =<< checkSubExp ressize
  _ <- rowTypeM arr2exp' -- Just check that it's an array.
  return $ Concat cs' arr1exp' arr2exp' ressize' pos

checkExp (Copy e pos) = do
  e' <- checkSubExp e
  return $ Copy e' pos

checkExp (Assert e pos) = do
  e' <- require [Basic Bool] =<< checkSubExp e
  return $ Assert e' pos

checkExp (Conjoin es pos) = do
  es' <- mapM (require [Basic Cert] <=< checkSubExp) es
  return $ Conjoin es' pos

checkExp (DoLoop respat merge (Ident loopvar loopvart loopvarloc)
                 boundexp loopbody loc) = do
  let (mergepat, mergeexps) = unzip merge
  unless (loopvart == Basic Int) $
    bad $ TypeError loopvarloc "Type annotation of loop variable is not int"
  (boundexp', boundarg) <- checkArg boundexp
  (mergeexps', mergeargs) <- unzip <$> mapM checkArg mergeexps
  let funparams :: [Param]
      funparams = undefined
      paramts   = map (toDecl . identType) funparams
      returnts  = returnType paramts (map diet paramts) $
                  map argType $ boundarg : mergeargs
      mergepat' = undefined mergepat returnts
  checkFuncall Nothing loc paramts paramts mergeargs
  (_, _, _, loopbody', _) <-
    noUnique $ checkFun (nameFromString "<loop body>", undefined paramts, funparams, loopbody, loc)

  respat' <-
    forM respat $ \res ->
      case find ((==identName res) . identName) mergepat' of
        Nothing -> bad $ TypeError loc $ "Loop result variable " ++
                                         textual (identName res) ++
                                         " is not a merge variable."
        Just v  -> do
          t' <- checkType $ identType res `setAliases` aliases (identType v)
          return res { identType = t' }

  return $ DoLoop respat' (zip mergepat' mergeexps')
                  (Ident loopvar (Basic Int) loopvarloc) boundexp'
                  loopbody' loc

checkExp (Map ass fun arrexps pos) = do
  ass' <- mapM (requireI [Basic Cert] <=< checkIdent) ass
  (arrexps', arrargs) <- unzip <$> mapM checkSOACArrayArg arrexps
  fun'    <- checkLambda fun arrargs
  return $ Map ass' fun' arrexps' pos

checkExp (Reduce ass fun inputs pos) = do
  let (startexps, arrexps) = unzip inputs
  ass' <- mapM (requireI [Basic Cert] <=< checkIdent) ass
  (startexps', startargs) <- unzip <$> mapM checkArg startexps
  (arrexps', arrargs)     <- unzip <$> mapM checkSOACArrayArg arrexps
  fun'                    <- checkLambda fun $ startargs ++ arrargs
  let startt      = map subExpType startexps'
      intupletype = map argType arrargs
      funret      = lambdaType fun' $ map argType $ startargs ++ arrargs
  unless (startt `subtypesOf` funret) $
      bad $ TypeError pos $ "Accumulator is of type " ++ ppTuple startt ++
                            ", but reduce function returns type " ++ ppTuple funret ++ "."
  unless (intupletype `subtypesOf` funret) $
      bad $ TypeError pos $ "Array element value is of type " ++ ppTuple intupletype ++
                            ", but scan function returns type " ++ ppTuple funret ++ "."
  return $ Reduce ass' fun' (zip startexps' arrexps') pos

-- ScanT is exactly identical to ReduceT.  Duplicate for clarity
-- anyway.
checkExp (Scan ass fun inputs pos) = do
  let (startexps, arrexps) = unzip inputs
  ass' <- mapM (requireI [Basic Cert] <=< checkIdent) ass
  (startexps', startargs) <- unzip <$> mapM checkArg startexps
  (arrexps', arrargs)     <- unzip <$> mapM checkSOACArrayArg arrexps
  fun'                    <- checkLambda fun $ startargs ++ arrargs
  let startt      = map subExpType startexps'
      intupletype = map argType arrargs
      funret      = lambdaType fun' $ map argType $ startargs ++ startargs
  unless (startt `subtypesOf` funret) $
    bad $ TypeError pos $ "Initial value is of type " ++ ppTuple startt ++
                          ", but scan function returns type " ++ ppTuple funret ++ "."
  unless (intupletype `subtypesOf` funret) $
    bad $ TypeError pos $ "Array element value is of type " ++ ppTuple intupletype ++
                          ", but scan function returns type " ++ ppTuple funret ++ "."
  return $ Scan ass' fun' (zip startexps' arrexps') pos

checkExp (Filter ass fun arrexps outer_shape pos) = do
  ass' <- mapM (requireI [Basic Cert] <=< checkIdent) ass
  outer_shape' <- require [Basic Int] =<< checkSubExp outer_shape
  (arrexps', arrargs) <- unzip <$> mapM checkSOACArrayArg arrexps
  fun' <- checkLambda fun arrargs
  let funret = lambdaType fun' $ map argType arrargs
  when (funret /= [Basic Bool]) $
    bad $ TypeError pos "Filter function does not return bool."
  when (any (unique . identType) $ lambdaParams fun) $
    bad $ TypeError pos "Filter function consumes its arguments."
  return $ Filter ass' fun' arrexps' outer_shape' pos

checkExp (Redomap ass outerfun innerfun accexps arrexps pos) = do
  ass' <- mapM (requireI [Basic Cert] <=< checkIdent) ass
  (arrexps', arrargs)  <- unzip <$> mapM checkSOACArrayArg arrexps
  (accexps', accargs)  <- unzip <$> mapM checkArg accexps
  (innerfun', innerRet) <- checkLambdaArg innerfun $ accargs ++ arrargs

  (outerfun', outerRet) <- checkLambdaArg outerfun $ innerRet ++ innerRet

  let acct = map subExpType accexps'
      innerRetType = map argType innerRet
      outerRetType = map argType outerRet
  unless (innerRetType `subtypesOf` acct) $
    bad $ TypeError pos $ "Initial value is of type " ++ ppTuple acct ++
          ", but redomapT inner reduction returns type " ++ ppTuple innerRetType ++ "."
  unless (outerRetType `subtypesOf` acct) $
    bad $ TypeError pos $ "Initial value is of type " ++ ppTuple acct ++
          ", but redomapT outer reduction returns type " ++ ppTuple outerRetType ++ "."

  return $ Redomap ass' outerfun' innerfun' accexps' arrexps' pos

checkSOACArrayArg :: SubExp -> TypeM (SubExp, Arg)
checkSOACArrayArg e = do
  (e', (t, dflow, argloc)) <- checkArg e
  case peelArray 1 t of
    Nothing -> bad $ TypeError argloc "SOAC argument is not an array"
    Just rt -> return (e', (rt, dflow, argloc))

checkType :: TypeBase als Shape -> TypeM (TypeBase als Shape)
checkType t = do dims <- mapM checkSubExp $ arrayDims t
                 return $ t `setArrayDims` dims

checkIdent :: Ident -> TypeM Ident
checkIdent (Ident name t pos) = do
  vt <- lookupVar name pos
  t'' <- checkAnnotation pos ("variable " ++ textual name)
         (t `setArrayDims` arrayDims vt) vt
  return $ Ident name t'' pos

checkBinOp :: BinOp -> SubExp -> SubExp -> Type -> SrcLoc -> TypeM Exp
checkBinOp Plus e1 e2 t pos = checkPolyBinOp Plus [Real, Int] e1 e2 t pos
checkBinOp Minus e1 e2 t pos = checkPolyBinOp Minus [Real, Int] e1 e2 t pos
checkBinOp Pow e1 e2 t pos = checkPolyBinOp Pow [Real, Int] e1 e2 t pos
checkBinOp Times e1 e2 t pos = checkPolyBinOp Times [Real, Int] e1 e2 t pos
checkBinOp Divide e1 e2 t pos = checkPolyBinOp Divide [Real, Int] e1 e2 t pos
checkBinOp Mod e1 e2 t pos = checkPolyBinOp Mod [Int] e1 e2 t pos
checkBinOp ShiftR e1 e2 t pos = checkPolyBinOp ShiftR [Int] e1 e2 t pos
checkBinOp ShiftL e1 e2 t pos = checkPolyBinOp ShiftL [Int] e1 e2 t pos
checkBinOp Band e1 e2 t pos = checkPolyBinOp Band [Int] e1 e2 t pos
checkBinOp Xor e1 e2 t pos = checkPolyBinOp Xor [Int] e1 e2 t pos
checkBinOp Bor e1 e2 t pos = checkPolyBinOp Bor [Int] e1 e2 t pos
checkBinOp LogAnd e1 e2 t pos = checkPolyBinOp LogAnd [Bool] e1 e2 t pos
checkBinOp LogOr e1 e2 t pos = checkPolyBinOp LogOr [Bool] e1 e2 t pos
checkBinOp Equal e1 e2 t pos = checkRelOp Equal [Int, Real] e1 e2 t pos
checkBinOp Less e1 e2 t pos = checkRelOp Less [Int, Real] e1 e2 t pos
checkBinOp Leq e1 e2 t pos = checkRelOp Leq [Int, Real] e1 e2 t pos

checkRelOp :: BinOp -> [BasicType]
           -> SubExp -> SubExp
           -> Type -> SrcLoc
           -> TypeM Exp
checkRelOp op tl e1 e2 t pos = do
  e1' <- require (map Basic tl) =<< checkSubExp e1
  e2' <- require (map Basic tl) =<< checkSubExp e2
  _ <- unifySubExpTypes e1' e2'
  t' <- checkAnnotation pos (opStr op ++ " result") t $ Basic Bool
  return $ BinOp op e1' e2' t' pos

checkPolyBinOp :: BinOp -> [BasicType]
               -> SubExp -> SubExp -> Type -> SrcLoc
               -> TypeM Exp
checkPolyBinOp op tl e1 e2 t pos = do
  e1' <- require (map Basic tl) =<< checkSubExp e1
  e2' <- require (map Basic tl) =<< checkSubExp e2
  t' <- unifySubExpTypes e1' e2'
  t'' <- checkAnnotation pos (opStr op ++ " result") t t'
  return $ BinOp op e1' e2' t'' pos

sequentially :: TypeM a -> (a -> Dataflow -> TypeM b) -> TypeM b
sequentially m1 m2 = do
  (a, m1flow) <- collectDataflow m1
  (b, m2flow) <- collectDataflow $ m2 a m1flow
  occur $ usageOccurences m1flow `seqOccurences`
          usageOccurences m2flow
  return b

checkBinding :: SrcLoc -> [Ident] -> ResType -> Dataflow
             -> TypeM (TypeM a -> TypeM a, [Ident])
checkBinding loc pat ts dflow = do
  (ts', restpat, shapepat) <- extractContext loc pat ts
  unless (length restpat == length ts') $
    bad $ InvalidPatternError (Several pat) (Several $ map toDecl ts) loc
  restpat' <-
    evalStateT (zipWithM checkBinding' restpat ts') []
  let pat' = shapepat ++ restpat'
  return (\m -> sequentially (tell dflow)
                (const . const $ binding pat' (checkPatSizes pat' >> m)),
          pat')
  where checkBinding' (Ident name namet pos) t = do
          t' <- lift $
                checkAnnotation (srclocOf pat)
                ("binding of variable " ++ textual name) namet t
          let t'' = subExpType $ Var $ Ident name t' pos
          add $ Ident name t'' pos
          return $ Ident name t'' pos

        add ident = do
          bnd <- gets $ find (==ident)
          case bnd of
            Nothing -> modify (ident:)
            Just (Ident name _ pos2) ->
              lift $ bad $ DupPatternError name (srclocOf ident) pos2

extractContext :: SrcLoc -> [Ident] -> ResType -> TypeM ([Type], [Ident], [Ident])
extractContext loc pat rt = do
  (rt', (restpat,_), shapepat) <- runRWST (mapM extract rt) () (pat, HM.empty)
  return (rt', restpat, shapepat)
  where extract t = setArrayShape t <$> Shape <$>
                    mapM extract' (extShapeDims $ arrayShape t)
        extract' (Free se) = return se
        extract' (Ext x)   = correspondingVar x
        correspondingVar x = do (remnames, m) <- get
                                case (remnames, HM.lookup x m) of
                                  (_, Just v)    -> return $ Var v
                                  (v:vs, Nothing)
                                    | Basic Int <- identType v -> do
                                      tell [v]
                                      put (vs, HM.insert x v m)
                                      return $ Var v
                                  (_, Nothing) ->
                                    lift $ bad $ TypeError loc "Pattern cannot match context"

checkPatSizes :: [IdentBase als Shape] -> TypeM ()
checkPatSizes = mapM_ checkBndSizes

checkBndSizes :: IdentBase als Shape -> TypeM ()
checkBndSizes (Ident _ t _) = do
  let dims = arrayDims t
  mapM_ (require [Basic Int] <=< checkSubExp) dims

validApply :: [DeclType] -> [Type] -> Bool
validApply expected got =
  length got == length expected &&
  all id (zipWith subtypeOf (map toDecl got) expected)

type Arg = (Type, Dataflow, SrcLoc)

argType :: Arg -> Type
argType (t, _, _) = t

checkArg :: SubExp -> TypeM (SubExp, Arg)
checkArg arg = do (arg', dflow) <- collectDataflow $ checkSubExp arg
                  return (arg', (subExpType arg', dflow, srclocOf arg'))

checkLambdaArg :: Lambda -> [Arg]
               -> TypeM (Lambda, [Arg])
checkLambdaArg lam args = do
  (lam', dflow) <- collectDataflow $ checkLambda lam args
  let lamt = lambdaType lam' $ map argType args
  return (lam', [ (t, dflow, srclocOf lam) | t <- lamt ])

checkFuncall :: Maybe Name -> SrcLoc
             -> [DeclType] -> [DeclType] -> [Arg]
             -> TypeM ()
checkFuncall fname loc paramtypes _ args = do
  let argts = map argType args

  unless (validApply paramtypes argts) $
    bad $ ParameterMismatch fname loc
          (Right $ map justOne paramtypes) $
          map (justOne . toDecl) argts
  forM_ (zip (map diet paramtypes) args) $ \(d, (t, dflow, argloc)) -> do
    maybeCheckOccurences $ usageOccurences dflow
    let occurs = [consumption (consumeArg t d) argloc]
    occur $ usageOccurences dflow `seqOccurences` occurs
  where consumeArg at Consume = aliases at
        consumeArg _  Observe = HS.empty

checkLambda :: Lambda -> [Arg] -> TypeM Lambda
checkLambda (Lambda params body ret loc) args = do
  ret' <- mapM checkType ret
  if length params == length args then do
    let setParamShape param shape =
          param { identType = identType param `setArrayDims` shape }
        params' = zipWith setParamShape params $
                  map (arrayDims . argType) args
    checkFuncall Nothing loc (map (toDecl . identType) params') (map toDecl ret') args
    (_, _, _, body', _) <-
      noUnique $ checkFun (nameFromString "<anonymous>", closedResult ret', params', body, loc)
    return $ Lambda params' body' ret' loc
  else bad $ TypeError loc $ "Anonymous function defined with " ++ show (length params) ++ " parameters, but expected to take " ++ show (length args) ++ " arguments."
