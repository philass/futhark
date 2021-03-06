{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Futhark.IR.PrimitiveTests
  ( tests,
    arbitraryPrimValOfType,
  )
where

import Control.Applicative
import Futhark.IR.Primitive
import Language.SexpGrammar
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Prelude

tests :: TestTree
tests = testGroup "PrimitiveTests" [propPrimValuesHaveRightType, doUnOpTests]

propPrimValuesHaveRightType :: TestTree
propPrimValuesHaveRightType =
  testGroup
    "propPrimValuesHaveRightTypes"
    [ testCase (show t ++ " has blank of right type") $
        primValueType (blankPrimValue t) @?= t
      | t <- [minBound .. maxBound]
    ]

doUnOpTests :: TestTree
doUnOpTests =
  testGroup
    "doUnOp"
    [ testCase "not" $
        let unop = decode @UnOp "(complement i32)"
            val = decode @PrimValue "42i32"
            res = doUnOp <$> unop <*> val
         in res @?= Right (Just (IntValue (Int32Value (-43))))
    ]

instance Arbitrary IntType where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary FloatType where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary PrimType where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary IntValue where
  arbitrary =
    oneof
      [ Int8Value <$> arbitrary,
        Int16Value <$> arbitrary,
        Int32Value <$> arbitrary,
        Int64Value <$> arbitrary
      ]

instance Arbitrary FloatValue where
  arbitrary =
    oneof
      [ Float32Value <$> arbitrary,
        Float64Value <$> arbitrary
      ]

instance Arbitrary PrimValue where
  arbitrary =
    oneof
      [ IntValue <$> arbitrary,
        FloatValue <$> arbitrary,
        BoolValue <$> arbitrary,
        pure Checked
      ]

arbitraryPrimValOfType :: PrimType -> Gen PrimValue
arbitraryPrimValOfType (IntType Int8) = IntValue . Int8Value <$> arbitrary
arbitraryPrimValOfType (IntType Int16) = IntValue . Int16Value <$> arbitrary
arbitraryPrimValOfType (IntType Int32) = IntValue . Int32Value <$> arbitrary
arbitraryPrimValOfType (IntType Int64) = IntValue . Int64Value <$> arbitrary
arbitraryPrimValOfType (FloatType Float32) = FloatValue . Float32Value <$> arbitrary
arbitraryPrimValOfType (FloatType Float64) = FloatValue . Float32Value <$> arbitrary
arbitraryPrimValOfType Bool = BoolValue <$> arbitrary
arbitraryPrimValOfType Cert = return Checked
