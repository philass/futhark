-- ==
-- error: Causality check

let main xs = (\f' -> f' (filter (>0) xs)) (\_ -> 0)
