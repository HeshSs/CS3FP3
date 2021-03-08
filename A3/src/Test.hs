module Test where

import Data.List (partition)
import Expr
import Law
import Matching
import Parser
import Rewriting

x , y, a, b, c, d, e :: Expr
x = Var 'x'
y = Var 'y'
a = Con "foo" [x, y]
b = Con "bar" [y, x]
c = Con "foo" [x, y]
d = Con "thing" [x, x, x]
e = Var 'z'

-------------------------------------------------------------------------------

-- | `compose`
t1, t2, t3, t4 :: Bool
t1 = compose [Var 'x'] == Var 'x'
t2 = compose [Con "zip" [x, y]] == Con "zip" [x, y]
t3 = compose [a, b] == Compose [a, b]
t4 =
  compose [Compose [a, b], Compose [c, d, e], d, e]
    == Compose [a, b, c, d, e, d, e]

basicLaw_test0 =
  let nil = Con "nil" []
   in basicLaw (Law "nil constant" (Compose [nil, Var 'f']) nil) == True

basicLaw_test1 =
  let ide = Con "id" []; mp_ide = Con "map" [ide]
   in basicLaw (Law "map id, reverse" ide mp_ide) == False

basicLaw_test2 =
  let nil = Con "nil" []; mapf = Con "map" [Var 'f']
   in basicLaw (Law "nil natural" (Compose [mapf, nil]) nil) == True

-------------------------------------------------------------------------------

-- | `conclusion`
c0 = (Var 'f', [("reason1", Var 'g'), ("reason2", Var 'h')])

c1 = (Var 'f', [])

e5 :: Expr
e5 = Compose [Con "fst" [], Con "pair" [Compose [Var 'f', Con "fst" []], Compose [Var 'g', Con "snd" []]], Con "zip" []]

conclusion_test0 = conclusion c0 == Var 'h'

conclusion_test1 = conclusion c1 == Var 'f'

conclusion_test2 = conclusion (e5, [("refl", e5)]) == e5

-------------------------------------------------------------------------------

-- | `reverseCalc`
t5, t6 :: Bool
t5 = reverseCalc (x, []) == (x, [])
t6 = reverseCalc (x, [("foo", a), ("bar", b)]) == (b, [("bar", a), ("foo", x)])

-------------------------------------------------------------------------------

-- | `applySubst`
s1 :: Subst
s1 = [('x', a), ('y', d)]

e6, e7, e8 :: Expr
e6 = Compose [x, y]
e7 = Compose [a, d]
e8 = Compose [Con "toto" [], Con "tata" []]

t7, t8, t9, t10 :: Bool
t7 = applySubst s1 e6 == e7
t8 = applySubst s1 e8 == e8
t9 = applySubst s1 x == a
t10 = applySubst s1 e == e

-------------------------------------------------------------------------------

-- | `parts`
t11, t12, t13, t14, t15 :: Bool
t11 =
  parts 1 [1, 2, 3, 4]
    == [[[1, 2, 3, 4]]]
t12 =
  parts 2 [1, 2, 3, 4]
    == [[[1], [2, 3, 4]], [[1, 2], [3, 4]], [[1, 2, 3], [4]]]
t13 =
  parts 3 [1, 2, 3, 4]
    == [[[1], [2], [3, 4]], [[1], [2, 3], [4]], [[1, 2], [3], [4]]]
t14 =
  parts 4 [1, 2, 3, 4]
    == [[[1], [2], [3], [4]]]
t15 =
  parts 5 [1, 2, 3, 4]
    == []

-- More tests for `parts`

t16 =
  parts 2 [1, 2, 3, 4, 5]
    == [[[1],[2, 3, 4, 5]], 
        [[1, 2], [3, 4, 5]], 
        [[1, 2, 3], [4, 5]], 
        [[1, 2, 3, 4], [5]]]

t17 =
  parts 3 [1, 2, 3, 4, 5]
    == [[[1], [2], [3, 4, 5]], 
        [[1], [2, 3], [4, 5]], 
        [[1], [2, 3, 4], [5]],
        [[1, 2], [3], [4, 5]],
        [[1, 2], [3, 4], [5]], 
        [[1, 2, 3], [4], [5]]]

-- Tests for the remaining functions
-------------------------------------------------------------------------------

-- | `intervals`

intervals_test0 = intervals (parts 4 [1,2,3,4]) == [(0,1),(1,1),(2,1),(3,1)]

intervals_test1 = intervals (parts 3 [1,2,3,4]) == [(0,1),(1,1),(2,2),(0,1),(1,2),(2,1),(0,2),(1,1),(2,1)]

intervals_test2 :: Bool
intervals_test2 = intervals (parts 2 [1,2,3]) == [(0,1),(1,2),(0,2),(1,1)]

-------------------------------------------------------------------------------

-- | `contains`

contains_test0 = contains (1,2) (intervals (parts 3 [1,2,3,4]))

contains_test1 = not $ contains (4,2) (intervals (parts 3 [1,2,3,4]))

contains_test2 = contains (4,2) (intervals (parts 5 [1..10]))

-------------------------------------------------------------------------------

-- | `uniqIntervals`

uniqIntervals_test0 = uniqIntervals (intervals (parts 4 [1,2,3,4])) [] == intervals (parts 4 [1,2,3,4])

uniqIntervals_test1 = uniqIntervals (intervals (parts 3 [1,2,3,4])) [] == [(0,1),(1,1),(2,2),(1,2),(2,1),(0,2)]

uniqIntervals_test2 = uniqIntervals (intervals (parts 2 [1,2,3])) [] == intervals (parts 2 [1,2,3])

-------------------------------------------------------------------------------

-- | `subList`

subList_test0 = subList 0 10 [1..10] == [1..10]

subList_test1 = subList 5 10 [1..10] == [6..10]

subList_test2 :: Bool
subList_test2 = subList 10 1 [0..10] == [10]

-------------------------------------------------------------------------------

-- | `segments`

segments_test0 = segments [a, b] == [SubExpr (Compose [a, b]) All]

segments_test1 = segments [a, b, c] == [SubExpr (Compose [a, b, c]) All, SubExpr (Compose [b, c]) (Seg 1 2), SubExpr (Compose [a, b]) (Seg 0 2)]

segments_test2 = segments [a, b, c, d] == [SubExpr (Compose [a, b, c, d]) All,
                                           SubExpr (Compose [b, c, d]) (Seg 1 3),
                                           SubExpr (Compose [a, b]) (Seg 0 2),
                                           SubExpr (Compose [b, c]) (Seg 1 2),
                                           SubExpr (Compose [a, b, c]) (Seg 0 3),
                                           SubExpr (Compose [c, d]) (Seg 2 2)]

-------------------------------------------------------------------------------

-- | `subExprs`

e2 = parseExpr "f . g . zip (m . n, p . q)"
subExprs_test0 = length (subExprs e2) == 12

subExprs_test1 = length (subExprs (Compose [a, b, c, d, e])) == 24

subExprs_test2 = subExprs (Compose [a, d]) == [SubExpr (Compose [a, d]) All,
                                               SubExpr a (Arg 0 All),
                                               SubExpr x (Arg 0 (Arg 0 All)),
                                               SubExpr y (Arg 0 (Arg 1 All)),
                                               SubExpr d (Arg 1 All),
                                               SubExpr x (Arg 1 (Arg 0 All)),
                                               SubExpr x (Arg 1 (Arg 1 All)),
                                               SubExpr x (Arg 1 (Arg 2 All))]

-------------------------------------------------------------------------------
laws, filters, ifs, pairs, others :: [Law]
laws = filters ++ ifs ++ pairs ++ others
filters =
  map
    parseLaw
    [ "definition filter: filter p = concat.map(guard p)",
      "definition guard:  guard p  = if(p, wrap, nil)"
    ]
ifs =
  map
    parseLaw
    [ "if over composition: if(p, f, g) . h = if(p.h, f.h, g.h)",
      "composition over if: h . if(p, f, g) = if(p,   h.f, h.g)"
    ]
pairs =
  map
    parseLaw
    [ "definition fst:     fst . pair(f, g) = f",
      "definition snd:     snd . pair(f, g) = g",
      "definition cross:   cross(f, g)      = pair(f . fst, g . snd)",
      "pair fusion:        pair(f,g) . h    = pair(f . h, g . h)"
    ]
others =
  map
    parseLaw
    [ "nil constant:   nil . f        = nil",
      "nil natural:    map f . nil    = nil",
      "wrap natural:   map f . wrap   = wrap . f",
      "concat natural: map f . concat = concat . map (map f)",
      "map functor:    map f . map g  = map (f . g)"
    ]

prove1 :: IO ()
prove1 = do
  let proof = prove laws "filter p . map f = map f . filter (p . f)"
  proofAnswer <- readFile "eq1_proof.txt"
  --  proofAnswer <- readFile "src/eq1_proof.txt" -- If you're using Stack and eq1_proof.txt is in src, you'll want this line instead
  print (proof == proofAnswer)
