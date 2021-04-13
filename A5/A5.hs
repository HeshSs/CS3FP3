{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module A5ans where

import Prelude hiding ((>>), drop)

import Data.Bifunctor ( Bifunctor(first) )

import Language.Haskell.TH ( Q, TExp, runQ, Lit (IntegerL) )
import Language.Haskell.TH.Syntax (Lift, unTypeQ)


{------------------------------------------------------------------------------
-- Recalling our Forth-like stack-based language...

Take the StackMachine from tutorial 10, and augment it with just 
enough features to be able to implement the heart of FizzBuzz 

https://skilldrick.github.io/easyforth/ is a helpful resource here for
better understanding Forth.

Note that the JVM is a stack machine, as is postscript (the heart of PDF),
so this is like "assembler" for quite a few languages.
------------------------------------------------------------------------------}

-- The signature is completely given, to make things simpler:
class StackMachine stk where
    empty :: stk ()

    push :: Lift a => a -> stk s -> stk (a, s)
    drop :: stk (a, s) -> stk s

    swap :: stk (a, (b, s)) -> stk (b, (a, s))
    dup  :: stk (a, s) -> stk (a, (a, s))
    rot   :: stk (a, (b, (c, s))) -> stk (b, (c, (a, s)))
    rot23 :: stk (a, (b, (c, s))) -> stk (a, (c, (b, s)))

    -- 's' prefix to obvious things to avoid name clashes
    sadd :: Num a => stk (a, (a, s)) -> stk (a, s)
    ssub :: Num a => stk (a, (a, s)) -> stk (a, s)
    smul :: Num a => stk (a, (a, s)) -> stk (a, s)
    sleq :: Ord a => stk (a, (a, s)) -> stk (Bool, s)
    seql :: Eq  a => stk (a, (a, s)) -> stk (Bool, s)
    smod :: Integral a => stk (a, (a, s)) -> stk (a, s)
    sand :: stk (Bool, (Bool, s)) -> stk (Bool, s)
    sor :: stk (Bool, (Bool, s)) -> stk (Bool, s)
    snot  :: stk (Bool, s) -> stk (Bool, s)

    sappend :: stk (String, (String, s)) -> stk (String, s)

    spair :: stk (a, (b, s)) -> stk ((a, b), s)
    unpair :: stk ((a, b), s) -> stk (a, (b, s))
    sfst  :: stk ((a, b), s) -> stk (a, s)
    ssnd  :: stk ((a, b), s) -> stk (b, s)

    skip :: stk s -> stk s

    ifThenElse :: stk (Bool, (a, (a, s))) -> stk (a, s)

(>>) :: (a -> b) -> (b -> c) -> (a -> c)
(>>) = flip (.)

{------------------------------------------------------------------------------
-- Q1.a
------------------------------------------------------------------------------}

-- Write a program with the following signature that takes as input
-- an Int, and returns a Bool for if the input is 0 mod 3, and
-- the string " Fizz" if True, "" otherwise
fizz :: (StackMachine stk) => stk (Int, s) -> stk (Bool, (String, s))
fizz = push int3              -- (3, (i, e))
      >> smod                 -- (i mod 3, e)
      >> push int0            -- (0, (i mod 3, e))
      >> seql                 -- (i mod 3 == 0, e)
      >> dup                  -- (i mod 3 == 0, (i mod 3 == 0, e))
      >> push (" Fizz" :: String) -- (" Fizz", (i mod 3 == 0, (i mod 3 == 0, e)))
      >> swap                 -- (i mod 3 == 0, (" Fizz", (i mod 3 == 0, e)))
      >> push ("" :: String)  -- ("", (i mod 3 == 0, (" Fizz", (i mod 3 == 0, e))))
      >> rot                  -- (i mod 3 == 0, (" Fizz", ("", (i mod 3 == 0, e))))
      >> ifThenElse           -- (if True then " Fizz" else "", (i mod 3 == 0, e))
      >> swap                 -- (i mod 3 == 0, (if True then " Fizz" else "", e))
  where
    int3 = 3 :: Int
    int0 = 0 :: Int

-- Write a program with the following signature that takes as input
-- an Int, and returns a Bool for if the input is 0 mod 5, and
-- the string " Buzz" if True, "" otherwise
buzz :: (StackMachine stk) => stk (Int, s) -> stk (Bool, (String, s))
buzz = push int5              -- (5, (i, e))
      >> smod                 -- (i mod 5, e)
      >> push int0            -- (0, (i mod 5, e))
      >> seql                 -- (i mod 5 == 0, e)
      >> dup                  -- (i mod 5 == 0, (i mod 5 == 0, e))
      >> push (" Buzz" :: String) -- (" Buzz", (i mod 5 == 0, (i mod 5 == 0, e)))
      >> swap                 -- (i mod 5 == 0, (" Buzz", (i mod 5 == 0, e)))
      >> push ("" :: String)  -- ("", (i mod 5 == 0, (" Buzz", (i mod 5 == 0, e))))
      >> rot                  -- (i mod 5 == 0, (" Buzz", ("", (i mod 5 == 0, e))))
      >> ifThenElse           -- (if True then " Buzz" else "", (i mod 5 == 0, e))
      >> swap                 -- (i mod 5 == 0, (if True then " Buzz" else "", e))
  where
    int5 = 5 :: Int
    int0 = 0 :: Int

-- Write a program with the following signature that takes as input
-- an Int, and returns the following:
-- let (b1, s1) the return of calling fizz
-- let (b2, s2) the return of calling buzz
-- the output will be (not (b1 or b2), s1 ++ s2)
-- this involves a lot of stack manipulation!  My version of this code
-- is 14 instructions long (but I don't guarantee that's optimal)
fizzbuzz :: (StackMachine stk) => stk (Int, s) -> stk (Bool, (String, s))
fizzbuzz = dup      -- (i, (i, e))
          >> fizz     -- (b1, (s1, (i, e)))
          >> rot23    -- (b1, (i, (s1, e)))
          >> swap     -- (i, (b1, (s1, e)))
          >> buzz     -- (b2, (s2, (b1, (s1, e))))
          >> rot23    -- (b2, (b1, (s2, (s1, e))))
          >> sor      -- (b2 or b1, (s2, (s1, e)))
          >> snot     -- (not (b2 or b1), (s2, (s1, e)))
          >> rot      -- (s2, (s1, (not (b2 or b1), e)))
          >> swap     -- (s1, (s2, (not (b2 or b1), e)))
          >> sappend  -- (s1 ++ s2, (not (b2 or b1), e))
          >> swap     -- (not (b2 or b1), (s1 ++ s2, e))

{------------------------------------------------------------------------------
-- Q1.b
------------------------------------------------------------------------------}

-- implement an instance of StackMachine for R.
-- The following lift combinators are useful.
-- (No Applicative or Monad instance, as all functions are unary, in a sense)
newtype R a = R {unR :: a}

instance Functor R where
  fmap f = R . f . unR

liftR1 :: (a -> b) -> R (a, s) -> R (b, s)
liftR1 f = R . (\(x,y) -> (f x , y)) . unR

liftR2 :: (a -> b -> c) -> R (a, (b, s)) -> R (c, s)
liftR2 f (R (a, (b, s))) = R (f a b, s)

instance StackMachine R where
    empty                         = R ()

    push x (R s)                  = R (x, s)
    drop (R (_, s))               = R s

    swap (R (x1, (x2, s)))        = R (x2, (x1, s))
    dup (R (x, s))                = R (x, (x, s))
    rot (R (x1, (x2, (x3, s))))   = R (x2, (x3, (x1, s)))
    rot23 (R (x1, (x2, (x3, s)))) = R (x1, (x3, (x2, s)))

  -- The order of input doesn't matter for commutative operators
  -- e.g. Order only matters for subtraction and modulo in the following operators
  -- smod (R (x1, (x2, e))) works as x2 mod x1 since top of the stack is given as a first parameter
  -- Then, the next element on the stack is given as a 2nd parameter to `mod`
    sadd (R (x1, (x2, s)))        = R (x1 + x2, s)
    ssub (R (x1, (x2, s)))        = R (x2 - x1, s)
    smul (R (x1, (x2, s)))        = R (x1 * x2, s)
    sleq (R (x1, (x2, s)))        = R (x2 <= x1, s)
    seql (R (x1, (x2, s)))        = R (x1 == x2, s)
    smod (R (x1, (x2, s)))        = R (mod x2 x1, s)
    sand (R (x1, (x2, s)))        = R (x1 && x2, s)
    sor (R (x1, (x2, s)))         = R (x1 || x2, s)
    snot (R (x1, s))              = R (not x1, s)

    sappend (R (x1, (x2, s)))     = R (x1 ++ x2, s)

    spair (R (x1, (x2, s)))       = R ((x1, x2), s)
    unpair (R ((x1, x2), s))      = R (x1, (x2, s))
    sfst (R ((x1, _), s))        = R (x1, s)
    ssnd (R ((_, x2), s))        = R (x2, s)

    skip s                        = s

    ifThenElse (R (b, (x1, (x2, s)))) = R (if b then x1 else x2, s)

{------------------------------------------------------------------------------
-- Q1.c, for the R instance above
------------------------------------------------------------------------------}

-- Write a function that returns to the top of the stack as the result
stkEvalResult :: R (a,s) -> a
stkEvalResult (R (x1, _)) = x1

-- Write a function that returns to the String which is the 2nd-most top
-- of the stack as result
stkPrintEvalOutput :: R (a, (String, s)) -> String
stkPrintEvalOutput (R (_, (s1, _))) = s1

{------------------------------------------------------------------------------
-- Q2

Implement a compiler. The following helper function is very handy.
Except for |empty|, ALL of the rest of the code looks like
  func  = clift1 [|| \XXX -> YYY ||]
where pattern-matching on tuples for XXX is crucial.
------------------------------------------------------------------------------}

data C a = C { unC :: Q (TExp a) }

clift1 :: Q (TExp (t -> a)) -> C t -> C a
clift1 g (C x) = C [|| $$g $$x ||]

instance StackMachine C where
  empty   = C [|| () ||]

  push    = \x -> clift1 [|| \s -> (x, s)  ||]
  drop    = clift1 [|| snd ||] 

  swap    = clift1 [|| \(x, (y, e)) -> (y, (x, e)) ||]
  dup     = clift1 [|| \(x, e) -> (x, (x, e)) ||]
  rot     = clift1 [|| \(x1, (x2, (x3, e))) -> (x2, (x3, (x1, e))) ||]
  rot23   = clift1 [|| \(x1, (x2, (x3, e))) -> (x1, (x3, (x2, e))) ||]
  
  -- The order of input doesn't matter for commutative operators
  -- e.g. Order only matters for subtraction and modulo in the following operators
  sadd    = clift1 [|| \(x1, (x2, e)) -> (x1 + x2, e) ||]
  ssub    = clift1 [|| \(x1, (x2, e)) -> (x2 - x1, e) ||]
  smul    = clift1 [|| \(x1, (x2, e)) -> (x1 * x2, e) ||]
  sleq    = clift1 [|| \(x1, (x2, e)) -> (x2 <= x1, e) ||]
  seql    = clift1 [|| \(x1, (x2, e)) -> (x1 == x2, e) ||]
  smod    = clift1 [|| \(x1, (x2, e)) -> (mod x2 x1, e) ||]
  sand    = clift1 [|| \(x1, (x2, e)) -> (x1 && x2, e) ||]
  sor     = clift1 [|| \(x1, (x2, e)) -> (x1 || x2, e) ||]
  snot    = clift1 [|| first not ||]
  -- snot    = clift1 [|| \(x1, e) -> (not x1, e) ||]

  sappend = clift1 [|| \(x1, (x2, e)) -> (x1 ++ x2, e) ||]

  spair   = clift1 [|| \(x1, (x2, e)) -> ((x1, x2), e) ||]
  unpair  = clift1 [|| \((x1, x2), e) -> (x1, (x2, e)) ||]
  sfst    = clift1 [|| \((x1, _), e) -> (x1, e) ||]
  ssnd    = clift1 [|| \((_, x2), e) -> (x2, e) ||]

  skip    = clift1 [|| id ||]

  ifThenElse = clift1 [|| \(b, (x1, (x2, e))) -> (if b then x1 else x2, e) ||]

-----------------------------------------------------------------

{-
  3
  Implement a partial de-compiler, meaning mapping from an
  instance of StackMachine to an Instance of classes
    IntSy, BoolSy, OrderSy, PairSy
  *only*, for the Mar18 version of the classes.

  Use RR below.  See the tutorial 10 material to get started.
-}

class IntSy repr where
  int :: Int -> repr Int
  add :: repr Int -> repr Int -> repr Int
  mul :: repr Int -> repr Int -> repr Int
  sub :: repr Int -> repr Int -> repr Int
  mod_ :: repr Int -> repr Int -> repr Int

class BoolSy repr where
  bool :: Bool -> repr Bool
  if_ :: repr Bool -> repr a -> repr a -> repr a
  and_ :: repr Bool -> repr Bool -> repr Bool
  or_ :: repr Bool -> repr Bool -> repr Bool
  not_ :: repr Bool -> repr Bool

class OrderSy repr where
  leq :: repr Int -> repr Int -> repr Bool

class PairSy repr where
  pair :: repr a -> repr b -> repr (a, b)
  fst_ :: repr (a, b) -> repr a
  snd_ :: repr (a, b) -> repr b

class EqSy repr where
  eql :: (Eq a) => repr a -> repr a -> repr Bool

class StringSy repr where
  append :: repr String -> repr String -> repr String 

------------------

newtype RR c a = RR { unRR :: forall s. c s -> c (a,s) }

-- The order of input doesn't matter for commutative operators
-- e.g. Order only matters for subtraction and modulo in the following operators
instance StackMachine c => IntSy (RR c) where
  int x     = RR (push x)
  add x y   = RR (sadd . unRR y . unRR x)
  mul x y   = RR (smul . unRR y . unRR x)
  sub x y   = RR (ssub . unRR y . unRR x)
  mod_ x y  = RR (smod . unRR y . unRR x)

instance StackMachine c => BoolSy (RR c) where
  bool x      = RR (push x)
  if_ b x1 x2 = RR (ifThenElse . unRR b . unRR x1 . unRR x2)
  and_ x1 x2  = RR (sand . unRR x1 . unRR x2)
  or_ x1 x2   = RR (sor . unRR x1 . unRR x2)
  not_ x1     = RR (snot . unRR x1)

instance StackMachine c => OrderSy (RR c) where
  leq x1 x2   = RR (sleq . unRR x2 . unRR x1)

instance StackMachine c => PairSy (RR c) where
  pair x y    = RR (spair . unRR x . unRR y)
  fst_ x      = RR (sfst . unRR x)
  snd_ x      = RR (ssnd . unRR x)

instance StackMachine c => EqSy (RR c) where 
  eql x y     = RR (seql . unRR x . unRR y)

instance StackMachine c => StringSy (RR c) where
  append x y  = RR (sappend . unRR x . unRR y)

{- 4
  Write test cases for all of this:

  - 10 non-trivial programs in the StackMachine language. Each should
     have at least 8 instructions.  
  - write code that runs all these programs and check that the answer is
    correct.

  - pass all your programs through the C compiler as well. use |pprint|
     in a |main| program to print out the result code for each

  - for all the programs that can be decompiled (there should be at least 5),
      - run them through the RR interpreter, instantiated with the R interpreter
        and then the Mar18.R interpreter to "run" them
      - do the same for the PP instance of Mar22
      - (bonus) do the same for the PE instance of Apr??
-}

-- Functions for running different compiler and interpreter

runR :: (R () -> R a) -> a
runR prog = unR $ prog empty

runC :: (C () -> C a) -> Q (TExp a)
runC prog = unC $ prog empty

runRRR :: RR R a -> (a, ())
runRRR prog = unR $ unRR prog empty 

runRRC :: RR C a -> Q (TExp (a, ()))
runRRC prog = unC $ unRR prog empty

-- Test cases

prog1 :: StackMachine stk => stk s -> stk (Bool, s)
prog1 = push (2 :: Int) 
     >> push (3 :: Int)
     >> sadd 
     >> push (3 :: Int) 
     >> smod
     >> push (2 :: Int) 
     >> seql
     >> push (4 :: Int)
     >> push (3 :: Int)
     >> smul
     >> push (11 :: Int)
     >> sleq
     >> snot
     >> sand

-- >>> runR prog1
-- (True,())

prog2 = push True
     >> push (2 :: Int)
     >> push (3 :: Int)
     >> rot
     >> rot
     >> ifThenElse
     >> fizzbuzz
     >> drop
     >> push " Buzz"
     >> swap
     >> sappend
     
-- >>> runR prog2
-- (" Fizz Buzz",())

prog3 = push (15 :: Int)
     >> fizzbuzz
     >> drop
     >> push True
     >> push (2 :: Int)
     >> push (3 :: Int)
     >> rot
     >> rot
     >> ifThenElse
     >> fizzbuzz
     >> drop
     >> push " Buzz"
     >> swap
     >> sappend
     >> seql

-- >>> runR prog3
-- (True,())

prog4 = push True
     >> push False
     >> push False
     >> rot23
     >> snot
     >> sand
     >> swap
     >> snot
     >> sand

-- >>> runR prog4
-- (True,())

-- (20 - (20 - 10)) mod 3 == 1
prog5 = push (20 :: Int)
     >> dup
     >> push (10 :: Int)
     >> ssub
     >> ssub 
     >> push (3 :: Int)
     >> smod
     >> push (1 :: Int)
     >> seql

-- >>> runR prog5
-- (True,())

prog6 = push True
     >> push False
     >> push False
     >> ifThenElse
     >> push (3 :: Int)
     >> push (5 :: Int)
     >> smul
     >> fizzbuzz
     >> snot
     >> rot23
     >> sand

-- >>> runR prog6
-- (True,(" Fizz Buzz",()))

prog7 = push " Fizz"
     >> push (5 :: Int)
     >> spair
     >> push " Buzz"
     >> push (3 :: Int)
     >> spair
     >> push True 
     >> ifThenElse 
     >> unpair
     >> fizz
     >> drop
     >> sappend

-- >>> runR prog7
-- (" Fizz Buzz",())

prog8 = push " Fizz"
     >> push (5 :: Int)
     >> spair
     >> push " Buzz"
     >> push (3 :: Int)
     >> spair
     >> push False 
     >> ifThenElse 
     >> unpair
     >> buzz
     >> drop
     >> sappend

-- >>> runR prog8
-- (" Buzz Fizz",())

prog9 = push (-5 :: Int)
     >> push (5 :: Int)
     >> spair
     >> push (-3 :: Int)
     >> push (3 :: Int)
     >> spair
     >> push True
     >> rot23
     >> ifThenElse
     >> dup
     >> ssnd
     >> swap
     >> sfst
     >> sadd
     >> push (0 :: Int)
     >> seql

-- >>> runR prog9
-- (True,())

prog10 = push False
      >> snot
      >> push False
      >> sor
      >> push (20 :: Int)
      >> swap
      >> push (-10 :: Int)
      >> rot
      >> drop
      >> ssub
      >> push (-30 :: Int)
      >> seql

-- >>> runR prog10
-- (True,())

test4 :: IntSy repr => repr Int
test4 = (int 1 `add` int 2) `add` (int 3 `add` int 4)

test5 :: (OrderSy repr, IntSy repr) => repr Bool
test5 = (int 1 `add` int 2) `mul` (int 3 `add` int 4) `leq` int 9

runQ1 = runQ $ unTypeQ $ runRRC test4
-- >>> runRRR test4
-- (10,())

-- main :: IO ()
-- main = do
--   let  = 
