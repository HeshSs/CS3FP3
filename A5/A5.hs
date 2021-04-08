{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module A5ans where

import Prelude hiding ((>>), drop)

import Data.Bifunctor ( Bifunctor(first) )

import Language.Haskell.TH ( Q, TExp )
import Language.Haskell.TH.Syntax (Lift)


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
fizz s = push 3 s     -- (3, (i, e))
      >> smod         -- (i mod 3, e)
      >> push 0       -- (0, (i mod 3, e))
      >> seql         -- (i mod 3 == 0, e)
      >> push " Fizz" -- (" Fizz", (i mod 3 == 0, e))
      >> swap         -- (i mod 3 == 0, (" Fizz", e))
      >> push ""      -- ("", (i mod 3 == 0, (" Fizz", e)))
      >> rot          -- (i mod 3 == 0, (" Fizz", ("", e)))
      >> ifThenElse   -- (if True then " Fizz" else "", e)

-- Write a program with the following signature that takes as input
-- an Int, and returns a Bool for if the input is 0 mod 5, and
-- the string " Buzz" if True, "" otherwise
buzz :: (StackMachine stk) => stk (Int, s) -> stk (Bool, (String, s))
buzz s = push 5 s     -- (5, (i, e))
      >> smod         -- (i mod 5, e)
      >> push 0       -- (0, (i mod 5, e))
      >> seql         -- (i mod 5 == 0, e)
      >> push " Buzz" -- (" Fizz", (i mod 5 == 0, e))
      >> swap         -- (i mod 5 == 0, (" Fizz", e))
      >> push ""      -- ("", (i mod 5 == 0, (" Fizz", e)))
      >> rot          -- (i mod 5 == 0, (" Fizz", ("", e)))
      >> ifThenElse   -- (if True then " Fizz" else "", e)

-- Write a program with the following signature that takes as input
-- an Int, and returns the following:
-- let (b1, s1) the return of calling fizz
-- let (b2, s2) the return of calling buzz
-- the output will be (not (b1 or b2), s1 ++ s2)
-- this involves a lot of stack manipulation!  My version of this code
-- is 14 instructions long (but I don't guarantee that's optimal)
fizzbuzz :: (StackMachine stk) => stk (Int, s) -> stk (Bool, (String, s))
fizzbuzz s = result


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

{------------------------------------------------------------------------------
-- Q1.c, for the R instance above
------------------------------------------------------------------------------}

-- Write a function that returns to the top of the stack as the result
stkEvalResult :: R (a,s) -> a
stkEvalResult = _

-- Write a function that returns to the String which is the 2nd-most top
-- of the stack as result
stkPrintEvalOutput :: R (a, (String, s)) -> String
stkPrintEvalOutput = _

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

-----------------------------------------------------------------

{-
  3
  Implement a partial de-compiler, meaning mapping from an
  instance of StackMachine to an Instance of classes
    IntSy, BoolSy, OrderSy, PairSy
  *only*, for the Mar18 version of the classes.

  Use RR below.  See the tutorial 10 material to get started.
-}

newtype RR c a = RR { unRR :: forall s. c s -> c (a,s) }

instance StackMachine c => IntSy (RR c) where
instance StackMachine c => BoolSy (RR c) where
instance StackMachine c => OrderSy (RR c) where
instance StackMachine c => PairSy (RR c) where

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
