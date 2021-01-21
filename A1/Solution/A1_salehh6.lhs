Assignment 1 - Compsci 3FA3 - Hishmat Salehi - Salehh6
\begin{code}
module A1_salehh6 where
\end{code}

\begin{enumerate}

\item Question 1:

The matches function returns all the elements in the last (2nd argument) that are equal to the 1st argument.
\begin{code}
matches :: Eq a => a -> [a] -> [a]
matches _ []       = []
matches e (x : xs) | e == x    = x : matches e xs
                   | otherwise = matches e xs
\end{code}

The element function returns True if the 1st argument is an element of the list (2nd argument), otherwise returns False.
\begin{code}
element :: Eq a => a -> [a] -> Bool
element _ []       = False
element e (x : xs) | e == x    = True
                   | otherwise = element e xs
\end{code}

The posHelper function returns a list of all the indexes at which the 1st argument occurs in the list (2nd argument).
The pos function just calls the posHelper function with index 0 and it's 1st and 2nd arguments.
\begin{code}
pos :: Eq a => a -> [a] -> [Integer]
pos = posHelper 0
    where
        posHelper _ _ [] = []
        posHelper i e (x : xs) | e == x    = i : posHelper (i + 1) e xs
                               | otherwise = posHelper (i + 1) e xs
\end{code}

Which of the 3 functions, 
1. Which do very related things?
The matches and pos function do almost the same thing, because one gets all the occurences of an element and the other
gets all the indices of that element.

2. Which is 'best'?
I think the pos function is the best, because it's implementation needs either a helper function or a list comprehension
with zip function because we gotta keep track of the indices. Also, the pos function gives you the index of where the element exists
, but the matches function just repeats the same element over and over again.

\item Question 2:

The applyAll function returns a list resulted from the cross-product of applying all the functions in the 1st list (1st argument) 
to the 2nd list (2nd argument).
\begin{code}
applyAll :: [a -> b] -> [a] -> [b]
applyAll _ []     = []
applyAll [] _     = []
applyAll (f:fs) l = map f l ++ applyAll fs l
\end{code}

\item Question 3:

The tripleNeg1 function triples every negative number in a list, recursively.
\begin{code}
tripleNeg1 :: (Ord a, Num a) => [a] -> [a]
tripleNeg1 []     = []
tripleNeg1 (x:xs) | x < 0     = (x * 3) : tripleNeg1 xs
                  | otherwise = x : tripleNeg1 xs
\end{code}

The tripleNeg2 function triples every negative number in a list, using either and map functions.
\begin{code}
tripleNeg2 :: (Ord a, Num a) => [a] -> [a]
tripleNeg2 l = map (either id (* 3)) s
        where 
            s = map (\x -> if x < 0 then Right x else Left x) l
\end{code}

\item Question 4:

Some help was taken from http://learnyouahaskell.com/making-our-own-types-and-typeclasses
Creating a new data type called OrBoth that contains elements of type a, b or both.
\begin{code}
data OrBoth a b = A a | B b | OrBoth a b deriving Show
\end{code}

The consume1 function applies the given function to a given OrBoth data type. 
\begin{code}
consume1 :: (a -> c) -> (b -> c) -> (a -> b -> c) -> OrBoth a b -> c
consume1 fa fb fboth (OrBoth a b) = fboth a b
consume1 fa fb fboth (A a)        = fa a
consume1 fa fb fboth (B b)        = fb b
\end{code}

The consume2 function applies the given function to a given OrBoth data type, but when called for both a and b, 
it applies the first 2 functions on the given inputs then applies the 3rd function on the result taken from their outputs.
\begin{code}
consume2 :: (a -> c) -> (b -> c) -> (c -> c -> c) -> OrBoth a b -> c
consume2 fa fb fboth (OrBoth a b) = fboth (fa a) (fb b)
consume2 fa fb fboth (A a)        = fa a
consume2 fa fb fboth (B b)        = fb b
\end{code}

Which of those two ways is "better" in your opinion? Why?
I think the consume1 function is better, because you can create the functionality of consume2 all from consume1. 
i.e. You can call consume1 in itself, but for consume2 you can't create the functionality of consume1.

\item Question 5:

Creating a new data type called Ternary, which represents a ternary tree.
\begin{code}
data Ternary a = TLeaf a | TNode (Ternary a) (Ternary a) (Ternary a) deriving Show
\end{code}

The mirror function makes a mirror reflection of the current tree, meaning everything in the left leaf a node is moved
to the right node and vice versa. 
\begin{code}
mirror :: Ternary a -> Ternary a
mirror (TNode a b c) = TNode (mirror c) (mirror b) (mirror a)
mirror (TLeaf a)     = TLeaf a
\end{code}

The flattenTernary function converts a ternary tree to a list, where the left most leaf/node is included first and the right
most leaf/node is added last.
\begin{code}
flattenTernary :: Ternary a -> [a]
flattenTernary (TLeaf a)     = [a]
flattenTernary (TNode a b c) = flattenTernary a ++ flattenTernary b ++ flattenTernary c
\end{code}

\item Question 6:

\begin{verbatim}
Given universal quantification over a list:
all :: (a -> Bool) -> [a] -> Bool
all p [] = True                         -- base case of all
all p (x : xs) = p x && all p xs        -- inductive step of all

Prove ∀p, xs, ys, all p (xs ++ ys) = all p xs ∧ all p ys 
by induction.

Let P(xs) = ∀p, xs, ys, all p (xs ++ ys) = all p xs ∧ all p ys,
we will prove P(xs) by weak inducion on xs.

Base case P([]): 
We have
    ∀p, ys, all p ([] ++ ys) = all p [] ∧ all p ys      (By base case of all)
    ∀p, ys, all p ([] ++ ys) = True ∧ all p ys          (By identity of ∧)
    ∀p, ys, all p ([] ++ ys) = all p ys                 

Let Q(ys) = ∀p, ys, all p ([] ++ ys) = all p ys,
we will prove Q(ys) by another induction on ys.

Base case Q([]):
We have
    ∀p, all p ([] ++ []) = all p []     (By identity of ++)
    ∀p, all p [] = all p []             (By reflexitivity of =)
    ∀p, True                            (True for all)
    True

Then Q([]) holds.

Inductive step Q(y:ys):
    ∀p, all p ([] ++ (y:ys)) = all p (y:ys)     (By identity of ++)
    ∀p, all p (y:ys) = all p (y:ys)             (By reflexitivity of =)
    ∀p, True                                    (True for all)
    True

Then, Q(ys) holds, as a result P([]) holds as well.

Inductive step P(x:xs):
We have
    ∀p, ys, all p ((x:xs) ++ ys) = all p (x:xs) ∧ all p ys

Let Q(ys) = ∀p, ys, all p ((x:xs) ++ ys) = all p (x:xs) ∧ all p ys, 
we will prove Q(ys) by another induction on ys.

Base case Q([]):
We have
    ∀p, all p ((x:xs) ++ []) = all p (x:xs) ∧ all p []      (By identity of ++)
    ∀p, all p (x:xs) = all p (x:xs) ∧ all p []              (By base case of all)
    ∀p, all p (x:xs) = all p (x:xs) ∧ true                  (By identity of ∧)
    ∀p, all p (x:xs) = all p (x:xs)                         (By reflexitivity of =)
    ∀p, True                                                (True for all)
    True

Then Q([]) holds.

Inductive step Q(y:ys):
    ∀p, all p ((x:xs) ++ (y:ys)) = all p (x:xs) ∧ all p (y:ys)              (By inductive step of all)
    ∀p, all p ((x:xs) ++ (y:ys)) = p x ∧ (all p xs) ∧ p y ∧ (all p ys)      (By associativity of ∧)
    ∀p, all p ((x:xs) ++ (y:ys)) = p x ∧ p y ∧ (all p xs) ∧ (all p ys)      (By inductive step of P(xs))
    ∀p, all p ((x:xs) ++ (y:ys)) = p x ∧ p y ∧ all p (xs ++ ys)             (By definition of :)
    ∀p, all p (x:(xs ++ (y:ys))) = p x ∧ p y ∧ all p (xs ++ ys)             (By inductive step of all)
    ∀p, p x ∧ all p (xs ++ (y:ys)) = p x ∧ p y ∧ all p (xs ++ ys)           (By reflexitivity of ++)
    ∀p, p x ∧ all p ((y:ys) ++ xs) = p x ∧ p y ∧ all p (xs ++ ys)           (By inductive step of Q(ys))
    ∀p, p x ∧ all p (y:ys) ∧ all p xs = p x ∧ p y ∧ all p (xs ++ ys)        (By inductive step of all)
    ∀p, p x ∧ p y ∧ all p ys ∧ all p xs = p x ∧ p y ∧ all p (xs ++ ys)      (By associativity of ∧)
    ∀p, p x ∧ p y ∧ all p xs ∧ all p ys = p x ∧ p y ∧ all p (xs ++ ys)      (By inductive step of P(xs))
    ∀p, p x ∧ p y ∧ all p (xs ++ ys) = p x ∧ p y ∧ all p (xs ++ ys)         (By reflexitivity of =)
    ∀p, True                                                                (True for all)
    True

Then, Q(ys) holds, as a result P(xs) holds as well.
\end{verbatim}

\item Question 7:

Explicitly define ∀xs, ys . mystery f xs ys = map f (zip xs yz)
\begin{code}
mystery :: ((a, b) -> c) -> [a] -> [b] -> [c]
mystery _ [] []         = []
mystery f (x:xs) (y:ys) = f (x, y) : mystery f xs ys
\end{code}

\item Question 8:

Some help was taken from 
https://medium.com/@StevenLeiva1/haskell-book-chapter-10-folding-lists-3e9e82ce5201#:~:text=To%20recap%2C%20with%20foldr%20%2C%20the,returns%20a%20new%20default%20value.
The reverse function reverses a list using the foldr function.
\begin{code}
reverse' :: [a] -> [a]
reverse' xs = foldr (\y ys -> ys ++ [y]) [] xs
\end{code}

Bonus (Proof):
\begin{verbatim}
TODO
\end{verbatim}

\item Question 9:

Creating a new data type called Tree, which represents a binary tree.
\begin{code}
data Tree a = Tip | Node (Tree a) a (Tree a) deriving Show
\end{code}

The mirror function returns a mirror reflection of the current tree.
\begin{code}
mirrorTree :: Tree a -> Tree a
mirrorTree Tip          = Tip
mirrorTree (Node l a r) = Node (mirrorTree r) a (mirrorTree l)
\end{code}

Info about pre-order and post-order at https://www.geeksforgeeks.org/tree-traversals-inorder-preorder-and-postorder/
The pre function traverses a tree in pre-order and returns a list. 
\begin{code}
pre :: Tree a -> [a]
pre Tip          = []
pre (Node l a r) = [a] ++ pre l ++ pre r
\end{code}

The post function traverses a tree in post-order and returns a list. 
\begin{code}
post :: Tree a -> [a]
post Tip          = []
post (Node l a r) = post l ++ post r ++ [a]
\end{code}

Prove that pre (mirrorTree t) = reverse (post t).
\begin{verbatim}
TODO
\end{verbatim}

\item Question 10:

Creating a new data type called Rose, which represents a rose tree.
\begin{code}
data Rose a = Rose a [Rose a] deriving Show
\end{code}

Creating a new data type called Fork, which represents a leaf/branch tree.
\begin{code}
data Fork a = Leaf a | Branch (Fork a) (Fork a) deriving Show
\end{code}

The to' function given a Binary tree converts it to a list of Rose trees.
\begin{code}
to' :: Tree a -> [Rose a]
to' Tip                        = []
to' (Node Tip a Tip)           = [Rose a []]
to' (Node (Node t1 a t2) b t3) = [Rose b ([Rose a (to' t1)] ++ (to' t2))] ++ to' t3
to' (Node t1 a (Node t2 b t3)) = [Rose a (to' t1)] ++ ([Rose b (to' t2)] ++ to' t3)
\end{code}

The from' function given a list of Rose trees converts it to a Binary tree.
\begin{code}
from' :: [Rose a] -> Tree a
from' []                             = Tip
from' [Rose a []]                    = Node Tip a Tip
from' ((Rose b ((Rose a t1):t2)):t3) = Node (Node (from' t1) a (from' t2)) b (from' t3)
from' ((Rose a t1):((Rose b t2):t3)) = Node (from' t1) a (Node (from' t2) b (from' t3))
\end{code}

The to function given a Rose tree converts it to a Fork tree (Leaf tree).
\begin{code}
to :: Rose a -> Fork a
to (Rose a [])     = Leaf a
to (Rose a (x:xs)) = brancher (Branch (Leaf a) (to x)) xs
    where 
        brancher fork []     = fork
        brancher fork (x:xs) = brancher (Branch fork (to x)) xs

\end{code}

The from function given a Fork tree (Leaf tree) converts it to a Rose tree.
\begin{code}
from :: Fork a -> Rose a
from (Leaf a) = Rose a []
from fork     = debrancher fork []
    where 
        debrancher (Branch (Leaf x) y) roses = Rose x ([from y] ++ roses)
        debrancher (Branch x (Leaf y)) roses = debrancher x [Rose y []]
        debrancher (Branch (Branch w x) (Branch y z)) roses = debrancher (Branch w x) [debrancher (Branch y z) roses]
\end{code}    


Bonus (Proof):
Prove that ∀xs. to (from xs) = xs.
\begin{verbatim}
TODO
\end{verbatim}

\end{enumerate}