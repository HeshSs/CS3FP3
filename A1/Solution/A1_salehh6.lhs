Assignment 1 - Compsci 3FA3 - Hishmat Salehi - Salehh6
\begin{code}
main :: IO ()
main = return ()
\end{code}

Question 1:

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
pos e l = [i | (i, x) <- zip [0..] l, x == e]
\end{code}

Which of the 3 functions, 
1. Which do very related things?
The matches and pos function do almost the same thing, because one gets all the occurences of an element and the other
gets all the indices of that element.

2. Which is 'best'?
I think the pos function is the best, because it's implementation needs either a helper function or a list comprehension
with zip function because we gotta keep track of the indices. Also, the pos function gives you the index of where the element exists
, but the matches function just repeats the same element over and over again.

Question 2:

The applyAll function returns a list resulted from the cross-product of applying all the functions in the 1st list (1st argument) 
to the 2nd list (2nd argument).
\begin{code}
applyAll :: [a -> b] -> [a] -> [b]
applyAll _ [] = []
applyAll [] _ = []
applyAll (f:fs) l = map f l ++ applyAll fs l
\end{code}

Question 3:

The tripleNeg1 function triples every negative number in a list, recursively.
\begin{code}
tripleNeg1 :: (Ord a, Num a) => [a] -> [a]
tripleNeg1 [] = []
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

Question 4:

Creating a new data type called OrBoth that contains elements of type a, b or both.
\begin{code}
data OrBoth a b = A a | B b | OrBoth a b
\end{code}

The consume1 function applies the given function to a given OrBoth data type. 
\begin{code}
consume1 :: (a -> c) -> (b -> c) -> (a -> b -> c) -> OrBoth a b -> c
consume1 fa fb fboth (OrBoth a b) = fboth a b
consume1 fa fb fboth (A a) = fa a
consume1 fa fb fboth (B b) = fb b
\end{code}

The consume2 function applies the given function to a given OrBoth data type, but when called for both a and b, 
it applies the first 2 functions on the given inputs then applies the 3rd function on the result taken from their outputs.
\begin{code}
consume2 :: (a -> c) -> (b -> c) -> (c -> c -> c) -> OrBoth a b -> c
consume2 fa fb fboth (OrBoth a b) = fboth (fa a) (fb b)
consume2 fa fb fboth (A a) = fa a
consume2 fa fb fboth (B b) = fb b
\end{code}

Which of those two ways is "better" in your opinion? Why?
I think the consume1 function is better, because you can create the functionality of consume2 all from consume1. 
i.e. You can call consume1 in itself, but for consume2 you can't create the functionality of consume1.

Question 5:

function description:
\begin{code}

\end{code}

Question 6:

function description:
\begin{code}

\end{code}

Question 7:

function description:
\begin{code}

\end{code}

Question 8:

function description:
\begin{code}

\end{code}

Question 9:

function description:
\begin{code}

\end{code}

Question 10:

function description:
\begin{code}

\end{code}