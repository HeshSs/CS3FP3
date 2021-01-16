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

Question 2:

function description
\begin{code}
--applyAll :: [a -> b] -> [a] -> [b]
--applyAll (x : xs) = 

\end{code}