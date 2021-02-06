\documentclass{article}
%include polycode.fmt
%include lhs2TeX.fmt

\usepackage{fullpage}
\usepackage{enumerate}
\usepackage{amsthm}
\usepackage{amsmath}
\usepackage{amssymb}

\newcommand{\pnote}[1]{{\langle \text{#1} \rangle}}

\title{Compsci 3FP3 - Assignment 2}
\date{\today}
\author{Hishmat Salehi}

\begin{document}
\maketitle

\begin{code}
module A2_salehh6 where
\end{code}

\begin{enumerate}[Q1)]
\item We have:
\begin{verbatim}
iter :: Integer -> (a -> a) -> (a -> a)         
iter n f                                        
    | n > 0     = f . iter (n-1) f  -- iter .1  
    | otherwise = id                -- iter .2  
\end{verbatim}

We will prove that for all natural numbers n, 

\textbf{iter n id = id}

\begin{proof}
Let P(n) = iter n id = id. We will prove P(n) for all $n \in \mathbb{N}$ by weak induction.

Base case: $n = 0$. We must show P(0):
\begin{align*}
    &\phantom{{}=} \text{iter 0 id} & \pnote{By iter .2}\\
    &= \text{id}
\end{align*}
So $P(0)$ holds.

Induction step: $n > 0$. We assume P(n) and we must show $P(n + 1)$:
\begin{align*}
    &\phantom{{}=} \text{iter $(n + 1)$ id} & \pnote{By iter .1}\\
    &= \text{id . iter $((n + 1) - 1)$ id} & \pnote{Arithmetic}\\
    &= \text{id . iter n id} & \pnote{By inductive hypothesis P(n)}\\
    &= \text{id . id} & \pnote{By definition of id}\\
    &= \text{id}
\end{align*}
So $P(n+1)$ holds.

Therefore, P(n) holds for all $n \in \mathbb{N}$ by weak induction.
\end{proof}

\item We have:
\begin{verbatim}
map :: (a -> b) -> [a] -> [b]               
map _ [] = []                   -- map .1   
map f (x:xs) = f x : map f xs   -- map .2   
                                            
(++) :: [a] -> [a] -> [a]                   
(++) []     ys = ys             -- (++) .1  
(++) (x:xs) ys = x : xs ++ ys   -- (++) .2  
\end{verbatim}

We will prove that for all ys and zs, 

\textbf{map f (ys ++ zs) = map f ys ++ map f zs}

\begin{proof}
Let P(ys) = map f (ys ++ zs) = map f ys ++ map f zs. We will prove P(ys) for all ys and zs by weak induction.

Base case: $ys = []$. We must show P([]):
\begin{align*}
    &\phantom{{}=} \text{map f ([] ++ zs)} & \pnote{By identity of ++}\\
    &= \text{map f zs} & \pnote{By identity of ++}\\
    &= \text{[] ++ map f zs} & \pnote{By map .1}\\
    &= \text{map f [] ++ map f zs} 
\end{align*}
So $P([])$ holds.

Induction step: $ys = (y:ys)$. We assume P(ys) and we must show $P(y:ys)$:
\begin{align*}
    &\phantom{{}=} \text{map f ((y:ys) ++ zs)} & \pnote{By (++) .2}\\
    &= \text{map f (y : (ys ++ zs))} & \pnote{By map .2}\\
    &= \text{f y : map f (ys ++ zs)} & \pnote{By induction hypothesis P(ys)}\\
    &= \text{f y : map f ys ++ map f zs} & \pnote{By map .2}\\
    &= \text{map f (y:ys) ++ map f zs}
\end{align*}
So $P(y:ys)$ holds.

Therefore, P(ys) holds for all ys and zs by weak induction.
\end{proof}

\item We have:
\begin{verbatim}
concat :: [[a]] -> [a]                                      
concat = foldr (++) []  -- concat .1                        
                                                            
foldr :: (a -> b -> b) -> b -> [a] -> b                     
foldr f s []     = s                        -- foldr .1     
foldr f s (x:xs) = x `f` (foldr f s xs)     -- foldr .2     
                                                            
map and (++) are also defined in Q2.                        
\end{verbatim}

We will prove that for all finite lists xs and functions f, 

\textbf{concat (map (map f) xs) = map f (concat xs)}

\begin{proof}
Let P(xs) = concat (map (map f) xs) = map f (concat xs). We will prove P(xs) for all finite lists xs and functions f by weak induction.

Base case: $xs = []$. We must show P([]):
\begin{align*}
    &\phantom{{}=} \text{concat (map (map f) [])} & \pnote{By map .1}\\
    &= \text{concat []} & \pnote{By concat .1}\\
    &= \text{foldr (++) [] []} & \pnote{By foldr .1}\\
    &= \text{[]} & \pnote{By map .1}\\
    &= \text{map f []} & \pnote{By foldr .1}\\
    &= \text{map f (foldr (++) [] [])} & \pnote{By concat .1}\\
    &= \text{map f (concat [])} 
\end{align*}
So $P([])$ holds.

Induction step: $xs = (x:xs)$. We assume P(xs) and we must show $P(x:xs)$:
\begin{align*}
    &\phantom{{}=} \text{concat (map (map f) (x:xs))} & \pnote{By concat .1}\\
    &= \text{foldr (++) [] (map (map f) (x:xs))} & \pnote{By map .2}\\
    &= \text{foldr (++) [] (map f x : (map (map f) xs))} & \pnote{By foldr .2}\\
    &= \text{(map f x) ++ (foldr (++) [] (map (map f) xs))} & \pnote{By concat .1}\\
    &= \text{(map f x) ++ (concat (map (map f) xs))} & \pnote{By Induction hypothesis P(xs)}\\
    &= \text{(map f x) ++ (map f (concat xs))} & \pnote{By map .2}\\
    &= \text{map f (x ++ (concat xs))} & \pnote{By concat .1}\\
    &= \text{map f (x ++ (foldr (++) [] xs))} & \pnote{By foldr .2}\\
    &= \text{map f (foldr (++) [] (x:xs))} & \pnote{By concat .1}\\
    &= \text{map f (concat (x:xs))}
\end{align*}
So $P(x:xs)$ holds.

Therefore, P(xs) holds for all finite lists xs and functions f by weak induction.
\end{proof}

\item
% TODO

\item
\begin{code}
data Expr =
      Lit Integer
    | Expr :+: Expr
    | Expr :-: Expr
    deriving (Show)

size :: Expr -> Integer
size (Lit a)     = 0
size (e1 :+: e2) = 1 + size e1 + size e2
size (e1 :-: e2) = 1 + size e1 + size e2

\end{code}

\item 
\begin{code}
data Expr' =
      Lit' Integer
    | Add' Expr' Expr'
    | Sub' Expr' Expr'
    | Mul' Expr' Expr'
    | Div' Expr' Expr'
    deriving (Show)

show' :: Expr' -> String
show' (Lit' a)   = "Lit' " ++ show a 
show' (Add' a b) = "Add' (" ++ show' a ++ ") (" ++ show' b ++ ")"
show' (Sub' a b) = "Sub' (" ++ show' a ++ ") (" ++ show' b ++ ")"
show' (Mul' a b) = "Mul' (" ++ show' a ++ ") (" ++ show' b ++ ")"
show' (Div' a b) = "Div' (" ++ show' a ++ ") (" ++ show' b ++ ")"

size' :: Expr' -> Integer
size' (Lit' a)   = 0
size' (Add' a b) = 1 + size' a + size' b
size' (Sub' a b) = 1 + size' a + size' b
size' (Mul' a b) = 1 + size' a + size' b
size' (Div' a b) = 1 + size' a + size' b

eval' :: Expr' -> Integer
eval' (Lit' a)   = a
eval' (Add' a b) = eval' a + eval' b
eval' (Sub' a b) = eval' a - eval' b
eval' (Mul' a b) = eval' a * eval' b
eval' (Div' a b) = div (eval' a) (eval' b)
\end{code}

What does your function do when asked to perform a division by zero?\\
Throws an exception with the message "division by zero"

\item
\begin{code}
data Expr2 =
     Lit2 Integer
    | Op Ops Expr2 Expr2
    deriving Show
data Ops = Add | Sub | Mul | Div
    deriving (Show, Eq)

show2 :: Expr2 -> String
show2 (Lit2 a)   = "Lit2 " ++ show a
show2 (Op a b c) = "Op " ++ show a ++ " (" ++ show2 b ++ ") (" ++ show2 c ++ ")"

size2 :: Expr2 -> Integer
size2 (Lit2 a)   = 0
size2 (Op a b c) = 1 + size2 b + size2 c

eval2 :: Expr2 -> Integer
eval2 (Lit2 a)   = a
eval2 (Op a b c) 
    | a == Add  = eval2 b + eval2 c
    | a == Sub  = eval2 b - eval2 c
    | a == Mul  = eval2 b * eval2 c
    | a == Div  = div (eval2 b) (eval2 c)
\end{code}

This implementation is the most ideal, because if we were to add an extra operator Mod, 
we only have to add an extra constructor in Ops, and only have to add a guard in eval2 method.
The rest is taken care of for us already.

\item
\begin{code}
join :: (a -> c) -> (b -> d) -> Either a b -> Either c d
join f1 _ (Left a) = Left (f1 a)
join _ f2 (Right a) = Right (f2 a)
\end{code}

\item
% TODO

\end{enumerate}

\end{document}