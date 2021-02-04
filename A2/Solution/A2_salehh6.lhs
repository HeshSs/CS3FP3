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
\begin{code}
iter :: Integer -> (a -> a) -> (a -> a)
iter n f
    | n > 0     = f . iter (n-1) f  -- iter .1
    | otherwise = id                -- iter .2
\end{code}

We prove that for all natural numbers n, 
>iter n id = id

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
    &= \text{id $\circ$ iter $((n + 1) - 1)$ id} & \pnote{Arithmetic}\\
    &= \text{id $\circ$ iter n id} & \pnote{By inductive hypothesis P(n)}\\
    &= \text{id $\circ$ id} & \pnote{By definition of id}\\
    &= \text{id}
\end{align*}
So $P(n+1)$ holds.

Therefore, P(n) holds for all $n \in \mathbb{N}$ by weak induction.
\end{proof}

\item We have:
\begin{code}
map :: (a -> b) -> [a] -> [b]  
map _ [] = []                   -- map .1
map f (x:xs) = f x : map f xs   -- map .2
\end{code}

We will prove that for all ys and zs,
>map f (ys ++ zs) = map f ys ++ map f zs

\begin{proof}
Let P(ys) = map f (ys ++ zs) = map f ys ++ map f zs. We will prove P(ys) for all ys and zs by weak induction.

Base case: $n = []$. We must show P([]):
\begin{align*}
    &\phantom{{}=} \text{map f ([] ++ zs)} & \pnote{By identity of ++}\\
    &= \text{map f zs} & \pnote{By identity of ++}\\
    &= \text{[] ++ map f zs} & \pnote{By map .1}\\
    &= \text{map f [] ++ map f zs} 
\end{align*}
So $P(0)$ holds.

Induction step: $n = (y:ys)$. We assume P(ys) and we must show $P(y:ys)$:
\begin{align*}
    &\phantom{{}=} \text{map f ((y:ys) ++ zs)} & \pnote{By Mutual associativity of : and ++}\\
    &= \text{map f (y:(ys ++ zs))} & \pnote{By map .2}\\
    &= \text{f y : map f (ys ++ zs)} & \pnote{By induction hypothesis P(ys)}\\
    &= \text{f y : map f ys ++ map f zs} & \pnote{By map .2}\\
    &= \text{map f (y:ys) ++ map f zs}
\end{align*}
So $P(n+1)$ holds.

Therefore, P(ys) holds for all ys and zs by weak induction.
\end{proof}

\item

\item

\item

\item

\item

\item

\end{enumerate}

\end{document}