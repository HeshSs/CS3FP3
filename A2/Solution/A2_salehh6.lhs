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
    &= \text{id $\circ$ id} & \pnote{By identity of id}\\
    &= \text{id}
\end{align*}
So $P(n+1)$ holds.

Therefore, P(n) holds for all $n \in \mathbb{N}$ by weak induction.
\end{proof}

\item

\item

\item

\item

\item

\item

\item

\end{enumerate}

\end{document}