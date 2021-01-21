\begin{code}
main :: IO ()
main = return ()
module A1_salehh6_tests where
\end{code}

\begin{code}
testConsume1_1 = consume1 (\x -> x + 1) (\x -> x * x) (\x y -> x * y) (OrBoth 2 8)
testConsume1_2 = consume2 (+1) (\x -> x * x) (*) (OrBoth 2 2)

testFlattenTernary1 = flattenTernary (TNode (TLeaf 1) (TLeaf 2) (TLeaf 3))
testFlattenTernary2 = flattenTernary (TNode (TNode (TLeaf 1) (TLeaf 2) (TLeaf 3)) (TLeaf 4) (TLeaf 5))

testMystery1 = mystery (\(x, y) -> x * y) [10,20,30] [3,2,0]
testMystery2 = mystery (\(x, y) -> x + y) [1,2,3] [3,2,1]

testMirrorTree1 = pre (Node (Node (Node Tip 4 Tip) 2 (Node Tip 5 Tip)) 1 (Node Tip 3 Tip)) == [1,2,4,5,3]
testMirrorTree2 = post (Node (Node (Node Tip 4 Tip) 2 (Node Tip 5 Tip)) 1 (Node Tip 3 Tip)) == [4,5,2,3,1]
testMirrorTree3 = reverse (post (Node (Node (Node Tip 4 Tip) 2 (Node Tip 5 Tip)) 1 (Node Tip 3 Tip))) == pre (mirrorTree (Node (Node (Node Tip 4 Tip) 2 (Node Tip 5 Tip)) 1 (Node Tip 3 Tip)))

tl = Node (Node Tip 2 Tip) 1 Tip
tr = Node Tip 1 (Node Tip 2 Tip)

rl = [Rose 1 [Rose 2 []]]
rr = [Rose 1 [],Rose 2 []]

testTo1 = to (Rose 0 []) == Leaf 0
testTo2 = to (Rose 0 [Rose 1 []]) == Branch (Leaf 0) (Leaf 1)
testTo3 = to (Rose 0 [Rose 1 [], Rose 2 []]) == Branch (Branch (Leaf 0) (Leaf 1)) (Leaf 2)
testTo4 = to (Rose 0 [Rose 1 [Rose 2 []]]) == Branch (Leaf 0) (Branch (Leaf 1) (Leaf 2))
testTo5 = to (Rose 0.0 [Rose 1.0 [Rose 1.5 []], Rose 2.0 [Rose 2.5 []]]) 
        == Branch (Branch (Leaf 0.0) (Branch (Leaf 1.0) (Leaf 1.5))) (Branch (Leaf 2.0) (Leaf 2.5))
testTo6 = to (Rose 0 [Rose 1 [Rose 1.5 [Rose 1.75 [], Rose 1.80 []]], Rose 2 [Rose 2.5 [], Rose 3.0 []]])
        == Branch (Branch (Leaf 0.0) (Branch (Leaf 1.0) (Branch (Branch (Leaf 1.5) (Leaf 1.75)) (Leaf 1.8)))) (Branch (Branch (Leaf 2.0) (Leaf 2.5)) (Leaf 3.0))


testFrom1 = from (Leaf 0) == Rose 0 []
testFrom2 = from (Branch (Leaf 0) (Leaf 1)) == Rose 0 [Rose 1 []]
testFrom3 = from (Branch (Branch (Leaf 0) (Leaf 1)) (Leaf 2)) == Rose 0 [Rose 1 [], Rose 2 []]
testFrom4 = from (Branch (Leaf 0) (Branch (Leaf 1) (Leaf 2))) == Rose 0 [Rose 1 [Rose 2 []]]
testFrom5 = from (Branch (Branch (Leaf 0.0) (Branch (Leaf 1.0) (Leaf 1.5))) (Branch (Leaf 2.0) (Leaf 2.5)))
          == Rose 0.0 [Rose 1.0 [Rose 1.5 []],Rose 2.0 [Rose 2.5 []]]
testFrom6 = from (Branch (Branch (Leaf 0.0) (Branch (Leaf 1.0) (Branch (Branch (Leaf 1.5) (Leaf 1.75)) (Leaf 1.8)))) (Branch (Branch (Leaf 2.0) (Leaf 2.5)) (Leaf 3.0)))
          == Rose 0.0 [Rose 1.0 [Rose 1.5 [Rose 1.75 [],Rose 1.8 []]],Rose 2.0 [Rose 2.5 [],Rose 3.0 []]]
\end{code}

