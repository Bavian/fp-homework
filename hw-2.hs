import Data.Maybe
import Data.List

-- list of adjacent vertexes
type Vertex = (Int, [Int])
type Graph = [Vertex]

nullVertex = (-1, [])

-- CORRECTNESS CHECK

-- checks that vertex number is equal to given number
isItVertexNumber :: Int -> Vertex -> Bool
isItVertexNumber number vertex = (==) number (fst vertex)

-- checks that vertex 'from' have pair edge from vertex 'to'
havePairEdge :: Graph -> Int -> Int -> Bool
havePairEdge graph from to = elem from (snd (fromMaybe nullVertex (find (isItVertexNumber to) graph)))

-- checks that every edge of the given vertex have pair
isAllEdgesHavePair :: Graph -> Vertex -> Bool
isAllEdgesHavePair graph vertex = all (havePairEdge graph (fst vertex)) (snd vertex)

-- checks that undirected graph is correct
isGraphCorrect :: Graph -> Bool
isGraphCorrect graph = all (isAllEdgesHavePair graph) graph

-- SINGLE COMPONENT CHECK

-- Concatinate integer lists exclude duplicates from second list
concatWithoutDuplicates :: [Int] -> [Int] -> [Int]
concatWithoutDuplicates list1 list2 = (++) list1 (filter (\x -> notElem x list1) list2)

-- Calculate verticies of not visited numbers in the component
bfs :: Graph -> [Int] -> Int -> Int
bfs graph queue current = if current == (length queue)
  then current
  else (bfs
    graph
    (concatWithoutDuplicates queue (snd (fromMaybe nullVertex (find (isItVertexNumber (queue!!current)) graph))))
    (current + 1)
  )

-- Checks that graph have single component
isComponentSingle :: Graph -> Bool
isComponentSingle graph = (==) (bfs graph [1] 0) (length graph)

-- TESTS

assert :: Bool -> [Char]
assert False = "Fail"
assert True = "OK"

singleComponentGraph = [ (1, [2]), (2, [3, 4, 1]), (3, [4, 2]), (4, [2, 3]) ]
multipleComponentsGraph = [ (1, []), (2, []), (3, [4, 5]), (4, [3, 5]), (5, [4, 3]), (6, [6]), (7, [8]), (8, [7]) ]
singleElementCycleGraph = [(1, [1])]
singleElementGraph = [(1, [])]
emptyEdgesGraph = [(1, []), (2, [])]
oneEdgeWithoutPairGraph = [(1, [2]), (2, [3, 4]), (3, [4, 2]), (4, [2, 3]) ]
edgesWithoutPairsGraph = [ (1, [2, 3, 4]), (2, [5]), (3, [2]), (4, [3]),(5, [1, 4, 3])]


main = do
  print "---------------------------------------"
  print "Correctness check"
  print "---------------------------------------"
  print ((++) "single commponent: " (assert (isGraphCorrect singleComponentGraph)))
  print ((++) "multiple commponents: " (assert (isGraphCorrect multipleComponentsGraph)))
  print ((++) "single element cycle: " (assert (isGraphCorrect singleElementCycleGraph)))
  print ((++) "single element: " (assert (isGraphCorrect singleElementGraph)))
  print ((++) "empty edges: " (assert (isGraphCorrect emptyEdgesGraph)))
  print ((++) "one egde without pair: " (assert (not (isGraphCorrect oneEdgeWithoutPairGraph))))
  print ((++) "edges without pair: " (assert (not (isGraphCorrect edgesWithoutPairsGraph))))
  print "---------------------------------------"
  print "Single component check"
  print "---------------------------------------"
  print ((++) "single component: " (assert (isComponentSingle singleComponentGraph)))
  print ((++) "multiple component: " (assert (not (isComponentSingle multipleComponentsGraph))))
  print ((++) "single element cycle: " (assert (isComponentSingle singleElementCycleGraph)))
  print ((++) "single element: " (assert (isComponentSingle singleElementGraph)))
  print ((++) "empty edges: " (assert (not (isComponentSingle emptyEdgesGraph))))
  print "---------------------------------------"
