

data Heap x = Empty
              | Node x (Heap x) (Heap x)  
                deriving (Show,Eq)
insert :: (Ord a) => Heap a -> a -> Heap a
insert (Empty) val = Node val Empty Empty
insert (Node val left right) x =
  if x > val
     then do let newHeap = updateRoot (Node val left right) x
             insertion newHeap (Node val Empty Empty)
     else insertion (Node val left right) (Node x Empty Empty)        
          
insertion :: Ord a => Heap a -> Heap a -> Heap a
insertion Empty heap = heap
insertion heap Empty = heap
insertion (Node val1 left1 right1) (Node val2 left2 right2) 
  | val2 < val1 = Node val1 (insertion (Node val2 left2 right2) right1) left1
  | otherwise = Node val2 (insertion (Node val1 left1 right1) right2) left2 
    
updateRoot :: Ord a => Heap a -> a -> Heap a
updateRoot (Node val Empty Empty) newVal = Node newVal Empty Empty
updateRoot (Node val Empty right) newVal 
  | newVal > (getMax right) = (Node newVal Empty right)
  | otherwise = Node (getMax right) Empty (updateRoot right newVal)                            
updateRoot (Node val left Empty) newVal                 
  | newVal > (getMax left) = (Node newVal left Empty)
  | otherwise = Node (getMax left) (updateRoot left newVal) Empty                           
updateRoot (Node val left right) newVal 
  | newVal > (getMax left) && newVal > (getMax right) = (Node newVal left right)
  | (getMax left) > (getMax right) = Node (getMax left) (updateRoot left newVal) right
  | otherwise = Node (getMax right) left (updateRoot right newVal)                           


getMax :: (Ord a) => Heap a -> a
getMax (Node val left right) = val

deleteMax :: (Ord a) => Heap a -> Heap a
deleteMax (Node val Empty Empty) = Empty
deleteMax (Node val Empty right) = right
deleteMax (Node val left Empty) = left
deleteMax (Node val left right) 
  | getMax left > getMax right = (Node (getMax left) (deleteMax left) right) 
  | otherwise = (Node (getMax right) left (deleteMax right))                                
                

testDeleteMax = deleteMax (Node 2 Empty Empty) == Empty && deleteMax (Node 4 Empty (Node 3 Empty Empty))== (Node 3 Empty Empty) && deleteMax (Node 4 (Node 3 (Node 1 Empty Empty) Empty) (Node 2 Empty Empty)) == (Node 3 (Node 1 Empty Empty) (Node 2 Empty Empty))                  

testInsert = insert (Node 5 (Node 3 Empty Empty) Empty) 4 == Node 5 (Node 4 Empty Empty) (Node 3 Empty Empty) && insert Empty 4 == Node 4 Empty Empty && insert (Node 5 (Node 3 Empty Empty) (Node 2 Empty Empty)) 6 == (Node 6 (Node 5 (Node 2 Empty Empty) Empty) (Node 3 Empty Empty))

testUpdateRoot = updateRoot (Node 2 Empty Empty) 3 == Node 3 Empty Empty && updateRoot (Node 5 (Node 4 Empty Empty) (Node 3 Empty Empty)) 6 == Node 6 (Node 4 Empty Empty) (Node 3 Empty Empty) && updateRoot (Node 7 (Node 5 (Node 3 Empty Empty) (Node 4 Empty Empty)) (Node 1 Empty Empty)) 2 == (Node 5 (Node 4 (Node 3 Empty Empty) (Node 2 Empty Empty)) (Node 1 Empty Empty)) && updateRoot (Node 7 (Node 5 (Node 3 Empty Empty) (Node 4 Empty Empty)) Empty) 2 == Node 5 (Node 4 (Node 3 Empty Empty) (Node 2 Empty Empty)) Empty

test = testDeleteMax && testInsert && testUpdateRoot

--Extra Credit
heapsort :: (Ord a) => [a] -> [a]
heapsort list = sortedList where
  heap = toHeap list Empty
  sortedList = sort heap 
  
sort :: (Ord a) => Heap a -> [a]  
sort Empty = []
sort heap = (++) (sort (deleteMax heap)) ((getMax heap) : [])   
                                
toHeap :: (Ord a) => [a] -> Heap a -> Heap a          
toHeap [] heap = heap
toHeap (x:xs) heap = toHeap xs (insert heap x)          

testHeapsort = heapsort [3,2,4] == [2,3,4] && heapsort [1,2,3] == [1,2,3] && heapsort [1] == [1] && heapsort [2,5,5,6,1,4,3] == [1,2,3,4,5,5,6]