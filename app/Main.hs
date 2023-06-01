module Main where

import Control.Monad (join)
import Data.Char (chr, digitToInt)
import qualified Data.Heap as H
import qualified Data.Map as M

main :: IO ()
main = do
  contents <- readFile "./test1.txt"
  let freqTable = frequency contents
      heap = charHeap freqTable
      tree = H.minimum (processHeap heap)
      encodingTable = buildEncodingTable "" tree
      encoding = encode encodingTable contents
      compressed = compress encoding
      encodedTree = encodeTree tree
      decodedTree = parseHuffmanTree encodedTree
  print decodedTree

type NodeData = H.Entry Int String

data HuffmanNode = Leaf NodeData | Node HuffmanNode NodeData HuffmanNode deriving (Show, Ord, Eq)

frequency :: String -> M.Map String Int
frequency = foldl (\m c -> M.insertWith (+) [c] 1 m) M.empty

charHeap :: M.Map String Int -> H.Heap HuffmanNode
charHeap m = H.fromList (map (\entry -> Leaf (H.Entry (snd entry) (fst entry))) (M.toList m))

combineNodes :: HuffmanNode -> HuffmanNode -> HuffmanNode
combineNodes (Leaf (H.Entry p1 v1)) (Leaf (H.Entry p2 v2)) = Node (Leaf (H.Entry p1 v1)) (H.Entry (p1 + p2) (v1 ++ v2)) (Leaf (H.Entry p2 v2))
combineNodes (Node l1 (H.Entry p1 v1) r1) (Node l2 (H.Entry p2 v2) r2) = Node (Node l1 (H.Entry p1 v1) r1) (H.Entry (p1 + p2) (v1 ++ v2)) (Node l2 (H.Entry p2 v2) r2)
combineNodes (Leaf (H.Entry p1 v1)) (Node l (H.Entry p2 v2) r) = Node (Leaf (H.Entry p1 v1)) (H.Entry (p1 + p2) (v1 ++ v2)) (Node l (H.Entry p2 v2) r)
combineNodes (Node l (H.Entry p1 v1) r) (Leaf (H.Entry p2 v2)) = Node (Node l (H.Entry p1 v1) r) (H.Entry (p1 + p2) (v1 ++ v2)) (Leaf (H.Entry p2 v2))

combineMins :: H.Heap HuffmanNode -> H.Heap HuffmanNode
combineMins h = H.insert newNode h''
  where
    newNode = combineNodes min min2
    h'' = H.deleteMin h'
    min2 = H.minimum h'
    h' = H.deleteMin h
    min = H.minimum h

processHeap :: H.Heap HuffmanNode -> H.Heap HuffmanNode
processHeap h
  | H.size h == 1 = h
  | otherwise = processHeap (combineMins h)

buildEncodingTable :: String -> HuffmanNode -> M.Map Char String
buildEncodingTable cs (Leaf (H.Entry _ (v : vs))) = M.singleton v cs
buildEncodingTable cs (Node l _ r) = M.union (buildEncodingTable (cs ++ "0") l) (buildEncodingTable (cs ++ "1") r)

getEncoding :: Char -> M.Map Char String -> String
getEncoding c encodingTable = case M.lookup c encodingTable of
  (Just encoding) -> encoding
  Nothing -> ""

encode :: M.Map Char String -> String -> String
encode encodingTable cs = join (map (`getEncoding` encodingTable) cs)

binStrToInt :: String -> Int
binStrToInt [] = 0
binStrToInt (c : cs) = (2 ^ length cs) * digitToInt c + binStrToInt cs

compress :: String -> String
compress cs
  | length cs < 7 = [chr $ binStrToInt cs]
  | otherwise = chr (binStrToInt $ take 7 cs) : compress (drop 7 cs)

encodeNode :: HuffmanNode -> String
encodeNode (Leaf (H.Entry p v)) = show p ++ "\2" ++ v ++ "\3"
encodeNode (Node l (H.Entry p v) r) = show p ++ "\2" ++ v ++ "\3" ++ encodeNode l ++ encodeNode r

encodeTree :: HuffmanNode -> String
encodeTree n = encodeNode n ++ "\4"

split :: (Char -> Bool) -> String -> [String]
split p s = case dropWhile p s of
  "" -> []
  s' -> w : split p s''
    where
      (w, s'') = break p s'

snap :: (Char -> Bool) -> String -> (String, String)
snap f s = (takeWhile (not . f) s, tail $ dropWhile (not . f) s)

processNodeData :: H.Entry Int String -> [HuffmanNode] -> [HuffmanNode]
processNodeData (H.Entry p v) nodes
  | length v == 1 = Leaf (H.Entry p v) : nodes
  | otherwise = Node (head nodes) (H.Entry p v) (nodes !! 1) : drop 2 nodes

parseHuffmanTree :: String -> HuffmanNode
parseHuffmanTree s = head $ foldr processNodeData [] nodeData
  where
    nodeData = map (\nd -> H.Entry (read $ fst nd :: Int) (snd nd)) nodeComponents
    nodeComponents = map (snap (=='\2')) nodeStrs
    nodeStrs = split (== '\3') treeStr
    treeStr = takeWhile (/= '\4') s
