import Data.IORef

data NodeValue = NodeValue {
  intVal_ :: Int,
  sndVal_ :: Int
  } deriving Show


data Node = Node {
  ndValue_ :: (IORef NodeValue),
  ndNextSibling_ :: (IORef Node)}
  | EmptyNode

ndCreate :: NodeValue -> IO Node
ndCreate nv = do
  rnv <- newIORef $ nv
  ren <- newIORef EmptyNode
  return $ Node rnv ren

ndValue :: Node -> IO NodeValue
ndValue nd = do
  let rv = ndValue_ nd
  v <- readIORef rv
  return v

ndSetValue :: Node -> NodeValue -> IO ()
ndSetValue nd ndv = do
  let rv = ndValue_ nd
  writeIORef rv ndv
  return ()

isEmpty_ :: Node -> Bool
isEmpty_ EmptyNode = True
isEmpty_ _ = False

hasNext :: Node -> IO Bool
hasNext nd = do
  let rndNext = ndNextSibling_ nd
  ndNext <- readIORef rndNext
  return $ isEmpty_ ndNext

ndNext :: Node -> IO Node
ndNext nd = do
  let rndNext = ndNextSibling_ nd
  ndNext <- readIORef rndNext
  return ndNext

ndStrExp nd = do
  s <- strExpNodeValue2 0 nd
  return s

strExpNodeValue2 :: Int -> Node -> IO String
strExpNodeValue2 _ EmptyNode = do { return "" }
strExpNodeValue2 indent nd = do
  ndv <- ndValue nd
  let v = intVal_ ndv
  let se = (replicate (indent * 2) ' ') ++ "Node " ++ (show v) ++ "\n"
  ndNxt <- ndNext nd
  se2 <- strExpNodeValue2 indent ndNxt
  return $ se ++ se2
  
ndPutStrLn :: Node -> IO ()
ndPutStrLn nd = do
  se <- ndStrExp nd
  putStr se


setNext :: Node -> Node -> IO ()
setNext nd ndNext = do
  let rndNext = ndNextSibling_ nd
  writeIORef rndNext ndNext
  return ()

main = do 
  let nv1 = NodeValue 1 0
  print nv1
  nd1 <- ndCreate nv1
  nd2 <- ndCreate $ NodeValue 3 0
  setNext nd1 nd2
  putStrLn "Node 1 -next-> Node 3"
  ndPutStrLn nd1
  putStrLn "1st Node Value changed to 4."
  ndSetValue nd1 $ NodeValue 4 0
  ndPutStrLn nd1
  putStrLn "2nd Node Value changed to 5."
  ndSetValue nd2 $ NodeValue 5 0
  ndPutStrLn nd1
  putStrLn "add Node 6 next to Node 5."
  nd3 <- ndCreate $ NodeValue 6 0
  setNext nd2 nd3
  ndPutStrLn nd1
