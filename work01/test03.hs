data MyNode = MyNode { itemId :: Int,
                       itemName :: String,
                       children :: [Int]
                     } deriving Show

n1 = MyNode { itemId=1, itemName="item01", children=[1]}

n2 = MyNode { itemId=2, itemName="item02", children=[]}

  
