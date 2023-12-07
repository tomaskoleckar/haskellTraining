data Component = TextBox {name :: String, text :: String}
               | Button {name :: String, value :: String}
               | Container {name :: String, children :: [Component]} deriving Show 

gui :: Component
gui = Container "My App" [
    Container "Menu" [
        Button "btn_new" "New",
        Button "btn_open" "Open",
        Button "btn_close" "Close"],
    Container "Body" [TextBox "textbox_1" "Some text goes here"],
    Container "Footer" []]


countAllComponents :: Component -> Int
countAllComponents (TextBox _ _) = 1 
countAllComponents (Button _ _) = 1
countAllComponents (Container _ children) = 1 + sum (map countAllComponents children)


removeEmptyContainers :: Component -> Component
removeEmptyContainers(TextBox name value) = TextBox name value
removeEmptyContainers(Button name value) = Button name value
removeEmptyContainers (Container name children) = 
    if not (isConEmpty (Container name children)) 
    then Container name (filter (not . isConEmpty) (map removeEmptyContainers children))
    else Container name []

isConEmpty :: Component -> Bool
isConEmpty (Container _ []) = True
isConEmpty _ = False



--Přidá kopii buttonu do kontejneru
copyElement :: Component -> String -> String -> Component
copyElement (Container name children) buttonName value
  | any (`hasButton` buttonName) children = 
      Container name (addNewButton buttonName value children)  
  | otherwise =
      Container name (map (\c -> copyElement c buttonName value) children)  
copyElement x _ _ = x 


hasButton :: Component -> String -> Bool
hasButton (Button name _) btn = name == btn  
hasButton _ _ = False  



addNewButton :: String -> String -> [Component] -> [Component]
addNewButton btn value [] = [Button (btn ++ "_copy") value]  
addNewButton btn value (c : cs) =
  if hasButton c btn
    then c : Button (btn ++ "_copy") value : cs  
    else c : addNewButton btn value cs  



--TernaryTree
data TernaryTree a
  = Leaf a
  | Node (TernaryTree a) (TernaryTree a) (TernaryTree a)
  deriving (Show)

example :: TernaryTree Int
example = Node (Node (Leaf 1) (Leaf 2) (Leaf 3)) (Leaf 4) (Node (Leaf 5) (Leaf 6) (Leaf 7))



countEmptyContainers :: Component -> Int
countEmptyContainers (TextBox _ _) = 0 
countEmptyContainers (Button _ _) = 0 
countEmptyContainers (Container _ []) = 1
countEmptyContainers (Container _ children) = sum (map countEmptyContainers children)


removeButton :: Component -> String -> Component
removeButton (TextBox name text) _ = TextBox name text
removeButton button@(Button name _) targetName
  | name == targetName = Container "" []
  | otherwise = button
removeButton (Container name children) targetName =
  Container name (map (`removeButton` targetName) (filter (not . isTargetButton targetName) children))



isTargetButton :: String -> Component -> Bool
isTargetButton targetName (Button name _) = name == targetName 
isTargetButton _ _ = False


listButtonNames :: Component -> [String]
listButtonNames (Button name _) = [name]
listButtonNames (TextBox _ _) = []
listButtonNames (Container _ children) = concatMap listButtonNames children



changeText :: Component -> String -> String -> Component
changeText (TextBox name t) str1 str2
  | name == str1 = TextBox name str2
  | otherwise = TextBox name t
changeText (Container name children) str1 str2 = Container name [changeText c str1 str2 | c <- children]
changeText x _ _ = x


listAllButtons ::Component -> [Component]
listAllButtons (Button name value) = [(Button name value)]
listAllButtons (TextBox name value) = []
listAllButtons (Container _ children) = concatMap (listAllButtons) (children)

isButton::Component -> Bool
isButton (Button _ _) = True
isButton _ = False

removeAllButtons :: Component -> Component 
removeAllButtons (Button name value) = Button name value
removeAllButtons (TextBox name value) = TextBox name value
removeAllButtons (Container name children) = Container name (filter (\x->not(isButton x)) (map (removeAllButtons) children))


listAllNames :: Component -> [String] 
listAllNames (TextBox name _) = [name]
listAllNames (Button name _) = [name]
listAllNames (Container name children ) = name : concatMap listAllNames children


isTarget' :: Component -> [String] -> Bool 
isTarget' (Container name _) x = if elem name x then True else False 
isTarget' (TextBox name _) x = if elem name x then True else False
isTarget' (Button name _) x = if elem name x then True else False

removeAllElements :: Component -> [String] -> Component
removeAllElements gui [] = gui 
removeAllElements (TextBox name value) target
    | elem name target = TextBox name value
    | otherwise = TextBox name value 
removeAllElements (Button name value) target 
    | elem name target = Button name value
    | otherwise = Button name value
removeAllElements (Container name children) target = 
    Container name (filter (\x -> not (elem (nameOf x) target)) (map (`removeAllElements` target) children)) 
    where 
        nameOf (TextBox n _) = n
        nameOf (Button n _) = n
        nameOf (Container n _) = n


printPaths :: Component -> String -> String
printPaths (Button name _) str
  | name == str = "/" ++ name 
  | otherwise = "" 
printPaths (TextBox name _) str
  | name == str = "/" ++ name 
  | otherwise = "" 
printPaths (Container name children) str
  | any (`containsComponent` str) children 
    =
      "/" ++ name ++ concat [printPaths c str | c <- children] 
  | otherwise = "" 


containsComponent :: Component -> String -> Bool
containsComponent (Button name _) porov = name == porov 
containsComponent (TextBox name _) porov = name == porov 
containsComponent (Container _ children) porov =

  any (`containsComponent` porov) children

removeComponentFromContainerAtIndex :: Component -> String -> Int -> Component
removeComponentFromContainerAtIndex (Container name children) containerName index
  | name == containerName = Container name (removeAtIndex children index) 
  | otherwise = Container name [removeComponentFromContainerAtIndex c containerName index | c <- children] 
removeComponentFromContainerAtIndex x _ _ = x 


removeAtIndex :: [a] -> Int -> [a]
removeAtIndex [] _ = [] 
removeAtIndex (x : xs) n
  | n == 0 = xs 
  | otherwise = x : removeAtIndex xs (n - 1)