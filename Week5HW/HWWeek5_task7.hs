main :: IO()
main = do
    print $ (repeater "I love Haskell") 3 " " == "I love Haskell I love Haskell I love Haskell"
    print $ (repeater "Quack") 5 "!" == "Quack!Quack!Quack!Quack!Quack"

repeater ::  String -> (Int -> String -> String)
repeater str = (\ x y -> (concat $ replicate (x - 1) (str ++ y)) ++ str)