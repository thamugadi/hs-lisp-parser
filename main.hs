data Lisp = Atom String | List [Lisp] deriving Show

parse1 :: [String] -> [Lisp] -> ([String], [Lisp])  
parse1 [] p = ([], p)
parse1 (")":xs) p = (xs,p)
parse1 ("(":xs) p = parse1 a  $ p++[List b] where (a,b) = parse1 xs []
parse1 (x:xs)   p = parse1 xs $ p++[Atom x]

parse :: [[String]] -> [Lisp]
parse a = map (\x -> last $ snd $ parse1 x []) a

lreq1 :: String -> Int -> Int -> Int -> [Int]
lreq1 "" _ _ _ = []
lreq1 (x:xs) i 0 0
  | x=='(' = lreq1 xs (i+1) 1 0
  | x==')' = lreq1 xs (i+1) 0 1
  | otherwise = lreq1 xs (i+1) 0 0
lreq1 (x:xs) i l r
  | l==r = i : lreq1 xs 0 0 0
  | l/=r && x=='(' = lreq1 xs (i+1) (l+1) r
  | l/=r && x==')' = lreq1 xs (i+1) l (r+1)
  | otherwise = lreq1 xs (i+1) l r

lreq :: String -> [Int]
lreq x = lreq1 x 0 0 0

divide1 :: String -> [Int] -> [String]
divide1 s [] = [s]
divide1 s (i:is) = fst spl : divide1 (snd spl) is where spl = splitAt i s

divide :: String -> [String]
divide s = divide1 s $ lreq s

quoteMac1 :: String -> Bool -> String
quoteMac1 "" _ = ""
quoteMac1 (x:xs) False
  | x=='\'' = "(quote " ++ quoteMac1 xs True
  | x/='\'' = x : quoteMac1 xs False
quoteMac1 (x:xs) True
  | x == ')' = "))" ++ quoteMac1 xs False
  | otherwise = x : quoteMac1 xs True

quoteMac :: String -> String
quoteMac x = quoteMac1 x False

replace1 :: Char -> String -> String -> String
replace1 a b "" = ""
replace1 a b (x:xs)
  | x==a = b ++ replace1 a b xs
  | x/=a = x :  replace1 a b xs

replace :: [(Char,String)] -> (String -> String)
replace [] = id
replace ((a,b):xs) = (\x -> replace1 a b x) . replace xs

tokens :: String -> [String]
tokens s = words $ replace [('('," ( "),(')'," ) "),('"', " \" ")] s

lexi :: String -> [[String]]
lexi s = map tokens $ divide $ quoteMac s

main = print $ (parse $ lexi "(define a (+ 14 333)) '(define b 412)")!!0
