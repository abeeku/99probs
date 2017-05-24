divides d n = rem n d == 0
ld n = ldf 2 n
ldf k n | divides k n = k
		| k^2 > n	   = n
		| otherwise   = ldf(k+1) n

prime0 :: Integer -> Bool
prime0 n | n < 1	 = error "not a pot integer"
		 | n == 1	 = False
		 | otherwise = ld n == n
		 
mnmInt :: [Int] -> Int
mnmInt [] = error "empty list"
mnmInt [x] = x
mnmInt (x:xs) = min x (mnmInt xs)

mnmStr :: [Char] -> Char
mnmStr "" = error "bad string"
mnmStr [x] = x
mnmStr (x:xs) = min x (mnmStr xs)
---uses the predefined min function

min' :: Int -> Int -> Int
min' x y | x <= y	 = x
		 | otherwise = y
maxOfList [] = error "No elements in list"
maxOfList [x] = x
maxOfList (x:xs) = max x (maxOfList xs)


removeFst :: Int -> [Int] -> [Int]
removeFst n [] = []
removeFst n [x] | n == x = []
				| n /= x = [x]

removeFst n (x:xs) | n == x  = xs 
				   | n /= x  = x : (removeFst n xs)
				
removeFst' n [] = []
removeFst' n [x] | n == x = []
				| n /= x = [x]

removeFst' n (x:xs) | n == x  = xs 
				   | n /= x  = x : (removeFst' n xs)

				
srtInts :: [Int] -> [Int]
srtInts [] = []
srtInts xs = m : (srtInts (removeFst m xs)) where m = mnmInt xs

srtInts' :: [Int] -> [Int]
srtInts' [] = []
srtInts' xs = let
				m = mnmInt xs
			  in m : (srtInts' (removeFst m xs))
			  
srtString :: String -> String	  
srtString "" = ""
srtString str = m : (srtString (removeFst' m str) ) where m = mnmStr str

			  
average :: [Int] -> Float
avegage [] = error "empty list"
average xs =  fromIntegral (sum xs)  /  fromIntegral (length xs) 

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

count :: Char -> String -> Int
count c [] = 0
count c (s:str) | c == s 	= 1 + count c str
				| c /= s 	= 0 + count c str

blowup :: String -> String
blowup "" = ""
blowup (s:str) = s : s : s : (blowup str)
