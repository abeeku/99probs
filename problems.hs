-- 99 Problems
myLast xs = head (reverse xs)

myButLast xs = head (tail (reverse xs))

element_at ls 1 = head ls
element_at ls x = element_at (tail ls) (x-1)

myLength xs = sum [1 | _ <- xs]

myReverse xs = reverse xs

isPalindrome xs = reverse xs == xs

--my-flatten xs

--compress xs

--pack xs
