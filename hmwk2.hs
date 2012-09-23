last_two xs = case xs of
	[] -> []
	[a] -> [a]
	[a,b] -> [a,b]
	(y:ys) -> last_two ys

dedup (x:y:xs) | x == y = dedup (y:xs)
	       | otherwise = x:(dedup (y:xs))
dedup [x] = [x]
dedup [] = []

myTakeWhile pred (x:xs) | (pred x) = x : (myTakeWhile pred xs)
		        | otherwise = myTakeWhile pred xs
myTakeWhile pred [] = []
