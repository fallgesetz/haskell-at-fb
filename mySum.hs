mySum xs = case xs of
	(y:ys) -> y + mySum ys
	[] -> 0
