step k
	| even k = v
	| otherwise = -v
	where v = 1 / fromIntegral ((2*k + 1) * (3 ^ k))
