import Data.List
import Prelude

replaceExt ext1 ext2 str
	| isSuffixOf ext1 str = take (length str - length ext1) str ++ ext2
	| otherwise = str
