import Prelude
import Data.List
-- encoding portion of burrows wheeler
-- Generated a lexicographically sorted list of rotations of the character array
rotate_builder i = (\xs -> (drop i xs) ++ (take i xs))
-- e.g. all_rotations "asdf" gives ["asdf", "sdfa", "dfas", "fasd"]
all_rotations xs = map (\f -> f xs) (map rotate_builder [0..(length xs - 1)])
-- sort the list
-- take the last element of the lexigraphically sorted list
bw_encode xs = map last (sort ( all_rotations ('^':xs)))

-- decoding portion of burrows wheeler.

make_list_of_list = map (\x -> [x])

sort_and_add matrix = addm matrix (map last (sort matrix))
	where addm matrix column = case (matrix, column) of
		((x:xs), (y:ys)) -> (x ++ [y]):(addm xs ys)
		([], []) -> []

iter_dot 1 f = f
iter_dot k f = f . (iter_dot (k-1) f)

bw_decode xs = tail $ head ( filter (\x -> (head x) == '^') ((iter_dot ((length xs) - 1) sort_and_add) (make_list_of_list xs)))
                                              

