import Data.List

skip :: Char -> String -> Int --determines the number of charcharacters skipped in BMH algorithm
skip c s = if (c == last s) && (not $ elem c (init s))
           then length s
           else case elemIndex c (reverse $ init s) of
                Just n -> n+1
                Nothing -> length s

bmh :: String -> String -> [Int] --detects all occurences of a given pattern in a given text by BMH algortihm
bmh text pat
 | tlgth >= plgth = let checked = take plgth text in
                        if  checked == pat
                        then 1 : map (+ plgth) (bmh (drop plgth text) pat)
                        else let skip_amount = skip (last checked) pat in
                                map (+ skip_amount) (bmh (drop skip_amount text) pat)
 | otherwise = []
 where tlgth = length text
       plgth = length pat