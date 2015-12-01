import Data.List

skip :: Char -> String -> Int --function for determining the number of charcharacters skipped in BMH algorithm
skip c s = if (c == last s) && (not $ elem c (init s))
           then length s
           else case elemIndex c (reverse $ init s) of
                Just n -> n+1
                Nothing -> length s

bm :: String -> String -> Maybe Int --implementation of BMH algorithm
bm text pat
 | tlgth >= plgth = let checked = take plgth text in
                        if  checked == pat
                        then Just 1
                        else let skip_amount = skip (last checked) pat in
                                case bm (drop skip_amount text) pat of
                                Just n -> Just $ n + skip_amount
                                Nothing -> Nothing
 | otherwise = Nothing
 where tlgth = length text
       plgth = length pat
