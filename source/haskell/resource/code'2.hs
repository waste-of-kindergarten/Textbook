import Data.Char (isAlpha)
string1 = Just "My name is "

string2 = Just "L fried."
-- just joking (Alfred)
string3 = Just "The textbook seems a mess 2 readers !"

maybeMonadDemo :: Maybe Int
maybeMonadDemo = 
    string1 >>= \x -> 
        (x ++) <$> string2 >>=
            \y -> 
                (y ++) <$> string3 >>=
                    \z -> 
                        Just ( foldr (+) 0 (((\x -> if x then 0 else 1) . isAlpha) <$> z))