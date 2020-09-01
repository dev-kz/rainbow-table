findStartPw :: Map.Map Hash Passwd-> Int -> Hash -> Maybe Passwd
findStartPw table (-1) hash = Nothing
findStartPw table width_ hash 
 | Map.lookup hash table == Nothing = findStartPw table (width_-1) (pwHash (pwReduce hash))
 | otherwise = Map.lookup hash table 


findRealPw :: Maybe Passwd -> Int -> Hash -> Maybe Passwd
findRealPw Nothing width_ hash = Nothing
findRealPw (Just password) width_ hash = findRealPwRecur password width_ hash
      where  
      findRealPwRecur password_ (-1) hash_ = Nothing
      findRealPwRecur password_ width__ hash_
        | (pwHash password_) == hash_ = Just password_
        | otherwise = findRealPwRecur (pwReduce (pwHash password_)) (width__-1) hash_


findPassword :: Map.Map Hash Passwd-> Int -> Hash -> Maybe Passwd
findPassword table width_ hash 
 | findStartPw table width_ hash == Nothing = Nothing 
 | otherwise = findRealPw (findStartPw table width_ hash) width_ hash

