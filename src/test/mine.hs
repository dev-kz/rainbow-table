----------------------------------------------- Reversing Hashes -----------------------------------------------
-- purpose: find the correct password given the rainbow table and a hash value
findPassword :: Map.Map Hash Passwd-> Int -> Hash -> Maybe Passwd
findPassword table 0 hash           = Map.lookup hash table             -- look the table if the chain is one element
findPassword table width 0          = Nothing
findPassword table width hash = do
    pwd <- getInitialPwd table width hash                               -- get the initial password as an input (pwd)
    searchTable pwd width hash                                          -- search the table with the initial password
    where
        searchTable [] width hash   = Nothing
        searchTable pwd width hash  = validatePwdHash pwd width hash    -- search table recursively even if initial pwd is found


-- purpose: lookup the hash value and find the corresponding password
getInitialPwd :: Map.Map Hash Passwd-> Int -> Hash -> Maybe Passwd
getInitialPwd table width hash 
    | width < 0                         = Nothing                       -- looking up the table didn't find anything
    | Map.lookup hash table == Nothing  = getInitialPwd table (width-1) nextHash
    | otherwise                         = Map.lookup hash table 
    where 
        nextHash = pwHash(pwReduce hash)

-- purpose: recursively search the table and keep going through the chain
-- note: handles false positive case (this function checks if the pwd has a hash value in the chain)
validatePwdHash :: Passwd -> Int -> Hash -> Maybe Passwd
validatePwdHash pwd width hash
    | width < 0                     = Nothing                           -- if not found then it means pwd is most likey false positive
    | hash2 == hash                 = Just pwd                          -- it means the initial hash value exists within the chain
    | otherwise                     = validatePwdHash nextPwd (width-1) hash
    where
        nextPwd = pwReduce(pwHash pwd)
        hash2 = pwHash pwd
