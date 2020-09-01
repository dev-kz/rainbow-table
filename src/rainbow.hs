import RainbowAssign
import qualified Data.Map as Map
import Data.Maybe as Maybe


-------------------------------------------- System Parameters --------------------------------------------
pwLength, nLetters, width, height :: Int
filename :: FilePath
pwLength = 8            -- length of each password
nLetters = 5            -- number of letters to use in passwords: 5 -> a-e
width = 40              -- length of each chain in the table
height = 1000           -- number of "rows" in the table
filename = "table.txt"  -- filename to store the table

------- you can change the parameters --------
-- pwLength = 5
-- nLetters = 18
-- width = 60
-- height = 800

-------------------------------------------- Hashing & Reducing --------------------------------------------

-- purpose: reduce the hash to plain text
pwReduce :: Hash -> Passwd
-- reverse the list because we read from right to left for reduce function (reference)
pwReduce hash = map toLetter (reverse (take pwLength (convertBase (convertInt32ToInt hash))))     
    where
        -- recursively divide until you reach pwLength (this handles negatives)
        convertBase hash = (hash `mod` nLetters) : convertBase (hash `div` nLetters)   
        convertInt32ToInt hash = fromEnum hash

-- references: 
-- https://lh3.googleusercontent.com/proxy/7V6PrdxyC_XWcaVptJv70v2pGOeal2poNTj78zS6M2QN_KP7P3f3HU2ule7J7zHBa2wVvz9uMJSJiFeoxtE9XjZ4pVOgSw
-- http://kestas.kuliukas.com/RainbowTables/


----------------------------------------------- Rainbow Table -----------------------------------------------

-- purpose: map final hash values onto the passwd value at the start of the chain
-- width is the number of hash/reduce operations to each chain.
rainbowTable :: Int -> [Passwd] -> Map.Map Hash Passwd
rainbowTable width pwList = Map.fromList(getListPairs(getHashVal width pwList) pwList)        
    where
        getListPairs xs ys      = zip xs ys                             -- zip creates tuples of two lists such as [(a,b)]
        getHashVal 0 pwList     = map pwHash pwList                     -- get the hash value of the password recursively
        getHashVal width pwList = getHashVal (width-1) (hashReduce pwList)
        hashReduce pwList       = map pwReduce (map pwHash pwList)      -- generate the plaintexts
        
-- references: 
-- https://stackoverflow.com/questions/20576229/an-example-of-using-data-map-in-haskell
-- http://www.cantab.net/users/antoni.diller/haskell/units/unit07.html
-- http://zvon.org/other/haskell/Outputprelude/zip_f.html


------------------------------------ Creating, Reading, and Writing Tables ------------------------------------

-- purpose: generate a new table and save it to disk
generateTable :: IO ()
generateTable = do
    table <- buildTable rainbowTable nLetters pwLength width height
    writeTable table filename

test1 = do
    table <- readTable filename
    return (Map.lookup 0 table)

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

-- References:
-- http://kestas.kuliukas.com/RainbowTables/
-- https://en.wikipedia.org/wiki/Rainbow_table
-- https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html
-- https://stackoverflow.com/questions/34266962/understanding-haskell-map-lookup-example-in-lyh

--------------------------------------------------------------------------------------------------------------------

-- provided test function
test2 :: Int -> IO ([Passwd], Int)
test2 n = do
  table <- readTable filename
  pws <- randomPasswords nLetters pwLength n
  let hs = map pwHash pws
  let result = Maybe.mapMaybe (findPassword table width) hs
  return (result, length result)


main :: IO ()
main = do
  generateTable
  res <- test2 10000
  print res
