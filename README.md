# rainbow-table
Implemented a Rainbow table using Haskell. The goal was to create a program that would use a rainbow table to crack a password based on a given hash.

## Files:
- rainbow_assign.hs: A module used inside rainbow.hs which provides a function such as pwHash.
- rainbow.hs: A module responsible for creating a rainbow table and cracking passwords.

## Features:
- pwHash: a function that converts a string password into a hash value.
- pwReduce: a function that converts a hash value to possible password.

## Steps to load:
    1) Open ghci  
    2) Use ':c' at the location of your saved files  
    3) Use ':l assignment1.hs' 
    4) Creating a table:  
    *Main> :t rainbowTable  
      rainbowTable :: Int -> [Passwd] -> Map.Map Hash Passwd  
    *Main> rainbowTable 2 ["dccdecee","cdeccaed","acbcaeec","eeeeaebd","ccdccbeb"]  
  
## Demo:

    *Main> let table = rainbowTable 40 ["abcdeabc", "aabbccdd", "eeeeeeee"]  
    *Main> findPassword table 40 1726491528  
    *Just "abcdeabc"*  
    *Main> findPassword table 40 (-206342227)  
    *Just "dbddecab"*    
    *Main> findPassword table 40 1726491529  
    *Nothing*  
    *Main> findPassword table 40 0  
    *Nothing*  
    
    
#### Note:
- Do "Steps to load" before "Demo".
