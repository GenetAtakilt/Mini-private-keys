{-# LANGUAGE InstanceSigs #-}
module Miniprivate where

import Crypto.Hash           (hash, Digest, SHA256)
import qualified Data.ByteString       as B 
import qualified Data.ByteString.Char8  as B8
import qualified Data.ByteString.Base58 as B58
import qualified Data.ByteString.Base16 as B16



--private key samples
smapleprivatekey1 :: String
smapleprivatekey1 = "S6c56bnXQiBjk9mqSYE7ykVQ7NzrRy" 

smapleprivatekey2 :: String
smapleprivatekey2 = "0C28FCA386C7A227600B2FE50B7CAE11EC86D3BF1FBE471BE89827E19D72AA1D"

smapleprivatekey3 :: String
smapleprivatekey3 = "5HueCGU8rMjxEXxiPuD5BDku4MkFqeZyd4dZ1jvhTVqvbTLvyTJ" 
 
-- Subtask 4.4.1
-- validator for mini private keys 
-- steps To determine whether the minikey is valid: 
  -- 1.Add a question mark to the end of the mini private key string.
  -- 2.Take the SHA256 hash of the entire string. However, we will only look at the first byte of the result
  -- 3.If the first byte is 00, the string is a well-formed minikey. If the first byte is not 00, the string should be rejected as a minikey.
  -- 4.if the key is valid then outputs the corresponding full private key


valid :: IO () 
valid = do
    putStrLn "Enter the Miniprivate key" >> getLine >>= \s -> valid' $ read $ s  

valid' :: String -> IO ()
valid' s =  let x = sha256 $ s ++ "?"
             in if (take 2 $ x )  == "00" then putStrLn (sha256 $ s)
                  else putStrLn ("invalid")


-- |Computes the SHA256 hash of a @`String`@.
sha256 :: String -> String
sha256 = sha256' . B8.pack

-- |Computes the SHA256 hash of a @`ByteString`@.
sha256' :: B.ByteString -> String
sha256' = (show :: Digest SHA256 -> String) . hash

-- Subtask 4.4.2
-- define function that transforms a private key into Wallet Import Format


transferToWIF1Hash1 :: String -> String
transferToWIF1Hash1 s = sha256' $ fst $ B16.decode $ B8.pack ("80" ++ s)  -- Add a 0x80 byte in front of private key
                                                                          -- change the string to base16 and select the first 
                                                                          -- perform the first hash

transferToWIF1Hash2 :: String -> String                                                                 --perfom the second hash
transferToWIF1Hash2 t = let x = take 8  $ sha256' $ fst $ B16.decode $ B8.pack $ transferToWIF1Hash1 t  -- take the first 4 bytes
                           in ("80" ++ t) ++ x                                                          -- and add the first 4 bytes to the value in first step


transferToWIF2Hash1 :: String -> String                                    --Add a 0xef byte in front of private key
transferToWIF2Hash1  s = sha256' $ fst $ B16.decode $ B8.pack ("ef" ++ s)  --change the string to base16 and select the first 
                                                                           -- perform the first hash

transferToWIF2Hash2 :: String -> String                                                                    --perfom the second hash
transferToWIF2Hash2  s = let x = take 8  $ sha256' $ fst $ B16.decode $ B8.pack $ transferToWIF2Hash1 s    -- take the first 4 bytes
                            in ("ef" ++ s) ++ x                                                            -- and add the first 4 bytes to the value in first step


-- base58 string using Base58Check encoding
byteStringToBase58 :: String -> IO ()  -- function that Convert the result from a byte string into a base58 string using Base58Check encoding
byteStringToBase58 s = putStrLn (show (B58.encodeBase58 B58.bitcoinAlphabet $ fst $ B16.decode $ B8.pack $ transferToWIF1Hash2 s))  


select :: IO () 
select = do
    putStrLn "Enter the Miniprivate key" >> getLine      >>= \s -> selectAdress $ read $ s

selectAdress :: String -> IO ()
selectAdress  s = do
    putStrLn "choose your network "  -- let the user select the adress mainnet or testnet
    name <- getLine
    putStrLn " "
    putStrLn (" network adress " ++ name ++ "!")
    putStrLn " "
    --putStrLn "enter private network"
    --loc <- getLine
    let
      answer
        | name == "mainnet"    = transferToWIF1Hash2 s      --if user select mainnet adress use function which add 0x80 in front of the private key
        | name == "testnet"    = transferToWIF2Hash2 s      -- if user select testnet adress use function which add 0xef in front of the private key
        | otherwise            = "Sorry, don't know that."
    putStrLn answer


-- transfer WIF to private key
  --steps
    -- 1. Take a Wallet Import Format string 
    -- 2. Convert it to a byte string using Base58Check encoding
    -- 3. Drop the last 4 checksum bytes from the byte string 
    -- 4. Drop the first byte

transferWIF :: IO () 
transferWIF = do
    putStrLn "Enter the Miniprivate key" >> getLine      >>= \s -> transferWIFTOprivate $ read $ s

transferWIFTOprivate :: String -> IO ()
transferWIFTOprivate s  = do 
                        case  B58.decodeBase58 B58.bitcoinAlphabet $ B8.pack $ s of                          -- since the result of B58 decode is maybe string we use case to handele the error
                                Just x  -> putStrLn (show (B.drop 2 (B.take ((B.length (B16.encode x)) - 8) (B16.encode x))))  -- we use take drop and length  from data.byte.string package 
                                Nothing -> putStrLn (show ("invalid"))
                         


                             
                            






                            
