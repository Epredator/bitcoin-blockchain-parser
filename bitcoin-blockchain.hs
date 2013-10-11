{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.Binary hiding (encode, decode)
import Data.Binary.Get
import Control.Monad
import Text.Bytedump
import System.Directory
import Data.List
import Numeric
import qualified Data.Text as T
import Data.ByteString.Base16 (encode, decode)


main = do
  blocks <- parseFiles
  print $ last $ concatMap getResponseScripts blocks

getResponseScripts block = scripts
  where trans = transactions block
        inp = concatMap transactioninputs trans
        scripts = map responsescript inp
parseFiles = do
  home <- getHomeDirectory
  c <- getDirectoryContents $ home++"/.bitcoin/blocks/"
  let blockfiles = init $ sort $ filter (\x -> take 3 x == "blk") c
  blocks <- mapM parseFile blockfiles
  return $ concat blocks
      
parseFile f = do
  raw <- BL.readFile $ "/home/fergie/.bitcoin/blocks/"++f
  let blocks = runGet getBlocks raw
  return blocks
      
data Block = Block {
               magic :: !T.Text,
               blocklength :: !Int,
               blockformatversion :: !Int,
               previousblockhash :: !BS.ByteString,
               merkletroot :: !BS.ByteString,
               timestamp :: !Int,
               bits :: !T.Text,
               nonce :: !T.Text,
               numberoftransactions :: !Int,
               transactions :: [Transaction]
               } deriving (Show)

data Transaction = Transaction {
                     transactionversion :: !Int,
                     transactioninputcount :: !Int,
                     transactioninputs :: [Input],
                     transactionoutputcount :: !Int,
                     transactionoutputs :: [Output],
                     transactionlocktime :: !Int
                     } deriving (Show)
                   
data Input = Input {
              inputhash :: !BS.ByteString,
              inputtransactionindex :: !T.Text,
              responsescriptlength :: !Int,
              responsescript :: !BS.ByteString,
              sequencenumber :: !T.Text
              } deriving (Show)

data Output = Output {
               outputvalue :: !Int,
               challengescriptlength :: !Int,
               challengescript :: !BS.ByteString
               } deriving Show

getBlock :: Get Block
getBlock = do
  mag <- getWord32be
  blength <- getWord32le
  bformatv <- getWord32le
  pblockh <- getByteString 32
  merkle <- getByteString 32
  t <- getWord32le
  b <- getWord32be
  n <- getWord32be
  tnumber <- getVarLengthInt
  trans <- replicateM (fromIntegral tnumber) getTransaction
  return $ Block (textHex mag)
                 (fromIntegral blength)
                 (fromIntegral bformatv)
                 (encode pblockh)
                 (encode merkle)
                 (fromIntegral t)
                 (textHex b)
                 (textHex n)
                 (fromIntegral tnumber)
                 trans

textHex x = T.pack $ showHex x ""

getTransaction :: Get Transaction
getTransaction = do
  ver <- getWord32le
  inputc <- getVarLengthInt
  inputs <- replicateM (fromIntegral inputc) getInput --is this fromIntegral safe?
  outputc <- getVarLengthInt
  outputs <- replicateM (fromIntegral outputc) getOutput
  locktime <- getWord32le
  return $ Transaction (fromIntegral ver)
                       (fromIntegral inputc)
                       inputs
                       (fromIntegral outputc)
                       outputs
                       (fromIntegral locktime)
                 

getInput :: Get Input
getInput = do
  hash <- getByteString 32
  index <- getWord32le
  rlength <- getVarLengthInt
  script <- getByteString (fromIntegral rlength)
  seqnum <- getWord32le
  return $ Input (encode hash)
                 (textHex index)
                 (fromIntegral rlength)
                 (encode script)
                 (textHex seqnum)

getOutput :: Get Output
getOutput = do
  val <- getWord64le
  slength <- getVarLengthInt
  script <- getByteString (fromIntegral slength)
  return $ Output (fromIntegral val)
                  (fromIntegral slength)
                  (encode script)

getBlocks :: Get [Block]
getBlocks = do
   empty <- isEmpty
   if empty
     then return []
     else do block <- getBlock
             blocks <- getBlocks
             return (block:blocks)

getVarLengthInt :: Get Integer
getVarLengthInt = do
  w <- getWord8
  let hex = hexString w
  case hex of
    "fd" -> do
      val <- getWord16le
      return $ fromIntegral val
    "fe" -> do
      val <- getWord32le
      return $ fromIntegral val
    "ff" -> do
      val <- getWord64le
      return $ fromIntegral val
    otherwise -> return $ fromIntegral w  
