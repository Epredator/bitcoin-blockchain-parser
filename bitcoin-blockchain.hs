{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.Binary hiding (encode)
import Data.Binary.Get
import Data.Binary.Put
import Control.Monad
import Text.Bytedump
import System.Directory
import Data.List (sort)
import Numeric
import qualified Data.Text as T
import Data.ByteString.Base16 (encode,decode)

import Data.Conduit.Serialization.Binary
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.Conduit
import System.IO

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Char8 as C
import Data.Char

--reads from stdin
--parses blocks
--prints to stdout
main = do
  c <- source $$ conduit =$ countBlocks --CL.mapM_ print
  print c

source :: Source IO BS.ByteString
source = CB.sourceHandle stdin

sink :: Sink Block IO ()
sink = CL.mapM_ print

conduit :: Conduit BS.ByteString IO Block
conduit = conduitGet getBlock


countBlocks = CL.fold (\x _ -> x+1) 0

parseFiles :: IO [Block]
parseFiles = do
  home <- getHomeDirectory
  c <- getDirectoryContents $ home++"/.bitcoin/blocks/"
  let blockfiles = init $ sort $ filter (\x -> take 3 x == "blk") c
  blocks <- mapM parseFile blockfiles
  return $ concat blocks

parseFile :: String -> IO [Block]
parseFile f = do
  home <- getHomeDirectory
  raw <- BL.readFile $ home++"/.bitcoin/blocks/"++f
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
               currenthash :: !BS.ByteString,
               numberoftransactions :: !Int,
               transactions :: [Transaction]
               } deriving (Show,Eq)

data Transaction = Transaction {
                     transactionversion :: !Int,
                     transactioninputcount :: !Int,
                     transactioninputs :: [Input],
                     transactionoutputcount :: !Int,
                     transactionoutputs :: [Output],
                     transactionlocktime :: !Int,
                     transactionhash :: BS.ByteString --must be lazy because of weird way I compute it later
                     } deriving (Show,Eq)
                   
data Input = Input {
              inputhash :: !BS.ByteString,
              inputtransactionindex :: !T.Text,
              responsescriptlength :: !Int,
              responsescript :: !BS.ByteString,
              sequencenumber :: !T.Text
              } deriving (Show,Eq)

data Output = Output {
               outputvalue :: !Int,
               challengescriptlength :: !Int,
               challengescript :: !BS.ByteString
               } deriving (Show,Eq)

getBlock :: Get Block
getBlock = do
  mag <- getWord32be
  --sometimes (only in blk00066.dat so far) there are long periods of zeros
  --I don't know what these mean, but it messes up the parsing.
  --So if a block doesn't start with f9beb4d9 then skip forward until it does
  case textHex mag of
    "f9beb4d9" -> do
      blength <- getWord32le
      header <- getByteString 80
      let bheader = runGet (getBlockHeader mag blength) (BL.fromStrict header)
      tnumber <- getVarLengthInt
      trans <- replicateM (fromIntegral tnumber) getTransaction
      return $! bheader (encode $ SHA256.hash $ SHA256.hash header)
                        (fromIntegral tnumber)
                        trans
    otherwise -> getBlock

getBlockHeader mag blength = do
  bformatv <- getWord32le 
  pblockh <- getByteString 32
  merkle <- getByteString 32
  t <- getWord32le
  b <- getWord32be
  n <- getWord32be
  return $ Block (textHex mag)
                 (fromIntegral blength)
                 (fromIntegral bformatv)
                 (encode pblockh)
                 (encode merkle)
                 (fromIntegral t)
                 (textHex b)
                 (textHex n)
  
textHex :: Word32 -> T.Text
textHex x = T.pack $! showHex x ""

getTransaction :: Get Transaction
getTransaction = do
  ver <- getWord32le
  inputc <- getVarLengthInt
  inputs <- replicateM (fromIntegral inputc) getInput --is this fromIntegral safe?
  outputc <- getVarLengthInt
  outputs <- replicateM (fromIntegral outputc) getOutput
  locktime <- getWord32le
  let t = Transaction (fromIntegral ver)
                      (fromIntegral inputc)
                      inputs
                      (fromIntegral outputc)
                      outputs
                      (fromIntegral locktime)
  return $ hashTransaction t

hashTransaction :: (C.ByteString -> Transaction) -> Transaction
hashTransaction t = t (encode hash)
  where trans = t undefined -- undefined is never evaluated. This is a bit weird
        p = runPut $ putTransaction trans
        hash = SHA256.hash $ SHA256.hash $ BS.concat $ BL.toChunks p

putTransaction :: Transaction -> Put
putTransaction t = do
  putWord32le $ fromIntegral $ transactionversion t
  putVarLengthInt $ transactioninputcount t
  mapM_ putInput $ transactioninputs t
  putVarLengthInt $ transactionoutputcount t
  mapM_ putOutput $ transactionoutputs t
  putWord32le $ fromIntegral $ transactionlocktime t
                 

getInput :: Get Input
getInput = do
  hash <- getByteString 32
  index <- getWord32le
  rlength <- getVarLengthInt
  script <- getByteString (fromIntegral rlength)
  seqnum <- getWord32le
  return $! Input (encode hash)
                 (textHex index)
                 (fromIntegral rlength)
                 (encode script)
                 (textHex seqnum)

putInput :: Input -> Put
putInput i = do
  putByteString $ fst $ Data.ByteString.Base16.decode $ inputhash i
  putByteString $ fst $ Data.ByteString.Base16.decode $ C.pack $ T.unpack $ inputtransactionindex i
  putVarLengthInt $ responsescriptlength i
  putByteString $ fst $ Data.ByteString.Base16.decode $ responsescript i
  putByteString $ fst $ Data.ByteString.Base16.decode $ C.pack $ T.unpack $ sequencenumber i
  
getOutput :: Get Output
getOutput = do
  val <- getWord64le
  slength <- getVarLengthInt
  script <- getByteString (fromIntegral slength)
  return $! Output (fromIntegral val)
                  (fromIntegral slength)
                  (encode script)

putOutput :: Output -> Put
putOutput o = do
  putWord64le $ fromIntegral $ outputvalue o
  putVarLengthInt $ challengescriptlength o
  putByteString $ fst $ Data.ByteString.Base16.decode $ challengescript o

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
      return $! fromIntegral val
    "fe" -> do
      val <- getWord32le
      return $! fromIntegral val
    "ff" -> do
      val <- getWord64le
      return $! fromIntegral val
    otherwise -> return $! fromIntegral w

putVarLengthInt :: Int -> Put
putVarLengthInt i
  | i < 0xfd = putWord8 (fromIntegral i)
  | i < 0xffff = do
      putWord8 0xfd
      putWord16le (fromIntegral i)
  | i < 0xffffffff = do
      putWord8 0xfe
      putWord32le (fromIntegral i)
  | otherwise = putWord64le (fromIntegral i)
