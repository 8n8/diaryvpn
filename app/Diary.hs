module Diary (Diary, initDiary, toFile, fromFile, summaries) where


import qualified Data.Word
import qualified Foreign.Ptr
import qualified System.IO
import qualified Foreign.Storable
import qualified Foreign.Marshal.Alloc
import qualified Data.ByteString.Lazy


{-| Say you write an entry a day for 70 years, that is 365 * 70 ~= 25000
entries. Say 30,000.
-}
maxEntries :: Int
maxEntries =
    30000


{-| I read in the Guardian that Tony Benn's diaries contain about 20m
words, and he wrote it for all of his life. Say he wrote 30,000 entries,
and that there were 6 characters per word on average,
that is 20,000,000 * 5 / 30,000 ~= 3333 characters per entry on average.
-}
entrySize :: Int
entrySize =
    3333


data Diary
    = Diary
    { num :: Foreign.Ptr.Ptr Data.Word.Word32
    , ends :: Foreign.Ptr.Ptr Data.Word.Word32
    , text :: Foreign.Ptr.Ptr Data.Word.Word8
    , timestamps :: Foreign.Ptr.Ptr Data.Word.Word32
    }


summaries :: Diary -> IO Data.ByteString.Lazy.ByteString
summaries diary =
    do
    num_ <- Foreign.Storable.peek (num diary)
    timestamps_ <- encodeTimestamps (timestamps diary)
    text_ <- encodeTexts (text diary) (ends diary) (fromIntegral num_)
    return $ text_ <> timestamps_


encodeTexts
    :: Foreign.Ptr.Ptr Data.Word.Word8
    -> Foreign.Ptr.Ptr Data.Word.Word32
    -> Int
    -> IO Data.ByteString.Lazy.ByteString
encodeTexts text_ ends_ num_ =
    encodeTextsHelp text_ ends_ num_ Data.ByteString.Lazy.empty 0


maxSummary :: Int
maxSummary =
    100
    

encodeTextsHelp
    :: Foreign.Ptr.Ptr Data.Word.Word8
    -> Foreign.Ptr.Ptr Data.Word.Word32
    -> Int
    -> Data.ByteString.Lazy.ByteString
    -> Int
    -> IO Data.ByteString.Lazy.ByteString
encodeTextsHelp text_ ends_ num_ accum counter =
    if counter == num_ then
        return accum

    else
    do
    text <- encodeText text_ ends_ num_ counter
    encodeTextsHelp text_ ends_ num_ (accum <> text) (counter + 1)


encodeText
    :: Foreign.Ptr.Ptr Data.Word.Word8
    -> Foreign.Ptr.Ptr Data.Word.Word32
    -> Int
    -> Data.ByteString.Lazy.ByteString
    -> Int
    -> IO Data.ByteString.Lazy.ByteString
encodeText text_ ends_ num_ counter =
    do
    summary <- encodePtr text_ 


encodePtr :: Foreign.Ptr.Ptr a -> Int -> IO Data.ByteString.Lazy.ByteString
encodePtr ptr n =
    encodePtrHelp ptr n 0 Data.ByteString.Lazy.empty


encodePtrHelp
    :: Foreign.Ptr.Ptr a
    -> Int
    -> Int
    -> Data.ByteString.Lazy.ByteString
    -> IO Data.ByteString.Lazy.ByteString
encodePtrHelp ptr size pos accum =
    if size == pos then
    return accum

    else
    do
    byte <- Foreign.Storable.peekByteOff pos
    return $ Data.ByteString.cons byte accum


toFile :: System.IO.Handle -> Diary -> IO ()
toFile handle entries =
    do
    System.IO.hPutBuf handle (num entries) 4
    System.IO.hPutBuf handle (ends entries) (4 * maxEntries)
    System.IO.hPutBuf handle (text entries) (entrySize * maxEntries)
    System.IO.hPutBuf handle (timestamps entries) (4 * maxEntries)


initDiary :: IO Diary
initDiary =
  do
    num_ <- Foreign.Marshal.Alloc.malloc
    Foreign.Storable.poke num_ 0
    -- It uses calloc rather than malloc because zeroed bytes will compress
    -- better for sending over the network.
    ends_ <- Foreign.Marshal.Alloc.callocBytes (4 * maxEntries)
    text_ <- Foreign.Marshal.Alloc.callocBytes (entrySize * maxEntries)
    timestamps_ <- Foreign.Marshal.Alloc.callocBytes (4 * maxEntries)
    return $ Diary num_ ends_ text_ timestamps_


fromFile :: System.IO.Handle -> Diary -> IO ()
fromFile handle entries =
  do

    (do
      n <- System.IO.hGetBuf handle (num entries) 4
      if n /= 4 then
          error "couldn't read number of entries from file"

      else
          return ())

    (do
      n <- System.IO.hGetBuf handle (ends entries) (4 * maxEntries)
      if n /= 4 * maxEntries then
          error "couldn't read entry ends from file"

      else
          return ())

    (do
      n <- System.IO.hGetBuf handle (text entries) (entrySize * maxEntries)
      if n /= entrySize * maxEntries then
          error "couldn't read the entries from file"

      else
          return ())

    (do
      n <- System.IO.hGetBuf handle (timestamps entries) (4 * maxEntries)
      if n /= 4 * maxEntries then
        error "couldn't read timestamps from file"

      else
        return ())
