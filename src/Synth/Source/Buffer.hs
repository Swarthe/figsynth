{-# LANGUAGE DataKinds, KindSignatures, ExplicitForAll #-}

module Synth.Source.Buffer
    ( Buffer (size), Sample
    , new, close
    , read
    , iFrom
    )
where

import Vivid
import Vivid.SC.Server.Commands qualified as Commands

import System.IO (Handle, IOMode (ReadMode), openFile, hClose)
import System.Directory (removeFile, doesFileExist)
import System.Posix.Files
    ( createNamedPipe
    , namedPipeMode, ownerReadMode, ownerWriteMode )

import Data.ByteString qualified as ByteString
import Data.ByteString.Internal qualified as ByteString

import Data.Vector.Storable (Vector)
import Data.Vector.Storable qualified as Vector

import Data.Word (Word32)
import Data.Bits ((.|.))

import GHC.TypeLits (Symbol, KnownSymbol)

import Foreign.Storable (sizeOf)
import Foreign.ForeignPtr (castForeignPtr)

import Control.Exception (onException)

import Prelude hiding (read)

-- | A readable and writeable audio buffer.
--
-- This is similar to a `Vivid.BufferId`, except that it allows reading its
-- contents.
data Buffer = Buffer
    { size   :: !Word32     -- ^ Size of the buffer in samples.
    , scId   :: !BufferId   -- ^ The server-side buffer.
    , path   :: FilePath    -- ^ Named pipe used to read the buffer.
    , handle :: Handle }    -- ^ Handle for the named pipe.

type Sample = Float

-- | Constructs a `Buffer` of the passed size.
new :: Word32 -> IO Buffer
new size = do
    scId <- newBuffer (fromIntegral size)
    path <- createTempPipe "figsynth-buf"
        `onException` closeBuffer scId
    handle <- openFile path ReadMode
        `onException` (closeBuffer scId >> removeFile path)
    pure Buffer{..}

close :: Buffer -> IO ()
close Buffer{..} = do
    closeBuffer scId
    hClose handle
    removeFile path

-- | Reads samples from a `Buffer`. The passed number specifies how many samples
-- to read.
--
-- This function never reads more samples than the `Buffer` contains.
--
-- This operation is rather expensive as it uses a named pipe to load the data.
read :: Buffer -> Word32 -> IO (Vector Sample)
read Buffer{..} n = do
    writeToPipe
    bytes <- ByteString.hGetSome handle nBytes
    pure $ toVector bytes
  where
    writeToPipe = oscWSync \syncId -> callOSC $
        Commands.b_write scId path "raw" "float"
           Nothing                          -- Write the entire buffer.
           0                                -- First frame.
           True                             -- Keep the file open.
           (Just $ Commands.sync syncId)    -- Write Synchronously.

    toVector bytes = Vector.unsafeFromForeignPtr
        (castForeignPtr ptr)
        (off `div` sizeOfSample)
        (len `div` sizeOfSample)
      where (ptr, off, len) = ByteString.toForeignPtr bytes

    nBytes = fromIntegral n * sizeOfSample
    sizeOfSample = sizeOf (0 :: Sample)

---- | Reads the contents of the `Buffer`.
----
---- This operation is rather expensive as it uses a named pipe to load the data.
--getSamples :: Buffer -> IO (IOVector Sample)
--getSamples Buffer{..} = do
--    writeToPipe
--    bytes <- ByteString.hGet handle sizeBytes
--    samples <- fromRightIO $ runGet getSampleVector bytes
--    Vector.thaw samples
--  where
--    writeToPipe = oscWSync \syncId -> callOSC $
--        Commands.b_write scId path "raw" "float"
--           Nothing                          -- Write the entire buffer.
--           0                                -- First frame.
--           True                             -- Keep the file open.
--           (Just $ Commands.sync syncId)    -- Write Synchronously.
--
--    getSampleVector = Vector.replicateM (fromIntegral size) get
--    fromRightIO = either fail pure
--
--    sizeBytes = fromIntegral (fromIntegral size * sizeOf (0 :: Sample))

-- | Reads the `Buffer` as a lazy list of samples.
--
-- The returned list is the same size as the `Buffer`.
--read :: Buffer -> IO [Sample]
--read Buffer{..} = writeToMem >> readFromMem
--  where
--    writeToMem = oscWSync $ \syncId -> callOSC $
--      Commands.b_write scId path "raw" "float"
--         Nothing                        -- Write the entire buffer.
--         0                              -- First frame.
--         True                           -- Keep the file open.
--         (Just $ Commands.sync syncId)  -- Write Synchronously.
--
--    readFromMem = withForeignPtr ptr \src ->
--        ByteString.create sizeBytes (\dest -> copyBytes dest src sizeBytes)
--        -- Don't use the default `Serialize` instance for lists, because it
--        -- expects the size (which we already have) to be stored with the data.
--        <&> fromRight . Serialize.runGet (getList size)
--
--    sizeBytes = (fromIntegral size :: Int) * sizeOf (0 :: Sample)
--
--    -- Adapted from the "Data.Serialize.Get" source code.
--    {-# INLINE getList #-}
--    getList = go []
--     where
--        go xs 0 =
--            pure $! reverse xs
--        go xs i = do
--            x <- Serialize.get
--            x `seq` go (x : xs) (i - 1)
--
--    fromRight = either error id

-- | Extracts an `I` value from a `Buffer`.
--
-- The returned value can be used to manipulate the buffer using the functions
-- from "Vivid.UGens.Buffer".
iFrom :: forall (a :: Symbol). KnownSymbol a => Buffer -> I a
iFrom = bufToI . scId

-- | Creates a temporary named pipe with the passed name (not path) as template.
createTempPipe :: String -> IO FilePath
createTempPipe name = go (0 :: Int)
  where
    go i = doesFileExist path >>= \case
        True -> go (i + 1)
        False -> createNamedPipe path mode >> pure path
      where path = basePath ++ show i

    basePath = "/tmp/" ++ name ++ "-"
    mode = namedPipeMode .|. ownerReadMode .|. ownerWriteMode
