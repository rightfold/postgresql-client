-- |
-- Frontend and backend message serialization and deserialization.
module Database.PostgreSql.Message
  ( -- * Frontend messages
    FMessage (..)
  , sendFMessage
  , serializeFMessage
  ) where

import Data.Binary.Put (Put, putWord8, putWord32be, runPut)
import Data.Foldable (for_)
import Network.Socket (Socket)

import qualified Data.Binary.Put as Put
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Network.Socket.ByteString.Lazy as Socket.BS.L

-- In this source file, text is included verbatim from the PostgreSQL protocol
-- documentation. You can find the original documentation at [1].
--
-- [1]: https://www.postgresql.org/docs/10/protocol-message-formats.html

--------------------------------------------------------------------------------
-- Frontend messages

-- |
-- Frontend message.
data FMessage
  = FStartupMessage (V.Vector (BS.ByteString, BS.ByteString))
  deriving stock (Eq, Show)

-- |
-- Serialize a frontend message.
serializeFMessage :: FMessage -> Put

serializeFMessage (FStartupMessage parameters) = do
  -- Length of message contents in bytes, including self.
  putWord32be . fromIntegral $
    4 + 4 + sum [ (BS.length k + 1 + BS.length v + 1)
                | (k, v) <- parameters ]

  -- The protocol version number. The most significant 16 bits are the major
  -- version number (3 for the protocol described here). The least significant
  -- 16 bits are the minor version number (0 for the protocol described here).
  putWord32be 196608

  -- The protocol version number is followed by one or more pairs of parameter
  -- name and value strings. A zero byte is required as a terminator after the
  -- last name/value pair. Parameters can appear in any order.
  for_ parameters $ \(name, value) -> do

    -- The parameter name.
    putString name

    -- The parameter value.
    putString value

-- |
-- Send a frontend message.
{-# INLINE sendFMessage #-}
sendFMessage :: Socket -> FMessage -> IO ()
sendFMessage = -- TODO: If Socket.BS.L.sendAll throws, rethrow wrapped.
               (. runPut . serializeFMessage) . Socket.BS.L.sendAll

--------------------------------------------------------------------------------
-- Miscellaneous

putString :: BS.ByteString -> Put
putString s = do
  Put.putByteString s
  putWord8 0
