-- |
-- Do not confuse DBMS connections and database connections. A DBMS connection
-- is a TCP connection or Unix domain socket connection to a DBMS instance. A
-- database connection is a DBMS connection over which the frontend has sent a
-- StartupMessage message which the backend has accepted.
module Database.PostgreSql.Connection
  ( -- * Connections
    Connection
  , fromDbmsConnection

    -- * Credentials
  , Credentials
  , startupMessageParameters
  ) where

import Network.Socket (Socket)

import qualified Data.ByteString as BS
import qualified Data.Vector as V

import Database.PostgreSql.Message (FMessage (..), sendFMessage)

--------------------------------------------------------------------------------
-- Connections

-- |
-- Database connection.
newtype Connection =
  Connection Socket

-- |
-- Create a database connection from a DBMS connection.
fromDbmsConnection :: Socket -> Credentials -> IO Connection
fromDbmsConnection s c = do
  sendFMessage s (FStartupMessage (startupMessageParameters c))
  pure $ Connection s

--------------------------------------------------------------------------------
-- Credentials

-- |
-- How to turn a DBMS connection into a database connection.
data Credentials =
  Credentials
    { credentialsDatabase :: BS.ByteString
    , credentialsUser     :: BS.ByteString }
  deriving stock (Eq, Show)

-- |
-- The parameters to be included in the StartupMessage message.
startupMessageParameters :: Credentials -> V.Vector (BS.ByteString, BS.ByteString)
startupMessageParameters c =
  [ ("database", credentialsDatabase c)
  , ("user",     credentialsUser     c) ]
