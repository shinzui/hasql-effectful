{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.Char8 (pack)
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int32)
import Data.Text (Text)
import Effectful
import Effectful.Error.Static
import Effectful.Hasql
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Pool (Pool, UsageError, acquire)
import Hasql.Session (Session, statement)
import Hasql.Statement (Statement (..))
import Hasql.Transaction qualified as T
import Hasql.Transaction.Sessions
import System.Environment (lookupEnv)

data User = User
  { firstName :: Text,
    lastName :: Text,
    userId :: Maybe Int32
  }
  deriving stock (Show)

userDecoder :: D.Row User
userDecoder =
  User
    <$> D.column (D.nonNullable D.text)
    <*> D.column (D.nonNullable D.text)
    <*> D.column (D.nullable D.int4)

userEncoder :: E.Params User
userEncoder =
  (firstName >$< E.param (E.nonNullable E.text))
    <> (lastName >$< E.param (E.nonNullable E.text))

insertUser_ :: Statement User Int32
insertUser_ =
  Statement sql encoder decoder True
  where
    sql = "INSERT INTO users (first_name, last_name) VALUES ($1, $2) RETURNING id"
    encoder = userEncoder
    decoder = D.singleRow ((D.column . D.nonNullable) D.int4)

findUserById_ :: Statement Int32 (Maybe User)
findUserById_ =
  Statement sql encoder decoder True
  where
    sql = "SELECT first_name, last_name, id FROM users WHERE id = $1"
    encoder = E.param $ E.nonNullable E.int4
    decoder = D.rowMaybe userDecoder

findUserById :: Int32 -> Session (Maybe User)
findUserById userId = statement userId findUserById_

insertUser :: User -> Session Int32
insertUser user = transaction Serializable Write $ do
  T.statement user insertUser_

createUser :: (Hasql :> es, Error UsageError :> es) => User -> Eff es Int32
createUser user = do
  runSession $ insertUser user

findUser :: (Hasql :> es, Error UsageError :> es) => Int32 -> Eff es (Maybe User)
findUser userId = do
  runSession $ findUserById userId

mkPool :: IO Pool
mkPool = do
  pgHost <- lookupEnv "PG_CONNECTION_STRING"
  case pgHost of
    Just h -> acquire 3 10 1_800 1_800 (pack h)
    Nothing -> error "PG_CONNECTION_STRING env is not defined or is invalid"

createTableStatement :: Statement () ()
createTableStatement = Statement sql mempty D.noResult False
  where
    sql = "CREATE TABLE IF NOT EXISTS users (id SERIAL PRIMARY KEY, first_name TEXT, last_name TEXT)"

createTable :: (Hasql :> es, Error UsageError :> es) => Eff es ()
createTable = do
  runSession $ statement () createTableStatement

newUser :: User
newUser = User "Sung-kyung" "Lee" Nothing

sampleApp :: (Hasql :> es, Error UsageError :> es) => Eff es (Maybe User)
sampleApp = do
  uId <- createUser newUser
  findUser uId

runToIO :: Pool -> Eff [Hasql, Error UsageError, IOE] a -> IO a
runToIO pool = runEff . runErrorWith @UsageError handleUsageError . runHasqlIO pool

handleUsageError :: (IOE :> es) => CallStack -> UsageError -> Eff es a
handleUsageError callStack err = do
  liftIO $ putStrLn $ "Usage error" <> show err <> prettyCallStack callStack
  error "hasql usage error"

main :: IO ()
main = do
  pool <- mkPool
  _ <- runToIO pool createTable
  u <- runToIO pool sampleApp
  print u
