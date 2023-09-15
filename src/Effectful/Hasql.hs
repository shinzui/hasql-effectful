module Effectful.Hasql where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Reader.Static
import Hasql.Pool (Pool, UsageError, use)
import Hasql.Session (Session)

-- | Provides the ability to execute actions against a postgresql database
data Hasql :: Effect where
  RunSession :: Session a -> Hasql m a

type instance DispatchOf Hasql = Dynamic

runSession :: (Hasql :> es) => Session a -> Eff es a
runSession = send . RunSession

runHasqlIO ::
  forall es a.
  ( IOE :> es,
    Reader Pool :> es,
    Error UsageError :> es
  ) =>
  Eff (Hasql : es) a ->
  Eff es a
runHasqlIO = interpret $ \_ -> \case
  RunSession session -> do
    pool <- ask @Pool
    r <- liftIO $ use pool session
    either throwError pure r

run :: Pool -> Eff [Hasql, Reader Pool, Error UsageError, IOE] a -> IO (Either (CallStack, UsageError) a)
run pool = runEff . runError @UsageError . runReader pool . runHasqlIO

runWithErrorHandler :: Pool -> (CallStack -> UsageError -> Eff '[IOE] a) -> Eff [Hasql, Reader Pool, Error UsageError, IOE] a -> IO a
runWithErrorHandler pool errHandler = runEff . runErrorWith @UsageError errHandler . runReader pool . runHasqlIO
