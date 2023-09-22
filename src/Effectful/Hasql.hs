module Effectful.Hasql where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Hasql.Pool (Pool, UsageError, use)
import Hasql.Session (Session)

-- | Provides the ability to execute actions against a postgresql database
data Hasql :: Effect where
  RunSession :: Error UsageError :> es => Session a -> Hasql (Eff es) a

type instance DispatchOf Hasql = Dynamic

runSession :: (HasCallStack, Error UsageError :> es, Hasql :> es) => Session a -> Eff es a
runSession = send . RunSession

runHasqlIO ::
  forall es a.
  ( IOE :> es
  ) =>
  Pool ->
  Eff (Hasql : es) a ->
  Eff es a
runHasqlIO pool = interpret $ \env -> \case
  RunSession session -> do
    r <- liftIO $ use pool session
    localSeqUnlift env $ \unlift -> unlift $ do
      either throwError pure r

run :: Pool -> Eff [Hasql, Error UsageError, IOE] a -> IO (Either (CallStack, UsageError) a)
run pool = runEff . runError @UsageError . runHasqlIO pool
