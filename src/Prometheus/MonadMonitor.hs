module Prometheus.MonadMonitor (
    MonadMonitor (..)
,   Monitor
,   runMonitor
,   MonitorT
,   runMonitorT
) where

import Control.Applicative (Applicative)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Writer.Strict (WriterT, runWriterT, tell)


-- | MonadMonitor describes a class of Monads that are capable of performing
-- asynchronous IO operations.
class Monad m => MonadMonitor m where
    doIO :: IO () -> m ()

instance MonadMonitor IO where
    doIO = id

-- | Monitor allows the use of Prometheus metrics in pure code. When using
-- Monitor, all of the metric operations will be collected and queued into
-- a single IO () value that can be run from impure code.
--
-- Because all of the operations are performed asynchronously use of this class
-- is not recommended for use with metrics that are time sensitive (e.g. for
-- measuring latency).
type Monitor a = MonitorT Identity a

-- | MonitorT is the monad transformer analog of Monitor and allows for
-- monitoring pure monad transformer stacks.
newtype MonitorT m a = MkMonitorT (WriterT [IO ()] m a)
    deriving (Applicative, Functor, Monad, MonadTrans)

instance Monad m => MonadMonitor (MonitorT m) where
    doIO f = MkMonitorT $ tell [f]

-- | Extract a value and the corresponding monitor update value from the Monitor
-- monad. For an example use see 'Monitor'.
runMonitor :: Monitor a -> (a, IO ())
runMonitor a = runIdentity $ runMonitorT a

-- | Extract a value and the corresponding monitor update value from the
-- MonitorT monad transformer.
runMonitorT :: Monad m => MonitorT m a -> m (a, IO ())
runMonitorT (MkMonitorT writerT) = do
    (v, operations) <- runWriterT writerT
    return (v, sequence_ operations)
