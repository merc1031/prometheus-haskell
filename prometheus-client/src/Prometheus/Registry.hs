{-# LANGUAGE ExistentialQuantification #-}
module Prometheus.Registry (
    register
,   register'
,   registerIO
,   registerIO'
,   unsafeRegister
,   unsafeRegister'
,   unsafeRegisterIO
,   unsafeRegisterIO'
,   collectMetrics
,   unregisterAll
,   unregister
) where

import Prometheus.Metric

import Control.Applicative ((<$>))
import Control.Monad.IO.Class
import Data.Semigroup ((<>))
import Data.Unique
import System.IO.Unsafe (unsafePerformIO)
import qualified Control.Concurrent.STM as STM


-- $setup
-- >>> :module +Prometheus
-- >>> unregisterAll

-- | A 'Registry' is a list of all registered metrics, currently represented by
-- their sampling functions.
type Registry = [(O, IO [SampleGroup])]

newtype O = O Unique
  deriving Eq

{-# NOINLINE globalRegistry #-}
globalRegistry :: STM.TVar Registry
globalRegistry = unsafePerformIO $ STM.newTVarIO []

-- | Registers a metric with the global metric registry.
register :: MonadIO m => Metric s -> m s
register = fmap fst . register'

-- | Registers a metric with the global metric registry and returns an opaque handle.
register' :: MonadIO m => Metric s -> m (s, O)
register' (Metric mk) = liftIO $ do
    (metric, sampleGroups) <- mk
    o <- O <$> newUnique
    let addToRegistry = ((o, sampleGroups) :)
    liftIO $ STM.atomically $ STM.modifyTVar' globalRegistry addToRegistry
    return (metric, o)

-- | Registers a metric with the global metric registry.
registerIO :: MonadIO m => m (Metric s) -> m s
registerIO metricGen = metricGen >>= register
--
-- | Registers a metric with the global metric registry and returns an opaque handle.
registerIO' :: MonadIO m => m (Metric s) -> m (s, O)
registerIO' metricGen = metricGen >>= register'

-- | Registers a metric with the global metric registry.
--
-- __IMPORTANT__: This method should only be used to register metrics as top
-- level symbols, it should not be run from other pure code.
unsafeRegister :: Metric s -> s
unsafeRegister = unsafePerformIO . register

-- | Registers a metric with the global metric registry and returns an opaque handle.
--
-- __IMPORTANT__: This method should only be used to register metrics as top
-- level symbols, it should not be run from other pure code.
unsafeRegister' :: Metric s -> (s, O)
unsafeRegister' = unsafePerformIO . register'

-- | Registers a metric with the global metric registry.
--
-- __IMPORTANT__: This method should only be used to register metrics as top
-- level symbols, it should not be run from other pure code.
--
-- For example,
--
-- >>> :{
--  {-# NOINLINE c #-}
--  let c = unsafeRegisterIO $ counter (Info "my_counter" "An example metric")
-- :}
-- ...
unsafeRegisterIO :: IO (Metric s) -> s
unsafeRegisterIO = unsafePerformIO . registerIO

-- | Registers a metric with the global metric registry and returns an opaque handle.
--
-- __IMPORTANT__: This method should only be used to register metrics as top
-- level symbols, it should not be run from other pure code.
--
-- For example,
--
-- >>> :{
--  {-# NOINLINE c #-}
--  let c = unsafeRegisterIO' $ counter (Info "my_counter" "An example metric")
-- :}
-- ...
unsafeRegisterIO' :: IO (Metric s) -> (s, O)
unsafeRegisterIO' = unsafePerformIO . registerIO'

-- | Removes all currently registered metrics from the registry.
unregisterAll :: MonadIO m => m ()
unregisterAll = liftIO $ STM.atomically $ STM.writeTVar globalRegistry []

-- | Removes a currently registered metrics from the registry using the opaque handle to it.
unregister :: MonadIO m => O -> m ()
unregister i = liftIO $ STM.atomically $ STM.modifyTVar' globalRegistry (filter ((== i) . fst))

-- | Collect samples from all currently registered metrics. In typical use cases
-- there is no reason to use this function, instead you should use
-- `exportMetricsAsText` or a convenience library.
--
-- This function is likely only of interest if you wish to export metrics in
-- a non-supported format for use with another monitoring service.
collectMetrics :: MonadIO m => m [SampleGroup]
collectMetrics = liftIO $ do
    registry <- STM.atomically $ STM.readTVar globalRegistry
    concat <$> sequence [snd r | r <- registry]
