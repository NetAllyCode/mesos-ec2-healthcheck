module Node.AWS.AutoScaling where

import Prelude
import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Error.Class

import Data.Either
import Data.Foreign
import Data.Foreign.Class

import Node.AWS

-- | Foreign data type for the AutoScaling client object
foreign import data AutoScaling :: *

-- | Creates an autoScaling object from a given partial configuration
-- |
-- | This configuration must contain a `region` key.
-- |
-- | ```purescript
-- | autoScaling {region: "us-east-1"}
-- | ```
foreign import autoScaling :: forall cfg. cfg -> AutoScaling

-- | Callback-based version of the `DescribeAutoScalingGroups` AWS call
foreign import describeAutoScalingGroups :: forall req err res eff. AutoScaling -> (err -> Eff eff Unit) -> (res -> Eff eff Unit) -> {|req} -> Eff eff Unit

-- | Callback-based version of the `SetInstanceHealth` AWS call
foreign import setInstanceHealth :: forall req err res eff. AutoScaling -> (err -> Eff eff Unit) -> (res -> Eff eff Unit) -> {|req} -> Eff eff Unit

-- | Response returned by the `DescribeAutoScalingGroups` call
data DescribeAutoScalingGroupsResponse = DescribeAutoScalingGroupsResponse
    { autoScalingGroups :: Array AutoScalingGroup
    }

instance describeAutoScalingGroupsResponseIsForeign :: IsForeign DescribeAutoScalingGroupsResponse where
    read value = do
        let readProp' = flip readProp $ value
        autoScalingGroups <- readProp' "AutoScalingGroups"
        return $ DescribeAutoScalingGroupsResponse { autoScalingGroups: autoScalingGroups }

-- | Object representing an AWS AutoScalingGroup
data AutoScalingGroup = AutoScalingGroup
    { instances :: Array AutoScalingInstance
    }

instance autoScalingGroupIsForeign :: IsForeign AutoScalingGroup where
    read value = do
        let readProp' = flip readProp $ value
        instances <- readProp' "Instances"
        return $ AutoScalingGroup { instances: instances }

-- | Object representing an instance member of an AWS AutoScalingGroup
data AutoScalingInstance = AutoScalingInstance
    { instanceId :: String
    }

instance autoScalingInstanceIsForeign :: IsForeign AutoScalingInstance where
    read value = do
        let readProp' = flip readProp $ value
        instanceId <- readProp' "InstanceId"
        return $ AutoScalingInstance { instanceId: instanceId }

-- | Aff-based version of the `DescribeAutoScalingGroups` AWS call
describeAutoScalingGroups' :: forall req eff. AutoScaling -> {|req} -> Aff eff DescribeAutoScalingGroupsResponse
describeAutoScalingGroups' autoScalingClient req = do
    res <- makeAff (\err success -> describeAutoScalingGroups autoScalingClient err success req)
    case (read res) :: Either _ DescribeAutoScalingGroupsResponse of
      Left _ -> throwError $ error "Couldn't parse DescribeAutoScalingGroupsResponse"
      Right res -> return res

-- | Response returned by the SetInstanceHealthResponse call
foreign import data SetInstanceHealthResponse :: *

-- | Aff-based version of the `SetInstanceHealth` AWS call
-- |
-- | Callback-based version is [`setInstanceHealth`](#v:setInstanceHealth)
setInstanceHealth' :: forall req eff. AutoScaling -> {|req} -> Aff eff SetInstanceHealthResponse
setInstanceHealth' autoScalingClient req =
    makeAff (\err success -> setInstanceHealth autoScalingClient err success req)
