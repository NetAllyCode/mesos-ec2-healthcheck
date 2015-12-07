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

import Unsafe.Coerce

foreign import data AutoScaling :: *

foreign import autoScaling :: forall cfg. cfg -> AutoScaling

foreign import describeAutoScalingGroups :: forall req err res eff. AutoScaling -> (err -> Eff eff Unit) -> (res -> Eff eff Unit) -> {|req} -> Eff eff Unit

data DescribeAutoScalingGroupsResponse = DescribeAutoScalingGroupsResponse
    { autoScalingGroups :: Array AutoScalingGroup
    }

instance describeAutoScalingGroupsResponseIsForeign :: IsForeign DescribeAutoScalingGroupsResponse where
    read value = do
        let readProp' = flip readProp $ value
        autoScalingGroups <- readProp' "AutoScalingGroups"
        return $ DescribeAutoScalingGroupsResponse { autoScalingGroups: autoScalingGroups }

data AutoScalingGroup = AutoScalingGroup
    { instances :: Array AutoScalingInstance
    }

instance autoScalingGroupIsForeign :: IsForeign AutoScalingGroup where
    read value = do
        let readProp' = flip readProp $ value
        instances <- readProp' "Instances"
        return $ AutoScalingGroup { instances: instances }

data AutoScalingInstance = AutoScalingInstance
    { instanceId :: String
    }

instance autoScalingInstanceIsForeign :: IsForeign AutoScalingInstance where
    read value = do
        let readProp' = flip readProp $ value
        instanceId <- readProp' "InstanceId"
        return $ AutoScalingInstance { instanceId: instanceId }

describeAutoScalingGroups' :: forall req eff. AutoScaling -> {|req} -> Aff eff DescribeAutoScalingGroupsResponse
describeAutoScalingGroups' autoScalingClient req = do
    res <- makeAff (\err success -> describeAutoScalingGroups autoScalingClient err success req)
    case (read res) :: Either _ DescribeAutoScalingGroupsResponse of
      Left _ -> throwError $ error "Couldn't parse DescribeAutoScalingGroupsResponse"
      Right res -> return res
