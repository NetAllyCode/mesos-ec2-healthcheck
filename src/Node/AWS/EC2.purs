module Node.AWS.EC2 where

import Prelude
import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Error.Class

import Data.Either
import Data.Foreign
import Data.Foreign.Class

import Node.AWS

-- | EC2 client object
foreign import data EC2 :: *

-- | Creates a new EC2 client for a given partial AWS configuration
foreign import ec2 :: forall cfg. cfg -> EC2

-- | Callback-based version of the `DescribeInstances` AWS call
foreign import describeInstances :: forall req err res eff. EC2 -> (err -> Eff eff Unit) -> (res -> Eff eff Unit) -> {|req} -> Eff eff Unit

-- | Response returned by `DescribeInstances`
data DescribeInstancesResponse = DescribeInstancesResponse
    { reservations :: Array EC2Reservation
    }

instance describeInstancesResponseIsForeign :: IsForeign DescribeInstancesResponse where
    read value = do
        let readProp' = flip readProp $ value
        reservations <- readProp' "Reservations"
        return $ DescribeInstancesResponse { reservations: reservations }

-- | Object representing a single reservation of EC2 resources
data EC2Reservation = EC2Reservation
    { instances :: Array EC2Instance
    }

instance ec2ReservationIsForeign :: IsForeign EC2Reservation where
    read value = do
        let readProp' = flip readProp $ value
        instances <- readProp' "Instances"
        return $ EC2Reservation { instances: instances }

-- | Object representing a single EC2 instance
data EC2Instance = EC2Instance
    { instanceId :: String
    , privateDnsName :: String
    }

instance ec2InstanceIsForeign :: IsForeign EC2Instance where
    read value = do
        let readProp' = flip readProp $ value
        instanceId <- readProp' "InstanceId"
        privateDnsName <- readProp' "PrivateDnsName"
        return $ EC2Instance { instanceId: instanceId
                             , privateDnsName: privateDnsName
                             }

-- | Aff-based version of `DescribeInstances` AWS call
-- |
-- | Callback-based version is [`describeInstances`](#v:describeInstances)
describeInstances' :: forall req eff. EC2 -> {|req} -> Aff eff DescribeInstancesResponse
describeInstances' ec2Client req = do
    res <- makeAff (\err success -> describeInstances ec2Client err success req)
    case (read res) :: Either _ DescribeInstancesResponse of
      Left _ -> throwError $ error "Couldn't parse DescribeInstancesResponse"
      Right res -> return res
