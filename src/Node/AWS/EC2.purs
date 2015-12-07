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

foreign import data EC2 :: *

foreign import ec2 :: forall cfg. cfg -> EC2

foreign import describeInstances :: forall req err res eff. EC2 -> (err -> Eff eff Unit) -> (res -> Eff eff Unit) -> {|req} -> Eff eff Unit

data DescribeInstancesResponse = DescribeInstancesResponse
    { reservations :: Array EC2Reservation
    }

instance describeInstancesResponseIsForeign :: IsForeign DescribeInstancesResponse where
    read value = do
        let readProp' = flip readProp $ value
        reservations <- readProp' "Reservations"
        return $ DescribeInstancesResponse { reservations: reservations }

data EC2Reservation = EC2Reservation
    { instances :: Array EC2Instance
    }

instance ec2ReservationIsForeign :: IsForeign EC2Reservation where
    read value = do
        let readProp' = flip readProp $ value
        instances <- readProp' "Instances"
        return $ EC2Reservation { instances: instances }

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

describeInstances' :: forall req eff. EC2 -> {|req} -> Aff eff DescribeInstancesResponse
describeInstances' ec2Client req = do
    res <- makeAff (\err success -> describeInstances ec2Client err success req)
    case (read res) :: Either _ DescribeInstancesResponse of
      Left _ -> throwError $ error "Couldn't parse DescribeInstancesResponse"
      Right res -> return res
