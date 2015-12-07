module Main where

import Prelude
import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (log, CONSOLE())
import Control.Monad.Eff.Exception (EXCEPTION())
import Control.Monad.ST

import Data.Either
import Data.Maybe
import Data.Monoid
import Data.Foldable

import qualified Node.AWS as AWS
import Node.AWS.EC2
import Node.AWS.AutoScaling
import Node.HTTP
import qualified Node.HTTP.Client as HTTP
import Node.Stream
import Node.Yargs
import Node.Yargs.Applicative
import Node.Yargs.Setup

import Util.Stream

doRequest uri =
    makeAff \_ success -> do
        req <- HTTP.requestFromURI uri success
        end (HTTP.requestAsStream req) (return unit)

data MyResponse = MyResponse 
    { response :: HTTP.Response
    , body :: String
    }

readResponseBody res = do
    let stream = HTTP.responseAsStream res
    str <- readStreamToString stream
    return $ MyResponse { response: res, body: str }

instance myResponseShow :: Show MyResponse where
    show (MyResponse res) = (show $ HTTP.statusCode res.response) ++ ": " ++ (show $ HTTP.statusMessage res.response) ++ "\n" ++ res.body

realMain :: forall t eff. String -> Array String -> String -> Eff (http :: HTTP, err :: EXCEPTION, st :: ST AWS.Config, console :: CONSOLE | eff) Unit
realMain mesosMaster groups region = do
    log $ "master:\n    " ++ mesosMaster
    log $ "groups:\n    " ++ (show groups)
    launchAff do
        slavesResponse <- doRequest $ mesosMaster ++ "/master/slaves"
        slavesR <- readResponseBody slavesResponse
        groupsResponse <- describeAutoScalingGroups' autoScalingClient {"AutoScalingGroupNames": groups}
        let instanceIds = ((map (\(AutoScalingInstance i) -> i.instanceId)) <<< mconcat <<< (map (\(AutoScalingGroup g) -> g.instances)) <<< (\(DescribeAutoScalingGroupsResponse r) -> r.autoScalingGroups)) groupsResponse
        forkAff do
            liftEff $ log "instances: "
            foldl (\a b -> a `bind` (const b)) mempty ((liftEff <<< log <<< (\a -> "    " ++ a)) <$> instanceIds)
        instancesResponse <- describeInstances' ec2Client {"InstanceIds": instanceIds}
        let instances = (mconcat <<< (map (\(EC2Reservation r) -> r.instances)) <<< (\(DescribeInstancesResponse r) -> r.reservations)) instancesResponse
        forkAff do
            liftEff $ log "describedInstances:"
            foldl (\a b -> a `bind` (const b)) mempty ((liftEff <<< log <<< (\(EC2Instance i) -> "    " ++ i.instanceId ++ "--" ++ i.privateDnsName)) <$> instances)
    where autoScalingClient = autoScaling {region: region}
          ec2Client = ec2 {region: region}

main =
    let setup = usage "$0 -m master" in
    runY setup $ realMain <$> yarg "m" ["master"] Nothing (Right "a master must be supplied") true
                          <*> yarg "g" ["group"] Nothing (Left mempty) true
                          <*> yarg "r" ["region"] Nothing (Left "us-east-1") true
