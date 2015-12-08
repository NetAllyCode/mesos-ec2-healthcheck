module Main where

import Prelude
import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (log, CONSOLE())
import Control.Monad.Eff.Exception (error, EXCEPTION())
import Control.Monad.ST
import Control.Monad.Error.Class

import Data.Array
import Data.Either
import Data.Maybe
import Data.Monoid
import Data.Foldable
import Data.Foreign
import Data.Foreign.Class
import qualified Data.StrMap as StrMap

import qualified Node.AWS as AWS
import Node.AWS.EC2
import Node.AWS.AutoScaling
import Node.HTTP
import qualified Node.HTTP.Client as HTTP
import Node.Stream
import Node.Yargs
import Node.Yargs.Applicative
import Node.Yargs.Setup

import Unsafe.Coerce

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

body :: MyResponse -> String
body (MyResponse res) = res.body

instance myResponseShow :: Show MyResponse where
    show (MyResponse res) = (show $ HTTP.statusCode res.response) ++ ": " ++ (show $ HTTP.statusMessage res.response) ++ "\n" ++ res.body

data SlavesResponse = SlavesResponse
    { slaves :: Array Slave
    }

instance slavesResponseIsForeign :: IsForeign SlavesResponse where
    read value = do
        let readProp' = flip readProp $ value
        slaves <- readProp' "slaves"
        return $ SlavesResponse { slaves: slaves }

data Slave = Slave
    { hostname :: String
    }

instance slaveIsForeign :: IsForeign Slave where
    read value = do
        let readProp' = flip readProp $ value
        hostname <- readProp' "hostname"
        return $ Slave { hostname: hostname }

realMain :: forall t eff. String -> Array String -> String -> Eff (http :: HTTP, err :: EXCEPTION, st :: ST AWS.Config, console :: CONSOLE | eff) Unit
realMain mesosMaster groups region = do
    log $ "master:\n    " ++ mesosMaster
    log $ "groups:\n    " ++ (show groups)
    launchAff do
        slavesResponse <- doRequest $ mesosMaster ++ "/master/slaves"
        slavesRRaw <- readResponseBody slavesResponse
        slavesR <- case (readJSON $ body slavesRRaw) :: F SlavesResponse of
                     Left _ -> throwError $ error "Couldn't parse SlavesResponse"
                     Right r -> pure r
        groupsResponse <- describeAutoScalingGroups' autoScalingClient {"AutoScalingGroupNames": groups}
        let instanceIds = ((map (\(AutoScalingInstance i) -> i.instanceId)) <<< mconcat <<< (map (\(AutoScalingGroup g) -> g.instances)) <<< (\(DescribeAutoScalingGroupsResponse r) -> r.autoScalingGroups)) groupsResponse
        instancesResponse <- describeInstances' ec2Client {"InstanceIds": instanceIds}
        let instances = (mconcat <<< (map (\(EC2Reservation r) -> r.instances)) <<< (\(DescribeInstancesResponse r) -> r.reservations)) instancesResponse
        forkAff do
            liftEff $ log "instances:"
            foldl (\a b -> a `bind` (const b)) mempty ((liftEff <<< log <<< (\(EC2Instance i) -> "    " ++ i.privateDnsName)) <$> instances)
        let slaves = ((map (\(Slave s) -> s.hostname)) <<< (\(SlavesResponse r) -> r.slaves)) slavesR
        forkAff do
            liftEff $ log "slaves:"
            foldl (\a b -> a `bind` (const b)) mempty ((liftEff <<< log <<< ((++) "    ")) <$> slaves)
        let slavesMap = foldl (\m a -> StrMap.insert a unit m) mempty slaves
        let member' = (flip StrMap.member $ slavesMap) <<< (\(EC2Instance i) -> i.privateDnsName)
        let deadInstances = filter (not member') instances
        forkAff do
            liftEff $ log "dead instances:"
            foldl (\a b -> a `bind` (const b)) mempty ((liftEff <<< log <<< ((++) "    ")) <<< (\(EC2Instance i) -> i.instanceId) <$> deadInstances)
    where autoScalingClient = autoScaling {region: region}
          ec2Client = ec2 {region: region}

main =
    let setup = usage "$0 -m master" in
    runY setup $ realMain <$> yarg "m" ["master"] Nothing (Right "a master must be supplied") true
                          <*> yarg "g" ["group"] Nothing (Left mempty) true
                          <*> yarg "r" ["region"] Nothing (Left "us-east-1") true
