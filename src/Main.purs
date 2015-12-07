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
    log $ "master: " ++ mesosMaster
    log $ "groups: " ++ (show groups)
    autoScalingClient <- (<$>) autoScaling $ AWS.awsConfig
    launchAff do
        slavesResponse <- doRequest $ mesosMaster ++ "/master/slaves"
        slavesR <- readResponseBody slavesResponse
        liftEff $ log $ show slavesR
        --groupsResponse <- describeAutoScalingGroups' autoScalingClient {}
        --let instanceIds = ((map (\AutoScalingInstance i) -> i.instanceId) <<< mconcat <<< (map (\(AutoScalingGroup g) -> g.instances)) <<< (\(DescribeAutoScalingGroupsResponse r) -> r.autoScalingGroups)) groupsResponse
        --foldl (\a b -> bind a (const b)) mempty (instanceIds <$> log)

main =
    let setup = usage "$0 -m master" in
    runY setup $ realMain <$> yarg "m" ["master"] Nothing (Right "a master must be supplied") true
                          <*> yarg "g" ["group"] Nothing (Left mempty) true
                          <*> yarg "r" ["region"] Nothing (Left "us-east-1") true
