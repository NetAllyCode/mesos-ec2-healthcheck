module Node.AWS.EC2 where

import Node.AWS

foreign import data EC2 :: *

foreign import ec2 :: Config -> EC2
