## Module Node.AWS.EC2

#### `EC2`

``` purescript
data EC2 :: *
```

EC2 client object

#### `ec2`

``` purescript
ec2 :: forall cfg. cfg -> EC2
```

Creates a new EC2 client for a given partial AWS configuration

#### `describeInstances`

``` purescript
describeInstances :: forall req err res eff. EC2 -> (err -> Eff eff Unit) -> (res -> Eff eff Unit) -> {  | req } -> Eff eff Unit
```

Callback-based version of the `DescribeInstances` AWS call

#### `DescribeInstancesResponse`

``` purescript
data DescribeInstancesResponse
  = DescribeInstancesResponse { reservations :: Array EC2Reservation }
```

Response returned by `DescribeInstances`

##### Instances
``` purescript
instance describeInstancesResponseIsForeign :: IsForeign DescribeInstancesResponse
```

#### `EC2Reservation`

``` purescript
data EC2Reservation
  = EC2Reservation { instances :: Array EC2Instance }
```

Object representing a single reservation of EC2 resources

##### Instances
``` purescript
instance ec2ReservationIsForeign :: IsForeign EC2Reservation
```

#### `EC2Instance`

``` purescript
data EC2Instance
  = EC2Instance { instanceId :: String, privateDnsName :: String }
```

Object representing a single EC2 instance

##### Instances
``` purescript
instance ec2InstanceIsForeign :: IsForeign EC2Instance
```

#### `describeInstances'`

``` purescript
describeInstances' :: forall req eff. EC2 -> {  | req } -> Aff eff DescribeInstancesResponse
```

Aff-based version of `DescribeInstances` AWS call

Callback-based version is [`describeInstances`](#v:describeInstances)


