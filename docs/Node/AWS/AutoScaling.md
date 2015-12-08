## Module Node.AWS.AutoScaling

#### `AutoScaling`

``` purescript
data AutoScaling :: *
```

Foreign data type for the AutoScaling client object

#### `autoScaling`

``` purescript
autoScaling :: forall cfg. cfg -> AutoScaling
```

Creates an autoScaling object from a given partial configuration

This configuration must contain a `region` key.

```purescript
autoScaling {region: "us-east-1"}
```

#### `describeAutoScalingGroups`

``` purescript
describeAutoScalingGroups :: forall req err res eff. AutoScaling -> (err -> Eff eff Unit) -> (res -> Eff eff Unit) -> {  | req } -> Eff eff Unit
```

Callback-based version of the `DescribeAutoScalingGroups` AWS call

#### `setInstanceHealth`

``` purescript
setInstanceHealth :: forall req err res eff. AutoScaling -> (err -> Eff eff Unit) -> (res -> Eff eff Unit) -> {  | req } -> Eff eff Unit
```

Callback-based version of the `SetInstanceHealth` AWS call

#### `DescribeAutoScalingGroupsResponse`

``` purescript
data DescribeAutoScalingGroupsResponse
  = DescribeAutoScalingGroupsResponse { autoScalingGroups :: Array AutoScalingGroup }
```

Response returned by the `DescribeAutoScalingGroups` call

##### Instances
``` purescript
instance describeAutoScalingGroupsResponseIsForeign :: IsForeign DescribeAutoScalingGroupsResponse
```

#### `AutoScalingGroup`

``` purescript
data AutoScalingGroup
  = AutoScalingGroup { instances :: Array AutoScalingInstance }
```

Object representing an AWS AutoScalingGroup

##### Instances
``` purescript
instance autoScalingGroupIsForeign :: IsForeign AutoScalingGroup
```

#### `AutoScalingInstance`

``` purescript
data AutoScalingInstance
  = AutoScalingInstance { instanceId :: String }
```

Object representing an instance member of an AWS AutoScalingGroup

##### Instances
``` purescript
instance autoScalingInstanceIsForeign :: IsForeign AutoScalingInstance
```

#### `describeAutoScalingGroups'`

``` purescript
describeAutoScalingGroups' :: forall req eff. AutoScaling -> {  | req } -> Aff eff DescribeAutoScalingGroupsResponse
```

Aff-based version of the `DescribeAutoScalingGroups` AWS call

#### `SetInstanceHealthResponse`

``` purescript
data SetInstanceHealthResponse :: *
```

Response returned by the SetInstanceHealthResponse call

#### `setInstanceHealth'`

``` purescript
setInstanceHealth' :: forall req eff. AutoScaling -> {  | req } -> Aff eff SetInstanceHealthResponse
```

Aff-based version of the `SetInstanceHealth` AWS call

Callback-based version is [`setInstanceHealth`](#v:setInstanceHealth)


