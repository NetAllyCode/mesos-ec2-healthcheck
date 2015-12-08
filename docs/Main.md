## Module Main

#### `MyResponse`

``` purescript
data MyResponse
  = MyResponse { response :: Response, body :: String }
```

##### Instances
``` purescript
instance myResponseShow :: Show MyResponse
```

#### `body`

``` purescript
body :: MyResponse -> String
```

#### `SlavesResponse`

``` purescript
data SlavesResponse
  = SlavesResponse { slaves :: Array Slave }
```

##### Instances
``` purescript
instance slavesResponseIsForeign :: IsForeign SlavesResponse
```

#### `Slave`

``` purescript
data Slave
  = Slave { hostname :: String }
```

##### Instances
``` purescript
instance slaveIsForeign :: IsForeign Slave
```

#### `seqCompose`

``` purescript
seqCompose :: forall m a b. (Bind m) => m a -> m b -> m b
```

#### `(>>)`

``` purescript
(>>) :: forall m a b. (Bind m) => m a -> m b -> m b
```

_left-associative / precedence 1_

#### `realMain`

``` purescript
realMain :: forall t eff. String -> Array String -> String -> Eff (http :: HTTP, err :: EXCEPTION, st :: ST Config, console :: CONSOLE | eff) Unit
```


