## Module Node.AWS

#### `Config`

``` purescript
data Config :: *
```

Foreign type for AWS configuration data

#### `awsConfig`

``` purescript
awsConfig :: forall eff. Eff (st :: ST Config | eff) Config
```

Returns the contents of the native `AWS.config` dictionary


