{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "bouzuya-http-client"
    , "debug"
    , "httpure"
    , "node-process"
    , "node-url"
    , "psci-support"
    , "simple-json"
    , "test-unit"
    ]
, packages =
    ./packages.dhall
}
