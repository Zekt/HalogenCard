{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
    [ "affjax"
    , "console"
    , "css"
    , "effect"
    , "halogen"
    , "halogen-css"
    , "psci-support"
    , "web-dom"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
