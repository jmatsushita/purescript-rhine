{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "rhine"
, dependencies = 
  [ "console"
  , "datetime"
  , "dunai"
  , "effect"
  , "either"
  , "free"
  , "freet"
  , "js-timers"
  , "leibniz"
  , "numerics"
  , "prelude"
  , "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
