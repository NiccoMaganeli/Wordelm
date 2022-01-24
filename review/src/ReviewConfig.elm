module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import Review.Rule exposing (Rule)
import NoDebug.Log
import NoExposingEverything
import NoMissingTypeAnnotation
import NoMissingTypeAnnotationInLetIn
import NoPrematureLetComputation
import NoUnused.Dependencies
import NoBooleanCase
import NoLeftPizza

config : List Rule
config =
    [ NoDebug.Log.rule
    , NoExposingEverything.rule
    , NoMissingTypeAnnotation.rule
    , NoMissingTypeAnnotationInLetIn.rule
    , NoPrematureLetComputation.rule
    , NoUnused.Dependencies.rule
    , NoBooleanCase.rule
    , NoLeftPizza.rule NoLeftPizza.Redundant
    ]
