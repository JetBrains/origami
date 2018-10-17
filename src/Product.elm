module Product exposing
    ( Product
    , getPalette
    , getName
    , decode
    )

type alias Color = String

type Product
    = JetBrains
    | Default1
    | Default2
    | IntelliJ
    | PhpStorm
    | PyCharm
    | RubyMine
    | WebStorm
    | CLion
    | DataGrip
    | AppCode
    | GogLand
    | Resharper
    | ResharperCpp
    | DotCover
    | DotMemory
    | DotPeek
    | DotTrace
    | Rider
    | TeamCity
    | YouTrack
    | UpSource
    | Hub
    | Kotlin
    | MPS
    | Unknown

getPalette : Product -> List Color
getPalette product =
    case product of
        JetBrains -> []
        _ -> []

getName : Product -> String
getName product =
    case product of
        JetBrains -> "JetBrains"
        _ -> ""

decode : String -> Product
decode id =
    case id of
        "jetbrains" -> JetBrains
        _ -> Unknown
