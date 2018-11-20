module Product exposing
    ( Product(..)
    , Palette
    , getPalette
    , getName
    , decode
    , encode
    , getLogoPath
    , getTextLinePath
    , getCoverTextSize
    )


type alias Color = String

type alias Palette = List Color


type Product
    = JetBrains
    | IntelliJ
    | PhpStorm
    | PyCharm
    | RubyMine
    | WebStorm
    | CLion
    | DataGrip
    | AppCode
    | GoLand
    | ReSharper
    | ReSharperCpp
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


-- These were the default ones

-- layerOneConfig.lights.ambient = [ '#000000', '#f45b69' ];
-- layerOneConfig.lights.diffuse = [ '#000000', '#e4fde1' ];

-- layerTwoConfig.lights.ambient = [ '#000000', '#4b4e76' ];
-- layerOneConfig.lights.diffuse = [ '#000000', '#fb4e76' ];

getPalette : Product -> Palette
getPalette product =
    case product of
        JetBrains -> [ "#ad3259",  "#aa489a", "#ffdb2e" ]
        IntelliJ -> [ "#003976",  "#fc31fe", "#ffd08d" ]
        PhpStorm ->  [ "#bb43e6", "#9034b1", "#f93394" ]
        PyCharm -> [ "#006137", "#fcf84a", "#f9ff93" ]
        RubyMine -> [ "#fc2555", "#fd8638", "#8f41cd" ]
        WebStorm -> [ "#22cdd6", "#2888d4", "#feee56" ]
        CLion -> [ "#32d791", "#1a9edd", "#ea3a8c" ]
        DataGrip -> [ "#32d791", "#9779f5", "#fd5fe4" ]
        AppCode -> [ "#2b7fe3", "#30de95", "#25daee" ]
        GoLand -> [ "#078efc", "#bb4efc", "#3bea62" ]
        ReSharper -> [ "#c21456", "#e14ce3", "#fdbc2c" ]
        ReSharperCpp ->  [ "#fdbc2c", "#e14ce3", "#c21456" ]
        DotCover -> [ "#fd7522", "#786bfb", "#e14ce3" ]
        DotMemory -> [ "#fdbc2c", "#786bfb", "#e14ce3" ]
        DotPeek -> [ "#23cbfc", "#786bfb", "#e14ce3" ]
        DotTrace -> [ "#fc1681", "#786bfb", "#e14ce3" ]
        Rider -> [ "#c40b55", "#e800ca", "#ffbd00" ]
        TeamCity -> [ "#22b1ef", "#9062f7", "#46e869" ]
        YouTrack -> [ "#22b1ef", "#9062f7", "#fc378c" ]
        UpSource -> [ "#22b1ef", "#9062f7", "#fd8224" ]
        Hub -> [ "#1fb9ee", "#965ff7", "#feec56" ]
        Kotlin -> [ "#1b84f2", "#24dea7", "#ed4baa" ]
        MPS -> [ "#31d68b", "#3188cd", "#f1e969" ]
        Unknown -> [ "#9151e1",  "#ec4476", "#fde74a" ]



getName : Product -> String
getName product =
    case product of
        JetBrains -> "JetBrains"
        IntelliJ -> "Intellij Idea"
        PhpStorm -> "PhpStorm"
        PyCharm -> "PyCharm"
        RubyMine -> "RubyMine"
        WebStorm -> "WebStorm"
        CLion -> "CLion"
        DataGrip -> "DataGrip"
        AppCode -> "AppCode"
        GoLand -> "GoLand"
        ReSharper -> "ReSharper"
        ReSharperCpp -> "ReSharper C++"
        DotCover -> "DotCover"
        DotMemory -> "DotMemory"
        DotPeek -> "DotPeek"
        DotTrace -> "DotTrace"
        Rider -> "Rider"
        TeamCity -> "TeamCity"
        YouTrack -> "YouTrack"
        UpSource -> "UpSource"
        Hub -> "Hub"
        Kotlin -> "Kotlin"
        MPS -> "MPS"
        Unknown -> "Unknown"


decode : String -> Product
decode id =
    case id of
        "jetbrains" -> JetBrains
        "intellij-idea" -> IntelliJ
        "phpstorm" -> PhpStorm
        "pycharm" -> PyCharm
        "rubymine" -> RubyMine
        "webstorm" -> WebStorm
        "clion" -> CLion
        "datagrip" -> DataGrip
        "appcode" -> AppCode
        "goland" -> GoLand
        "resharper" -> ReSharper
        "resharper-cpp" -> ReSharperCpp
        "dotcover" -> DotCover
        "dotmemory" -> DotMemory
        "dotpeek" -> DotPeek
        "dottrace" -> DotTrace
        "rider" -> Rider
        "teamcity" -> TeamCity
        "youtrack" -> YouTrack
        "upsource" -> UpSource
        "hub" -> Hub
        "kotlin" -> Kotlin
        "mps" -> MPS
        _ -> Unknown


encode : Product -> String
encode product =
    case product of
        JetBrains -> "jetbrains"
        IntelliJ -> "intellij-idea"
        PhpStorm -> "phpstorm"
        PyCharm -> "pycharm"
        RubyMine -> "rubymine"
        WebStorm -> "webstorm"
        CLion -> "clion"
        DataGrip -> "datagrip"
        AppCode -> "appcode"
        GoLand -> "goland"
        ReSharper -> "resharper"
        ReSharperCpp -> "resharper-cpp"
        DotCover -> "dotcover"
        DotMemory -> "dotmemory"
        DotPeek -> "dotpeek"
        DotTrace -> "dottrace"
        Rider -> "rider"
        TeamCity -> "teamcity"
        YouTrack -> "youtrack"
        UpSource -> "upsource"
        Hub -> "hub"
        Kotlin -> "kotlin"
        MPS -> "mps"
        Unknown -> "unknown"


getLogoPath : Product -> Maybe String
getLogoPath product =
    (case product of
        Unknown -> Nothing
        product -> Just (encode product))
            |> Maybe.map (\fileName -> fileName ++ ".svg")


getTextLinePath : Product -> Maybe String
getTextLinePath product =
    (case product of
        Unknown -> Nothing
        product -> Just (encode product))
            |> Maybe.map (\fileName -> fileName ++ "-text.svg")


getCoverTextSize : Product -> ( Int, Int )
getCoverTextSize product =
    case product of
        IntelliJ -> ( 616, 90 )
        PhpStorm -> ( 518, 108 )
        PyCharm ->  ( 479, 108 )
        RubyMine -> ( 502, 108 )
        WebStorm -> ( 567, 90 )
        CLion -> ( 299, 90 )
        DataGrip -> ( 468, 108 )
        AppCode -> ( 518, 108 )
        GoLand -> ( 419, 90 )
        ReSharper -> ( 546, 108 )
        ReSharperCpp -> ( 763, 108 )
        DotCover -> ( 490, 90 )
        DotMemory -> ( 620, 108 )
        DotPeek -> ( 444, 90 )
        DotTrace -> ( 461, 90 )
        Rider -> ( 273, 90 )
        TeamCity -> ( 495, 108 )
        YouTrack -> ( 485, 90 )
        UpSource -> ( 490, 104 )
        Hub -> ( 211, 90 )
        Kotlin -> ( 323, 99 )
        MPS -> ( 200, 77 )
        _ -> ( 90, 90 )

