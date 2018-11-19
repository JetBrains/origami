module Product exposing
    ( Product(..)
    , Palette
    , getPalette
    , getName
    , decode
    , encode
    , getLogoPath
    , getTextLinePath
    )


type alias Color = String

type alias Palette = List Color


type Product
    = JetBrains
    | Mono
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


-- These were the default ones

-- layerOneConfig.lights.ambient = [ '#000000', '#f45b69' ];
-- layerOneConfig.lights.diffuse = [ '#000000', '#e4fde1' ];

-- layerTwoConfig.lights.ambient = [ '#000000', '#4b4e76' ];
-- layerOneConfig.lights.diffuse = [ '#000000', '#fb4e76' ];

getPalette : Product -> Palette
getPalette product =
    case product of
        JetBrains -> [ "#ad3259",  "#aa489a", "#ffdb2e" ]
        Mono -> [ "#ffffff",  "#aaaaaa", "#000000" ]
        IntelliJ -> [ "#003976",  "#fc31fe", "#ffd08d" ]
        PhpStorm ->  [ "#bb43e6", "#9034b1", "#f93394" ]
        PyCharm -> [ "#006137", "#fcf84a", "#f9ff93" ]
        RubyMine -> [ "#fc2555", "#fd8638", "#8f41cd" ]
        WebStorm -> [ "#22cdd6", "#2888d4", "#feee56" ]
        CLion -> [ "#32d791", "#1a9edd", "#ea3a8c" ]
        DataGrip -> [ "#32d791", "#9779f5", "#fd5fe4" ]
        AppCode -> [ "#2b7fe3", "#30de95", "#25daee" ]
        GogLand -> [ "#078efc", "#bb4efc", "#3bea62" ]
        Resharper -> [ "#c21456", "#e14ce3", "#fdbc2c" ]
        ResharperCpp ->  [ "#fdbc2c", "#e14ce3", "#c21456" ]
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
        GogLand -> "GogLand"
        Resharper -> "Resharper"
        ResharperCpp -> "Resharper C++"
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
        Mono -> "Mono"
        Unknown -> "Unknown"


decode : String -> Product
decode id =
    case id of
        "jetbrains" -> JetBrains
        "mono" -> Mono
        "intellij-idea" -> IntelliJ
        "phpstorm" -> PhpStorm
        "pycharm" -> PyCharm
        "rubymine" -> RubyMine
        "webstorm" -> WebStorm
        "clion" -> CLion
        "datagrip" -> DataGrip
        "appcode" -> AppCode
        "gogland" -> GogLand
        "resharper" -> Resharper
        "resharper-cpp" -> ResharperCpp
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
        GogLand -> "gogland"
        Resharper -> "resharper"
        ResharperCpp -> "resharper-cpp"
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
        Mono -> "mono"
        Unknown -> "unknown"


getLogoPath : Product -> Maybe String
getLogoPath product =
    (case product of
        Unknown -> Nothing
        Mono -> Nothing
        product -> Just (encode product))
            |> Maybe.map (\fileName -> fileName ++ ".svg")


getTextLinePath : Product -> Maybe String
getTextLinePath product =
    (case product of
        Unknown -> Nothing
        Mono -> Nothing
        product -> Just (encode product))
            |> Maybe.map (\fileName -> fileName ++ "-text.svg")