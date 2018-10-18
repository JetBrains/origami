module Product exposing
    ( Product(..)
    , getPalette
    , getName
    , decode
    , getLogoPath
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
        JetBrains -> [ "#9151e1",  "#ec4476", "#fde74a" ]
        Default1 -> [ "#f45b69",  "#e4fde1", "#rgba(0,0,0,0)" ]
        Default2 -> [ "#4b4e76",  "#fb4e76", "#rgba(0,0,0,0)" ]
        IntelliJ -> [ "#087cfa",  "#fe315d", "#f97a12" ]
        PhpStorm ->  [ "#b24eee", "#7660f4", "#fc378c" ]
        PyCharm -> [ "#21d789", "#fcf84a", "#07c3f2" ]
        RubyMine -> [ "#fc2555", "#fd8638", "#8f41cd" ]
        WebStorm -> [ "#22cdd6", "#2888d4", "#feee56" ]
        CLion -> [ "#32d791", "#1a9edd", "#ea3a8c" ]
        DataGrip -> [ "#32d791", "#9779f5", "#fd5fe4" ]
        AppCode -> [ "#2b7fe3", "#25daee", "#30de95" ]
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
        Default1 -> "Default 1"
        Default2 -> "Default 2"
        Unknown -> "Unknown"


decode : String -> Product
decode id =
    case id of
        "jetbrains" -> JetBrains
        "default1" -> Default1
        "default2" -> Default2
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


getLogoPath : Product -> Maybe String
getLogoPath product =
    (case product of
        JetBrains -> Just "jetbrains"
        IntelliJ -> Just "intellij-idea"
        PhpStorm -> Just "phpstorm"
        PyCharm -> Just "pycharm"
        RubyMine -> Just "rubymine"
        WebStorm -> Just "webstorm"
        CLion -> Just "clion"
        DataGrip -> Just "datagrip"
        AppCode -> Just "appcode"
        GogLand -> Just "gogland"
        Resharper -> Just "resharper"
        ResharperCpp -> Just "resharper-cpp"
        DotCover -> Just "dotcover"
        DotMemory -> Just "dotmemory"
        DotPeek -> Just "dotpeek"
        DotTrace -> Just "dottrace"
        Rider -> Just "rider"
        TeamCity -> Just "teamcity"
        YouTrack -> Just "youtrack"
        UpSource -> Just "upsource"
        Hub -> Just "hub"
        Kotlin -> Just "kotlin"
        MPS -> Just "mps"
        _ -> Nothing)
            |> Maybe.map (\fileName -> fileName ++ ".svg")
