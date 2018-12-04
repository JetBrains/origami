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
    , getId
    , ProductId
    )

type alias Color = String
type alias ProductId = Int

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
        JetBrains -> [ "#ad3259",  "#aa48ff", "#ffdb00" ]
        IntelliJ -> [ "#0b67cc",  "#fc31fe", "#ffd08d" ]
        PhpStorm ->  [ "#b345f1", "#765af8", "#ff318c" ]
        PyCharm -> [ "#09f58f", "#ed8b00", "#ffe400" ]
        RubyMine -> [ "#e52763", "#8f41cd", "#ea7211" ]
        WebStorm -> [ "#00cdd7", "#2086d7", "#fff045" ]
        CLion -> [ "#22d88f", "#029de0", "#ed358c" ]
        DataGrip -> [ "#22d88f", "#9775f8", "#ff59e6" ]
        AppCode -> [ "#247ce6", "#00daf0", "#1ddf93" ]
        GoLand -> [ "#0670c7", "#ea4fff", "#3bea62" ]
        ReSharper -> [ "#c21456", "#e14ce3", "#fdbc2c" ]
        ReSharperCpp ->  [ "#fdbc2c", "#e14ce3", "#c21456" ]
        DotCover -> [ "#ff7500", "#7866ff", "#e343e6" ]
        DotMemory -> [ "#ffbd00", "#7866ff", "#e343e6" ]
        DotPeek -> [ "#00caff", "#7866ff", "#e343e6" ]
        DotTrace -> [ "#fc1681", "#786bfb", "#e14ce3" ]
        Rider -> [ "#c90f5e", "#077cfb", "#fdb60d" ]
        TeamCity -> [ "#0cb0f2", "#905cfb", "#3bea62" ]
        YouTrack -> [ "#0cb0f2", "#905cfb", "#ff318c" ]
        UpSource -> [ "#22b1ef", "#9062f7", "#fd8224" ]
        Hub -> [ "#00b8f1", "#9758fb", "#ffee45" ]
        Kotlin -> [ "#22b1ef", "#9062f7", "#fd8224" ]
        MPS -> [ "#0b8fff", "#21d789", "#ffdc52" ]
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


getId : Product -> ProductId
getId product =
    let 
        labelToId = 
            [ "jetbrains"
            , "intellij-idea"
            , "phpstorm"
            , "pycharm"
            , "rubymine"
            , "webstorm"
            , "clion"
            , "datagrip"
            , "appcode"
            , "goland"
            , "resharper"
            , "resharper-cpp"
            , "dotcover"
            , "dotmemory"
            , "dotpeek"
            , "dottrace"
            , "rider"
            , "teamcity"
            , "youtrack"
            , "upsource"
            , "hub"
            , "kotlin"
            , "mps"
            ]
            |> List.indexedMap 
                (\index label -> ( index, label ))    
    in            
        labelToId 
            |> List.filterMap
                (\(index, label) -> 
                    if (encode product == label) 
                    then Just index
                    else Nothing
                )
            |> List.head
            |> Maybe.withDefault -1