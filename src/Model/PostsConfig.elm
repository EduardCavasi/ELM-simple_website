module Model.PostsConfig exposing (Change(..), PostsConfig, SortBy(..), applyChanges, defaultConfig, filterPosts, sortFromString, sortOptions, sortToCompareFn, sortToString)

import Html.Attributes exposing (scope)
import Model.Post exposing (Post)
import Time


type SortBy
    = Score
    | Title
    | Posted
    | None


sortOptions : List SortBy
sortOptions =
    [ Score, Title, Posted, None ]


sortToString : SortBy -> String
sortToString sort =
    case sort of
        Score ->
            "Score"

        Title ->
            "Title"

        Posted ->
            "Posted"

        None ->
            "None"


{-|

    sortFromString "Score" --> Just Score

    sortFromString "Invalid" --> Nothing

    sortFromString "Title" --> Just Title

-}
sortFromString : String -> Maybe SortBy
sortFromString string =
    case string of
        "Score" -> Just Score
        "Title" -> Just Title
        "Posted" -> Just Posted
        _ -> Nothing
    


sortToCompareFn : SortBy -> (Post -> Post -> Order)
sortToCompareFn sort =
    case sort of
        Score ->
            \postA postB -> compare postB.score postA.score

        Title ->
            \postA postB -> compare postA.title postB.title

        Posted ->
            \postA postB -> compare (Time.posixToMillis postB.time) (Time.posixToMillis postA.time)

        None ->
            \_ _ -> EQ


type alias PostsConfig =
    { postsToFetch : Int
    , postsToShow : Int
    , sortBy : SortBy
    , showJobs : Bool
    , showTextOnly : Bool
    }


defaultConfig : PostsConfig
defaultConfig =
    PostsConfig 50 10 None False True


{-| A type that describes what option changed and how
-}
type Change
    = Fetch Int | Show Int | SortBy String | ShowJobs Bool | ShowTextOnly Bool


{-| Given a change and the current configuration, return a new configuration with the changes applied
-}
applyChanges : Change -> PostsConfig -> PostsConfig
applyChanges change postConfig =
    case change of
        Fetch x -> {postConfig | postsToFetch = x}
        Show x -> {postConfig | postsToShow = x}
        SortBy s -> {postConfig | sortBy = (Maybe.withDefault None (sortFromString s))}
        ShowJobs b -> {postConfig | showJobs = b}
        ShowTextOnly b -> {postConfig | showTextOnly = b}


{-| Given the configuration and a list of posts, return the relevant subset of posts according to the configuration

Relevant local functions:

  - sortToCompareFn

Relevant library functions:

  - List.sortWith

-}
filterPosts : PostsConfig -> List Post -> List Post
filterPosts filters posts =
    posts 
        |> List.take filters.postsToShow
        |> List.sortWith (sortToCompareFn filters.sortBy) 
        |> List.filter (\post -> if filters.showTextOnly == True then not (post.url == Nothing) else True)
        |> List.filter (\post -> if filters.showJobs == True then not (post.type_ == "job") else True)
        
    
