module View.Posts exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing(..)
import Model exposing (Msg(..))
import Model.Post exposing (Post)
import Model.PostsConfig exposing (Change(..), PostsConfig, SortBy(..), filterPosts, sortFromString, sortOptions, sortToCompareFn, sortToString)
import Time
import Util.Time
import List exposing (filter)
import Model.PostsConfig as PostsConfig


{-| Show posts as a HTML [table](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table)

Relevant local functions:

  - Util.Time.formatDate
  - Util.Time.formatTime
  - Util.Time.formatDuration (once implemented)
  - Util.Time.durationBetween (once implemented)

Relevant library functions:

  - [Html.table](https://package.elm-lang.org/packages/elm/html/latest/Html#table)
  - [Html.tr](https://package.elm-lang.org/packages/elm/html/latest/Html#tr)
  - [Html.th](https://package.elm-lang.org/packages/elm/html/latest/Html#th)
  - [Html.td](https://package.elm-lang.org/packages/elm/html/latest/Html#td)

-}

displayTime : Time.Posix -> Time.Posix -> String
displayTime time timeNow = 
  let
    date = Util.Time.formatDate (Util.Time.posixToDate Time.utc time) 
    hour = Util.Time.formatTime Time.utc time 
    diff = Util.Time.formatDuration (Maybe.withDefault {seconds = 0, minutes = 0, hours = 0, days = 0} (Util.Time.durationBetween time timeNow))
  in
    date ++ " " ++ hour ++ " (" ++ diff ++ ")"

displayTableRows : List Post -> Time.Posix -> List (Html Msg)
displayTableRows posts timeNow= 
  let
    transformPostToRow = \post -> 
      tr []
        [
          td [class "post-score"] [text (String.fromInt(post.score))],
          td [class "post-title"] [text post.title],
          td [class "post-url"] [text (Maybe.withDefault "" post.url)],
          td [class "post-type"] [text post.type_],
          td [class "post-time"] [text (displayTime post.time timeNow)]
        ]
  in
    List.map transformPostToRow posts

postTable : PostsConfig -> Time.Posix -> List Post -> Html Msg
postTable postsConfig time postList =
  div [] 
    [
      table [] 
        [
          thead [] 
            [
              tr [] 
                [
                  th [] [text "Score"],
                  th [] [text "Title"],
                  th [] [text "Link"],
                  th [] [text "Type"],
                  th [] [text "Posted"]
                ]
            ],
          tbody [] (displayTableRows (filterPosts postsConfig postList) time)
        ]
    ]
    


{-| Show the configuration options for the posts table

Relevant functions:

  - [Html.select](https://package.elm-lang.org/packages/elm/html/latest/Html#select)
  - [Html.option](https://package.elm-lang.org/packages/elm/html/latest/Html#option)
  - [Html.input](https://package.elm-lang.org/packages/elm/html/latest/Html#input)
  - [Html.Attributes.type\_](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#type_)
  - [Html.Attributes.checked](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#checked)
  - [Html.Attributes.selected](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#selected)
  - [Html.Events.onCheck](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onCheck)
  - [Html.Events.onInput](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onInput)

-}
postsConfigView : PostsConfig -> Html Msg
postsConfigView postsConfig =
    div []
      [
        select [id "select-posts-per-page", onInput (\s -> s |> String.toInt |> Maybe.withDefault 0 |> PostsConfig.Show |> ConfigChanged)]
          [
            option [value "10", selected (postsConfig.postsToShow == 10)] [text "10"],
            option [value "25",selected (postsConfig.postsToShow == 25)] [text "25"],
            option [value "50", selected (postsConfig.postsToShow == 50)] [text "50"]
          ],
        select [id "select-sort-by", onInput (\s -> s |> PostsConfig.SortBy |> ConfigChanged)]
          [
            option [value "Score", selected (postsConfig.sortBy == Score)] [text "Score"],
            option [value "Title", selected (postsConfig.sortBy == Title)] [text "Title"],
            option [value "Posted", selected (postsConfig.sortBy == Posted)] [text "Posted"],
            option [value "None", selected (postsConfig.sortBy == None)] [text "None"]
          ],
        label [] 
          [
            input [type_ "checkbox", id "checkbox-show-job-posts", checked postsConfig.showJobs, onCheck (\b -> b |> PostsConfig.ShowJobs |> ConfigChanged)] [],
            text "Show job posts"
          ],
        label []
          [
            input [type_ "checkbox", id "checkbox-show-text-only-posts", checked postsConfig.showTextOnly, onCheck (\b -> b |> PostsConfig.ShowTextOnly |> ConfigChanged)] [],
            text "Show text only posts"
          ]
      ]
