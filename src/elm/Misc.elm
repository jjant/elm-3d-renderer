module Misc exposing (unwrap)


unwrap : String -> Maybe a -> a
unwrap message mA =
    case mA of
        Just a ->
            a

        Nothing ->
            Debug.todo message
