effect module Debouncer where { command = MyCmd } exposing (bounce)

{-|

This is a delay-based debouncer. You can create it by defining a delay time.
For every bounce the timer "restarts" and the new message takes over. 
If there is no new message for a given id in the specified delay time frame, the last message is sent to your app. 


## Starting and bouncing the Debouncer

@docs bounce


-}

import Time exposing (Time)
import Task exposing (Task)
import Process
import Dict exposing (Dict)
import Platform


type alias Id =
    String


type MyCmd msg 
    = Bounce Id Time msg


{-| Call bounce to start the debouncer as well as anytime the desired outcome changes.
-}
bounce : Id -> Time -> msg -> Cmd msg
bounce id delay msg =
    command (Bounce id delay msg)


cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap tagger (Bounce id delay msg) =
    Bounce id delay (tagger msg)    




-- MANAGER

{-| The state of tagged debouncers
-}
type alias State =  
    Dict Id Int


init : Task Never State
init =
    Task.succeed Dict.empty


{-| Forward SelfMsgs to the process function
-}
type SelfMsg msg
    = DelayExpired Id msg


(&>) t1 t2 =
  Task.andThen (\_ -> t2) t1
  

onEffects : Platform.Router msg (SelfMsg msg) -> List (MyCmd msg) -> State -> Task Never State
onEffects router newCmds state =
    let
        counterInc =
            Maybe.map ((+) 1) >> Maybe.withDefault 1 >> Just

        toDelayedSelfMsg (Bounce id delay msg) =
            Process.spawn
                (Process.sleep delay &> Platform.sendToSelf router (DelayExpired id msg))

        sleepTasks =
            newCmds
                |> List.map toDelayedSelfMsg
                |> Task.sequence

        updatedState =
            newCmds
                |> List.foldr
                    (\(Bounce id _ _) state -> state |> Dict.update id counterInc)
                    state
    in
        sleepTasks &> Task.succeed updatedState


{-| The debouncer's `SelfMsg`s are forwarded to this function so that debouncer can manage its state 
and send back the delayed messages to the app when it is neccessary
-}
onSelfMsg : Platform.Router msg (SelfMsg msg) -> (SelfMsg msg) -> State -> Task Never State
onSelfMsg router (DelayExpired id msg) state =
    let
        remainingMessages =
            (state |> Dict.get id |> Maybe.withDefault 0) - 1

    in
        if remainingMessages == 0 then
            Platform.sendToApp router msg &> Task.succeed (state |> Dict.remove id)

        else if remainingMessages > 0 then
            Task.succeed (state |> Dict.insert id remainingMessages)

        else
            Task.succeed  (state |> Debug.log "Invalid debouncer state: there was no state informtion for the supplied Id.")
