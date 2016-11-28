module Debouncer
    exposing
        ( DebouncerState
        , BounceDetails
        , SelfMsg
        , create
        , bounce
        , process
        )

{-|

This is a delay-based debouncer. You can create it by defining a delay time.
For every bounce the timer "restarts" and the new message takes over. 
If there is no new message for a given id in the specified delay time frame, the last message is sent to your app. 


## Debouncer State

@docs DebouncerState
@docs create


## Starting and bouncing the Debouncer

@docs BounceDetails
@docs bounce


## Internal message handling

@docs SelfMsg
@docs process

-}

import Time exposing (Time)
import Task exposing (Task)
import Process
import Dict exposing (Dict)


type alias Id =
    String


{-| The state of tagged debouncers
-}
type DebouncerState
    = DebouncerState
        { delayTime : Time
        , pendingMessages : Dict Id Int
        }


{-| Pass these details to the bounce function to start / bounce the debouncer
-}
type alias BounceDetails a =
    { id : Id
    , msgToSend : a
    }


{-| Create tagged debouncers with a given delay time (shared accross all of them)
-}
create : Time -> DebouncerState
create delayTime =
    DebouncerState
        { delayTime = delayTime
        , pendingMessages = Dict.empty
        }


{-| Forward SelfMsgs to the process function
-}
type SelfMsg a
    = DelayExpired Id a


{-| Call bounce to start the debouncer as well as anytime the desired outccome changes.
-}
bounce : BounceDetails a -> DebouncerState -> ( DebouncerState, Cmd (SelfMsg a) )
bounce { id, msgToSend } (DebouncerState currentState) =
    let
        counterInc =
            Maybe.map ((+) 1) >> Maybe.withDefault 1 >> Just

        delayedCmd =
            Task.perform
                (always (DelayExpired id msgToSend))
                (Process.sleep currentState.delayTime)

        updatedState =
            { currentState
                | pendingMessages = currentState.pendingMessages |> Dict.update id counterInc
            }
    in
        ( DebouncerState updatedState, delayedCmd )


{-| Forward the debouncer's `SelfMsg`s to this function so that debouncer can manage its state and fire the delayed commands when it is neccessary
-}
process : SelfMsg a -> DebouncerState -> ( DebouncerState, Cmd a )
process (DelayExpired id msg) (DebouncerState state) =
    let
        remainingMessages =
            (state.pendingMessages |> Dict.get id |> Maybe.withDefault 0) - 1

    in
        if remainingMessages == 0 then
            DebouncerState
                { state
                    | pendingMessages = state.pendingMessages |> Dict.remove id
                }
                ! [ (Task.perform identity (Task.succeed msg)) ]
        else if remainingMessages > 0 then
            DebouncerState
                { state
                    | pendingMessages = state.pendingMessages |> Dict.insert id remainingMessages
                }
                ! []
        else
            Debug.crash "Invalid debouncer state: there was no state informtion for the supplied Id."
