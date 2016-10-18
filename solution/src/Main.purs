module Main where

import Prelude (Unit, bind, (>>=), ($))

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Var (($=))

import Data.Argonaut (jsonParser, decodeJson, encodeJson)
import Data.Argonaut.Core (stringify)
import Data.Either (Either(..))

import WebSocket
import WSC.Messages (IncomingMessage(..), OutgoingMessage(..))

main :: ∀ e. Eff (console :: CONSOLE, ws :: WEBSOCKET, err :: EXCEPTION | e) Unit
main = do
  Connection socket <- newWebSocket (URL "ws://localhost:8080") []

  socket.onopen $= \event -> do
    let json = stringify $ encodeJson $ ChallengeAccepted { name: "DaQuirm" }
    socket.send (Message json)

  socket.onmessage $= \event -> do
    let received = runMessage (runMessageEvent event)
    let decoded = jsonParser received >>= decodeJson
    case decoded of
      Left error -> do log error
      Right message -> do
        case message of
          Auth { authToken } -> do log authToken

  socket.onclose $= \event -> do
    log "onclose: Connection closed"
