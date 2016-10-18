module WSC.Messages
  ( OutgoingMessage(..),
    IncomingMessage(..)
  ) where

import Prelude
import Data.Either (Either(..))

import Data.Argonaut
  ( class DecodeJson
  , class EncodeJson
  , fromString
  , decodeJson
  , jsonEmptyObject
  , (:=), (~>), (.?)
  )

data IncomingMessage
  = Auth { authToken :: String }

instance decodeJson :: DecodeJson IncomingMessage where
decodeJson json = do
  message <- decodeJson json
  msg <- message .? "msg"
  case msg of
    "auth" -> do
      authToken <- message .? "auth_token"
      pure $ Auth { authToken }
    _ -> Left "Bad message :("

data OutgoingMessage
  = ChallengeAccepted { name :: String }
  | TaskOneRequest { authToken :: String }

instance encodeJson :: EncodeJson OutgoingMessage where
encodeJson (ChallengeAccepted { name })
     = "msg" := "challenge_accepted"
    ~> "name" := name
    ~> jsonEmptyObject
encodeJson _ = fromString "nope"
