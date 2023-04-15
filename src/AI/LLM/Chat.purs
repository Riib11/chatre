module AI.LLM.Chat where

import Prelude
import Data.Array (uncons)
import Data.Default (class Default)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, error, throwError)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)

type ChatRequest
  = { model :: String
    , messages :: Array ChatMessage
    , temperature :: Number
    }

type ChatMessage
  = { role :: String
    , content :: String
    }

type ChatResponse
  = { id :: String
    , object :: String
    , created :: Number
    , usage ::
        { prompt_tokens :: Number
        , completion_tokens :: Number
        , total_tokens :: Number
        }
    , choices :: Array ChatChoice
    }

type ChatChoice
  = { message :: ChatMessage
    , finish_reason :: String
    , index :: Int
    }

foreign import _createChatCompletion :: ChatRequest -> EffectFnAff ChatResponse

createChatCompletion :: ChatRequest -> Aff ChatResponse
createChatCompletion = fromEffectFnAff <<< _createChatCompletion

newtype ChatOptions
  = ChatOptions
  { model :: String
  , temperature :: Number
  }

gpt_4__model :: String
gpt_4__model = "gpt-4"

gpt_3_5_turbo__model :: String
gpt_3_5_turbo__model = "gpt-3.5-turbo"

instance defaultChatOptions :: Default ChatOptions where
  default =
    ChatOptions
      { model: gpt_3_5_turbo__model
      , temperature: 0.6
      }

chat :: ChatOptions -> Array ChatMessage -> Aff ChatMessage
chat (ChatOptions opts) messages = do
  response <-
    createChatCompletion
      { model: opts.model
      , temperature: opts.temperature
      , messages
      }
  case uncons response.choices of
    Nothing -> throwError <<< error $ "chat: response.choices is empty"
    Just { head: choice }
      | choice.finish_reason == "stop" -> pure choice.message
      | otherwise -> throwError <<< error $ "chat: response.finish_reason == " <> show choice.finish_reason
