module AI.LLM.Chat where

import Prelude
import Data.Array (uncons)
import Data.Default (class Default)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff, error, throwError)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.File (FilePath(..), writeFile)
import Partial.Unsafe (unsafeCrashWith)

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

createChatCompletion :: forall m. MonadAff m => ChatRequest -> m ChatResponse
createChatCompletion = liftAff <<< fromEffectFnAff <<< _createChatCompletion

type ChatOptions
  = { model :: String
    , temperature :: Number
    }

gpt_4__model :: String
gpt_4__model = "gpt-4"

gpt_3_5_turbo__model :: String
gpt_3_5_turbo__model = "gpt-3.5-turbo"

defaultChatOptions :: ChatOptions
defaultChatOptions =
  { model: gpt_3_5_turbo__model
  , temperature: 0.6
  }

chat :: forall m. MonadAff m => ChatOptions -> Maybe String -> Array ChatMessage -> m ChatMessage
chat opts mb_system messages = do
  let
    messages' = maybe [] (system >>> pure) mb_system <> messages
  writeFile (show messages') (FilePath "messages.json")
  response <-
    createChatCompletion
      { model: opts.model
      , temperature: opts.temperature
      , messages: messages'
      }
  case uncons response.choices of
    Nothing -> unsafeCrashWith $ "chat: response.choices is empty"
    Just { head: choice }
      | choice.finish_reason == "stop" -> pure choice.message
      | otherwise -> unsafeCrashWith $ "chat: response.finish_reason == " <> show choice.finish_reason

user :: String -> ChatMessage
user content = { role: "user", content }

assistant :: String -> ChatMessage
assistant content = { role: "assistant", content }

system :: String -> ChatMessage
system content = { role: "system", content }
