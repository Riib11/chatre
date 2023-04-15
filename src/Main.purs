module Main where

import Prelude
import AI.LLM.Chat as Chat
import Data.Default (default)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (logShow)

main :: Effect Unit
main =
  launchAff_ do
    msg <-
      Chat.chat default
        $ [ { role: "user", content: "What is a real and common cognitive bias that is widely aknowledged to be true by laypeople yet often denied by experts?" } ]
    logShow msg.content
