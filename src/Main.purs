module Main where

import Prelude
import AI.LLM.Chat as Chat
import Chatre.DoTask (doTaskZipper)
import Data.Default (default)
import Data.List (List(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (logShow)

main :: Effect Unit
main =
  launchAff_ do
    -- msg <-
    --   Chat.chat default
    --     $ [ { role: "user", content: "What is a real and common cognitive bias that is widely aknowledged to be true by laypeople yet often denied by experts?" } ]
    -- logShow msg.content
    let
      -- prompt = "Calculate the best diameter that a new city should have in order to maximize economic efficiency."
      -- prompt = "Write an interesting complicated creative original science-fiction short story on the topic of immortality."
      -- prompt = "Create a new trading card game with the theme of alchemy."
      prompt = "Design the rules for a new stategic card game about cryptography and politics."
    void $ doTaskZipper 1 { path: Nil, prompt: { string: prompt } }
