module Main where

import Pathy
import Prelude
import AI.LLM.Chat as Chat
import Chatre.DoTask (doTaskZipper)
import Control.Monad.Reader (runReaderT)
import Data.Default (default)
import Data.List (List(..))
import Data.Maybe (fromJust)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log, logShow)

main :: Effect Unit
main =
  launchAff_ do
    let
      -- prompt = "Calculate the best diameter that a new city should have in order to maximize economic efficiency."
      -- prompt = "Write an interesting complicated creative original science-fiction short story on the topic of immortality."
      -- prompt = "Create a new trading card game with the theme of alchemy."
      -- prompt = "Design the rules for a new stategic card game about cryptography and politics."
      -- prompt = "Design a sustainable weight-loss plan for a chinese woman with low activity level, high sleep duration, and high fried food intake."
      -- prompt = "Design a new original simple deep strategic card game with a programming theme."
      -- prompt = "Design a new Magic the Gathering expansion using the themes 'flowers' and 'aliens'."
      -- prompt = "Design a new house concept that focusses on efficiency, indoor areas that are open to outdoors, and verticality"
      prompt = "Design a new house concept that focusses on water features and extreme verticality."
    void
      $ flip runReaderT
          { maxDepth: 1
          , outputDir: "results/"
          , title: prompt
          }
      $ doTaskZipper { path: Nil, prompt: { string: prompt } }
