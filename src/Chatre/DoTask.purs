module Chatre.DoTask where

import Chatre.Task
import Data.Tuple.Nested
import Prelude
import AI.LLM.Chat (ChatMessage, defaultChatOptions, gpt_3_5_turbo__model, gpt_4__model)
import AI.LLM.Chat as Chat
import Control.Semigroupoid (composeFlipped)
import Data.Array as Array
import Data.Default (default)
import Data.Either (Either(..))
import Data.Foldable (foldMap, foldl, intercalate, traverse_)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String as CodePoint
import Data.String as String
import Data.String.CodePoints as CodePoints
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Partial.Unsafe (unsafeCrashWith)

type M
  = Aff

logTaskZipper :: TaskZipper -> M Unit
logTaskZipper zipper = do
  log $ "==[ task zipper ]====================================================================="
  goPath 0 (List.reverse zipper.path)
  log $ "======================================================================================="
  where
  indent n = intercalate "" (Array.replicate n "  ")

  logIndented n = log <<< (indent n <> _)

  goPath n Nil = logIndented n $ "[>] " <> zipper.prompt.string

  goPath n (Cons tooth path') = do
    logIndented n $ "[>] " <> tooth.prompt.string
    goDone (n + 1) `traverse_` List.reverse tooth.subtasksDoneRev
    goPath (n + 1) path'
    (\prompt -> logIndented (n + 1) $ "[ ] " <> prompt.string) `traverse_` tooth.subtaskPromptsTodo

  goDone n (LeafDoneTaskTree t) = do
    logIndented n $ "[X] " <> t.prompt.string
    logIndented n $ "  % " <> t.result.string

  goDone n (BranchDoneTaskTree t) = do
    logIndented n $ "[X] " <> t.prompt.string
    goDone (n + 1) `traverse_` t.subtasks

doTaskZipper :: Int -> TaskZipper -> M DoneTaskTree
doTaskZipper maxDepth zipper = do
  logTaskZipper zipper
  let
    systemMessage = taskZipperToSystemMessage maxDepth zipper

    messages = taskZipperToChatMessages zipper
  -- log $ "[messages] " <> show messages
  response <- Chat.chat defaultChatOptions { model = gpt_4__model } systemMessage messages
  interpretResponse response zipper
    >>= case _ of
        Left zipper' -> doTaskZipper maxDepth zipper'
        Right done -> pure done

taskZipperToSystemMessage :: Int -> TaskZipper -> Maybe String
taskZipperToSystemMessage maxDepth zipper = do
  let
    prelude = "You are a general problem-solving agent. The user will give an initial task, and then you will have a dialogue with the user where you break the task into smaller sub-tasks. You are not allowed to use any external resources; you must only use your own knowledge and creativity."
  if List.length zipper.path < maxDepth then
    Just <<< intercalate "\n"
      $ [ prelude
        , ""
        , "You should respond EXACTLY in only one of the following ways:"
        , "  - If you are confident in the right result for a task, then you should answer with \"Result:\" followed by your result for the task. Your result can be only be up to one paragraph long. Only respond with your result."
        , "  - Otherwise, you should respond with \"To do:\" followed by a bulleted list of simpler sub-tasks that are sufficient to do this task. Each sub-task can be only one sentence long. Only respond with this bulleted list."
        ]
  else
    Just <<< intercalate "\n"
      $ [ prelude
        , ""
        , "You should respond with \"Result:\" followed by your result for the task. Your result can be only be up to one paragraph long. Only respond with your result."
        ]

summarizeDoneTaskTree :: DoneTaskTree -> Array Chat.ChatMessage
summarizeDoneTaskTree tree@(BranchDoneTaskTree _) = [ Chat.user $ go tree ]
  where
  go (BranchDoneTaskTree t) =
    intercalate "\n"
      $ renderTaskPrompt t.prompt
      : map
          (("- " <> _) >>> indentString)
          (go <$> t.subtasks)

  go (LeafDoneTaskTree t) = renderLeafDoneTaskTree t

  indentString :: String -> String
  indentString = String.split (String.Pattern "\n") >>> map ("  " <> _) >>> intercalate "\n"

summarizeDoneTaskTree (LeafDoneTaskTree t) =
  [ Chat.user $ renderTaskPrompt t.prompt
  , Chat.assistant $ renderTaskResult t.result
  ]

taskZipperToChatMessages :: TaskZipper -> Array Chat.ChatMessage
taskZipperToChatMessages zipper =
  Array.concat
    [ flip foldMap (List.reverse zipper.path) \tooth ->
        Array.concat
          [ [ Chat.user $ renderTaskPrompt tooth.prompt ]
          , Array.concat <<< Array.fromFoldable $ List.reverse tooth.subtasksDoneRev
              <#> summarizeDoneTaskTree
          ]
    , [ Chat.user $ renderTaskPrompt zipper.prompt ]
    ]

data Action
  = Subdivide
  | Complete

ignorePrefixCodePoints :: Array CodePoint.CodePoint
ignorePrefixCodePoints = CodePoints.toCodePointArray " -*"

interpretResponse :: ChatMessage -> TaskZipper -> M (Either TaskZipper DoneTaskTree)
interpretResponse msg zipper = do
  case extractAction msg of
    Subdivide /\ str -> do
      let
        prompts =
          str
            # String.split (String.Pattern "\n")
            >>> Array.filter (not <<< String.null)
            >>> map (String.dropWhile (_ `Array.elem` ignorePrefixCodePoints))
            >>> map String.trim
            >>> map (\prompt -> { string: prompt })
            >>> List.fromFoldable
      case prompts of
        Nil -> unsafeCrashWith "[interpretResponse] message has no prompts"
        Cons prompt prompts' ->
          pure
            $ Left
                { path:
                    { prompt: zipper.prompt
                    , subtasksDoneRev: Nil
                    , subtaskPromptsTodo: prompts'
                    }
                      : zipper.path
                , prompt
                }
    Complete /\ str -> do
      let
        result :: TaskResult
        result = { string: str }

        doneTaskTree :: DoneTaskTree
        doneTaskTree = LeafDoneTaskTree { prompt: zipper.prompt, result }
      case zipper.path of
        Nil ->
          pure <<< Right <<< LeafDoneTaskTree
            $ { prompt: zipper.prompt
              , result
              }
        Cons tooth path' -> do
          let
            next :: DoneTaskTree -> TaskZipper -> M (Either TaskZipper DoneTaskTree)
            next done zipper' = case zipper'.path of
              Nil -> pure $ Right done
              Cons tooth' path'' -> case tooth'.subtaskPromptsTodo of
                -- keep going up 
                Nil ->
                  -- it's correct that tooth'.prompt is in both the done tree and the zipper, since the zipper is meant to be to a head that has not been done yet 
                  next
                    (BranchDoneTaskTree { prompt: tooth'.prompt, subtasks: List.reverse (done : tooth'.subtasksDoneRev) })
                    { path: path'', prompt: tooth'.prompt }
                -- more subtasks to do in this subtree, to go to the next one
                Cons prompt prompts ->
                  pure <<< Left
                    $ { path: { prompt: tooth'.prompt, subtasksDoneRev: done : tooth'.subtasksDoneRev, subtaskPromptsTodo: prompts } : path''
                      , prompt
                      }
          case tooth.subtaskPromptsTodo of
            -- this was the last subtask of this subtree, so go up
            Nil -> next doneTaskTree zipper
            -- more subtasks to do in this subtree, to go to the next one
            Cons prompt prompts ->
              pure <<< Left
                $ { path: { prompt: tooth.prompt, subtasksDoneRev: doneTaskTree : tooth.subtasksDoneRev, subtaskPromptsTodo: prompts } : path'
                  , prompt
                  }

extractAction :: ChatMessage -> Action /\ String
extractAction msg = do
  let
    str = String.trim msg.content
  case String.stripPrefix (String.Pattern "To do:") str of
    Just str' -> Subdivide /\ str'
    Nothing -> case String.stripPrefix (String.Pattern "Solution:") str of
      Just str' -> Complete /\ str'
      -- Nothing -> unsafeCrashWith $ "[extractAction] bad response: " <> show msg
      Nothing -> Complete /\ str
