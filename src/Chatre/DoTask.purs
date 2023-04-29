module Chatre.DoTask where

import Chatre.Task
import Data.Tuple.Nested
import Prelude
import AI.LLM.Chat (ChatMessage, defaultChatOptions, gpt_3_5_turbo__model, gpt_4__model)
import AI.LLM.Chat as Chat
import Control.Monad.Reader (ReaderT, asks)
import Control.Monad.Writer (WriterT, censor, runWriterT, tell)
import Control.Semigroupoid (composeFlipped)
import Data.Array as Array
import Data.Default (default)
import Data.Either (Either(..))
import Data.Foldable (foldMap, foldl, intercalate, traverse_)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.String as String
import Data.String.CodePoints as CodePoints
import Data.String.NonEmpty (fromString)
import Data.String.NonEmpty as NonEmptyString
import Data.Tuple (fst, snd)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Effect.File (FilePath(..), writeFile)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

type M
  = ReaderT Ctx Aff

type Ctx
  = { title :: String
    , outputDir :: String
    , maxDepth :: Int
    }

type PrettyPrinter
  = WriterT (Array String) M Unit

prettyDoneTaskTree :: DoneTaskTree -> PrettyPrinter
prettyDoneTaskTree (BranchDoneTaskTree t) = do
  prettyTaskPrompt t.prompt
  indented do
    prettyDoneTaskTree `traverse_` t.subtasks

prettyDoneTaskTree (LeafDoneTaskTree t) = do
  prettyTaskPrompt t.prompt
  indented do
    prettyTaskResult t.result

prettyDoneTaskTree1 :: DoneTaskTree -> PrettyPrinter
prettyDoneTaskTree1 (BranchDoneTaskTree t) = do
  prettyDoneTaskPrompt t.prompt
  indented do
    prettyDoneTaskTree `traverse_` t.subtasks

prettyDoneTaskTree1 (LeafDoneTaskTree t) = do
  prettyDoneTaskPrompt t.prompt
  indented do
    prettyTaskResult t.result

-- if String.length str < column then
--     tell [ str ]
--   else do
--     let
--       { before: str1, after: str2 } = String.splitAt column str
--     tell [ str1 ]
--     prettyWrapString str2
--   where
--   column = 60
prettyWrapString :: String -> PrettyPrinter
prettyWrapString str = tell [ str ]

prettyTaskResult :: TaskResult -> PrettyPrinter
prettyTaskResult result = prettyWrapString (result.string <> "\n")

ensureEndPeriod :: String -> String
ensureEndPeriod str =
  let
    str' = String.trim str
  in
    str'
      # String.stripSuffix (String.Pattern ".")
      >>> maybe
          -- didn't have a period
          (str' <> ".")
          -- already had a preiod
          (_ <> ".")

prettyDoneTaskPrompt :: TaskPrompt -> PrettyPrinter
prettyDoneTaskPrompt prompt = addHeader "- [x] " $ prettyWrapString (ensureEndPeriod prompt.string)

prettyTaskPrompt :: TaskPrompt -> PrettyPrinter
prettyTaskPrompt prompt = addHeader "- " $ prettyWrapString (ensureEndPeriod prompt.string)

addHeader :: String -> PrettyPrinter -> PrettyPrinter
addHeader h =
  censor
    $ Array.mapWithIndex \i str ->
        if i == 0 then
          h <> str
        else
          spaces <> str
  where
  spaces = String.fromCodePointArray $ Array.replicate (String.length h) (String.codePointFromChar ' ')

prettyCurrentTaskPrompt :: TaskPrompt -> PrettyPrinter
prettyCurrentTaskPrompt prompt = addHeader "- [ ] " $ prettyWrapString (ensureEndPeriod prompt.string)

prettyTodoTaskPrompt :: TaskPrompt -> PrettyPrinter
prettyTodoTaskPrompt prompt = addHeader "- [ ] " $ prettyWrapString (ensureEndPeriod prompt.string)

prettyTaskZipper :: TaskZipper -> PrettyPrinter
prettyTaskZipper zipper = goPathRev zipper.path
  where
  goPathRev Nil = prettyCurrentTaskPrompt zipper.prompt

  goPathRev (Cons tooth Nil) = do
    prettyCurrentTaskPrompt tooth.prompt
    indented do
      prettyDoneTaskTree1 `traverse_` List.reverse tooth.subtasksDoneRev
      goPathRev Nil
      prettyTodoTaskPrompt `traverse_` tooth.subtaskPromptsTodo

  goPathRev (Cons tooth path) = do
    prettyCurrentTaskPrompt tooth.prompt
    indented do
      prettyDoneTaskTree `traverse_` List.reverse tooth.subtasksDoneRev
      goPathRev path
      prettyTodoTaskPrompt `traverse_` tooth.subtaskPromptsTodo

indented :: PrettyPrinter -> PrettyPrinter
indented = censor (map ("  " <> _))

logTaskZipper :: TaskZipper -> M Unit
logTaskZipper zipper = do
  log $ "==[ task zipper ]====================================================================="
  log =<< (intercalate "\n" <<< snd) <$> runWriterT (prettyTaskZipper zipper)
  log $ "======================================================================================="

writeTaskZipper :: TaskZipper -> M Unit
writeTaskZipper zipper = do
  content <- (intercalate "\n" <<< snd) <$> runWriterT (prettyTaskZipper zipper)
  title <- asks _.title
  outputDir <- asks _.outputDir
  writeFile content (FilePath $ outputDir <> "/" <> title <> ".md")

writeDoneTaskTree :: DoneTaskTree -> M Unit
writeDoneTaskTree done = do
  content <- (intercalate "\n" <<< snd) <$> runWriterT (prettyDoneTaskTree done)
  title <- asks _.title
  outputDir <- asks _.outputDir
  writeFile content (FilePath $ outputDir <> "/" <> title <> ".md")

doTaskZipper :: TaskZipper -> M DoneTaskTree
doTaskZipper zipper = do
  logTaskZipper zipper
  writeTaskZipper zipper
  systemMessage <- taskZipperToSystemMessage zipper
  let
    messages = taskZipperToChatMessages zipper
  response <-
    Chat.chat
      defaultChatOptions
        { model = gpt_4__model }
      systemMessage
      messages
  interpretResponse response zipper
    >>= case _ of
        Left zipper' -> doTaskZipper zipper'
        Right done -> do
          -- logShow done
          writeDoneTaskTree done
          pure done

taskZipperToSystemMessage :: TaskZipper -> M (Maybe String)
taskZipperToSystemMessage zipper = do
  maxDepth <- asks _.maxDepth
  let
    prelude = "You are a general problem-solving agent. The user will give an initial task, and then you will have a dialogue with the user where you break the task into smaller sub-tasks. You are not allowed to use any external resources; you must only use your own knowledge and creativity."
  if List.length zipper.path < maxDepth then
    pure <<< Just <<< intercalate "\n"
      $ [ prelude
        , ""
        , "You should respond EXACTLY in only one of the following ways:"
        , "  - If you are confident in the right result for a task, then you should answer with \"Result:\" followed by your result for the task. Your result can be only be up to one paragraph long. Only respond with your result."
        , "  - Otherwise, you should respond with \"To do:\" followed by a bulleted list of simpler sub-tasks that are sufficient to do this task. Each sub-task can be only one sentence long. Only respond with this bulleted list."
        ]
  else
    pure <<< Just <<< intercalate "\n"
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
  -- -- write BOTH prompt and result
  -- [ Chat.user $ renderTaskPrompt t.prompt
  -- , Chat.assistant $ renderTaskResult t.result
  -- ]
  -- write ONLY result
  [ Chat.assistant $ renderTaskResult t.result
  ]

taskZipperToChatMessages :: TaskZipper -> Array Chat.ChatMessage
taskZipperToChatMessages zipper =
  Array.concat
    [ flip foldMap (List.reverse zipper.path) \tooth ->
        Array.concat
          -- -- write BOTH prompt and result
          -- [ [ Chat.user $ renderTaskPrompt tooth.prompt ]
          -- , Array.concat <<< Array.fromFoldable $ List.reverse tooth.subtasksDoneRev
          --     <#> summarizeDoneTaskTree
          -- ]
          -- -- write ONLY result
          [ Array.concat <<< Array.fromFoldable $ List.reverse tooth.subtasksDoneRev
              <#> summarizeDoneTaskTree
          ]
    , [ Chat.user $ renderTaskPrompt zipper.prompt ]
    ]

data Action
  = Subdivide
  | Complete

ignorePrefixCodePoints :: Array CodePoints.CodePoint
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
                { path: { prompt: zipper.prompt, subtasksDoneRev: Nil, subtaskPromptsTodo: prompts' } : zipper.path
                , prompt
                }
    Complete /\ str -> do
      let
        result :: TaskResult
        result = { string: str }

        doneTaskTree :: DoneTaskTree
        doneTaskTree = LeafDoneTaskTree { prompt: zipper.prompt, result }
      case zipper.path of
        Nil -> pure <<< Right $ doneTaskTree
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
