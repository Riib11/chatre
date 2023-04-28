module Chatre.Task where

import Prelude
import Data.List (List)

type TaskPrompt
  = { string :: String
    }

renderTaskPrompt :: TaskPrompt -> String
renderTaskPrompt tp = tp.string

type TaskResult
  = { string :: String
    }

renderTaskResult :: TaskResult -> String
renderTaskResult tr = tr.string

data DoneTaskTree
  = BranchDoneTaskTree
    { prompt :: TaskPrompt
    , subtasks :: List DoneTaskTree
    }
  | LeafDoneTaskTree
    { prompt :: TaskPrompt
    , result :: TaskResult
    }

type LeafDoneTaskTree
  = { prompt :: TaskPrompt
    , result :: TaskResult
    }

renderLeafDoneTaskTree :: LeafDoneTaskTree -> String
renderLeafDoneTaskTree t = t.prompt.string <> " Result: " <> t.result.string

type TaskTooth
  = { prompt :: TaskPrompt
    -- subtasks that are already done
    , subtasksDoneRev :: List DoneTaskTree
    -- subtasks that haven't been done yet
    , subtaskPromptsTodo :: List TaskPrompt
    }

type TaskPath
  = List TaskTooth

-- necessarily has an prompt to do in it i.e. a task tree zipper cannot encode a
-- done tree
type TaskZipper
  = { path :: TaskPath
    , prompt :: TaskPrompt
    }
