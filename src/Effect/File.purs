module Effect.File where

import Prelude
import Effect.Class (class MonadEffect)

newtype FilePath
  = FilePath String

writeFile :: forall m. MonadEffect m => String -> FilePath -> m Unit
writeFile content (FilePath fp) = do
  let
    _ = _writeFile content fp
  pure unit

foreign import _writeFile :: String -> String -> Void
