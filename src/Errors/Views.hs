module Errors.Views where

import           Errors.Diagnostics
import           Errors.Render
import           Sources

digitCount :: Int -> Int
digitCount = go 1 . abs
  where go ds n = if n >= 10 then go (ds + 1) (n `div` 10) else ds

renderDiagnostic :: Diagnostic -> RenderD ()
renderDiagnostic (Diagnostic _style _kind _annotations sn _note) = do
  (sid, s, e) <- case sn of
    Span sid s e -> return (sid, s, e)
    Eof sid      -> do
      file <- getFile sid
      return (sid, length file, length file + 1)
  (_lineS, _colS) <- getLineCol sid s
  (_lineE, _colE) <- getLineCol sid e
  return ()
