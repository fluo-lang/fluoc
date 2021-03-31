module Errors.Views where

import           Errors.Diagnostics
import           Errors.Render
import           Control.Monad.Reader           ( asks )
-- import           Control.Monad.Writer           ( tell )
import           Control.Monad                  ( foldM_
                                                , foldM
                                                , when
                                                )
import           Sources
import           Data.Foldable                  ( forM_
                                                , maximumBy
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import qualified Data.Sequence                 as S

digitCount :: Int -> Int
digitCount = go 1 . abs
  where go ds n = if n >= 10 then go (ds + 1) (n `div` 10) else ds

absoluteSpan :: Span -> RenderD (SourceId, Int, Int)
absoluteSpan (Span sid s e) = return (sid, s, e)
absoluteSpan (Eof sid     ) = do
  file <- getFile sid
  return (sid, length file, length file + 1)

lineNumbersSpan :: Span -> RenderD (SourceId, Int, Int, Int, Int)
lineNumbersSpan sn = do
  (sid, s, e) <- absoluteSpan sn
  (sL, sC)    <- getLineCol sid s
  (eL, eC)    <- getLineCol sid e
  return (sid, sL, sC, eL, eC)

data Line = Line
  { singleLabels :: [SingleLabel]
  , multiLabels  :: MultiLabels
  , mustRender   :: Bool
  , source       :: String
  }
  deriving Show
data File = File
  { fSourceId      :: SourceId
  , fLines         :: Map Int Line
  , paddingLeft    :: Int
  , numMultiLabels :: Int
  }
  deriving Show
type Files = Map SourceId File

mergeLabels :: Line -> Line -> Line
mergeLabels (Line ls ms render source') (Line ls' ms' render' _) =
  Line (ls ++ ls') (ms ++ ms') (render || render') source'


batchAnnotations :: [Annotation] -> RenderD Files
batchAnnotations = foldM
  (\files (Annotation sn msg style) -> do
    (sid, startL, startC, endL, endC) <- lineNumbersSpan sn
    cLines                            <- asks $ contextLines . config
    lns                               <- getFileLines sid
    let multiId = maybe 0 numMultiLabels (M.lookup sid files)
    let newAdditions =
          (if startL == endL
            then M.singleton
              startL
              ( Line [SingleLabel style startC endC msg] [] True
              $ S.index lns startL
              )
            else
              M.insert
                startL
                ( Line [] [(multiId, style, TopMultiLabel startC)] True
                $ S.index lns startL
                )
              $ M.insert
                  endL
                  ( Line [] [(multiId, style, BottomLabel endC msg)] True
                  $ S.index lns endL
                  )
              $ M.fromList
                  (map
                    (\lineno ->
                      ( lineno
                      , Line
                          []
                          [(multiId, style, LeftLabel)]
                          (  (lineno - startL <= cLines)
                          || (endL - lineno <= cLines)
                          )
                        $ S.index lns lineno
                      )
                    )
                    [(startL + 1) .. (endL - 1)]
                  )
          )
    return $ M.insertWith
      (\(File _ ls pLeft n) (File sid' ls' pLeft' n') ->
        File sid' (M.unionWith mergeLabels ls' ls) (max pLeft pLeft') $ max n n'
      )
      sid
      (File sid newAdditions (digitCount (endL + 1)) $ multiId + 1)
      files
  )
  M.empty

getAnnValue :: SourceId -> Annotation -> Int
getAnnValue sid (Annotation sn msg priority) = if sid == sourceId sn
  then
    (case priority of
        Error   -> 3
        Warning -> 2
        Info    -> 1
      )
      + fromEnum (maybe False (not . null) msg)
  else -1

mostSevere :: SourceId -> [Annotation] -> Annotation
mostSevere sid =
  maximumBy (\a b -> getAnnValue sid a `compare` getAnnValue sid b)

renderFile :: Int -> [Annotation] -> File -> RenderD ()
renderFile pLeft anns (File sid lns _ maxMl) = do
  let (Annotation spn _ _) = mostSevere sid anns
  (_, s, _) <- absoluteSpan spn
  renderSnippetState pLeft sid s
  renderEmptyLine pLeft maxMl []
  foldM_
    (\(prevLine, renderBreak) (lineNo, Line singles multis mustRender' source') ->
      do
        let renderedBreak =
              maybe False (\a -> abs (lineNo - a) > 1) prevLine && renderBreak
        when renderedBreak $ renderLineBreak pLeft maxMl multis
        when mustRender' $ do
          renderSnippetSource pLeft (lineNo + 1) source' singles maxMl multis
        return $ if mustRender'
          then (Just lineNo, True)
          else (prevLine, renderBreak && not renderedBreak)
    )
    (Nothing, True)
    (M.assocs lns)
  renderEmptyLine pLeft maxMl []

renderDiagnostic :: Diagnostic -> RenderD ()
renderDiagnostic (Diagnostic style kind annotations _ notes) = do
  renderHeader style (show style) (getId kind) (show kind)
  batched     <- M.elems <$> batchAnnotations annotations
  indentation <- asks $ sidePadding . config
  let paddingLeft' = indentation + maximum (map paddingLeft batched)
  forM_ batched (renderFile paddingLeft' annotations)
  renderNotes paddingLeft' notes

renderDiagnostics :: Diagnostics -> RenderD ()
renderDiagnostics (Diagnostics ds) = do
  forM_ ds renderDiagnostic
  renderNumDiagnostics (map dTy ds)
