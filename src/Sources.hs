{-# LANGUAGE DeriveDataTypeable #-}
module Sources where

import           Data.Map
import           Data.Sequence                  ( Seq )
import           Data.Data                      ( Data )
import           Data.Typeable                  ( Typeable )

data Span = Span SourceId Int Int
          | Eof SourceId deriving (Show, Eq, Data, Typeable)
fromPos :: SourceId -> Int -> Span
fromPos sid pos = Span sid pos (pos + 1)

sourceId :: Span -> SourceId
sourceId (Span sid _ _) = sid
sourceId (Eof sid     ) = sid

btwn :: Span -> Span -> Span
btwn (Span sid s _) (Span _ _ e) = Span sid s e
btwn (Eof sid     ) _            = Eof sid
btwn _              (Eof sid)    = Eof sid

gap :: Span -> Span -> Span
gap (Span sid _ e) (Span _ s _) = Span sid e s
gap (Eof sid     ) _            = Eof sid
gap _              (Eof sid)    = Eof sid

gp :: (Spanned a, Spanned b) => a -> b -> Span
gp t1 t2 = gap (getSpan t1) (getSpan t2)

bt :: (Spanned a, Spanned b) => a -> b -> Span
bt t1 t2 = btwn (getSpan t1) (getSpan t2)

class Spanned a where
  getSpan :: a -> Span

newtype SourceId = SourceId Int deriving (Show, Eq, Ord, Data, Typeable)
mapSid :: (Int -> Int) -> SourceId -> SourceId
mapSid f (SourceId x) = SourceId $ f x

type Sources = Map SourceId String
type SourceLines = Map SourceId (Seq String)
type FileMap = Map SourceId FilePath
