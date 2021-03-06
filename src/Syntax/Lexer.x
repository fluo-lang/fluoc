{
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-matches #-}
module Syntax.Lexer where

import           Control.Monad.Except
import           Sources                        ( Span(..)
                                                , SourceId(..)
                                                , fromPos
                                                , Spanned(..)
                                                )
import           Syntax.Token
import           Errors.Diagnostics
import           Display
}

%wrapper "posn"

$digit    = 0-9
$alpha    = [a-zA-Z]
$large    = [A-Z \xc0-\xd6 \xd8-\xde]
$small    = [a-z \xdf-\xf6 \xf8-\xff \_]
$operator = [\+\*\-\/\<\>\|\:\$\^\@\!\~\%\&\.\,\=\?\\]
$special  = [\(\)\,\;\[\]\`\{\}]

$octit	   = 0-7
$hexit     = [0-9 A-F a-f]

@decimal     = $digit+
@octal       = $octit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+] @decimal

$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$symbol    = [$ascsymbol] # [$special \_\:\"\']
$graphic   = [$small $large $symbol $digit $special \:\"\']

$charesc   = [abfnrtv\\\"\'\&]
$large     = [A-Z \xc0-\xd6 \xd8-\xde]
$cntrl     = [$large \@\[\\\]\^\_]

@ascii    = \^ $cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
	 | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
	 | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM
	 | SUB | ESC | FS | GS | RS | US | SP | DEL
@escape   = \\ ($charesc | @ascii | @decimal | o @octal | x @hexadecimal)
@gap      = \\ $white+ \\
@string   = $graphic # [\"\\] | " " | @escape | @gap

-- Sorcery taken from https://stackoverflow.com/a/36328890/9470078 
$notHashTag    = [\x00-\x10ffff] # \#
$notComment    = [\x00-\x10ffff] # [\#\/]
@commentStart  = $notHashTag* (\#+)
@commentMiddle = $notComment $notHashTag* (\#+)
@commentMulti  = "/#" @commentStart "/"

tokens :-
  -- Whitespace insensitive
  $white+ ;

  -- Comments
  @commentMulti ;
  "#".*\n       ;

  -- Syntax
  let                                   { makeTok LetTok }
  import                                { makeTok ImportTok }
  rec                                   { makeTok RecTok }
  impl                                  { makeTok ImplTok }
  trait                                 { makeTok TraitTok }
  dec                                   { makeTok DecTok }
  in                                    { makeTok InTok }
  if                                    { makeTok IfTok }
  else                                  { makeTok ElseTok }
  match                                 { makeTok MatchTok }
  elif                                  { makeTok ElifTok }
  assign                                { makeTok AssignTok }
  opdef                                 { makeTok OpDefTok }

  $digit+ \. $digit+                    { makeTokCmplx FloatTok read }
  $digit+                               { makeTokCmplx IntegerTok read }
  $operator+                            { makeTokCmplx OperatorTok id }
  \(                                    { makeTok LParenTok }
  \)                                    { makeTok RParenTok }
  \[                                    { makeTok LBracketTok }
  \]                                    { makeTok RBracketTok }
  \{                                    { makeTok LCurlyTok }
  \}                                    { makeTok RCurlyTok }
  [$alpha \_] [$alpha $digit \_ \' \?]* { makeTokCmplx IdentTok id }
  \' [$alpha \_] [$alpha $digit \_ \?]* { makeTokCmplx PolyTok id }
  \" @string* \"		                { makeTokCmplx StrTok read }

{

makeTok :: TokenKind -> AlexPosn -> String -> SourceId -> Token
makeTok kind (AlexPn c _ _) s sid =
  MkToken (Span sid c $ c + (length s)) kind

makeTokCmplx
  :: (a -> TokenKind) -> (String -> a) -> AlexPosn -> String -> SourceId -> Token
makeTokCmplx cons f (AlexPn c _ _) s sid =
  MkToken (Span sid c $ c + (length s)) (cons (f s))

data Token = MkToken Span TokenKind
  deriving (Eq, Show)
instance Display Token where
  display (MkToken _ t) = display t
instance Spanned Token where
  getSpan (MkToken s _) = s

fstIdx :: String -> String
fstIdx [x    ] = [x]
fstIdx (x : _) = [x]
fstIdx _       = "EOF"

scanTokens :: SourceId -> String -> Except Diagnostics [Token]
scanTokens sourceId str = go (alexStartPos, '\n', [], str) where
  go inp@(pos, _, _bs, str) = case alexScan inp 0 of
    AlexEOF -> return [MkToken (Eof sourceId) EofTok]
    AlexError ((AlexPn c _ _), _, _, stream) -> throwError $ intoDiagnostics $ Diagnostic
      Error
      UnexpectedCharacterError
      [ Annotation (fromPos sourceId c)
                   (Just $ "unexpected character `" ++ fstIdx stream ++ "`")
                   Error
      ]
      (fromPos sourceId c)
      []
    AlexSkip inp' len      -> go inp'
    AlexToken inp' len act -> do
      res <- go inp'
      let rest = act pos (take len str) sourceId 
      return (rest : res)
}
