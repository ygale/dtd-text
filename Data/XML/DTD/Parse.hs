{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------------------------
-- |
-- Module      :  Data.XML.DTD.Parse
-- Copyright   :  Suite Solutions Ltd., Israel 2011
--
-- Maintainer  :  Yitzchak Gale <gale@sefer.org>
-- Portability :  portable
--
-- This module provides a "Data.Attoparsec.Text" parser for XML
-- Document Type Declaration (DTD) documents.

{-
Copyright (c) 2011 Suite Solutions Ltd., Israel. All rights reserved.

For licensing information, see the BSD3-style license in the file
license.txt that was originally distributed by the author together
with this file.
-}

module Data.XML.DTD.Parse
  ( -- * DTD structure
    parseDTD
  , textDecl
  , dtdComponent

    -- * Entity declarations and references
  , entityDecl
  , entityValue
  , pERef
  , notation
  , notationSrc

    -- * Element declarations
  , elementDecl
  , contentDecl
  , contentModel
  , repeatChar

    -- * Attribute declarations
  , attList
  , attDecl
  , attType
  , attDefault

    -- * Declarations of comments and processing instructions
  , instruction
  , comment

    -- * Parsing combinators for general DTD syntax
  , externalID
  , name
  , nameSS
  , quoted
  ) 
  where

import Data.XML.DTD.Types
import Data.XML.Types (ExternalID(PublicID, SystemID),
  Instruction(Instruction))
import Data.Attoparsec.Text (Parser, try, satisfy, takeTill,
  anyChar, char, digit, (<*.), (.*>))
import qualified Data.Attoparsec.Text as A -- for takeWhile
import Data.Attoparsec.Combinator (many, manyTill, choice, sepBy1)
import Data.Functor ((<$>))
import Control.Applicative (pure, optional, (<*>), (<*), (*>), (<|>))
import Control.Monad (guard)
import Data.Text (Text)
import Data.Char (isSpace)
import qualified Data.Text as T

-- | Parse a 'DTD', possibly preceded by white space.
parseDTD :: Parser DTD
parseDTD = DTD <$> (skipWS *> optional (try textDecl <* skipWS)) <*>
                   many (dtdComponent <* skipWS)

-- | Parse an @?xml@ text declaration at the beginning of a 'DTD'.
textDecl :: Parser DTDTextDecl
textDecl = do
    "<?" .*> xml .*> ws *> skipWS
    enc1 <- optional $ try encoding
    ver  <- optional $ try (maybeSpace version enc1)
    enc  <- maybe (maybeSpace encoding ver) return enc1
    skipWS *> "?>" .*> pure (DTDTextDecl ver enc)
  where
    xml = ("X" <|> "x") .*> ("M" <|> "m") .*> ("L" <|> "l")
    version = attr "version" $ const versionNum
    versionNum = T.append <$> "1." <*> (T.singleton <$> digit)
    encoding = attr "encoding" $ takeTill . (==)
    attr name val = try (attrQ '"' name val) <|> attrQ '\'' name val
    attrQ q name val = name .*> skipWS *> "=" .*> skipWS *>
                       char q *> val q <* char q
    maybeSpace p = maybe p (const $ ws *> skipWS *> p)

-- | Parse a single component of a 'DTD'. Conditional sections are
-- currently not supported.
dtdComponent :: Parser DTDComponent
dtdComponent = choice $ map try
  [ DTDPERef       <$> pERef
  , DTDEntityDecl  <$> entityDecl
  , DTDElementDecl <$> elementDecl
  , DTDAttList    <$> attList
  , DTDNotation    <$> notation
  , DTDInstruction <$> instruction
  ] ++ -- no try needed for last choice
  [ DTDComment     <$> comment
  ]

-- | Parse a processing instruction.
instruction :: Parser Instruction
instruction = Instruction <$> ("<?" .*> skipWS *> nameSS) <*>
                              idata <*. "?>"
  where
    idata = T.concat . concat <$> manyTillS chunk "?>"
    chunk = list2 . T.singleton <$> anyChar <*> takeTill (== '?')
    list2 x y = [x, y]

-- | Parse an entity declaration.
entityDecl :: Parser EntityDecl
entityDecl = "<!ENTITY" .*> ws *> skipWS *>
                choice [try internalParam, try externalParam,
                        try internalGen,   externalGen]
              <* skipWS <*. ">"
  where
    internalParam = InternalParameterEntityDecl <$>
                      (param *> nameSS) <*> entityValue
    externalParam = ExternalParameterEntityDecl <$>
                      (param *> nameSS) <*> externalID
    internalGen = InternalGeneralEntityDecl <$> nameSS <*> entityValue
    externalGen = ExternalGeneralEntityDecl <$>
                    nameSS <*> externalID <*> optional (try ndata)
    param = "%" .*> ws *> skipWS
    ndata = skipWS *> "NDATA" .*> ws *> skipWS *> name

-- | Parse a DTD name. We are much more liberal than the spec: we
-- allow any characters that will not interfere with other DTD
-- syntax. This parser subsumes both @Name@ and @NmToken@ in the spec,
-- and more.
name :: Parser Text
name = nonNull $ takeTill notNameChar
  where
    notNameChar c = isSpace c || c `elem` syntaxChars
    syntaxChars = "()[]<>!%&;'\"?*+|,="
    nonNull parser = do
      text <- parser
      guard . not . T.null $ text
      return text

-- | Parse a DTD 'name' followed by optional white space.
nameSS :: Parser Text
nameSS = name <* skipWS

-- | Parse an entity value. An entity value is a quoted string
-- possibly containing parameter entity references.
entityValue :: Parser [EntityValue]
entityValue = try (quotedVal '"') <|> quotedVal '\''
  where
    quotedVal q = char q *> manyTill content (char q)
    content =  EntityPERef <$> try pERef <|> EntityText <$> text
    text = takeTill $ \c -> c == '%' || c == '"'

-- | Parse a parameter entity reference
pERef :: Parser PERef
pERef = "%" .*> name <*. ";"

-- | Parse the declaration of an element.
elementDecl :: Parser ElementDecl
elementDecl = ElementDecl <$> ("<!ELEMENT" .*> ws *> skipWS *> nameSS) <*>
                              contentDecl <* skipWS <*. ">"

-- | Parse the content that can occur in an element.
contentDecl :: Parser ContentDecl
contentDecl = choice $ map try
    [ pure ContentEmpty <*. "EMPTY"
    , pure ContentAny   <*. "ANY"
    ,      ContentMixed <$> pcdata
    ] ++
    [      ContentElement <$> contentModel
    ]
  where
    pcdata = "(" .*> skipWS *> "#PCDATA" .*> skipWS *>
             (try tags <|> noTagsNoStar)
    tags = many ("|" .*> skipWS *> nameSS) <*. ")*"
    noTagsNoStar = ")" .*> pure []

-- | Parse the model of structured content for an element.
contentModel = choice $ map (<*> repeatChar)
    [ CMChoice <$> try (cmList '|')
    , CMSeq    <$> try (cmList ',')
    , CMName   <$> name
    ]
  where
    cmList sep = "(" .*> skipWS *>
      ((contentModel <* skipWS) `sepBy1` (char sep *> skipWS)) <*. ")"

-- | Parse a repetition character.
repeatChar :: Parser Repeat
repeatChar = choice
  [ char '?' *> pure ZeroOrOne
  , char '*' *> pure ZeroOrMore
  , char '+' *> pure OneOrMore
  ,             pure One
  ]

-- | Parse a list of attribute declarations for an element.
attList :: Parser AttList
attList = AttList <$> ("<!ATTLIST" .*> ws *> skipWS *> nameSS) <*>
                       many attDecl <*. ">"

-- | Parse the three-part declaration of an attribute.
attDecl :: Parser AttDecl
attDecl = AttDecl <$>
           nameSS <*> attType <* skipWS <*> attDefault <* skipWS

-- | Parse the type of an attribute.
attType :: Parser AttType
attType = choice $ map try
    [ "CDATA"    .*> pure AttStringType
    , "ID"       .*> pure AttIDType
    , "IDREF"    .*> pure AttIDRefType
    , "IDREFS"   .*> pure AttIDRefsType
    , "ENTITY"   .*> pure AttEntityType
    , "ENTITIES" .*> pure AttEntitiesType
    , "NMTOKEN"  .*> pure AttNmTokenType
    , "NMTOKENS" .*> pure AttNmTokensType
    ,  AttEnumType <$> try enumType
    ] ++
    [ AttNotationType <$> notationType
    ]
  where
    enumType = nameList
    notationType = "NOTATION" .*> ws *> skipWS *> nameList
    nameList = "(" .*> skipWS *>
               (nameSS `sepBy1` ("|" .*> skipWS)) <*. ")"

-- | Parse a default value specification for an attribute.
attDefault :: Parser AttDefault
attDefault = choice $ map try
    [ "#REQUIRED" .*> pure AttRequired
    , "#IMPLIED"  .*> pure AttImplied
    , AttFixed <$> ("#FIXED" .*> ws *> skipWS *> quoted)
    ] ++
    [ AttDefaultValue <$> quoted
    ]

-- | A single-quoted or double-quoted string.
quoted :: Parser Text
quoted = quotedWith '"' <|> quotedWith '\''
  where
    quotedWith q = char q *> takeTill (== q) <* char q

-- | Parse a declaration of a notation.
notation :: Parser Notation
notation = Notation <$>
  ("<!NOTATION" .*> ws *> skipWS *> name) <* ws <* skipWS <*>
  notationSrc <*. ">"

-- | Parse a source for a notation.
notationSrc :: Parser NotationSource
notationSrc = try system <|> public
  where
    system = NotationSysID <$>
      ("SYSTEM" .*> ws *> skipWS *> quoted <* ws <* skipWS)
    public = mkPublic <$>
      ("PUBLIC" .*> ws *> skipWS *> quoted) <*>
      optional (try $ ws *> skipWS *> quoted) <* skipWS
    mkPublic pubID = maybe (NotationPubID pubID) (NotationPubSysID pubID)

-- | Parse an external ID.
externalID :: Parser ExternalID
externalID = try system <|> public
  where
    system = SystemID <$> ("SYSTEM" .*> ws *> skipWS *> quoted)
    public = PublicID <$> ("PUBLIC" .*> ws *> skipWS *> quoted) <*
                          ws <* skipWS <*> quoted

-- | Parse a comment
comment :: Parser Text
comment = "<!--" .*> (T.concat . concat <$> manyTillS chunk "--") <*. ">"
  where
    chunk = list2 . T.singleton <$> anyChar <*> takeTill (== '-')
    list2 x y = [x, y]

-- | Definition of white space characters, from the XML specification.
isXMLSpace :: Char -> Bool
isXMLSpace = (`elem` "\x20\x9\xD\xA")

-- | Parse one character of white space.
ws :: Parser Char
ws = satisfy isXMLSpace

-- | Skip zero or more characters of white space
skipWS :: Parser ()
skipWS = A.takeWhile isXMLSpace *> pure ()

-- | Type-specialized version of manyTill, so we can use the 'IsString'
-- instance for 'Parser' 'Text' with it.
manyTillS :: Parser a -> Parser Text -> Parser [a]
manyTillS = manyTill
