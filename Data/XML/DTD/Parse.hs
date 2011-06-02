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
  , attrList
  , attrDecl
  , attrType
  , attrDefault

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
import Data.Attoparsec.Text (Parser, try, space, skipSpace, takeTill,
  anyChar, char, digit, (<*.), (.*>))
import Data.Attoparsec.Combinator (many, manyTill, choice, sepBy1)
import Data.Functor ((<$>))
import Control.Applicative (pure, optional, (<*>), (<*), (*>), (<|>))
import Control.Monad (guard)
import Data.Text (Text)
import Data.Char (isSpace)
import qualified Data.Text as T

-- | Parse a 'DTD', possibly preceded by white space.
parseDTD :: Parser DTD
parseDTD = DTD <$> (skipSpace *> optional (try textDecl <* skipSpace)) <*>
                   many (dtdComponent <* skipSpace)

-- | Parse an @?xml@ text declaration at the beginning of a 'DTD'.
textDecl :: Parser DTDTextDecl
textDecl = do
    "<?" .*> xml .*> space *> skipSpace
    enc1 <- optional $ try encoding
    ver  <- optional $ try (maybeSpace version enc1)
    enc  <- maybe (maybeSpace encoding ver) return enc1
    skipSpace *> "?>" .*> pure (DTDTextDecl ver enc)
  where
    xml = ("X" <|> "x") .*> ("M" <|> "m") .*> ("L" <|> "l")
    version = attr "version" $ const versionNum
    versionNum = T.append <$> "1." <*> (T.singleton <$> digit)
    encoding = attr "encoding" $ takeTill . (==)
    attr name val = try (attrQ '"' name val) <|> attrQ '\'' name val
    attrQ q name val = name .*> skipSpace *> "=" .*> skipSpace *>
                       char q *> val q <* char q
    maybeSpace p = maybe p (const $ space *> skipSpace *> p)

-- | Parse a single component of a 'DTD'. Conditional sections are
-- currently not supported.
dtdComponent :: Parser DTDComponent
dtdComponent = choice $ map try
  [ DTDPERef       <$> pERef
  , DTDEntityDecl  <$> entityDecl
  , DTDElementDecl <$> elementDecl
  , DTDAttrList    <$> attrList
  , DTDNotation    <$> notation
  , DTDInstruction <$> instruction
  ] ++ -- no try needed for last choice
  [ DTDComment     <$> comment
  ]

-- | Parse a processing instruction.
instruction :: Parser Instruction
instruction = Instruction <$> ("<?" .*> skipSpace *> nameSS) <*>
                              idata <*. "?>"
  where
    idata = T.concat . concat <$> manyTillS chunk "?>"
    chunk = list2 . T.singleton <$> anyChar <*> takeTill (== '?')
    list2 x y = [x, y]

-- | Parse an entity declaration.
entityDecl :: Parser EntityDecl
entityDecl = "<!ENTITY " .*> skipSpace *>
                choice [try internalParam, try externalParam,
                        try internalGen,   externalGen]
              <* skipSpace <*. ">"
  where
    internalParam = InternalParameterEntityDecl <$>
                      (param *> nameSS) <*> entityValue
    externalParam = ExternalParameterEntityDecl <$>
                      (param *> nameSS) <*> externalID
    internalGen = InternalGeneralEntityDecl <$> nameSS <*> entityValue
    externalGen = ExternalGeneralEntityDecl <$>
                    nameSS <*> externalID <*> optional (try ndata)
    param = "% " .*> skipSpace
    ndata = skipSpace *> "NDATA " .*> skipSpace *> name

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
nameSS = name <* skipSpace

-- | Parse an entity value. An entity value is a quoted string
-- possibly containing parameter entity references.
entityValue :: Parser [EntityContent]
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
elementDecl = ElementDecl <$> ("<!ELEMENT " .*> skipSpace *> nameSS) <*>
                              contentDecl <* skipSpace <*. ">"

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
    pcdata = "(" .*> skipSpace *> "#PCDATA" .*> skipSpace *>
             (try tags <|> noTagsNoStar)
    tags = many ("|" .*> skipSpace *> nameSS) <*. ")*"
    noTagsNoStar = ")" .*> pure []

-- | Parse the model of structured content for an element.
contentModel = choice $ map (<*> repeatChar)
    [ CMChoice <$> try (cmList '|')
    , CMSeq    <$> try (cmList ',')
    , CMName   <$> name
    ]
  where
    cmList sep = "(" .*> skipSpace *>
      ((contentModel <* skipSpace) `sepBy1` (char sep *> skipSpace)) <*. ")"

-- | Parse a repetition character.
repeatChar :: Parser Repeat
repeatChar = choice
  [ char '?' *> pure ZeroOrOne
  , char '*' *> pure ZeroOrMore
  , char '+' *> pure OneOrMore
  ,             pure One
  ]

-- | Parse a list of attribute declarations for an element.
attrList :: Parser AttrList
attrList = AttrList <$> ("<!ATTLIST " .*> skipSpace *> nameSS) <*>
                        many attrDecl <*. ">"

-- | Parse the three-part declaration of an attribute.
attrDecl :: Parser AttrDecl
attrDecl = AttrDecl <$>
           nameSS <*> attrType <* skipSpace <*> attrDefault <* skipSpace

-- | Parse the type of an attribute.
attrType :: Parser AttrType
attrType = choice $ map try
    [ "CDATA"    .*> pure AttrStringType
    , "ID"       .*> pure AttrIDType
    , "IDREF"    .*> pure AttrIDRefType
    , "IDREFS"   .*> pure AttrIDRefsType
    , "ENTITY"   .*> pure AttrEntityType
    , "ENTITIES" .*> pure AttrEntitiesType
    , "NMTOKEN"  .*> pure AttrNmTokenType
    , "NMTOKENS" .*> pure AttrNmTokensType
    ,  AttrEnumType <$> try enumType
    ] ++
    [ AttrNotationType <$> notationType
    ]
  where
    enumType = nameList
    notationType = "NOTATION " .*> skipSpace *> nameList
    nameList = "(" .*> skipSpace *>
               (nameSS `sepBy1` ("|" .*> skipSpace)) <*. ")"

-- | Parse a default value specification for an attribute.
attrDefault :: Parser AttrDefault
attrDefault = choice $ map try
    [ "#REQUIRED" .*> pure AttrRequired
    , "#IMPLIED"  .*> pure AttrImplied
    , AttrFixed <$> ("#FIXED " .*> skipSpace *> quoted)
    ] ++
    [ AttrDefaultValue <$> quoted
    ]

-- | A single-quoted or double-quoted string.
quoted :: Parser Text
quoted = quotedWith '"' <|> quotedWith '\''
  where
    quotedWith q = char q *> takeTill (== q) <* char q

-- | Parse a declaration of a notation.
notation :: Parser Notation
notation = Notation <$>
  ("<!NOTATION " .*> skipSpace *> nameSS) <* space <* skipSpace <*>
  notationSrc <*. ">"

-- | Parse a source for a notation.
notationSrc :: Parser NotationSource
notationSrc = try system <|> public
  where
    system = NotationSysID <$>
      ("SYSTEM " .*> skipSpace *> quoted <* space <* skipSpace)
    public = mkPublic <$>
      ("PUBLIC " .*> skipSpace *> quoted) <*>
      optional (try $ space *> skipSpace *> quoted) <* skipSpace
    mkPublic pubID = maybe (NotationPubID pubID) (NotationPubSysID pubID)

-- | Parse an external ID.
externalID :: Parser ExternalID
externalID = try system <|> public
  where
    system = SystemID <$> ("SYSTEM " .*> skipSpace *> quoted)
    public = PublicID <$> ("PUBLIC " .*> skipSpace *> quoted) <*
                          space <* skipSpace <*> quoted

-- | Parse a comment
comment :: Parser Text
comment = "<!--" .*> (T.concat . concat <$> manyTillS chunk "--") <*. ">"
  where
    chunk = list2 . T.singleton <$> anyChar <*> takeTill (== '-')
    list2 x y = [x, y]

-- | Type-specialized version of manyTill, so we can use the 'IsString'
-- instance for 'Parser' 'Text' with it.
manyTillS :: Parser a -> Parser Text -> Parser [a]
manyTillS = manyTill
