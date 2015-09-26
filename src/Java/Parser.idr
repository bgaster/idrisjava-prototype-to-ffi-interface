-- ---------------------------------------------------------------- [ Parser.idr ]
-- Module      : Java.Parser
-- Description : Parser for a subset of Java
--
-- This code is distributed under the BSD 2-clause license.
-- See the file LICENSE in the root directory for its full text.
--
-- This code is based on the Haskell library Lanuage-Java, however,
-- it does not (currently) support all of Java as the given application 
-- does not require it. It would be straightforward to complete the job.
-- --------------------------------------------------------------------- [ EOH ]
module Java.Parser

import public Lightyear
import public Lightyear.Char
import public Lightyear.Strings

import Java.Syntax

%access public

--------------------------------------------------------------------------------

bopt : Parser a -> Parser Bool
bopt p = opt p >>= \ma => pure $ isJust ma

lopt : Parser (List a) -> Parser (List a)
lopt p = do mas <- opt p
            case mas of
             Nothing => return []
             Just as => return as
             
--------------------------------------------------------------------------------

keyword : String -> a -> Parser a
keyword s a = string s *> pure a 

keywords : List String
keywords = [
  "abstract", "continue", "for", "new", "switch",
  "assert", "default", "goto", "package", "synchronized",
  "boolean", "do", "if", "private", "this",
  "break", "double", "implements", "protected", "throw",
  "byte", "else", "import", "public", "throws",
  "case", "enum", "instanceof", "return", "transient",
  "catch", "extends", "int", "short", "try",
  "char", "final", "interface", "static", "void",
  "class", "finally", "long", "strictfp", "volatile",
  "const", "float", "native", "super", "while"]

isKeyword : String -> Bool
isKeyword s = elem s keywords

--------------------------------------------------------------------------------
-- Names

identifier : Parser Identifier
identifier = do
  c <- letter
  rest <- many alphaNum
  let str = strCons c $ pack rest
  if isKeyword str
    then fail "keyword not an identifier"
    else pure $ Ident str

name : Parser Name
name = sepBy1 identifier dot >>= return . N

--------------------------------------------------------------------------------
-- Modifiers

modifier : Parser Modifier
modifier = 
  (keyword "private" Private)
  <|>
  (keyword "public" Public)
  <|>
  (keyword "protected" Protected)
  <|>
  (keyword "abstract" Abstract)
  <|>
  (keyword "final" Final)
  <|>
  (keyword "static" Static)
  <|>
  (keyword "volatile" Volatile)
  <|>
  (keyword "synchronised" Synchronised)

ellipsis : Parser ()
ellipsis = dot *> dot *> dot

--------------------------------------------------------------------------------
-- Vairable declarations

arrBrackets : Parser ()
arrBrackets = brackets $ pure ()

varDeclId : Parser VarDeclId
varDeclId = do
  id <- identifier
  abs <- many arrBrackets
  pure $ foldl (\f, _ => VarDeclArray . f) VarId abs id
  

--------------------------------------------------------------------------------
-- Statements

--------------------------------------------------------------------------------
-- Expressions


--------------------------------------------------------------------------------
-- Types, Type parameters and arguments

primType : Parser PrimType
primType =
  (keyword "boolean" BooleanT)
  <|>
  (keyword "byte" ByteT)
  <|>
  (keyword "short" ShortT)
  <|>
  (keyword "int" IntT)
  <|>
  (keyword "long" LongT)
  <|>
  (keyword "char" CharT)
  <|>
  (keyword "float" FloatT)
  <|>
  (keyword "double" DoubleT)
  <|>
  (keyword "void" VoidT)

mutual
  typeParams : Parser (List TypeParam)
  typeParams = angles $ sepBy1 typeParam comma
  
  typeParam : Parser TypeParam
  typeParam = do
    i <- identifier
    bs <- lopt bounds
    pure $ TParam i bs
    
  bounds : Parser (List RefType)
  bounds = do
    keyword "extends" ()
    sepBy1 refType (char '&')
  
  typeArgs : Parser (List TypeArgument)
  typeArgs = angles $ sepBy1 typeArg comma
  
  typeArg : Parser TypeArgument
  typeArg = 
    (do x <- char '?'
        w <- opt wildcardBound
        pure $ Wildcard w) <|>
    (refType >>= pure . ActualType)
    
  wildcardBound : Parser WildcardBound
  wildcardBound = 
    (do x <- keyword "extends" ()
        rt <- refType
        pure $ ExtendsBound rt) <|>
    (do x <- keyword "super" ()
        refType >>= pure . SuperBound)
  
  -- types
  ttype : Parser JType
  ttype = (refType >>= pure . RType) <|> (primType >>= pure . PType)
  
  refTypeArgs : Parser (List RefType)
  refTypeArgs = angles refTypeList
  
  refTypeList : Parser (List RefType)
  refTypeList = sepBy1 refType comma
  
  classTypeSpec : Parser (Identifier, List TypeArgument)
  classTypeSpec = do
    i <- identifier
    tas <- lopt typeArgs
    pure (i, tas)

  classType : Parser ClassType
  classType = CType <$> sepBy1 classTypeSpec dot

  refType : Parser RefType
  refType =
    (do pt <- primType
        (_::bs) <- some arrBrackets
        pure $ Prelude.Foldable.foldl (\f, _ => ArrayType . RType . f)
                                        (ArrayType . PType) bs pt) <|>
    (do ct <- classType
        bs <- many arrBrackets
        pure $ foldl (\f, _ => ArrayType . RType . f)
                            ClassRefType bs ct) <?> "refType"
                            
  nonArrayType : Parser JType
  nonArrayType = 
    (primType >>= pure . PType) <|> (classType >>= pure . RType . ClassRefType)

--------------------------------------------------------------------------------
-- Formal parameters

formalParam : Parser FormalParam
formalParam = do
    ms  <- many modifier
    spaces
    typ <- ttype
    spaces
    var <- bopt ellipsis
    spaces
    vid <- varDeclId
    pure $ FParam ms typ var vid
  
