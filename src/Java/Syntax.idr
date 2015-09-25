-- ---------------------------------------------------------------- [ Syntax.idr ]
-- Module      : Java.Syntax
-- Description : Parser for a subset of Java
--
-- This code is distributed under the BSD 2-clause license.
-- See the file LICENSE in the root directory for its full text.

-- This code is based on the Haskell library Lanuage-Java, however,
-- it does not (currently) support all of Java as the given application 
-- does not require it. It would be straightforward to complete the job.
-- --------------------------------------------------------------------- [ EOH ]
module Java.Syntax

%access public

data Identifier = Ident String

instance Show Identifier where
  show (Ident s) = s

data Name = N (List Identifier)

instance Show Name where
  show (N ns) = show ns

data Modifier = 
  Public |
  Private |
  Protected |	 
  Abstract |
  Final |	 
  Static |	 
  Volatile |
  Synchronised
        
instance Show Modifier where
  show Public       = "public"
  show Private      = "private"
  show Protected    = "protected"
  show Abstract     = "abstract"
  show Final        = "final"
  show Static       = "static"
  show Volatile     = "volatile"
  show Synchronised = "synchronised"

data PrimType =
  BooleanT     | 
  ByteT	       |
  ShortT       |	 
  IntT         |	 
  LongT	       |
  CharT	       |
  FloatT       | 
  DoubleT      |	 
  VoidT

instance Show PrimType where
  show BooleanT = "boolean"
  show ByteT	= "byte"
  show ShortT   = "short"
  show IntT     = "int"
  show LongT	= "long"
  show CharT	= "char"
  show FloatT   = "float"
  show DoubleT	= "double"
  show VoidT	= "void"

mutual
  data JType =
    PType PrimType |
    RType RefType

  instance Show JType where
    show (PType pt) = show  pt
    show (RType rt) = show rt
  
  data RefType =
    ClassRefType ClassType |
    ArrayType JType
    
  instance Show RefType where
    show (ClassRefType ct) = show ct
    show (ArrayType t)     = show t
  
  data ClassType =
    CType (List (Identifier, List TypeArgument))

  instance Show ClassType where
    -- @fixme
    show (CType ls) = foldl (\s, (i, tparams) => show i ++ " " ++ s) "" ls   

  data TypeArgument =
    Wildcard (Maybe WildcardBound) |	 
    ActualType RefType
    
  instance Show TypeArgument where
    show (Wildcard (Just wcb)) = show wcb
    show (Wildcard _)          = ""
    show (ActualType rt)       = show rt
    
  data WildcardBound =
    ExtendsBound RefType |
    SuperBound RefType	    
    
  instance Show WildcardBound where
    show (ExtendsBound rt) = show rt
    show (SuperBound rt)   = show rt


data TypeParam = TParam Identifier (List RefType)

instance Show TypeParam where
  show (TParam id rts) = show id

data Block = BStmts (List ()) -- @fixme

instance Show Block where
  show (BStmts stmts) = "{}"

data MethodBody = MBody (Maybe Block)

instance Show MethodBody where
  show (MBody (Just b)) = show b
  show _                = ""

data VarDeclId = 
  VarId Identifier	  |
  VarDeclArray VarDeclId
    
instance Show VarDeclId where
  show (VarId id) = show id
  show (VarDeclArray vdecl) = "[" ++ show vdecl ++ "]"

data FormalParam = FParam (List Modifier) JType Bool VarDeclId

instance Show FormalParam where
  show (FParam ms t b vdeclid) = 
    foldr (\x, s => s ++ " " ++ show x) "" ms ++ 
    " " ++ show t ++ (if b then "..." else "") ++ show vdeclid

data MemberDecl =
  MethodDecl (List Modifier) 
             (List TypeParam) 
             JType 
             Identifier 
             (List FormalParam) 
             {-[ExceptionType]-} 
             MethodBody

instance Show MemberDecl where
  show (MethodDecl ms tprams rt name fparams body) =
    show ms
--    show ms ++ show rt ++ show name Prelude.Strings.++ "(" ++ fparams ++ ")" ++ show body
