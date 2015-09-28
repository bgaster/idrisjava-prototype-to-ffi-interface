-- ---------------------------------------------------------------- [ Main.idr ]
-- Module      : Main
-- Description : Simple program to convert Java method prototypes to 
--               foreign function calls for the Idris Java backend. 
--               For example,
--
--    static GLFWCharCallback glfwSetCharCallback(long window, 
--                                                GLFWCharCallback cbfun)
--
--               is converted too:
--
--    GlfwSetCharCallbackT : Type
--    GlfwSetCharCallbackT = Bits64 -> GLFWCharCallback -> 
--                                            JAVA_IO GLFWCharCallback
--
--    glfwSetCharCallback : GlfwSetCharCallbackT
--    glfwSetCharCallback window cbfun = 
--         invoke "lfwSetCharCallback" GlfwSetCharCallbackT window cbfun
--
--                Dynamic calls require a prototype and class name, e.g.:
--
--    class  = Foo
--    method = public int add(int y)
--
--               is converted too:
--
--    AddT : Type
--    AddT : Int -> JAVA_IO Foo
--
--    add : AddT
--    add y = invokedyn "add" AddT y
--  
-- This code is distributed under the BSD 2-clause license.
-- See the file LICENSE in the root directory for its full text.
--
-- --------------------------------------------------------------------- [ EOH ]
module Main

import System

import Java.Syntax
import Java.Parser

import Text.PrettyPrint.Leijen

--------------------------------------------------------------------------------
-- parser for Java method "prototypes"

prototype : Parser (List Modifier, JType, VarDeclId, List FormalParam)
prototype = do
  mods   <- many (spaces *> modifier)
  resTy  <- space *> ttype
  id     <- space *> varDeclId
  params <- parens (commaSep formalParam)
  pure (mods, resTy, id, params)

--------------------------------------------------------------------------------
-- pretty-printing routines, translating Java types to IdrisJava types and 
-- so on.

-- print modifier 
pprintModifier : List Modifier -> Doc
pprintModifier = hsep . map (\m => text (show m))

-- convert Java primitive type to Idris Java type
pprintPrim : PrimType -> Doc
pprintPrim BooleanT = text "Bool"
pprintPrim ByteT    = text "Bits8"
pprintPrim ShortT   = text "Bits16"
pprintPrim IntT     = text "Int"
pprintPrim LongT    = text "Bits64"
pprintPrim FloatT   = text "Float"
pprintPrim DoubleT  = text "Double"
pprintPrim VoidT    = text "()"

-- wrap type in JAVA_IO monad
pprintWrapIO : Doc -> Doc
pprintWrapIO ty  = text "JAVA_IO" |++| ty

-- print identifier
pprintIdentifier : Identifier -> Doc
pprintIdentifier (Ident id) = text id

-- print variable decl, note only identifer actually expected
pprintVarDecId : VarDeclId -> Doc
pprintVarDecId (VarId id)         = pprintIdentifier id
pprintVarDecId (VarDeclArray vid) = text "vardeclarray"

mutual
  -- print Java reftype
  pprintRefType : RefType -> Doc
  pprintRefType (ClassRefType ct) = pprintClassType ct
  pprintRefType (ArrayType t) = text "Array" |++| parens (pprintJType t) 

  -- print Java class type
  pprintClassType : ClassType -> Doc
  pprintClassType (CType ls) with (nonEmpty ls)
    pprintClassType (CType ls) | (Yes ls') = 
      pprintIdentifier $ fst $ Prelude.List.last ls
    pprintClassType (CType ls) | (No contra) = empty

  -- convert Java type to Idris Java type
  pprintJType : JType -> Doc
  pprintJType (PType pt) = pprintPrim pt
  pprintJType (RType rt) = pprintRefType rt

-- convert var decl to identifier
varDecIdToIdentifier : VarDeclId -> Identifier
varDecIdToIdentifier (VarId id)         = id
varDecIdToIdentifier (VarDeclArray vid) = Ident "vardeclarray"

-- extract the type of a formal parameter
pprintFormalParamType : FormalParam -> Doc
pprintFormalParamType (FParam _ ty _ _) = pprintJType ty

-- extract the identifier of a formal parameter
pprintFormalParamTypeId : FormalParam -> Doc
pprintFormalParamTypeId (FParam _ _ _ id) = 
  let Ident i = varDecIdToIdentifier id
  in text i

--------------------------------------------------------------------------------
-- routines to generate "typedefs" and definitions

-- generate idris type from a result type and a list of argument types
genType : JType -> List FormalParam -> Doc
genType resTy params =  
  (hsep (map (\t => pprintFormalParamType t |++| text "->") params)) |++| 
         pprintWrapIO (pprintJType resTy)

-- helper function to convert first letter of name to uppercase---avoids
-- issues with Idris' implicits in types assumed to be lower case
typeID : Identifier -> Doc
typeID (Ident id) = let h = strHead id
                        t = strTail id
                    in text $ toUpper h `strCons` t

-- generate idris static or dynamic definition 
genDef : String ->       -- "invoke" or "invokedynamic"
         List Doc ->     -- parameter names
         Doc ->          -- method/function name
         Doc ->          -- method/function/FFI type
         Doc
genDef dname pnames name ty = 
  let ps   = hsep pnames
      typ  = name |++| text ":" |++| ty
      body = name |++| ps |++| text "= " |++| text dname |++|
             dquotes name |++| ty |++| ps  
  in typ |$| body

-- parse Java prototype and generate Idris typedef and function
genTypeFun : Maybe String -> -- class name provided if dynamic call, 
                             -- otherwise static 
             String ->       -- input from command line
             IO' m ()
genTypeFun cl input = case parse prototype input of
  Right  (ms, resTy, id, params) =>
    let name   = varDecIdToIdentifier id
    
        -- generate "typedef"
        tId    = typeID $ name

        -- handle static or dynamic call, as implied cl = Just classname, 
        -- for the later case
        tparams = maybe params (\c => let i = Ident c
                                          t = RType $ ClassRefType $ CType [(i, [])]
                                          fp = FParam [] t False (VarId i)
                                      in fp :: params) cl
                                      
        ty     = genType resTy tparams
        tDef   = (tId |++| text ": Type") |$| (tId |++| text "=" |++| ty)
        
        -- generate "definition"
        def    = genDef (maybe "invoke" (\_ => "invokedyn") cl)
                           (map pprintFormalParamTypeId params) 
                           (pprintVarDecId id) tId
        
    in putStrLn $ show $ tDef |$| line |$| def 
  Left err => printLn err

--------------------------------------------------------------------------------
-- routines for command line processing and main

usage : IO' m ()
usage = putStrLn "Usage: " *>
        putStrLn "  javaprototypes prototype [classtype]"

Arguments : Type
Arguments = (String, Maybe String)

processArgs : IO (Maybe Arguments)
processArgs = case !getArgs of
                   [prog, prototype]     => pure $ Just (prototype, Nothing)
                   [prog, prototype, cl] => pure $ Just (prototype, Just cl)
                   _                     => pure $ Nothing

main : IO ()
main = do (Just (proto, cl)) <- processArgs
             | _   => usage
          genTypeFun cl proto
          return ()
