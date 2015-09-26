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

--import IdrisJava

import Java.Syntax
import Java.Parser

import Text.PrettyPrint.Leijen

%default partial

prototype : Parser (List Modifier, JType, VarDeclId, List FormalParam)
prototype = do
  mods   <- many (spaces *> modifier)
  resTy  <- space *> ttype
  id     <- space *> varDeclId
  params <- parens (commaSep formalParam)
  pure (mods, resTy, id, params)

pprintModifier : List Modifier -> Doc
pprintModifier = hsep . map (\m => text (show m))

pprintPrim : PrimType -> Doc
pprintPrim BooleanT = text "Bool"
pprintPrim ByteT    = text "Bits8"
pprintPrim ShortT   = text "Bits16"
pprintPrim IntT     = text "Int"
pprintPrim LongT    = text "Bits64"
pprintPrim FloatT   = text "Float"
pprintPrim DoubleT  = text "Double"
pprintPrim VoidT    = text "()"

pprintWrapIO : Bool -> Doc -> Doc
pprintWrapIO True ty  = text "JAVA_IO" |++| parens ty
pprintWrapIO False ty = ty

pprintIdentifier : Identifier -> Doc
pprintIdentifier (Ident id) = text id

mutual
  pprintRefType : RefType -> Doc
  pprintRefType (ClassRefType ct) = pprintClassType ct
  pprintRefType (ArrayType t) = text "Array" |++| parens (pprintJType t) 

  pprintClassType : ClassType -> Doc
  pprintClassType (CType ls) with (nonEmpty ls)
    pprintClassType (CType ls) | (Yes ls') = 
      pprintIdentifier $ fst $ Prelude.List.last ls
    pprintClassType (CType ls) | (No contra) = empty

  pprintJType : JType -> Doc
  pprintJType (PType pt) = pprintPrim pt
  pprintJType (RType rt) = pprintRefType rt

pprintFormalParamType : FormalParam -> Doc
pprintFormalParamType (FParam _ ty _ _) = pprintJType ty

genType : JType -> List FormalParam -> Bool -> Doc
genType resTy params wrapIO =  
  (hsep (map (\t => pprintFormalParamType t |++| text "->") params)) |++| 
         pprintWrapIO wrapIO (pprintJType resTy)

genTypeFun : String -> IO' m ()
genTypeFun input = case parse prototype input of
  Right  (ms, resTy, id, params) => 
    putStrLn $ show $ genType resTy params True
  Left err => printLn err

--------------------------------------------------------------------------------

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

generate : Arguments -> IO' m ()
generate (proto, Nothing) = genTypeFun proto
generate (proto, Just cl) = pure ()

main : IO ()
main = do (Just (proto, cl)) <- processArgs
             | _   => usage
          generate (proto, cl)
          return ()
