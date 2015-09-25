-- ---------------------------------------------------------------- [ Main.idr ]
-- Module      : Main
-- Description : Simple program to convert Java method prototypes to 
--               foreign function calls for the Idris Java backend. 
--               For example,
--
--    static GLFWCharCallback glfwSetCharCallback(long window, GLFWCharCallback cbfun)
--
--               is converted too:
--
--    GlfwSetCharCallbackT : Type
--    GlfwSetCharCallbackT = Bits64 -> GLFWCharCallback -> JAVA_IO GLFWCharCallback
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

import public Lightyear
import public Lightyear.Strings

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

test : Show a => Parser a -> String -> IO' m ()
test p input = case parse p input of
  Left  e => putStrLn e
  Right x => printLn x

--------------------------------------------------------------------------------

usage : IO ()
usage = putStrLn "Usage: " *>
        putStrLn "  javaprototypes prototype [classtype]"

processArgs : IO (Maybe (String, Maybe String))
processArgs = case !getArgs of
                   [prog, prototype]     => pure $ Just (prototype, Nothing)
                   [prog, prototype, cl] => pure $ Just (prototype, Just cl)
                   _                     => pure $ Nothing

main : IO ()
main = do (Just (proto, cl)) <- processArgs
             | _   => usage
          test prototype proto
          return ()
