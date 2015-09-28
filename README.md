IrisJava method prototype to FFI converter
==========================================

[IdrisJava](https://github.com/idris-hackers/idris-java) is a Java
backend for the programming language Idris, which allows
interactaction with the underlying JVM, including calling Java from
Idris code.

This Idris package implements a simple traanslator from a given
Java method's prototype to the IdrisJava FFI code. For example, the following
static Java method:

```java
static GLFWCharCallback glfwSetCharCallback(long window, 
                                            GLFWCharCallback cbfun)
```
											
is converted too:

```haskell
GlfwSetCharCallbackT : Type
GlfwSetCharCallbackT = Bits64 -> GLFWCharCallback -> JAVA_IO GLFWCharCallback
glfwSetCharCallback : GlfwSetCharCallbackT
glfwSetCharCallback window cbfun = invoke "lfwSetCharCallback" GlfwSetCharCallbackT window cbfun
```

and for dynamic method, assuming a class Foo:

```java
public int add(int y)
```

is converted too

```haskell
AddT : Type
AddT : Int -> JAVA_IO Foo
add : AddT
add y = invokedyn "add" AddT y
```

Build
===========

You need to have a recent version of Idris installed. Then simple run the command:

    idris --build java-prototypes.ikpg

to generate the command *javaprototypes*.

Usage
=======

Run *javaprototypes* with a Java method prototype as the first
argument and optionally a class name, for dynamic calls, as the second
argument. For example:

    javaprototypes "static GLFWCharCallback glfwSetCharCallback(long window, GLFWCharCallback cbfun)"
