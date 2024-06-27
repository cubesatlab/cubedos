
Mercury
=======

A tool for compiling CubedOS message definitions into message encoding/decoding subprograms.

Mercury is the CubedOS interface definition language (IDL) compiler. This tool converts a
description of CubedOS messages into provable SPARK packages containing message encoding and
decoding subprograms. The IDL is an extended version of the eXternal Data Representation (XDR)
standard described in RFC-4506. We call our extended XDR "modified" XDR, or MXDR.

Build Instructions
------------------

Mercury is an [SBT](https://www.scala-sbt.org/)-based project. After [installing a suitable
JVM](https://www.oracle.com/java/technologies/downloads/) (we recommend version 21.x.y for some
x, y) and SBT, run an SBT interactive session while inside the top level Merc folder. There you
can use the usual SBT commands such as `compile`, `package`, `test`, and `assembly` to build and
exercise the code. Any IDE that supports SBT integration, such as IntelliJ or VSCode can also be
used.
