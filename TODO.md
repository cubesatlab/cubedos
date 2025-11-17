
TODO
====

This is the master "todo" file for the CubedOS project. It summarizes various lines of work that
need to be pursued. Some of the todo-items here are simple, and some are quite complex. Look for
`TODO.md` files in subproject folders for more specific todo-items related to those subprojects.

+ **Benchmarks**. The only benchmark program in the `bench` folder is out-of-date. It is still
  using an older architecture for message passing that predates the introduction of multi-domain
  support. It should be updated. Additional benchmark programs should also be created.

+ **Tutorial**. There is a tutorial being developed in the documentation (a section in the
  "Manual" chapter). The intention is that new CubedOS developers could build a simple CubedOS
  application following the tutorial to learn how the system works. The `moonshot` sample
  program is intended to be our version of the tutorial application. Both the tutorial and
  `moonshot` need to be completed.

+ **Samples**. There are several other samples that need various degrees of work. See the
  `TODO.md` file in the `samples` folder for more information.

+ **CubedLib**. The CubedOS support library in `src/library` is intended to contains various
  packages of interest to flight software. Some of the packages that exist are incomplete, being
  little more than skeletons. Other packages could be added. See the `TODO.md` file in
  `src/library` for more information.

+ **Interpreter**. The interpreter module in `src/modules` is nothing more than a placeholder.
  It needs to be completed and exercised with a suitable test or sample program. The interpreter
  requirements are outlined in the main documentation in `doc`.

+ **ScriptBuilder**. The ScriptBuilder application in `scriptbuilder` is also nothing more than
  a placeholder. It is intended to facilitate the creation of command scripts that will be
  "executed" by the interpreter. Thus, finishing ScriptBuilder and finishing the interpreter go
  hand-in-hand. Note that ScriptBuilder is in Python, but the interpreter module is in
  SPARK/Ada.

+ **Mecury**. The Mercury IDL compiler is almost, but not quite a usable tool. It should be
  finished/fixed so that it is usable and then deployed to the greatest degree possible. Note
  that Mercury is in Scala.

+ **Paper**. It has been a while since we've written a paper about CubedOS. It may make sense to
  think about writing a new one that reflects the architectural changes since our last published
  work (around 2017?).

+ **CFS Bridge**. This is an ambitious idea to build a bridge of some sort between CubedOS and
  CFS. It isn't clear if this should be done in this repository or in a separate repository
  (but probably separate).

+ **DTN Protocols**. The Delay/Disruption Tolerant Networking protocols could be implemented as
  CubedOS modules. This is also an ambitious idea and is currently being pursued in a [separate
  repository](https://github.com/cubesatlab/dtn).
