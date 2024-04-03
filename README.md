
CubedOS
=======

A flight software framework for CubeSat spacecraft written in SPARK/Ada.

This folder contains the core CubedOS system. The sibling Merc repository contains the Merc tool
used for generating CubedOS API encoder/decoder subprograms. The subfolders are as follows:

+ `bench`: Benchmark programs for measuring the performance of CubedOS.

+ `doc`: The documentation of CubedOS.

+ `samples`: This folder contains various sample programs that illustrate CubedOS in action.

+ `src`: The CubedOS source code repository.

+ `templates`: Various templates to facilitate the construction of CubedOS applications. The
  developer can use these templates to simplify the programming of new applications, however
  their use is not strictly required.
  
  
Development Environment
-----------------------

We develop CubedOS on x86-64 platforms running Ubuntu Linux 22.04. Development on Windows and
macOS is probably possible, but we do not use those systems. Most of the test, sample, and
benchmark programs can be compiled and run on the development host. Some programs that target
other platforms are also included. Final testing and performance evaluations should be done on
your specific target platform, of course.

With generous support from AdaCore, we are able to use the GNAT Pro tool set (currently version
24.1) for our development. Some experimentation with using Alire and the community tool chain
has been done, but that build environment is not officially supported by us at this time.

Some experimentation has also been done with using Visual Studio Code as a development
environment. The file `CubedOS.code-workspace` defines a VSCode workspace for the project.


Build & Test Instructions
-------------------------

The basic build of CubedOS can be done by opening the `cubedos.gpr` project file using
GNATstudio. Various executables are defined as follows.

+ `cubedos_check` is a unit test program for various parts of the CubedOS core system.

+ `main_file` demonstrates the file server interface with a program that reads and displays the
  contents of a file using verbose output. This program does not terminate since the CubedOS
  message handling loops are non-terminating. However, when the file has been completely read
  you should see output about Close_Request messages being sent for both input and output files.
  
+ `main_time` demonstrates the timeserver module. It prints messages for periodic ticks and
  a one-shot tick for about 30 seconds. It then cancels the periodic ticks. The program does not
  terminate for the same reason as `main_file` but after the cancelation of Series #1 there
  should be no further output.
  
These programs are all written to the `src/check/build` folder.

The sample programs have their own project files. At this time there is no master project file
defined for all CubedOS components.


Merc Helper Application
-----------------------

The program Merc, in a separate repository, is a Scala program for generating CubedOS API
packages. It is possible to write these packages manually following, for example, the templates
in the `template` folder (which are extensively commented). Merc makes the job much easier.
Information about Merc, and how to build and use it, can be found in the Merc repository.
*Current there is an issue with Merc that limits its usefulness. Most API packages in CubedOS
have been manually written!*

