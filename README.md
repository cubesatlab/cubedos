
CubedOS
=======

A flight software framework for CubeSat spacecraft written in SPARK/Ada.

This folder contains the CubedOS system. It also contains two supporting applications: Mercury,
a tool for generating CubedOS API encoder/decoder subprograms, and ScriptBuilder, a command
script editor for CubedOS.

The core of CubedOS is written in SPARK/Ada with the verification goal of being proved free of
runtime error. Mercury is written in Scala. ScriptBuilder is written in Python.

The subfolders are as follows:

+ `bench`: Benchmark programs for measuring the performance of CubedOS.

+ `doc`: The documentation of CubedOS.

+ `mercury`: The interface definition language compiler for CubedOS.

+ `samples`: This folder contains various sample programs that illustrate CubedOS in action.

+ `scriptbuilder`: The command script editor for CubedOS.

+ `src`: The CubedOS source code repository.

+ `templates`: Various templates to facilitate the construction of CubedOS applications. The
  developer can use these templates to simplify the programming of new applications, however
  their use is not strictly required.
  
  
Development Environment
-----------------------

The official development platform for CubedOS is x86-64-based Ubuntu Linux 22.04. Development on
Windows and macOS is probably possible, but we do not use those systems. Most of the test,
sample, and benchmark programs can be compiled and run on the development host. Some programs
that target other platforms are also included.

With generous support from AdaCore, we are able to use the GNAT Pro tool set (currently version
24.1) for our development. Some experimentation with using Alire and the community tool chain
has been done, but that build environment is not officially supported by us at this time.

In addition to GNATstudio for the SPARK/Ada components of the project, we also support using
Visual Studio Code. The advantage of VSCode is that it supports development of the entire
project from a single environment: Ada, Scala, Python, and LaTeX. We describe how to set up
VSCode in more detail below.

The README files in the `mercury` and `scriptbuilder` folders give more information about those
subprojects, including what tools we use with them and how to configure those tools.


Build & Test Instructions
-------------------------

The basic build of CubedOS can be done by opening the `src/cubedos.gpr` project file using
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


Visual Studio Code
------------------

The file `CubedOS.code-workspace` contains a workspace definition for VSCode. It includes the
folders for all the subprojects, including the documentation folder. To use VSCode, simply open
that workspace and navigate to the folders and files you wish to edit.

The following extensions are recommended:

+ **Ada & SPARK** by AdaCore. This extension gives VSCode Ada-awareness and supports building
  the Ada portions of the project.

+ **LaTeX Workshop** by James Yu. This extension gives VSCode support for LaTeX editing. You
  might want to configure this extension to turn off auto-building. Since many of the
  documentation files are subdocuments, they cannot be built in isolation (*Can LaTeX Workshop
  be configured to build the master document whenever a subdocument is modified?*)

+ **LTeX LanguageTool** by Julian Valentin (Optional). This extension provides spelling and
  grammar checking for LaTeX and Markdown documents. You might want to disable it on program
  source code (e.g., Python).

+ **Python** by Microsoft. This extension provides Python support for VSCode.

+ **Rewrap** by stkb. This extension supports easy paragraph rewrapping in text files and
  program comments. We use a line wide of 96 characters for LaTeX and Markdown files, so you may
  need to configure that value.

+ **Scala (Metals)** by Scalameta. This extension provides Scala and SBT support for VSCode.

+ **Todo Tree** by Gruntfuggly (Optional). This extension summarizes all "todo" items in the entire
  workspace in one, easy-to-navigate location.

The configurations for some or all of these extensions may be committed to version control, so
you may not have to adjust any configurations. Also, be aware that some of these extensions will
install additional extensions as dependencies.

The Ada & SPARK extension expects the Ada compiler and tools to be in the PATH environment when
VSCode is started. This is true for recently releases of the Skynet virtual machine used by the
CubeSat Laboratory team, but you may have to configure this separately if you are not using
Skynet.
