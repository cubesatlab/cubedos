
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


Build & Test Instructions
-------------------------

We develop CubedOS on x86-64 platforms running Ubuntu Linux 22.04 (primary). Development on
Windows or macOS is probably possible, but we do not use those systems. Most of the test,
sample, and benchmark programs can be compiled and run on the development host. Some programs
that target other platforms are also included. Final testing and performance evaluations should
be done on your specific target platform, of course.

We use the GNAT Pro v24 toolset for development. Some experimentation has been done with using
[Alire](https://alire.ada.dev/docs/) as a build tool for CubedOS and its sample programs, but
that work is incomplete. The `alire.toml` file at the root of the CubedOS source tree should be
considered experimental.

CubedOS is an application framework and, as such, not a stand-alone program. However, there are
several executables that can be built to test various aspects of the system. Also the `samples`
folders contain several other samples that might be of interest.

+ `cubedos_check` is a limited unit test program for various parts of CubedOS.

+ `main` is a sort of "no-op" CubedOS program. It does nothing, but it does include all the core
  modules. As such it can be a useful starting point for analysis. Also building it will force
  the compilation of all the core modules and the library components those modules depend upon.

+ `main_file` demonstrates the file server interface with a program that reads and displays the
  contents of a file using verbose output. This program does not terminate since the CubedOS
  message handling loops are non-terminating. However, when the file has been completely read
  you should see output about Close_Request messages being sent for both input and output files.
  *This program appears to not entirely work at this time*
  
+ `main_time` demonstrates the time server module. It prints messages for periodic ticks and
  a one-shot tick for about 30 seconds. It then cancels the periodic ticks. The program does not
  terminate for the same reason as `main_file` but after the cancelation of Series #1 there
  should be no further output.
  
These programs are all written to the `src/check/build` folder.

The program Merc, in a separate repository, is a Scala program for generated CubedOS API
packages. It is possible to write these packages manually following, for example, the templates
in the `template` folder (which are extensively commented). Merc makes the job much easier, of
course. Information about Merc, and how to build and use it, can be found in the Merc
repository. Note that Merc is not fully implemented and may not be able to generate all API
files.

