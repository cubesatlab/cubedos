
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

We develop CubedOS on x86-64 platforms running Ubuntu Linux 18.04 (primary) and Windows 10
(secondary). Development on the MacOS is probably possible, but we do not use that system. Most
of the test, sample, and benchmark programs can be compiled and run on the development host.
Some programs that target other platforms are also included. Final testing and performance
evaluations should be done on your specific target platform, of course.

The [GNAT Community](http://www.adacore.com/community/) Ada compiler and toolset (v2019) is
sufficient for development at this time. Some non-x86 targets are provided by GNAT Community,
but other targets are available from AdaCore via their professional subscription.

The basic build of CubedOS can be done by opening the `cubedos.gpr` project file in the `src`
folder using GPS. Two build scenarios are defined. The "Check" scenario builds various test
programs. Specifically:

+ `cubedos_check` is a limited unit test program for various parts of CubedOS.

+ `main_file` demonstrates the file server interface with a program that reads and displays the
  contents of a file using verbose output. This program does not terminate since the CubedOS
  message handling loops are non-terminating. However, when the file has been completely read
  you should see output about Close_Request messages being sent for both input and output files.
  
+ `main_sorters` demonstrates some sorting algorithms (this will eventually be merged with the
  unit test program).
  
+ `main_tick` demonstrates the tick generator module. It prints messages for periodic ticks and
  a one-shot tick for about 30 seconds. It then cancels the periodic ticks. The program does not
  terminate for the same reason as `main_file` but after the cancelation of Series #1 there
  should be no further output.
  
These programs are all written to the `src/build/Check` folder.

In addition there is a "Simulator" scenario that builds a CubeSat simulator running CubedOS.
*This build is skeletal only at this time!* The simulator program is written to the
`src/build/Simulator` folder.

The PingPong, LineRider, and STM32F4 programs have their own project files. At this time there
is no master project file defined for all CubedOS components.

The program Merc, in a separate repository, is a Scala program for generated CubedOS API
packages. It is possible to write these packages manually following, for example, the templates
in the `template` folder (which are extensively commented). Merc makes the job much easier, of
course. Information about Merc, and how to build and use it, can be found in the Merc
repository. *Current there is an issue with Merc that limits its usefulness. Most API packages
in CubedOS have been manually written!*

