
README
======

This folder contains the CubedOS core modules, supporting library packages, and test programs
for the same. The additional files are as follows:

+ `cubedos.aru`. This is a configuration file for the AdaControl style checker. We are not
  currently using AdaControl. This file is being retained for historic reasons and to facilitate
  using AdaControl in the future if we decide to do that.
  
+ `cubedos-casing-exceptions.txt`. This file contains exceptions to the casing rules when
  forming identifiers. It used by `gnatcheck`.
  
+ `cubedos.gpr`. The GNAT project file that controls the build of the core modules, library, and
  test programs.
  
+ `cubedos.hdr`. This is the standard header that appears on all CubedOS source files. It is
  used by AdaControl.
  
+ `cubedos-rules.txt`. This is the rules file used by `gnatcheck` to check the style of CubedOS
  code.
  
+ `gnat.adc`. This is the GNAT configuration file that contains program-wide compilation
  settings. It is currently not used in that its entire contents are commented out.
