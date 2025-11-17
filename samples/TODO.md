
TODO
====

Here are some todo-items related to the sample programs.

+ The `BBB` sample is the `echo` sample executing on a BeagleBone Black (a Linux-based
  single-board computer). It has not been updated to reflect the architectural changes made for
  multi-domain support. Also, since it is intended to essentially be a duplicate of the `echo`
  sample, it should probably really be implemented as a new build scenario in the `echo` sample
  rather than as a separate sample of its own.

+ The `echo_alire` sample is intended to demonstrate the use of Alire as a build/packaging tool
  for CubedOS. Does it still work? Should we consider deleting it? We are very far from using
  Alire, but we would need to start somewhere.

+ The `line_rider` sample is in the process of being converted to the new architecture. Can we
  find any hardware to run this on to properly test it? Even once it is converted, it may not
  work at all.

+ The `moonshot` sample is our version of the tutorial application described in the
  documentation.

+ The `pathfinder` sample compiles and runs. It is intended to demonstrate priority inversion,
  but it hasn't been checked to see if that is really true.

+ The `STM32F4` sample compiles and runs on a target STM32F4DISCOVERY board. However, it doesn't
  work 100%. In particular, there is no response to pressing the "User" button. This might be a
  problem with the publish/subscribe server not forwarding button-press messages, but that is
  unclear.

+ The contents of the `tests` folder should be reviewed, moved, and eliminated.
