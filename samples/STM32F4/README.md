
README
======

This folder contains a demonstration of CubedOS running on an STM32F4DISCOVERY board.

Board Information
-----------------

The board we assume [can be
purchased](https://www.mouser.com/ProductDetail/STMicroelectronics/STM32F407G-DISC1?qs=mKNKSX85ZJejxc9JOGT45A%3D%3D&gclid=EAIaIQobChMIg8CF9d305gIViZWzCh3ONg1MEAQYASABEgJN0fD_BwE)
from, for example, Mouser Electronics. The data brief and user's manual for the board can be
found on STMicroelectronics's website (it is also on the VTSU CubeSat Laboratory References
Server). It is possible that the program here will work on other, similar boards.

Note that CubedOS requires Jorvik support. The "zero footprint" runtime system is not adequate.
Fortunately AdaCore supports a "light tasking" runtime system for this board.

Getting Started
---------------

To run this demonstration, you first need to install the ARM generating Ada cross compiler from
AdaCore. You are looking for the compiler that targets ARM in ELF format. AdaCore also has ARM
generating compilers that target the Linux platform, but the STM32F4DISCOVERY board is a
bareboard system and does not run Linux. You can install the ARM compiler alongside a native
generating version of GNAT with no problems.

Next you want to follow the instructions, more or less, in the [ARM-ELF
tutorial](https://docs.adacore.com/gnat_ugx-docs/html/gnat_ugx/gnat_ugx/arm-elf_topics_and_tutorial.html)
on AdaCore's site. The (highly) abbreviated version of that tutorial is as follows:

+ Install the `stlink` tools for your development platform. These tools allows software on your
  development station to communicate with the board, e. g., for flashing and debugging purposes.

+ Run GPS on the project file in this folder. Use GPS to build the program.

+ Run the `st-util` tool from the `stlink` tools package. This is a debug server to which GPS
  will connect. The debug server will find and interact with your board.

+ You can use GPS to flash the board with your executable and to debug the program on the board.

Enjoy!
