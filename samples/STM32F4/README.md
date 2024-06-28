
README
======

This folder contains a demonstration of CubedOS running on an STM32F4-discovery board.
*Currently, this program can't be compiled because the Log Server requires Ada.Calendar which is
not supported by this platform*.

Board Information
-----------------

The board we assume [can be
purchased](https://www.mouser.com/ProductDetail/STMicroelectronics/STM32F407G-DISC1?qs=mKNKSX85ZJejxc9JOGT45A%3D%3D&gclid=EAIaIQobChMIg8CF9d305gIViZWzCh3ONg1MEAQYASABEgJN0fD_BwE)
from, for example, Mouser Electronics. The data brief on the board is also included in this
repository as a convenience in the file `stm32f4discovery.pdf`. It is possible that the program
here will work on other, similar boards.

Note that CubedOS requires Ravenscar support. The "zero footprint" runtime system is not
adequate. Fortunately AdaCore supports Ravenscar runtime systems for this board.

Getting Started
---------------

To run this demonstration you first need to install the ARM generating Ada cross compiler from
AdaCore. Start by getting the [community edition of GNAT GPL](http://www.adacore.com/community)
that targets ARM in ELF format. You can install this alongside a native generating version of
GNAT with no problems.

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
