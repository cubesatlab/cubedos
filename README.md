
CubedOS
=======

A flight software framework for CubeSat spacecraft written in SPARK/Ada.

This folder contains the core CubedOS system. The sibling Merc repository contains the Merc tool
used for generating CubedOS API encoder/decoder subprograms. The subfolders are as follows:

+ bench: Benchmark programs for measuring the performance of CubedOS.

+ doc: The documentation of CubedOS.

+ LineRider: A sample application using the STMF4DISCOVERY platform demonstrate some of
  CubedOS's abilities.

+ PingPong: A sample application that where two modules send messages back and forth infinitely.
  It is the "Hello, World" application of message passing! We use to to evaluate performance and
  as a base for other, more interesting programs.

+ src: The CubedOS source code repository.

+ STM32F4: This folder is old. I'm not even sure what is in it!

+ templates: Various templates to facilitate the construction of CubedOS applications. The
  developer can use these templates to simplify the programming of new applications, however
  their use is not strictly required.


Build Instructions
------------------

These instructions assume you are using the official CubeSat Laboratory virtual machine at
Vermont Technical College. If you are not, you will need to install a fairly large collection of
tools and libraries before these instructions will work. Details on how to do that should
eventually be added to this document!

To install the CubeSat Laboratory VM do the following:

1. Install the latest VirtualBox with its corresponding extension pack.

2. Download the virtual appliance file from its designated location. Note this file is currently
   only available to VTC faculty and students working on this project, since it contains
   commercial software specifically licensed to us.
    
3. In VirtualBox do an import of a "virtual appliance." Select the OVA file you just downloaded.

4. After the import completes, you might check the settings of the VM to be sure a) it isn't
   configured to use too much memory for your host (not more than 50% of your host memory), and
   b) it isn't configured to use more cores than you actually have. If these conditions are not
   satisfied, performance of the VM will be very poor.

5. Boot the VM by double clicking its name in the VirtualBox console. You might have to adjust
   the window size for your host display. That will also synchronize the virtual display size to
   match your new window size. Note: I've had VirtualBox occasionally lock up when the window is
   resized. Hopefully you won't have that problem.

Once inside the VM I recommend immediately updating the CubedOS clone. Open a terminal window
(start at the upper left corner of the display and find Terminal in the menus). Then do:

    $ cd Projects/CubedOS
    $ git pull

I recommend doing this regularly to ensure you have the latest version of the system.

Building our current code base is complicated by the fact that we are using a custom tool, Merc,
to generate the module API code. Merc is stored in a separate repository. Thus it is necessary
to first update Merc using commands such as:

    $ cd Projects/Merc
    $ git pull

To build Merc proceed as follows:

1. Start IntelliJ IDEA by executing the command: `idea &`

2. If it doesn't load automatically, load the project Merc in `~/Projects/Merc` (select the name
   of the folder).

3. From the menus do "Build -> Build Project." This will fail, but that is expected.

4. In the terminal change to the `Merc` folder and the run the command `bin/build-parser.sh`.

5. Back in IntelliJ again do "Build -> Build Project" and it should succeed. It is necessary to
   attempt a build first to stimulate IntelliJ to download the ANTLR library needed by the
   project. Only then will the build-parser script actually work. Yes, this is a horrible hack
   and, yes, it could be better. We just haven't gotten around to fixing it.

6. In IntelliJ do "Build -> Build Artifacts -> Merc:jar -> Build" to build the executable jar
   file.

7. Close IntelliJ. You only need to do this once each time the Merc code base is updated (and
   you'll most likely be able to skip step #3 except for the rare case when we change which
   version of ANTLR we're using). Ultimately this process can be simplified by using an Ant
   build script (under construction).

Now you need to use the Merc tool to build the API packages for the CubedOS core modules.
Actually, at the moment, there is only one module to worry about. In the terminal change to the
`CubedOS/src/MXDR` folder and run the command: `Merc tick_generator.mxdr`. This should use
the jar file you previously created to generate the files `cubedos-tick_generator-api.ads` and
`cubedos-tick_generator-api.adb` in the same folder.

Finally you are ready to build and run the test programs. Do the following:

1. In the terminal run the command: `source useGNAT.sh`. This configures that terminal for the Ada
   compiler and tools.

2. In the folder `CubedOS/src` run the command: `gps &`

3. Once GPS starts, click on the "Scenario" tab on the left edge of the window. There are two
   settings for BUILD: "Check" and "Simulator." Right now only the "Check" scenario does
   anything interesting so be sure it is selected.

4. On GPS's Build menu go to the "Project" submenu. You will see a list of test programs. Not
   all of them currently compile! You can compile the unit test program by selecting
   "cubedos_check.adb"

5. In the terminal find the executable in `CubedOS/src/build/Check` named `cubedos_check` and
   run it to execute the unit tests. Every line should have an "(Ok)" at the end.

If you get all that to work, then you should be in good shape!
