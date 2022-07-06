
CubedOS Samples
===============

+ `BBB`: A sample application that runs on the BeagleBone Black. This sample demonstrates that
  CubedOS applications will run on that platform. [This sample should be changed to be another
  build scenario in the Echo sample. This code has *not* yet been updated to reflect the
  Name_Resolver discipline.]
  
+ `Echo`: A sample application that where two modules send messages back and forth indefinitely.
  It is the "Hello, World" application of message passing! We use this sample to evaluate rough
  performance and as a base for other, more interesting programs.
  
+ `LineRider`: A sample application using the STMF4DISCOVERY platform to demonstrate some of
  CubedOS's abilities. Specifically, the program controls a simple robot that uses optical
  sensors to follow a black line drawn on a light, concrete floor. This is an adaptation of a
  lab assignment used in the electro-mechanical program at Vermont Technical College. [This code
  has *not* yet been updated to reflect the Name_Resolver discipline.]
  
+ `Networking`: A sample application that illustrates message passing across domains using UDP
  on a conventional TCP/IP network. This is essentially the Echo sample reworked as a
  multi-domain application.
  
+ `Pathfinder`: A sample application that demonstrates priority inversion in the CubedOS message
  passing system. Roughly this sample replicates the issue that existed in the Mars Pathfinder
  mission, albeit as a limited demonstration. When (if) priority inheritance is implemented in
  CubedOS, this sample can be used to check that the implementation solves the inversion
  problem.
  
+ `PubSub`: A sample application that demonstrates the Publish/Subscribe server. [Not yet
  implemented.]

+ `STM32F4`: This folder contains a simple demonstration of CubedOS running on the
  STM32F4-discovery board.

+ `Tests`: I'm not sure what this is. I think it is just a holding area for CubedOS tests that
  need to be integrated into the test program.

