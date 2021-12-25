
Demo
====

This folder contains a CubedOS demonstration program consisting of two modules that trade
messages back and forth (echo client/server modules). This is effectively the "Hello, World"
program of message passing systems.

This program is intended to be compiled for a conventional operating system such as Windows or
Linux. It allows one to experiment with the CubedOS system in an environment that is a little
easier to manipulate than an embedded environment.

Notice that in this demonstration program there is no Echo_Client API. This is because only
server modules define messages. In a realistic CubedOS application a module might play the role
of both a client and a server. However, in that case it would only define messages related to
its server role.
