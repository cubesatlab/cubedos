--------------------------------------------------------------------------------
-- FILE   : spi-internals.ads
-- SUBJECT: Specification of a package that implements the main part of the SPI driver module.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;  use Message_Manager;

private package SPI.Internals is
   
   -- The initialization procedure, if required.
   procedure Initialize;
   
   -- The procedure that handles messages. This may be the only "working" subprogram needed.
   -- Because this is a private child package, it will only be used from the message loop and
   -- API package. Thus types and subprograms declared here are for internal use only and will
   -- not be accessible by module users.
   procedure Process_Message(Incoming_Message : in Message_Record);

end SPI.Internals;
