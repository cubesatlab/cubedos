--------------------------------------------------------------------------------
-- FILE   : receiver-internals.ads
-- SUBJECT: Specification of a package that implements the main part of the receiver.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;

use Message_Manager;

private package Receiver.Internals is

   procedure Process_Message(Incoming_Message : in Message_Record);

end Receiver.Internals;
