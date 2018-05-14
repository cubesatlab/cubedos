--------------------------------------------------------------------------------
-- FILE   : receiver-api.ads
-- SUBJECT: Specification of a package that simplifies use of the receiver.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
-- All the subprograms in this package must be task safe. They can be simultaneously called
-- from multiple tasks. If possible, make every subprogram here a pure function.
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;

use Message_Manager;

package Receiver.API is

   function Make_Message(Sender : Module_ID) return Message_Record;

end Receiver.API;
