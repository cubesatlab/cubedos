--------------------------------------------------------------------------------
-- FILE   : cubedos.log_server-messages.ads
-- SUBJECT: Specification of a package that implements the main part of the log server.
-- AUTHOR : (C) Copyright 2024 by Vermont State University
--
-- This implementation makes use of the consol IO facilities of a hosted implemenation (i.e., an Ada
-- implementation running on a full operating system such as Linux). In a mission, the application developers
-- would likely create a specialized body for this package that stores logged messages in some other way.
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

-- This implementation makes use of console IO.
with Ada.Real_Time;
with Ada.Text_IO;
with System;

package CubedOS.Log_Server.Messages is

   -- These are declared here so they can be mentioned in the Global aspect of Message_Loop.
   -- My attempts to use Abstract_State didn't work; it required an initialization procedure for the state.
   Boot_Time    : Ada.Real_Time.Time; -- When the system starts.
   Current_Time : Ada.Real_Time.Time; -- When the current message is fetched.

   task Message_Loop
     with
       Global => (Input  => Ada.Real_Time.Clock_Time,
                  In_Out => (Ada.Text_IO.File_System, Message_Manager.Mailboxes),
                  Output => (Boot_Time, Current_Time))
   is
      pragma Priority(System.Default_Priority);
   end Message_Loop;

end CubedOS.Log_Server.Messages;
