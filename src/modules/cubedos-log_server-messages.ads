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
with Ada.Text_IO;
with Ada.Calendar;
with System;

package CubedOS.Log_Server.Messages is

   task Message_Loop
     with
       Global => (Input  => Ada.Calendar.Clock_Time,
                  In_Out => (Ada.Text_IO.File_System, Message_Manager.Mailboxes))
   is
      pragma Priority(System.Default_Priority);
   end Message_Loop;

end CubedOS.Log_Server.Messages;
