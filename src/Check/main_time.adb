--------------------------------------------------------------------------------
-- FILE   : main_time.adb
-- SUBJECT: The main file to test the time server package.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
-- In order to test the time server module, you must run the produced executable in a console
-- window, and ^C out of it when a suitable time has passed. Then observe the behavior to ensure
-- the desired effects are happening.
--------------------------------------------------------------------------------
with Ada.Integer_Text_IO;
with Ada.Real_Time;
with Ada.Text_IO;
with CubedOS.Time_Server;
with CubedOS.Time_Server.API;
with CubedOS.Time_Server.Messages;
pragma Unreferenced(CubedOS.Time_Server.Messages);
with Message_Manager;
with GNAT.Time_Stamp;

use Ada.Integer_Text_IO;
use Ada.Text_IO;
use CubedOS.Time_Server;
use CubedOS.Time_Server.API;
use Message_Manager;

procedure Main_Time is
   use type Ada.Real_Time.Time;

   package Duration_IO is new Fixed_IO(Duration);
   use Duration_IO;

   -- Be sure this module ID doesn't conflict with any of the CubedOS core modules.
   My_Module_ID : constant Message_Manager.Module_ID_Type := Module_ID_Type'Last;

   Incoming_Message : Message_Manager.Message_Record;
   Series_ID  : Series_ID_Type;
   Count      : Natural;
   Status     : Message_Status_Type;
   Start_Time : Ada.Real_Time.Time;
   Relative_Time : Ada.Real_Time.Time_Span;
   Relative_Duration : Duration;
   Absolute_Time : String := GNAT.Time_Stamp.Current_Time;



begin
   Start_Time := Ada.Real_Time.Clock;

   -- Do some setup...
   Message_Manager.Route_Message
     (Relative_Request_Encode(Domain_ID, My_Module_ID, 1, Ada.Real_Time.Milliseconds(3000), Periodic, 1));
   Put_Line("TX : Relative_Request message sent for 3 second periodic ticks; Series_ID = 1");

   Message_Manager.Route_Message
     (Relative_Request_Encode(Domain_ID, My_Module_ID, 1, Ada.Real_Time.Milliseconds(10000), One_Shot, 2));
   Put_Line("TX : Relative_Request message sent for 10 second one shot; Series_ID = 2");

   loop
      Message_Manager.Fetch_Message(My_Module_ID, Incoming_Message);
      --Put_Line("+++ Fetch returned!");
      --Put("+++    Sender    : "); Put(Incoming_Message.Sender); New_Line;
      --Put("+++    Receiver  : "); Put(Incoming_Message.Receiver); New_Line;
      --Put("+++    Message_ID: "); Put(Integer(Incoming_Message.Message_ID)); New_Line(2);

      Relative_Time := Ada.Real_Time.Clock - Start_Time;
      Relative_Duration := Ada.Real_Time.To_Duration(Relative_Time);
      Absolute_Time := GNAT.Time_Stamp.Current_Time;
      if Is_Tick_Reply(Incoming_Message) then
         Tick_Reply_Decode(Incoming_Message, Series_ID, Count, Status);
         if Status = Success then
            Put("Time Duration: "); Put(Relative_Duration); Put("     Time Stamp:      "); Put(Absolute_Time);
            Put("      Series " & Series_ID_Type'Image(Series_ID) & " -- "); Put(Count); New_Line;

            -- Cancel series #1 after 10 ticks.
            if Series_ID = 1 and then Count = 10 then
               Message_Manager.Route_Message
                 (Cancel_Request_Encode(Domain_ID, My_Module_ID, 1, Series_ID => 1));
               Put_Line("TX : Cancel_Request message sent for Series_ID = 1");
            end if;

         end if;
      end if;
   end loop;

end Main_Time;
