--------------------------------------------------------------------------------
-- FILE   : cubedos-logger-messages.adb
-- SUBJECT: Body of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(Off);

with Ada.Text_IO;
with CubedOS.Lib.Bounded_Strings;
with CubedOS.Logger.API;

use CubedOS.Lib.Bounded_Strings;

package body CubedOS.Logger.Messages is
   use Message_Manager;
   use type Logger.API.Status_Type;

   type Module_Name_Array is array(Module_ID_Type) of Bounded_String(17);
   Module_Names : constant Module_Name_Array :=
     ( 1 => Make(Upper_Bound => 17, Initializer => "Tick Generator"),
       2 => Make(Upper_Bound => 17, Initializer => "Publish/Subscribe"),
       3 => Make(Upper_Bound => 17, Initializer => "File Server"),
       4 => Make(Upper_Bound => 17, Initializer => "CFDP"),
       5 => Make(Upper_Bound => 17, Initializer => "Spiral Thruster"),
       6 => Make(Upper_Bound => 17, Initializer => "BIT3"),
       7 => Make(Upper_Bound => 17, Initializer => "Logger"),
       8 => Make(Upper_Bound => 17, Initializer => "*** unused ***"),
       9 => Make(Upper_Bound => 17, Initializer => "*** unused ***"),
      10 => Make(Upper_Bound => 17, Initializer => "*** unused ***"),
      11 => Make(Upper_Bound => 17, Initializer => "*** unused ***"),
      12 => Make(Upper_Bound => 17, Initializer => "*** unused ***"),
      13 => Make(Upper_Bound => 17, Initializer => "*** unused ***"),
      14 => Make(Upper_Bound => 17, Initializer => "*** unused ***"),
      15 => Make(Upper_Bound => 17, Initializer => "*** unused ***"),
      16 => Make(Upper_Bound => 17, Initializer => "*** unused ***"));

   -------------------
   -- Message Handling
   -------------------

   procedure Handle_Log_Text(Message : in Message_Record)
     with Pre => Logger.API.Is_A_Log_Text(Message)
   is
      Text : String(Logger.API.Log_Message_Index_Type);
      Size : Logger.API.Log_Message_Size_Type;
      Status : Message_Status_Type;
   begin
      Logger.API.Log_Text_Decode(Message, Text, Size, Status);

      -- Ignore log messages that don't decode properly.
      -- TODO: We should also time stamp the messages.
      if Status = Success then
         Ada.Text_IO.Put_Line
           (Module_ID_Type'Image(Message.Sender) &
              "(" & To_String(Module_Names(Message.Sender)) & "): " & Text(1 .. Size));
      end if;
   end Handle_Log_Text;

   -----------------------------------
   -- Message Decoding and Dispatching
   -----------------------------------

   -- This procedure processes exactly one message.
   procedure Process(Message : in Message_Record) is
   begin
      if Logger.API.Is_A_Log_Text(Message) then
         Handle_Log_Text(Message);
      else
         -- An unknown message type has been received. What should be done about that?
         -- It seems like this should be logged somehow.
         null;
      end if;
   end Process;

   ---------------
   -- Message Loop
   ---------------

   task body Message_Loop is
      Incoming_Message : Message_Manager.Message_Record;
   begin
      loop
         Message_Manager.Fetch_Message(ID, Incoming_Message);
         Ada.Text_IO.Put_Line("#");
         Process(Incoming_Message);
      end loop;
   end Message_Loop;

end CubedOS.Logger.Messages;
