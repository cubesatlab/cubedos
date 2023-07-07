--------------------------------------------------------------------------------
-- FILE   : cubedos-log_server-messages.adb
-- SUBJECT: Body of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2022 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);
with CubedOS.Message_Types; use CubedOS.Message_Types;


package body CubedOS.Log_Server.Messages is

   Mailbox : aliased constant Module_Mailbox := Make_Module_Mailbox(This_Module, Mail_Target);

   procedure Initialize
   is
   begin
      Message_Manager.Register_Module(Mailbox, 8);
   end Initialize;

   -------------------
   -- Message Handling
   -------------------

   procedure Handle_Log_Text(Message : in Message_Record)
     with Pre => Log_Server.API.Is_Log_Text(Message)
       and Is_Valid(Message)
   is
      Log_Level : Log_Server.API.Log_Level_Type;
      Text      : Log_Server.API.Log_Message_Type;
      Size      : Log_Server.API.Log_Message_Size_Type;
      Status    : Message_Status_Type;

      Level_Strings : constant array(Log_Server.API.Log_Level_Type) of String(1 .. 3) :=
        ("DBG", "INF", "WRN", "ERR", "CRI");
   begin
      Log_Server.API.Log_Text_Decode(Message, Log_Level, Text, Size, Status);

      -- Ignore log messages that don't decode properly.
      -- TODO: We should also time stamp the messages.
      if Status = Success then
         Ada.Text_IO.Put_Line
           (Level_Strings(Log_Level) &
            " ("  & Domain_ID_Type'Image(Sender_Address(Message).Domain_ID) &
            ","   & Module_ID_Type'Image(Sender_Address(Message).Module_ID) &
            "): " & Text(1 .. Size));
      end if;
   end Handle_Log_Text;

   -----------------------------------
   -- Message Decoding and Dispatching
   -----------------------------------

   -- This procedure processes exactly one message.
   procedure Process(Message : in Message_Record)
     with Pre => Is_Valid(Message)
   is
   begin
      if Log_Server.API.Is_Log_Text(Message) then
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
      Incoming_Message : Message_Record;
   begin
      Message_Manager.Wait;

      loop
         Read_Next(Mailbox, Incoming_Message);
         Process(Incoming_Message);
         Delete(Incoming_Message);
         pragma Loop_Invariant(Payload(Incoming_Message) = null);
      end loop;
   end Message_Loop;

end CubedOS.Log_Server.Messages;
