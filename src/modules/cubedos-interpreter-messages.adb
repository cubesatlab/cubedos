--------------------------------------------------------------------------------
-- FILE   : SAMPLE_MODULE-messages.adb
-- SUBJECT: Body of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);
pragma Warnings(Off);

with CubedOS.Message_Types; use CubedOS.Message_Types;
with CubedOS.Interpreter.API; use CubedOS.Interpreter.API;

package body CubedOS.Interpreter.Messages is

   Mailbox : aliased constant Module_Mailbox := Make_Module_Mailbox(This_Module, Mail_Target);

   procedure Init is
   begin
      Register_Module(Mailbox, 8);
   end Init;

   -------------------
   -- Message Handling
   -------------------

   procedure Handle_Clear_Request(Message : in Message_Record)
     with Pre => CubedOS.Interpreter.API.Is_Clear_Request(Message)
   is
      Status : Message_Status_Type;
   begin
      null;
      -- Act on the request message.
   end Handle_Clear_Request;


   procedure Handle_Set_Request(Message : in Message_Record)
     with Pre => CubedOS.Interpreter.API.Is_Set_Request(Message)
   is
      Status : Message_Status_Type;
   begin
      null;
      -- Act on the request message.
   end Handle_Set_Request;


   procedure Handle_Add_Request(Message : in Message_Record)
     with Pre => CubedOS.Interpreter.API.Is_Add_Request(Message)
   is
      Status : Message_Status_Type;
   begin
      null;
      -- Act on the request message.
   end Handle_Add_Request;

   -----------------------------------
   -- Message Decoding and Dispatching
   -----------------------------------

   -- This procedure processes exactly one message.
   procedure Process(Message : in Message_Record) is
   begin
      if CubedOS.Interpreter.API.Is_Clear_Request(Message) then
         Handle_Clear_Request(Message);
      elsif CubedOS.Interpreter.API.Is_Set_Request(Message) then
         Handle_Set_Request(Message);
      elsif CubedOS.Interpreter.API.Is_Add_Request(Message) then
         Handle_Add_Request(Message);
      end if;
      -- When this procedure returns the message loop will immediately try to receive the next
      -- message. Note that all CubedOS send operations are non-blocking so sending an outgoing
      -- message will not delay execution.
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

end CubedOS.Interpreter.Messages;
