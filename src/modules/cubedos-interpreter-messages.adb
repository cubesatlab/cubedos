--------------------------------------------------------------------------------
-- FILE   : SAMPLE_MODULE-messages.adb
-- SUBJECT: Body of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.Interpreter.API;  -- Needed so that the types in the API can be used here.
with Name_Resolver;
with CubedOS.Log_Server.API;

package body CubedOS.Interpreter.Messages is
   use Message_Manager;

   procedure Initialize is
   begin
      null;
   end Initialize;

   -------------------
   -- Message Handling
   -------------------

   procedure Handle_Clear_Request(Message : in Message_Record)
     with Pre => CubedOS.Interpreter.API.Is_Clear_Request(Message)
   is
      Status : Message_Status_Type;
   begin
      CubedOS.Interpreter.API.Clear_Request_Decode(Message, Status);
      -- Act on the request message.
   end Handle_Clear_Request;


   procedure Handle_Set_Request(Message : in Message_Record)
     with Pre => CubedOS.Interpreter.API.Is_Set_Request(Message)
   is
      Status : Message_Status_Type;
   begin
      CubedOS.Interpreter.API.Set_Request_Decode(Message, Status);
      -- Act on the request message.
   end Handle_Set_Request;


   procedure Handle_Add_Request(Message : in Message_Record)
     with Pre => CubedOS.Interpreter.API.Is_Add_Request(Message)
   is
      Status : Message_Status_Type;
   begin
      CubedOS.Interpreter.API.Add_Request_Decode(Message, Status);
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
      else
         CubedOS.Log_Server.API.Log_Message(Name_Resolver.Interpreter,
                                            CubedOS.Log_Server.API.Error,
                                            "An unknown message type has been received!");
      end if;
      -- When this procedure returns the message loop will immediately try to receive the next
      -- message. Note that all CubedOS send operations are non-blocking so sending an outgoing
      -- message will not delay execution.
   end Process;

   ---------------
   -- Message Loop
   ---------------

   task body Message_Loop is
      Incoming_Message : Message_Manager.Message_Record;
   begin
      Initialize;

      loop
         Message_Manager.Fetch_Message(Name_Resolver.Interpreter.Module_ID, Incoming_Message);
         Process(Incoming_Message);
      end loop;
   end Message_Loop;

end CubedOS.Interpreter.Messages;
