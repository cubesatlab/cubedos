--------------------------------------------------------------------------------
-- FILE   : SAMPLE_MODULE-messages.adb
-- SUBJECT: Body of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2024 by Vermont State University
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;    -- See the comments in SAMPLE_MODULE-api.ads.
with Name_Resolver;      -- See the comments in SAMPLE_MODULE-api.ads.
with Sample_Module.API;  -- Needed so that the types in the API can be used here.
with CubedOS.Message_Types;

package body Sample_Module.Messages is
   use Message_Manager;
   use CubedOS.Message_Types;

   -- Create the module's mailbox. This object contains metadata about the module
   -- imported from the API file which is necessary to use the message system.
   -- Every module has one and should keep it privately visible.
   Mailbox : aliased constant Module_Mailbox := Make_Module_Mailbox(This_Module, Mail_Target);

   -- The package initializer, if needed. This procedure might be called as the message loop
   -- (see below) is starting, or perhaps during package elaboration. If this procedure is not
   -- needed, it should be removed to avoid SPARK flow issues.
   --
   procedure Initialize is
   begin
      null;
   end Initialize;

   procedure Init
   is
   begin
      -- Register the mailbox object with the message system. The message system will
      -- not allow messages to be sent or received until all modules in the domain have
      -- registered themselves.
      Register_Module(Mailbox, 8);
   end Init;

   -------------------
   -- Message Handling
   -------------------

   -- Here is where the details of handing the messages is done. Probably there will be a
   -- separate subprogram for each message, but the exact organization is, obviously, dependent
   -- on the needs of the module. It might be useful to put these message handling subprograms
   -- into a private sibling package. That would move the complex details of message handling to
   -- a different file and reduce clutter in this file. This file is really just about decoding
   -- and dispatching the messages. We recommend that if a single internal package is used that
   -- it sould be called Sample_Module.Core (for example).

   procedure Handle_A_Request(Message : in Message_Record)
     with Pre => Sample_Module.API.Is_A_Request(Message)
     and Payload(Incoming_Message) /= null,
     Post => Payload(Incoming_Message) /= null
   is
      Status : Message_Status_Type;
   begin
      -- Sample_Module.API.A_Request_Decode(Message, Status);
      -- Act on the request message.
      -- If the message contained data, it would have a decode function.
   end Handle_A_Request;

   -----------------------------------
   -- Message Decoding and Dispatching
   -----------------------------------

   -- This procedure processes exactly one message.
   procedure Process(Message : in Message_Record)
     with Pre => Payload(Incoming_Message) /= null,
     Post => Payload(Incoming_Message) /= null
   is
   begin
      if Sample_Module.API.Is_A_Request(Message) then
         Handle_A_Request(Message);
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
      -- Wait for the message system to finish initializing.
      -- This guarantees it is safe to send and receive messages.
      Message_Manager.Wait;

      -- Initialize the internal workings of the module (if required) before processing any of
      -- its messages. It may instead be appropriate for the package to initialize itself at
      -- elaboration time. Note that SPARK requires that applications use the configuration
      -- pragma of Partition_Elaboration_Policy set to Sequential. This ensures that all
      -- packages are elaborated before any library level tasks start.
      Initialize;

      -- Process messages as they arrive. This simple loop may be all that is needed. It may
      -- also be appropriate to do some prechecks or preprocessing of messages before calling
      -- Process_Message.
      loop
         pragma Loop_Invariant(Payload(Incoming_Message) = null);
         -- The message read is guaranteed to be of a type this module
         -- is allowed to receive, as specified by the API definition.
         Read_Next(Mailbox, Incoming_Message);
         Process (Incoming_Message);
         -- Message payloads are dynamically allocated and so must be
         -- deleted manually when discarding the message.
         Delete(Incoming_Message);
      end loop;

   end Message_Loop;

end Sample_Module.Messages;
