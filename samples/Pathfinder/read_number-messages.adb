--------------------------------------------------------------------------------
--  FILE   : read_number-messages.adb
--  SUBJECT: Body of a package that implements the main part of the module.
--  AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

--  Needed so that the types in the API can be used here.
with Read_Number.API;
with Random_Number_Generator.API;

package body Read_Number.Messages is
   use Message_Manager;
   ML1: Message_Loop;
   
   --  The package initializer, if needed.  This procedure might be
   --  called as the message loop (see below) is starting, or perhaps
   --  during package elaboration.  If this procedure is not needed,
   --  it should be removed to avoid SPARK flow issues.
   procedure Initialize is
      Outgoing_Message : Message_Record;
   begin
      Outgoing_Message := Read_Number.API.Read_Number_Request_Encode
        (Sender_Domain => Domain_ID,
         Sender        => ID,
         Request_ID    => R_ID,
         Priority      => Priority);
      
         Message_Manager.Route_Message(Outgoing_Message);    
   end Initialize;
   
   ----------------- Message Handling ---------------
   
   --  Here is where the details of handing the messages is done.
   --  Probably there will be a separate subprogram for each message,
   --  but the exact organization is, obviously, dependent on the
   --  needs of the module.  It might be useful to put these message
   --  handling subprograms into a private sibling package.  That
   --  would move the complex details of message handling to a
   --  different file and reduce clutter in this file.  This file is
   --  really just about decoding and dispatching the messages.  We
   --  recommend that if a single internal package is used that it
   --  sould be called Sample_Module.Core (for example).
   
   procedure Handle_Read_Number_Request(Message : in Message_Record)
     with Pre => Read_Number.API.Is_Read_Number_Request(Message)
   is
      Status : Message_Status_Type;
      Outgoing_Message, Self_Message : Message_Record;
   begin
      Read_Number.API.Read_Number_Request_Decode(Message, Status);
      --  Act on the request message.
      
      Self_Message := Read_Number.API.Read_Number_Request_Encode
        (Sender_Domain => Domain_ID,
         Sender        => ID,
         Request_ID    => R_ID,
         Priority      => Priority);
      
      Outgoing_Message := Random_Number_Generator.API.Generate_Number_Request_Encode
        (Sender_Domain => Domain_ID,
         Sender        => ID,
         Request_ID    => R_ID,
         Priority      => Priority);
      
      Message_Manager.Route_Message(Outgoing_Message);
      Message_Manager.Route_Message(Self_Message);
   end Handle_Read_Number_Request;
   
   -----------------------------------
   --  Message Decoding and Dispatching
   -----------------------------------
   
   -- This procedure processes exactly one message.
   procedure Process(Message : in Message_Record) is
   begin
      if Read_Number.API.Is_Read_Number_Request(Message) then
         Handle_Read_Number_Request(Message);
      else
	 --  An unknown message type has been received. What should be
	 --  done about that?
         null;
      end if;
      --  When this procedure returns the message loop will
      --  immediately try to receive the next message.  Note that all
      --  CubedOS send operations are non-blocking so sending an
      --  outgoing message will not delay execution.
   end Process;
   
   ---------------
   -- Message Loop
   ---------------
   
   task body Message_Loop is
      Incoming_Message : Message_Manager.Message_Record;
   begin
      --  Initialize the internal workings of the module (if required)
      --  before processing any of its messages.  It may instead be
      --  appropriate for the package to initialize itself at
      --  elaboration time.  Note that SPARK requires that
      --  applications use the configuration pragma of
      --  Partition_Elaboration_Policy set to Sequential.  This
      --  ensures that all packages are elaborated before any library
      --  level tasks start.
      --
      Initialize;

      --  Process messages as they arrive.  This simple loop may be
      --  all that is needed.  It may also be appropriate to do some
      --  prechecks or preprocessing of messages before calling
      --  Process_Message.
      loop
         Message_Manager.Fetch_Message(ID, Incoming_Message);
         Process(Incoming_Message);
      end loop;
   end Message_Loop;

end Read_Number.Messages;
