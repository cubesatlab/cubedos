--------------------------------------------------------------------------------
-- FILE   : cubedos-interpreter-api.ads
-- SUBJECT: Specification of a package that simplifies use of the interpreter.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
-- All the subprograms in this package must be task safe. They can be simultaneously called
-- from multiple tasks. If possible, make every subprogram here a pure function.
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;  use Message_Manager;
with Name_Resolver;

with System;

package CubedOS.Interpreter.API is

   This_Module : constant Module_ID_Type := Name_Resolver.Interpreter.Module_ID;

   type Status_Type is (Success, Failure);

   type Message_Type is
     (Clear_Request,  -- Remove all pending commands.
      Set_Request,    -- Replaces pending commands with incoming command set.
      Set_Reply,      -- Indicates if replacement was successful. Failure => insufficient space.
      Add_Request,    -- Adds incoming command set to the pending commands.
      Add_Reply);     -- Indicates if the addition was successful. Failure => insufficient space.

   function Clear_Request_Encode
     (Sender_Address : in Message_Address;
      Request_ID     : in Request_ID_Type;
      Priority       : in System.Priority := System.Default_Priority) return Message_Record
   with Global => null;

   function Set_Request_Encode
     (Sender_Address : in Message_Address;
      Request_ID     : in Request_ID_Type;
      Priority       : in System.Priority := System.Default_Priority) return Message_Record
   with Global => null;

   function Set_Reply_Encode
     (Receiver_Address : in Message_Address;
      Request_ID       : in Request_ID_Type;
      Status           : in Status_Type;
      Priority         : in System.Priority := System.Default_Priority) return Message_Record
   with Global => null;

   function Add_Request_Encode
     (Sender_Address : in Message_Address;
      Request_ID     : in Request_ID_Type;
      Priority       : in System.Priority := System.Default_Priority) return Message_Record
   with Global => null;

   function Add_Reply_Encode
     (Receiver_Address : in Message_Address;
      Request_ID       : in Request_ID_Type;
      Status           : in Status_Type;
      Priority         : in System.Priority := System.Default_Priority) return Message_Record
   with Global => null;

   function Is_Clear_Request(Message : in Message_Record) return Boolean is
     (Message.Message_Type = (This_Module, Message_Type'Pos(Clear_Request)));

   function Is_Set_Request(Message : in Message_Record) return Boolean is
     (Message.Message_Type = (This_Module, Message_Type'Pos(Set_Request)));

   function Is_Set_Reply(Message : in Message_Record) return Boolean is
     (Message.Message_Type = (This_Module, Message_Type'Pos(Set_Reply)));

   function Is_Add_Request(Message : in Message_Record) return Boolean is
     (Message.Message_Type = (This_Module, Message_Type'Pos(Add_Request)));

   function Is_Add_Reply(Message : in Message_Record) return Boolean is
     (Message.Message_Type = (This_Module, Message_Type'Pos(Add_Reply)));

   procedure Clear_Request_Decode(Message : in  Message_Record; Decode_Status : out Message_Status_Type)
     with
       Global => null,
       Depends => (Decode_Status => Message),
       Pre => Is_Clear_Request(Message);

   procedure Set_Request_Decode(Message : in  Message_Record; Decode_Status : out Message_Status_Type)
     with
       Global => null,
       Depends => (Decode_Status => Message),
       Pre => Is_Set_Request(Message);

   procedure Set_Reply_Decode(Message : in  Message_Record; Decode_Status : out Message_Status_Type)
     with
       Global => null,
       Depends => (Decode_Status => Message),
       Pre => Is_Set_Reply(Message);

   procedure Add_Request_Decode(Message : in  Message_Record; Decode_Status : out Message_Status_Type)
     with
       Global => null,
       Depends => (Decode_Status => Message),
       Pre => Is_Set_Request(Message);

   procedure Add_Reply_Decode(Message : in  Message_Record; Decode_Status : out Message_Status_Type)
     with
       Global => null,
       Depends => (Decode_Status => Message),
       Pre => Is_Set_Reply(Message);

end CubedOS.Interpreter.API;
