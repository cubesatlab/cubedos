--------------------------------------------------------------------------------
-- FILE   : cubedos-file_server-api.ads
-- SUBJECT: Specification of a package that defines the CubedOS.File_Server API
-- AUTHOR : (C) Copyright 2024 by Vermont State University
--
-- All the subprograms in this package are task safe.
--
-- THIS FILE WAS GENERATED BY Merc. DO NOT EDIT!!
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);
pragma Warnings(Off);

with Name_Resolver;
with CubedOS.Lib; use CubedOS.Lib;
with Message_Manager;  use Message_Manager;
with CubedOS.Message_Types; use CubedOS.Message_Types;
with System;
with CubedOS.Lib.XDR; use CubedOS.Lib.XDR;
with Ada.Unchecked_Deallocation;


package CubedOS.File_Server.API is

   pragma Elaborate_Body;
   type Octet_Array_Ptr is access CubedOS.Lib.Octet_Array;
   type String_Ptr is access String;

   This_Module : constant Module_ID_Type := Name_Resolver.File_Server;

   type Message_Type is
      (Write_Reply,
      Read_Request,
      Write_Request,
      Close_Request,
      Open_Request,
      Read_Reply,
      Open_Reply);

   Write_Reply_Msg : constant Universal_Message_Type := (This_Module, Message_Type'Pos(Write_Reply));
   Read_Request_Msg : constant Universal_Message_Type := (This_Module, Message_Type'Pos(Read_Request));
   Write_Request_Msg : constant Universal_Message_Type := (This_Module, Message_Type'Pos(Write_Request));
   Close_Request_Msg : constant Universal_Message_Type := (This_Module, Message_Type'Pos(Close_Request));
   Open_Request_Msg : constant Universal_Message_Type := (This_Module, Message_Type'Pos(Open_Request));
   Read_Reply_Msg : constant Universal_Message_Type := (This_Module, Message_Type'Pos(Read_Reply));
   Open_Reply_Msg : constant Universal_Message_Type := (This_Module, Message_Type'Pos(Open_Reply));

   This_Receives : aliased constant Message_Type_Array := (
   Read_Request_Msg,
   Write_Request_Msg,
   Close_Request_Msg,
   Open_Request_Msg);
   Mail_Target : aliased constant Module_Metadata := Define_Module(This_Module, This_Receives'Access);

   type Mode_Type is (Read, Write);

   type File_Handle_Type is range (0) .. (64);

   subtype Valid_File_Handle_Type is File_Handle_Type range (1) .. (64);

   Invalid_Handle : constant File_Handle_Type := 0;

   Max_Read_Size : constant Integer := 256;

   Max_Write_Size : constant Integer := 256;

   type Read_Result_Size_Type is range (0) .. (Max_Read_Size);

   type Read_Size_Type is range (1) .. (Max_Read_Size);

   type Write_Result_Size_Type is range (0) .. (Max_Write_Size);

   type Write_Size_Type is range (1) .. (Max_Write_Size);

   subtype File_Name_Type is String
      with Dynamic_Predicate => File_Name_Type'Length <= 256;
   type File_Name_Type_Ptr is access File_Name_Type;
   procedure Free is new Ada.Unchecked_Deallocation(File_Name_Type, File_Name_Type_Ptr);

   procedure Open_Request_Encode
     (Receiver_Address : in Message_Address;
      Sender_Address : in Message_Address;
      Request_ID : in Request_ID_Type;
      Mode : in Mode_Type;
      Name : in File_Name_Type;
      Priority : in System.Priority := System.Default_Priority;
      Result : out  Message_Record)
   with
      Pre => True
         and then (0 < Name'Length and Name'Length <= XDR_Size_Type'Last - 12)
         and then Receiver_Address.Module_ID = This_Module,
      Post => CubedOS.Message_Types.Message_Type(Result) = Open_Request_Msg
         and CubedOS.Message_Types.Sender_Address(Result) = Sender_Address
         and CubedOS.Message_Types.Receiver_Address(Result) = Receiver_Address
         and Payload(Result) /= null;

   procedure Send_Open_Request
     (Sender : in Module_Mailbox;
      Receiver_Address : in Message_Address;
      Request_ID : in Request_ID_Type;
      Mode : in Mode_Type;
      Name : in File_Name_Type;
      Priority : in System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => Messaging_Ready
         and then (0 < Name'Length and Name'Length <= XDR_Size_Type'Last - 12)
         and then Receiver_Address.Module_ID = This_Module;

   procedure Send_Open_Request
     (Sender : in Module_Mailbox;
      Receiver_Address : in Message_Address;
      Request_ID : in Request_ID_Type;
      Mode : in Mode_Type;
      Name : in File_Name_Type;
      Status : out Status_Type;
      Priority : in System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => Messaging_Ready
         and then (0 < Name'Length and Name'Length <= XDR_Size_Type'Last - 12)
         and then Receiver_Address.Module_ID = This_Module
         and then Receiver_Address.Domain_ID = Domain_ID;

   procedure Send_Open_Request
     (Sender : in Module_Mailbox;
      Receiving_Module : in Module_Metadata;
      Request_ID : in Request_ID_Type;
      Mode : in Mode_Type;
      Name : in File_Name_Type;
      Receiving_Domain : in Domain_Metadata := This_Domain;
      Priority : in System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => Messaging_Ready
         and then (0 < Name'Length and Name'Length <= XDR_Size_Type'Last - 12)
         and then Receiving_Module.Module_ID = This_Module
         and then Receives(Receiving_Module, Open_Request_Msg)
         and then Has_Module(Receiving_Domain, Receiving_Module.Module_ID);

   procedure Send_Open_Request
     (Sender : in Module_Mailbox;
      Receiving_Module : in Module_Metadata;
      Request_ID : in Request_ID_Type;
      Mode : in Mode_Type;
      Name : in File_Name_Type;
      Status : out Status_Type;
      Priority : in System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => Messaging_Ready
         and then (0 < Name'Length and Name'Length <= XDR_Size_Type'Last - 12)
         and then Receiving_Module.Module_ID = This_Module
         and then Receives(Receiving_Module, Open_Request_Msg)
         and then Has_Module(This_Domain, Receiving_Module.Module_ID);

   function Is_Open_Request(Message : in Message_Record) return Boolean is
      (CubedOS.Message_Types.Message_Type(Message) = Open_Request_Msg);
   procedure Open_Request_Decode
     (Message : in Message_Record;
      Mode : out Mode_Type;
      Name : out File_Name_Type_Ptr;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_Open_Request(Message) and Payload(Message) /= null;


   procedure Open_Reply_Encode
     (Receiver_Address : in Message_Address;
      Sender_Address : in Message_Address;
      Request_ID : in Request_ID_Type;
      Handle : in File_Handle_Type;
      Priority : in System.Priority := System.Default_Priority;
      Result : out  Message_Record)
   with
      Pre => True
         and then Sender_Address.Module_ID = This_Module,
      Post => CubedOS.Message_Types.Message_Type(Result) = Open_Reply_Msg
         and CubedOS.Message_Types.Sender_Address(Result) = Sender_Address
         and CubedOS.Message_Types.Receiver_Address(Result) = Receiver_Address
         and Payload(Result) /= null;

   procedure Send_Open_Reply
     (Sender : in Module_Mailbox;
      Receiver_Address : in Message_Address;
      Request_ID : in Request_ID_Type;
      Handle : in File_Handle_Type;
      Priority : in System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => Messaging_Ready
         and then Module_ID(Sender) = This_Module;

   procedure Send_Open_Reply
     (Sender : in Module_Mailbox;
      Receiver_Address : in Message_Address;
      Request_ID : in Request_ID_Type;
      Handle : in File_Handle_Type;
      Status : out Status_Type;
      Priority : in System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => Messaging_Ready
         and then Module_ID(Sender) = This_Module
         and then Receiver_Address.Domain_ID = Domain_ID;

   procedure Send_Open_Reply
     (Sender : in Module_Mailbox;
      Receiving_Module : in Module_Metadata;
      Request_ID : in Request_ID_Type;
      Handle : in File_Handle_Type;
      Receiving_Domain : in Domain_Metadata := This_Domain;
      Priority : in System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => Messaging_Ready
         and then Module_ID(Sender) = This_Module
         and then Receives(Receiving_Module, Open_Reply_Msg)
         and then Has_Module(Receiving_Domain, Receiving_Module.Module_ID);

   procedure Send_Open_Reply
     (Sender : in Module_Mailbox;
      Receiving_Module : in Module_Metadata;
      Request_ID : in Request_ID_Type;
      Handle : in File_Handle_Type;
      Status : out Status_Type;
      Priority : in System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => Messaging_Ready
         and then Module_ID(Sender) = This_Module
         and then Receives(Receiving_Module, Open_Reply_Msg)
         and then Has_Module(This_Domain, Receiving_Module.Module_ID)
      ;

   function Is_Open_Reply(Message : in Message_Record) return Boolean is
      (CubedOS.Message_Types.Message_Type(Message) = Open_Reply_Msg);
   procedure Open_Reply_Decode
     (Message : in Message_Record;
      Handle : out File_Handle_Type;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_Open_Reply(Message) and Payload(Message) /= null;


   procedure Read_Request_Encode
     (Receiver_Address : in Message_Address;
      Sender_Address : in Message_Address;
      Request_ID : in Request_ID_Type;
      Handle : in Valid_File_Handle_Type;
      Amount : in Read_Size_Type;
      Priority : in System.Priority := System.Default_Priority;
      Result : out  Message_Record)
   with
      Pre => True
         and then Receiver_Address.Module_ID = This_Module,
      Post => CubedOS.Message_Types.Message_Type(Result) = Read_Request_Msg
         and CubedOS.Message_Types.Sender_Address(Result) = Sender_Address
         and CubedOS.Message_Types.Receiver_Address(Result) = Receiver_Address
         and Payload(Result) /= null;

   procedure Send_Read_Request
     (Sender : in Module_Mailbox;
      Receiver_Address : in Message_Address;
      Request_ID : in Request_ID_Type;
      Handle : in Valid_File_Handle_Type;
      Amount : in Read_Size_Type;
      Priority : in System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => Messaging_Ready
         and then Receiver_Address.Module_ID = This_Module
      ;

   procedure Send_Read_Request
     (Sender : in Module_Mailbox;
      Receiver_Address : in Message_Address;
      Request_ID : in Request_ID_Type;
      Handle : in Valid_File_Handle_Type;
      Amount : in Read_Size_Type;
      Status : out Status_Type;
      Priority : in System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => Messaging_Ready
         and then Receiver_Address.Module_ID = This_Module
         and then Receiver_Address.Domain_ID = Domain_ID;

   procedure Send_Read_Request
     (Sender : in Module_Mailbox;
      Receiving_Module : in Module_Metadata;
      Request_ID : in Request_ID_Type;
      Handle : in Valid_File_Handle_Type;
      Amount : in Read_Size_Type;
      Receiving_Domain : in Domain_Metadata := This_Domain;
      Priority : in System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => Messaging_Ready
         and then Receiving_Module.Module_ID = This_Module
         and then Receives(Receiving_Module, Read_Request_Msg)
         and then Has_Module(Receiving_Domain, Receiving_Module.Module_ID);

   procedure Send_Read_Request
     (Sender : in Module_Mailbox;
      Receiving_Module : in Module_Metadata;
      Request_ID : in Request_ID_Type;
      Handle : in Valid_File_Handle_Type;
      Amount : in Read_Size_Type;
      Status : out Status_Type;
      Priority : in System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => Messaging_Ready
         and then Receiving_Module.Module_ID = This_Module
         and then Receives(Receiving_Module, Read_Request_Msg)
         and then Has_Module(This_Domain, Receiving_Module.Module_ID);

   function Is_Read_Request(Message : in Message_Record) return Boolean is
      (CubedOS.Message_Types.Message_Type(Message) = Read_Request_Msg);
   procedure Read_Request_Decode
     (Message : in Message_Record;
      Handle : out Valid_File_Handle_Type;
      Amount : out Read_Size_Type;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_Read_Request(Message) and Payload(Message) /= null;


   procedure Read_Reply_Encode
     (Receiver_Address : in Message_Address;
      Sender_Address : in Message_Address;
      Request_ID : in Request_ID_Type;
      Handle : in Valid_File_Handle_Type;
      File_Data : in Octet_Array;
      Priority : in System.Priority := System.Default_Priority;
      Result : out  Message_Record)
   with
      Pre => True
         and then Sender_Address.Module_ID = This_Module,
      Post => CubedOS.Message_Types.Message_Type(Result) = Read_Reply_Msg
         and CubedOS.Message_Types.Sender_Address(Result) = Sender_Address
         and CubedOS.Message_Types.Receiver_Address(Result) = Receiver_Address
         and Payload(Result) /= null;

   procedure Send_Read_Reply
     (Sender : in Module_Mailbox;
      Receiver_Address : in Message_Address;
      Request_ID : in Request_ID_Type;
      Handle : in Valid_File_Handle_Type;
      File_Data : in Octet_Array;
      Priority : in System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => Messaging_Ready
         and then Module_ID(Sender) = This_Module;

   procedure Send_Read_Reply
     (Sender : in Module_Mailbox;
      Receiver_Address : in Message_Address;
      Request_ID : in Request_ID_Type;
      Handle : in Valid_File_Handle_Type;
      File_Data : in Octet_Array;
      Status : out Status_Type;
      Priority : in System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => Messaging_Ready
         and then Module_ID(Sender) = This_Module
         and then Receiver_Address.Domain_ID = Domain_ID;

   procedure Send_Read_Reply
     (Sender : in Module_Mailbox;
      Receiving_Module : in Module_Metadata;
      Request_ID : in Request_ID_Type;
      Handle : in Valid_File_Handle_Type;
      File_Data : in Octet_Array;
      Receiving_Domain : in Domain_Metadata := This_Domain;
      Priority : in System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => Messaging_Ready
         and then Module_ID(Sender) = This_Module
         and then Receives(Receiving_Module, Read_Reply_Msg)
         and then Has_Module(Receiving_Domain, Receiving_Module.Module_ID);

   procedure Send_Read_Reply
     (Sender : in Module_Mailbox;
      Receiving_Module : in Module_Metadata;
      Request_ID : in Request_ID_Type;
      Handle : in Valid_File_Handle_Type;
      File_Data : in Octet_Array;
      Status : out Status_Type;
      Priority : in System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => Messaging_Ready
         and then Module_ID(Sender) = This_Module
         and then Receives(Receiving_Module, Read_Reply_Msg)
         and then Has_Module(This_Domain, Receiving_Module.Module_ID);

   function Is_Read_Reply(Message : in Message_Record) return Boolean is
     (CubedOS.Message_Types.Message_Type(Message) = Read_Reply_Msg);

   procedure Read_Reply_Decode
     (Message : in Message_Record;
      Handle : out Valid_File_Handle_Type;
      File_Data : out Octet_Array_Ptr;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_Read_Reply(Message) and Payload(Message) /= null;


   procedure Write_Request_Encode
     (Receiver_Address : in Message_Address;
      Sender_Address : in Message_Address;
      Request_ID : in Request_ID_Type;
      Handle : in Valid_File_Handle_Type;
      File_Data : in Octet_Array;
      Priority : in System.Priority := System.Default_Priority;
      Result : out  Message_Record)
   with
      Pre => True
         and then Receiver_Address.Module_ID = This_Module,
      Post => CubedOS.Message_Types.Message_Type(Result) = Write_Request_Msg
         and CubedOS.Message_Types.Sender_Address(Result) = Sender_Address
         and CubedOS.Message_Types.Receiver_Address(Result) = Receiver_Address
         and Payload(Result) /= null;

   procedure Send_Write_Request
     (Sender : in Module_Mailbox;
      Receiver_Address : in Message_Address;
      Request_ID : in Request_ID_Type;
      Handle : in Valid_File_Handle_Type;
      File_Data : in Octet_Array;
      Priority : in System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => Messaging_Ready
         and then Receiver_Address.Module_ID = This_Module;

   procedure Send_Write_Request
     (Sender : in Module_Mailbox;
      Receiver_Address : in Message_Address;
      Request_ID : in Request_ID_Type;
      Handle : in Valid_File_Handle_Type;
      File_Data : in Octet_Array;
      Status : out Status_Type;
      Priority : in System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => Messaging_Ready
         and then Receiver_Address.Module_ID = This_Module
         and then Receiver_Address.Domain_ID = Domain_ID;

   procedure Send_Write_Request
     (Sender : in Module_Mailbox;
      Receiving_Module : in Module_Metadata;
      Request_ID : in Request_ID_Type;
      Handle : in Valid_File_Handle_Type;
      File_Data : in Octet_Array;
      Receiving_Domain : in Domain_Metadata := This_Domain;
      Priority : in System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => Messaging_Ready
         and then Receiving_Module.Module_ID = This_Module
         and then Receives(Receiving_Module, Write_Request_Msg)
         and then Has_Module(Receiving_Domain, Receiving_Module.Module_ID);

   procedure Send_Write_Request
     (Sender : in Module_Mailbox;
      Receiving_Module : in Module_Metadata;
      Request_ID : in Request_ID_Type;
      Handle : in Valid_File_Handle_Type;
      File_Data : in Octet_Array;
      Status : out Status_Type;
      Priority : in System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => Messaging_Ready
         and then Receiving_Module.Module_ID = This_Module
         and then Receives(Receiving_Module, Write_Request_Msg)
         and then Has_Module(This_Domain, Receiving_Module.Module_ID);

   function Is_Write_Request(Message : in Message_Record) return Boolean is
     (CubedOS.Message_Types.Message_Type(Message) = Write_Request_Msg);

   procedure Write_Request_Decode
     (Message : in Message_Record;
      Handle : out Valid_File_Handle_Type;
      File_Data : out Octet_Array_Ptr;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_Write_Request(Message) and Payload(Message) /= null;


   procedure Write_Reply_Encode
     (Receiver_Address : in Message_Address;
      Sender_Address : in Message_Address;
      Request_ID : in Request_ID_Type;
      Handle : in Valid_File_Handle_Type;
      Amount : in Write_Result_Size_Type;
      Priority : in System.Priority := System.Default_Priority;
      Result : out  Message_Record)
   with
      Pre => True
         and then Sender_Address.Module_ID = This_Module,
      Post => CubedOS.Message_Types.Message_Type(Result) = Write_Reply_Msg
         and CubedOS.Message_Types.Sender_Address(Result) = Sender_Address
         and CubedOS.Message_Types.Receiver_Address(Result) = Receiver_Address
         and Payload(Result) /= null;

   procedure Send_Write_Reply
     (Sender : in Module_Mailbox;
      Receiver_Address : in Message_Address;
      Request_ID : in Request_ID_Type;
      Handle : in Valid_File_Handle_Type;
      Amount : in Write_Result_Size_Type;
      Priority : in System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => Messaging_Ready
         and then Module_ID(Sender) = This_Module;

   procedure Send_Write_Reply
     (Sender : in Module_Mailbox;
      Receiver_Address : in Message_Address;
      Request_ID : in Request_ID_Type;
      Handle : in Valid_File_Handle_Type;
      Amount : in Write_Result_Size_Type;
      Status : out Status_Type;
      Priority : in System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => Messaging_Ready
         and then Module_ID(Sender) = This_Module
         and then Receiver_Address.Domain_ID = Domain_ID;

   procedure Send_Write_Reply
     (Sender : in Module_Mailbox;
      Receiving_Module : in Module_Metadata;
      Request_ID : in Request_ID_Type;
      Handle : in Valid_File_Handle_Type;
      Amount : in Write_Result_Size_Type;
      Receiving_Domain : in Domain_Metadata := This_Domain;
      Priority : in System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => Messaging_Ready
         and then Module_ID(Sender) = This_Module
         and then Receives(Receiving_Module, Write_Reply_Msg)
         and then Has_Module(Receiving_Domain, Receiving_Module.Module_ID);

   procedure Send_Write_Reply
     (Sender : in Module_Mailbox;
      Receiving_Module : in Module_Metadata;
      Request_ID : in Request_ID_Type;
      Handle : in Valid_File_Handle_Type;
      Amount : in Write_Result_Size_Type;
      Status : out Status_Type;
      Priority : in System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => Messaging_Ready
         and then Module_ID(Sender) = This_Module
         and then Receives(Receiving_Module, Write_Reply_Msg)
         and then Has_Module(This_Domain, Receiving_Module.Module_ID);

   function Is_Write_Reply(Message : in Message_Record) return Boolean is
     (CubedOS.Message_Types.Message_Type(Message) = Write_Reply_Msg);

   procedure Write_Reply_Decode
     (Message : in Message_Record;
      Handle : out Valid_File_Handle_Type;
      Amount : out Write_Result_Size_Type;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_Write_Reply(Message) and Payload(Message) /= null;


   procedure Close_Request_Encode
     (Receiver_Address : in Message_Address;
      Sender_Address : in Message_Address;
      Request_ID : in Request_ID_Type;
      Handle : in Valid_File_Handle_Type;
      Priority : in System.Priority := System.Default_Priority;
      Result : out  Message_Record)
   with
      Pre => True
         and then Receiver_Address.Module_ID = This_Module,
      Post => CubedOS.Message_Types.Message_Type(Result) = Close_Request_Msg
         and CubedOS.Message_Types.Sender_Address(Result) = Sender_Address
         and CubedOS.Message_Types.Receiver_Address(Result) = Receiver_Address
         and Payload(Result) /= null;

   procedure Send_Close_Request
     (Sender : in Module_Mailbox;
      Receiver_Address : in Message_Address;
      Request_ID : in Request_ID_Type;
      Handle : in Valid_File_Handle_Type;
      Priority : in System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => Messaging_Ready
         and then Receiver_Address.Module_ID = This_Module;

   procedure Send_Close_Request
     (Sender : in Module_Mailbox;
      Receiver_Address : in Message_Address;
      Request_ID : in Request_ID_Type;
      Handle : in Valid_File_Handle_Type;
      Status : out Status_Type;
      Priority : in System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => Messaging_Ready
         and then Receiver_Address.Module_ID = This_Module
         and then Receiver_Address.Domain_ID = Domain_ID;

   procedure Send_Close_Request
     (Sender : in Module_Mailbox;
      Receiving_Module : in Module_Metadata;
      Request_ID : in Request_ID_Type;
      Handle : in Valid_File_Handle_Type;
      Receiving_Domain : in Domain_Metadata := This_Domain;
      Priority : in System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => Messaging_Ready
         and then Receiving_Module.Module_ID = This_Module
         and then Receives(Receiving_Module, Close_Request_Msg)
         and then Has_Module(Receiving_Domain, Receiving_Module.Module_ID);

   procedure Send_Close_Request
     (Sender : in Module_Mailbox;
      Receiving_Module : in Module_Metadata;
      Request_ID : in Request_ID_Type;
      Handle : in Valid_File_Handle_Type;
      Status : out Status_Type;
      Priority : in System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => Messaging_Ready
         and then Receiving_Module.Module_ID = This_Module
         and then Receives(Receiving_Module, Close_Request_Msg)
         and then Has_Module(This_Domain, Receiving_Module.Module_ID);

   function Is_Close_Request(Message : in Message_Record) return Boolean is
     (CubedOS.Message_Types.Message_Type(Message) = Close_Request_Msg);

   procedure Close_Request_Decode
     (Message : in Message_Record;
      Handle : out Valid_File_Handle_Type;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_Close_Request(Message) and Payload(Message) /= null;

end CubedOS.File_Server.API;
