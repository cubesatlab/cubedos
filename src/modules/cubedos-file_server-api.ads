--------------------------------------------------------------------------------
-- FILE   : cubedos-file_server-api.ads
-- SUBJECT: Specification of a package that simplifies use of the file server.
-- AUTHOR : (C) Copyright 2018 by Vermont Technical College
--
-- All the subprograms in this package must be task safe. They can be simultaneously called
-- from multiple tasks. If possible, make every subprogram here a pure function.
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.Lib;
with Message_Manager;
with System;
with Name_Resolver;
use Message_Manager;
with CubedOS.Lib.XDR; use CubedOS.Lib.XDR;
with CubedOS.Message_Types; use CubedOS.Message_Types;

package CubedOS.File_Server.API is

   This_Module : constant Module_ID_Type := Name_Resolver.File_Server;

   type Message_Type is
     (Open_Request,
      Open_Reply,
      Read_Request,
      Read_Reply,
      Write_Request,
      Write_Reply,
      Close_Request);

   -- Describe the message types
   Open_Request_Msg : constant Universal_Message_Type := (This_Module, Message_Type'Pos(Open_Request));
   Open_Reply_Msg : constant Universal_Message_Type := (This_Module, Message_Type'Pos(Open_Reply));
   Read_Request_Msg : constant Universal_Message_Type := (This_Module, Message_Type'Pos(Read_Request));
   Read_Reply_Msg : constant Universal_Message_Type := (This_Module, Message_Type'Pos(Read_Reply));
   Write_Request_Msg : constant Universal_Message_Type := (This_Module, Message_Type'Pos(Write_Request));
   Write_Reply_Msg : constant Universal_Message_Type := (This_Module, Message_Type'Pos(Write_Reply));
   Close_Request_Msg : constant Universal_Message_Type := (This_Module, Message_Type'Pos(Close_Request));

   This_Receives: aliased constant Message_Type_Array := (Open_Request_Msg,
                                       Read_Request_Msg,
                                       Write_Request_Msg,
                                       Close_Request_Msg
                                      );
   Mail_Target : aliased constant Module_Metadata := Declare_Receives(This_Module, This_Receives'Access);

   type Mode_Type is (Read, Write);
   type File_Handle_Type is range 0 .. 64;
   subtype Valid_File_Handle_Type is File_Handle_Type range 1 .. File_Handle_Type'Last;

   Invalid_Handle : constant File_Handle_Type := 0;
   Maximum_Read_Size : constant := 256;
   Maximum_Write_Size : constant := 256;

   -- Attempted reads and writes must be for at least one octet. The reply messages contain the
   -- number of octets successfully read or written and must allow for zero to handle the error
   -- or end-of-file case.
   --
   subtype Read_Result_Size_Type is Natural range 0 .. Maximum_Read_Size;
   subtype Read_Size_Type is Natural range 1 .. Read_Result_Size_Type'Last;
   subtype Write_Result_Size_Type is Natural range 0 .. Maximum_Write_Size;
   subtype Write_Size_Type is Natural range 1 .. Write_Result_Size_Type'Last;

   procedure Open_Request_Encode
      (Sender_Address : Message_Address;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Mode : Mode_Type;
      Name : String;
      Result : out Message_Record;
      Priority : System.Priority := System.Default_Priority)
   with
      Pre => true
         and then (0 < Name'Length and Name'Length <= XDR_Size_Type'Last - 12)
         and then Receiver_Address.Module_ID = This_Module
         and then Receives(Receiver_Address.Module_ID, Open_Request_Msg),
      Post => Message_Manager.Message_Type(Result) = Open_Request_Msg
       and Message_Manager.Receiver_Address(Result) = Receiver_Address;

   procedure Send_Open_Request
      (Sender : Module_Mailbox;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Mode : Mode_Type;
      Name : String;
      Priority : System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => true
         and then (0 < Name'Length and Name'Length <= XDR_Size_Type'Last - 12)
         and then Receiver_Address.Module_ID = This_Module
         and then Receives(Receiver_Address.Module_ID, Open_Request_Msg)
      ;

   procedure Open_Reply_Encode
      (Sender_Address : Message_Address;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Handle : File_Handle_Type;
      Result : out Message_Record;
      Priority : System.Priority := System.Default_Priority)
   with
      Pre => true
         and then Sender_Address.Module_ID = This_Module
         and then Receives(Receiver_Address.Module_ID, Open_Reply_Msg),
      Post => Message_Manager.Message_Type(Result) = Open_Reply_Msg
       and Message_Manager.Receiver_Address(Result) = Receiver_Address;

   procedure Send_Open_Reply
      (Sender : Module_Mailbox;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Handle : File_Handle_Type;
      Priority : System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => true
         and then Address(Sender).Module_ID = This_Module
         and then Receives(Receiver_Address.Module_ID, Open_Reply_Msg)
      ;

   procedure Read_Request_Encode
      (Sender_Address : Message_Address;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Handle : Valid_File_Handle_Type;
      Amount : Read_Size_Type;
      Result : out Message_Record;
      Priority : System.Priority := System.Default_Priority)
   with
      Pre => true
         and then Receiver_Address.Module_ID = This_Module
         and then Receives(Receiver_Address.Module_ID, Read_Request_Msg),
      Post => Message_Manager.Message_Type(Result) = Read_Request_Msg
         and Message_Manager.Receiver_Address(Result) = Receiver_Address;

   procedure Send_Read_Request
      (Sender : Module_Mailbox;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Handle : Valid_File_Handle_Type;
      Amount : Read_Size_Type;
      Priority : System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => true
         and then Receiver_Address.Module_ID = This_Module
         and then Receives(Receiver_Address.Module_ID, Read_Request_Msg)
      ;

   procedure Read_Reply_Encode
      (Sender_Address : Message_Address;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Handle : Valid_File_Handle_Type;
      Amount : Read_Result_Size_TYpe;
      File_Data : CubedOS.Lib.Octet_Array;
      Result : out Message_Record;
      Priority : System.Priority := System.Default_Priority)
   with
      Pre => Amount <= File_Data'Length
         and then Sender_Address.Module_ID = This_Module
         and then Receives(Receiver_Address.Module_ID, Read_Reply_Msg),
      Post => Message_Manager.Message_Type(Result) = Read_Reply_Msg
         and Message_Manager.Receiver_Address(Result) = Receiver_Address;

   procedure Send_Read_Reply
      (Sender : Module_Mailbox;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Handle : Valid_File_Handle_Type;
      Amount : Read_Result_Size_TYpe;
      File_Data : CubedOS.Lib.Octet_Array;
      Priority : System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => Amount <= File_Data'Length
         and then Address(Sender).Module_ID = This_Module
         and then Receives(Receiver_Address.Module_ID, Read_Reply_Msg)
      ;

   procedure Write_Request_Encode
      (Sender_Address : Message_Address;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Handle : Valid_File_Handle_Type;
      Amount : Write_Size_Type;
      File_Data : CubedOS.Lib.Octet_Array;
      Result : out Message_Record;
      Priority : System.Priority := System.Default_Priority)
   with
      Pre => Amount <= File_Data'Length
         and then Receiver_Address.Module_ID = This_Module
         and then Receives(Receiver_Address.Module_ID, Write_Request_Msg),
      Post => Message_Manager.Message_Type(Result) = Write_Request_Msg
         and Message_Manager.Receiver_Address(Result) = Receiver_Address;

   procedure Send_Write_Request
      (Sender : Module_Mailbox;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Handle : Valid_File_Handle_Type;
      Amount : Write_Size_Type;
      File_Data : CubedOS.Lib.Octet_Array;
      Priority : System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => Amount <= File_Data'Length
         and then Receiver_Address.Module_ID = This_Module
         and then Receives(Receiver_Address.Module_ID, Write_Request_Msg)
      ;

   procedure Write_Reply_Encode
      (Sender_Address : Message_Address;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Handle : Valid_File_Handle_Type;
      Amount : Write_Result_Size_Type;
      Result : out Message_Record;
      Priority : System.Priority := System.Default_Priority)
   with
      Pre => true
         and then Sender_Address.Module_ID = This_Module
         and then Receives(Receiver_Address.Module_ID, Write_Reply_Msg),
      Post => Message_Manager.Message_Type(Result) = Write_Reply_Msg
         and Message_Manager.Receiver_Address(Result) = Receiver_Address;

   procedure Send_Write_Reply
      (Sender : Module_Mailbox;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Handle : Valid_File_Handle_Type;
      Amount : Write_Result_Size_Type;
      Priority : System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => true
         and then Address(Sender).Module_ID = This_Module
         and then Receives(Receiver_Address.Module_ID, Write_Reply_Msg)
      ;

   procedure Close_Request_Encode
      (Sender_Address : Message_Address;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Handle : Valid_File_Handle_Type;
      Result : out Message_Record;
      Priority : System.Priority := System.Default_Priority)
   with
      Pre => true
         and then Receiver_Address.Module_ID = This_Module
         and then Receives(Receiver_Address.Module_ID, Close_Request_Msg),
      Post => Message_Manager.Message_Type(Result) = Close_Request_Msg
         and Message_Manager.Receiver_Address(Result) = Receiver_Address;

   procedure Send_Close_Request
      (Sender : Module_Mailbox;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Handle : Valid_File_Handle_Type;
      Priority : System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => true
         and then Receiver_Address.Module_ID = This_Module
         and then Receives(Receiver_Address.Module_ID, Close_Request_Msg)
      ;



   function Is_Open_Request(Message : Message_Record) return Boolean is
      (Message_Manager.Message_Type(Message) = Open_Request_Msg);

   function Is_Open_Reply(Message : Message_Record) return Boolean is
      (Message_Manager.Message_Type(Message) = Open_Reply_Msg);

   function Is_Read_Request(Message : Message_Record) return Boolean is
      (Message_Manager.Message_Type(Message) = Read_Request_Msg);

   function Is_Read_Reply(Message : Message_Record) return Boolean is
     (Message_Manager.Message_Type(Message) = Read_Reply_Msg);

   function Is_Write_Request(Message : Message_Record) return Boolean is
      (Message_Manager.Message_Type(Message) = Write_Request_Msg);

   function Is_Write_Reply(Message : Message_Record) return Boolean is
      (Message_Manager.Message_Type(Message) = Write_Reply_Msg);

      function Is_Close_Request(Message : Message_Record) return Boolean is
      (Message_Manager.Message_Type(Message) = Close_Request_Msg);



   procedure Open_Request_Decode
     (Message    : in  Message_Record;
      Mode       : out Mode_Type;
      Name       : out String;
      Name_Size  : out Natural;
      Decode_Status : out Message_Status_Type)
     with
       Global => null,
       Pre => Is_Open_Request(Message),
       Depends => ((Mode, Name_Size, Decode_Status) => Message, Name =>+ Message);

   procedure Open_Reply_Decode
     (Message    : in  Message_Record;
      Handle     : out File_Handle_Type;
      Decode_Status : out Message_Status_Type)
     with
       Global => null,
       Pre => Is_Open_Reply(Message),
       Depends => ((Handle, Decode_Status) => Message);

   procedure Read_Request_Decode
     (Message    : in Message_Record;
      Handle     : out Valid_File_Handle_Type;
      Amount     : out Read_Size_Type;
      Decode_Status : out Message_Status_Type)
     with
       Global => null,
       Pre => Is_Read_Request(Message),
       Depends => ((Handle, Amount, Decode_Status) => Message);

   procedure Read_Reply_Decode
     (Message    : in  Message_Record;
      Handle     : out Valid_File_Handle_Type;
      Amount     : out Read_Result_Size_Type;
      Data       : out CubedOS.Lib.Octet_Array;
      Decode_Status : out Message_Status_Type)
     with
       Global => null,
       Pre => Is_Read_Reply(Message) and Data'Length = Read_Result_Size_Type'Last,
       Depends => ((Handle, Amount, Decode_Status) => Message, Data =>+ Message);

   procedure Write_Request_Decode
     (Message    : in  Message_Record;
      Handle     : out Valid_File_Handle_Type;
      Amount     : out Write_Size_Type;
      Data       : out CubedOS.Lib.Octet_Array;
      Decode_Status : out Message_Status_Type)
     with
       Global => null,
       Pre => Is_Write_Request(Message) and Data'Length = Read_Result_Size_Type'Last,
       Depends => ((Handle, Amount, Decode_Status) => Message, Data =>+ Message);

   procedure Write_Reply_Decode
     (Message    : in  Message_Record;
      Handle     : out Valid_File_Handle_Type;
      Amount     : out Write_Result_Size_Type;
      Decode_Status : out Message_Status_Type)
     with
       Global => null,
       Pre => Is_Write_Reply(Message),
       Depends => ((Handle, Amount, Decode_Status) => Message);

   procedure Close_Request_Decode
     (Message    : in  Message_Record;
      Handle     : out Valid_File_Handle_Type;
      Decode_Status : out Message_Status_Type)
     with
       Global => null,
       Pre => Is_Close_Request(Message),
       Depends => ((Handle, Decode_Status) => Message);


end CubedOS.File_Server.API;
