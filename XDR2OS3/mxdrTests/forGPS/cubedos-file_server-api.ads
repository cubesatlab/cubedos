--------------------------------------------------------------------------------
-- FILE   : cubedos-File_Server-api.ads
-- SUBJECT: Specification of a package that defines the File_Server API
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
-- All the subprograms in this package must be task safe. They can be simultaneously
--called from multiple tasks. If possible, make every subprogram here a pure function.
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.Lib;
with Message_Manager;  use Message_Manager;
with System;

package CubedOS.File_Server.API is

   type Message_Type is
      (Read_Reply, 
      Open_Request, 
      Close_Request, 
      Read_Request, 
      Write_Reply, 
      Open_Reply, 
      Write_Request);

   type Mode_Type is 
         (Read, 
         Write);

   type File_Handle_Type is new Lib.Quadruple_Octet range 0 .. 64;

   subtype Valid_File_Handle_Type is File_Handle_Type range 1 .. 64.0;

   Invalid_Handle: constant File_Handle_Type := 0;

   Maximum_Read_Size: constant := 256;

   Maximum_Write_Size: constant := 256;

   subtype Read_Result_Size_Type is Natural range 0 .. 256;

   subtype Read_Size_Type is Read_Result_Size_Type range 1 .. 256.0;

   subtype Write_Result_Size_Type is Natural range 0 .. 256;

   subtype Write_Size_Type is Write_Result_Size_Type range 1 .. 256.0;

   type Request_ID_Type is new Lib.Quadruple_Octet range 1 .. 64;

   function Open_Request_Encode
      (Sender_Domain : Domain_ID_Type;
      Sender  : Module_ID_Type;
      Mode : Mode_Type;
      Name : String;
      Request_ID : Request_ID_Type;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   with
      Global => null,
      Pre => (0 < Name'Length and Name'Length <= XDR_Size_Type'Last - 12);

   function Is_Open_Request(Message : Message_Record) return Boolean is
      (Message.Sender = ID and Message.Message_ID = Message_Type'Pos(Open_Request));

   procedure Open_Request_Decode
      (Message : in  Message_Record;
      Mode : out Mode_Type;
      Name : out String;
      Name_Size : out Natural;
      Request_ID : out Request_ID_Type;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_Open_Request(Message),
      Depends => ((Mode, Name, Name_Size, Request_ID, Decode_Status) => Message);


   function Open_Reply_Encode
      (Receiver_Domain : Domain_ID_Type;
      Receiver  : Module_ID_Type;
      Handle : File_Handle_Type;
      Request_ID : Request_ID_Type;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   with
      Global => null;

   function Is_Open_Reply(Message : Message_Record) return Boolean is
      (Message.Receiver = ID and Message.Message_ID = Message_Type'Pos(Open_Reply));

   procedure Open_Reply_Decode
      (Message : in  Message_Record;
      Handle : out File_Handle_Type;
      Request_ID : out Request_ID_Type;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_Open_Reply(Message),
      Depends => ((Handle, Request_ID, Decode_Status) => Message);


   function Read_Request_Encode
      (Sender_Domain : Domain_ID_Type;
      Sender  : Module_ID_Type;
      Handle : File_Handle_Type;
      Amount : Read_Size_Type;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   with
      Global => null;

   function Is_Read_Request(Message : Message_Record) return Boolean is
      (Message.Sender = ID and Message.Message_ID = Message_Type'Pos(Read_Request));

   procedure Read_Request_Decode
      (Message : in  Message_Record;
      Handle : out File_Handle_Type;
      Amount : out Read_Size_Type;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_Read_Request(Message),
      Depends => ((Handle, Amount, Decode_Status) => Message);


   function Read_Reply_Encode
      (Receiver_Domain : Domain_ID_Type;
      Receiver  : Module_ID_Type;
      Handle : Valid_File_Handle_Type;
      Amount : Read_Result_Size_Type;
      Message_Data : CubedOS.Lib.Octet_Array;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   with
      Global => null,
      Pre => Amount <= Message_Data'Length;

   function Is_Read_Reply(Message : Message_Record) return Boolean is
      (Message.Receiver = ID and Message.Message_ID = Message_Type'Pos(Read_Reply));

   procedure Read_Reply_Decode
      (Message : in  Message_Record;
      Handle : out Valid_File_Handle_Type;
      Amount : out Read_Result_Size_Type;
      Message_Data : out CubedOS.Lib.Octet_Array;
      Size : out CubedOS.Lib.Octet_Array_Count;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_Read_Reply(Message),
      Depends => ((Handle, Amount, Message_Data, Size, Decode_Status) => Message),
      Post => Amount <= Message_Data'Length;


   function Write_Request_Encode
      (Sender_Domain : Domain_ID_Type;
      Sender  : Module_ID_Type;
      Handle : Valid_File_Handle_Type;
      Amount : Write_Size_Type;
      Message_Data : CubedOS.Lib.Octet_Array;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   with
      Global => null,
      Pre => Amount <= Message_Data'Length;

   function Is_Write_Request(Message : Message_Record) return Boolean is
      (Message.Sender = ID and Message.Message_ID = Message_Type'Pos(Write_Request));

   procedure Write_Request_Decode
      (Message : in  Message_Record;
      Handle : out Valid_File_Handle_Type;
      Amount : out Write_Size_Type;
      Message_Data : out CubedOS.Lib.Octet_Array;
      Size : out CubedOS.Lib.Octet_Array_Count;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_Write_Request(Message),
      Depends => ((Handle, Amount, Message_Data, Size, Decode_Status) => Message),
      Post => Amount <= Message_Data'Length;


   function Write_Reply_Encode
      (Receiver_Domain : Domain_ID_Type;
      Receiver  : Module_ID_Type;
      Handle : Valid_File_Handle_Type;
      Amount : Write_Result_Size_Type;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   with
      Global => null;

   function Is_Write_Reply(Message : Message_Record) return Boolean is
      (Message.Receiver = ID and Message.Message_ID = Message_Type'Pos(Write_Reply));

   procedure Write_Reply_Decode
      (Message : in  Message_Record;
      Handle : out Valid_File_Handle_Type;
      Amount : out Write_Result_Size_Type;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_Write_Reply(Message),
      Depends => ((Handle, Amount, Decode_Status) => Message);


   function Close_Request_Encode
      (Sender_Domain : Domain_ID_Type;
      Sender  : Module_ID_Type;
      Handle : Valid_File_Handle_Type;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   with
      Global => null;

   function Is_Close_Request(Message : Message_Record) return Boolean is
      (Message.Sender = ID and Message.Message_ID = Message_Type'Pos(Close_Request));

   procedure Close_Request_Decode
      (Message : in  Message_Record;
      Handle : out Valid_File_Handle_Type;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_Close_Request(Message),
      Depends => ((Handle, Decode_Status) => Message);



end CubedOS.File_Server.API;
