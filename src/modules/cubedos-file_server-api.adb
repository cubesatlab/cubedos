--------------------------------------------------------------------------------
-- FILE   : cubedos-file_server-api.adb
-- SUBJECT: Body of a package that simplifies use of the file server.
-- AUTHOR : (C) Copyright 2018 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.Lib.XDR;

use CubedOS.Lib;

package body CubedOS.File_Server.API is
   use type XDR.XDR_Unsigned;

   -- Encodes:

   -- Requester-side Use
   function Open_Request_Encode
     (Sender_Domain : Domain_ID_Type;
      Sender     : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Mode       : Mode_Type;
      Name       : String;
      Priority   : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record := Make_Empty_Message
        (Sender_Domain => Sender_Domain,
         Receiver_Domain => Domain_ID,
         Sender     => Sender,
         Receiver   => ID,
         Request_ID => Request_ID,
         Message_ID => Message_Type'Pos(Open_Request),
         Priority   => Priority);

      Position : Data_Index_Type;
      Last : Data_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Mode_Type'Pos(Mode)), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Name'Length), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(Name, Message.Payload, Position, Last);
      Message.Size := Last + 1;
      return Message;
   end Open_Request_Encode;


   -- Server-side Use
   function Open_Reply_Encode
     (Receiver_Domain : Domain_ID_Type;
      Receiver   : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Handle     : API.File_Handle_Type;
      Priority   : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record := Make_Empty_Message
        (Sender_Domain => Domain_ID,
         Receiver_Domain => Receiver_Domain,
         Sender     => ID,
         Receiver   => Receiver,
         Request_ID => Request_ID,
         Message_ID => Message_Type'Pos(API.Open_Reply),
         Priority   => Priority);

      Position : Data_Index_Type;
      Last : Data_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Handle), Message.Payload, Position, Last);
      Message.Size := Last + 1;
      return Message;
   end Open_Reply_Encode;


   function Read_Request_Encode
     (Sender_Domain : Domain_ID_Type;
      Sender     : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Handle     : Valid_File_Handle_Type;
      Amount     : Read_Size_Type;
      Priority   : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record := Make_Empty_Message
        (Sender_Domain => Sender_Domain,
         Receiver_Domain => Domain_ID,
         Sender     => Sender,
         Receiver   => ID,
         Request_ID => Request_ID,
         Message_ID => Message_Type'Pos(Read_Request),
         Priority   => Priority);

      Position : Data_Index_Type;
      Last : Data_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Handle), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Amount), Message.Payload, Position, Last);
      Message.Size := Last + 1;
      return Message;
   end Read_Request_Encode;


   -- Server-side use.
   function Read_Reply_Encode
     (Receiver_Domain : Domain_ID_Type;
      Receiver   : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Handle     : Valid_File_Handle_Type;
      Amount     : Read_Result_Size_Type;
      Data       : Octet_Array;
      Priority   : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record := Make_Empty_Message
        (Sender_Domain => Domain_ID,
         Receiver_Domain => Receiver_Domain,
         Sender     => ID,
         Receiver   => Receiver,
         Request_ID => Request_ID,
         Message_ID => Message_Type'Pos(API.Read_Reply),
         Priority   => Priority);

      Position : Data_Index_Type;
      Last : Data_Extended_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Handle), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Amount), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(Data(Data'First ..  Data'First + (Amount - 1)), Message.Payload, Position, Last);
      Message.Size := Last + 1;
      return Message;
   end Read_Reply_Encode;


   function Write_Request_Encode
     (Sender_Domain : Domain_ID_Type;
      Sender     : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Handle     : Valid_File_Handle_Type;
      Amount     : Write_Size_Type;
      Data       : Octet_Array;
      Priority   : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record := Make_Empty_Message
        (Sender_Domain => Sender_Domain,
         Receiver_Domain => Domain_ID,
         Sender     => Sender,
         Receiver   => ID,
         Request_ID => Request_ID,
         Message_ID => Message_Type'Pos(Write_Request),
         Priority   => Priority);

      Position : Data_Index_Type;
      Last : Data_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Handle), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Amount), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(Data(Data'First .. Data'First + (Amount - 1)), Message.Payload, Position, Last);
      Message.Size := Last + 1;
      return Message;
   end Write_Request_Encode;


   -- Server-side use.
   function Write_Reply_Encode
     (Receiver_Domain : Domain_ID_Type;
      Receiver   : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Handle     : Valid_File_Handle_Type;
      Amount     : Write_Result_Size_Type;
      Priority   : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record := Make_Empty_Message
        (Sender_Domain => Domain_ID,
         Receiver_Domain => Receiver_Domain,
         Sender     => ID,
         Receiver   => Receiver,
         Request_ID => Request_ID,
         Message_ID => Message_Type'Pos(API.Write_Reply),
         Priority   => Priority);

      Position : Data_Index_Type;
      Last : Data_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Handle), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Amount), Message.Payload, Position, Last);
      Message.Size := Last + 1;
      return Message;
   end Write_Reply_Encode;


   function Close_Request_Encode
     (Sender_Domain : Domain_ID_Type;
      Sender     : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Handle     : Valid_File_Handle_Type;
      Priority   : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record := Make_Empty_Message
        (Sender_Domain => Sender_Domain,
         Receiver_Domain => Domain_ID,
         Sender     => Sender,
         Receiver   => ID,
         Request_ID => Request_ID,
         Message_ID => Message_Type'Pos(Close_Request),
         Priority   => Priority);

      Position : Data_Index_Type;
      Last : Data_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Handle), Message.Payload, Position, Last);
      Message.Size := Last + 1;
      return Message;
   end Close_Request_Encode;

   -- Decodes:

   -- Server-side Use
   procedure Open_Request_Decode
     (Message    : in  Message_Record;
      Mode       : out Mode_Type;
      Name       : out String;
      Name_Size  : out Natural;
      Decode_Status : out Message_Status_Type)
   is
      Position : Data_Index_Type;
      Raw_Mode : XDR.XDR_Unsigned;
      Raw_Name_Size : XDR.XDR_Unsigned;
      Last : Data_Index_Type;
   begin
       pragma Warnings
        (Off, "unused assignment to ""Last""", Reason => "No further decoding required");
      Decode_Status := Success;
      Name := (others => ' ');

      -- TODO: Need to verify the conversions below. Returned Malformed if they won't work.
      Position := 0;
      XDR.Decode(Message.Payload, Position, Raw_Mode, Last);
      Position := Last + 1;
      if Raw_Mode <= Mode_Type'Pos(Mode_Type'Last) then
         Mode := Mode_Type'Val(Raw_Mode);
      else
         Decode_Status := Malformed;
         Mode := Mode_Type'First; --appropriate?
      end if;
      XDR.Decode(Message.Payload, Position, Raw_Name_Size, Last);
      Position := Last + 1;
      if Raw_Name_Size <= XDR.XDR_Unsigned(Natural'Last) then
         Name_Size := Natural(Raw_Name_Size);
      else
         Name_Size := 0;
      end if;
      XDR.Decode(Message.Payload, Position, Name(Name'First .. Name'First + (Name_Size - 1)), Last);
   end Open_Request_Decode;


   -- Requester-side use.
   procedure Open_Reply_Decode
     (Message    : in  Message_Record;
      Handle     : out File_Handle_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position : Data_Index_Type;
      Raw_Handle : XDR.XDR_Unsigned;
      Last : Data_Index_Type;
   begin
      pragma Warnings
        (Off, "unused assignment to ""Last""", Reason => "No further decoding required");

      Position := 0;
      XDR.Decode(Message.Payload, Position, Raw_Handle, Last);
      if Raw_Handle <= XDR.XDR_Unsigned(File_Handle_Type'Last)
      then
         Handle := File_Handle_Type(Raw_Handle);
         Decode_Status := Success;
      else
         Handle := File_Handle_Type'First;
         Decode_Status := Malformed;
      end if;
   end Open_Reply_Decode;


   -- Server-side use.
   procedure Read_Request_Decode
     (Message : in Message_Record;
      Handle  : out Valid_File_Handle_Type;
      Amount  : out Read_Size_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position : Data_Index_Type;
      Raw_Handle : XDR.XDR_Unsigned;
      Raw_Amount : XDR.XDR_Unsigned;
      Last : Data_Index_Type;
   begin
       pragma Warnings
        (Off, "unused assignment to ""Last""", Reason => "No further decoding required");
      Decode_Status := Success;

      -- TODO: Need to verify the conversions below. Returned Malformed if they won't work.
      Position := 0;
      XDR.Decode(Message.Payload, Position, Raw_Handle, Last);
      Position := Last + 1;
      if Raw_Handle in XDR.XDR_Unsigned(Valid_File_Handle_Type'First) ..
                       XDR.XDR_Unsigned(Valid_File_Handle_Type'Last) then
         Handle := Valid_File_Handle_Type(Raw_Handle);
      else
         Decode_Status := Malformed;
         Handle := Valid_File_Handle_Type'First;
      end if;
      XDR.Decode(Message.Payload, Position, Raw_Amount, Last);
      if Raw_Amount in XDR.XDR_Unsigned(Read_Size_Type'First) ..
                       XDR.XDR_Unsigned(Read_Size_Type'Last) then
         Amount := Read_Size_Type(Raw_Amount);
      else
         Decode_Status := Malformed;
         Amount := Read_Size_Type'First; --what type is this?
      end if;
   end Read_Request_Decode;


   procedure Read_Reply_Decode
     (Message : in  Message_Record;
      Handle  : out Valid_File_Handle_Type;
      Amount  : out Read_Result_Size_Type;
      Data    : out Octet_Array;
      Decode_Status : out Message_Status_Type)
   is
      Raw_Handle : XDR.XDR_Unsigned;
      Raw_Amount : XDR.XDR_Unsigned;
      Position   : Data_Index_Type;
      Last       : Data_Index_Type;
   begin
      pragma Warnings
        (Off, "unused assignment to ""Last""", Reason => "No further decoding required");
      Position := 0;
      XDR.Decode(Message.Payload, Position, Raw_Handle, Last);
      Position := Last + 1;
      XDR.Decode(Message.Payload, Position, Raw_Amount, Last);
      Position := Last + 1;
      Data := (others => 0);
      if Raw_Handle in XDR.XDR_Unsigned(Valid_File_Handle_Type'First) ..
          XDR.XDR_Unsigned(Valid_File_Handle_Type'Last) and
         Raw_Amount <= XDR.XDR_Unsigned(Read_Result_Size_Type'Last)
      then
         Handle := Valid_File_Handle_Type(Raw_Handle);
         Amount := Read_Result_Size_Type(Raw_Amount);
         XDR.Decode(Message.Payload, Position, Data(Data'First .. Data'First + (Amount - 1)), Last);
         Decode_Status := Success;
      else
         Handle := Valid_File_Handle_Type'First;
         Amount := Read_Result_Size_Type'First;
         Decode_Status := Malformed;
      end if;
   end Read_Reply_Decode;


   -- Server-side use.
   procedure Write_Request_Decode
     (Message : in  Message_Record;
      Handle  : out Valid_File_Handle_Type;
      Amount  : out Write_Size_Type;
      Data    : out CubedOS.Lib.Octet_Array;
      Decode_Status  : out Message_Status_Type)
   is
      Position : Data_Index_Type;
      Raw_Handle : XDR.XDR_Unsigned;
      Raw_Amount : XDR.XDR_Unsigned;
      Last : Data_Index_Type;
   begin
      pragma Warnings
        (Off, "unused assignment to ""Last""", Reason => "No further decoding required");
      Decode_Status := Success;

      -- TODO: Need to verify the conversions below. Returned Malformed if they won't work.
      Position := 0;
      XDR.Decode(Message.Payload, Position, Raw_Handle, Last);
      if Raw_Handle in XDR.XDR_Unsigned(Valid_File_Handle_Type'First) ..
        XDR.XDR_Unsigned(Valid_File_Handle_Type'Last) then
         Handle := Valid_File_Handle_Type(Raw_Handle);
      else
         Decode_Status := Malformed;
         Handle := Valid_File_Handle_Type'First; --is that what this should be?
      end if;
      Position := Last + 1;
      XDR.Decode(Message.Payload, Position, Raw_Amount, Last);
      if Raw_Amount in XDR.XDR_Unsigned(Write_Result_Size_Type'First + 1) ..
        XDR.XDR_Unsigned(Write_Result_Size_Type'Last) then
         Amount := Write_Result_Size_Type(Raw_Amount);
      else
         Amount := 1;
      end if;
      Position := Last + 1;
      Data := (others => 0);
      XDR.Decode(Message.Payload, Position, Data(Data'First .. Data'First + (Amount - 1)), Last);
   end Write_Request_Decode;


   procedure Write_Reply_Decode
     (Message : in  Message_Record;
      Handle  : out Valid_File_Handle_Type;
      Amount  : out Write_Result_Size_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position : Data_Index_Type;
      Raw_Handle : XDR.XDR_Unsigned;
      Raw_Amount : XDR.XDR_Unsigned;
      Last : Data_Index_Type;
   begin
      pragma Warnings
        (Off, "unused assignment to ""Last""", Reason => "No further decoding required");
      Position := 0;
      XDR.Decode(Message.Payload, Position, Raw_Handle, Last);
      Position := Last + 1;
      XDR.Decode(Message.Payload, Position, Raw_Amount, Last);
      if Raw_Handle in XDR.XDR_Unsigned(Valid_File_Handle_Type'First) ..
          XDR.XDR_Unsigned(Valid_File_Handle_Type'Last) and
         Raw_Amount <= XDR.XDR_Unsigned(Write_Result_Size_Type'Last)
      then
         Handle := Valid_File_Handle_Type(Raw_Handle);
         Amount := Write_Result_Size_Type(Raw_Amount);
         Decode_Status := Success;
      else
         Handle := Valid_File_Handle_Type'First;
         Amount := Write_Result_Size_Type'First;
         Decode_Status := Malformed;
      end if;
   end Write_Reply_Decode;


   -- Server-side use.
   procedure Close_Request_Decode
     (Message : in  Message_Record;
      Handle  : out Valid_File_Handle_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position   : Data_Index_Type;
      Raw_Handle : XDR.XDR_Unsigned;
      Last       : Data_Index_Type;
   begin
       pragma Warnings
        (Off, "unused assignment to ""Last""", Reason => "No further decoding required");
      Decode_Status := Success;

      -- TODO: Need to verify the conversions below. Returned Malformed if they won't work.
      Position := 0;
      XDR.Decode(Message.Payload, Position, Raw_Handle, Last);
      if Raw_Handle in XDR.XDR_Unsigned(Valid_File_Handle_Type'First) ..
        XDR.XDR_Unsigned(Valid_File_Handle_Type'Last) then
         Handle := Valid_File_Handle_Type(Raw_Handle);
      else
         Decode_Status := Malformed;
         Handle := Valid_File_Handle_Type'First; --again, is that what this should be?
      end if;
   end Close_Request_Decode;

end CubedOS.File_Server.API;
