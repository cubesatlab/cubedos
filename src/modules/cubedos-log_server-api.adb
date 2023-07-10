--------------------------------------------------------------------------------
-- FILE   : %FILENAME%
-- SUBJECT: Body of a package that implements the Log_Server API
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
-- All the subprograms in this package are task safe.
--
-- THIS FILE WAS GENERATED BY Merc. DO NOT EDIT!!
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.Lib;
use  CubedOS.Lib;

package body CubedOS.Log_Server.API is

   procedure Log_Text_Encode
      (Sender_Address : Message_Address;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Level : Log_Level_Type;
      Msg_Content : String;
      Result : out Message_Record;
      Priority : System.Priority := System.Default_Priority)
   is
      subtype Data_Index_Type is XDR_Index_Type range 0 .. 1023;
      Position   : Data_Index_Type;
      Last       : Data_Index_Type;
      subtype Definite_Data_Array is Data_Array(Data_Index_Type);
      Payload : Data_Array_Owner := new Definite_Data_Array'(others => 0);
      Message : Mutable_Message_Record;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Log_Level_Type'Pos(Level)), Payload.all, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Msg_Content'Length), Payload.all, Position, Last);
      Position := Last + 1;
      XDR.Encode(Msg_Content, Payload.all, Position, Last);
      Position := Last + 1;
      Make_Empty_Message (
         Sender_Address   => Sender_Address,
         Receiver_Address => Receiver_Address,
         Request_ID   => Request_ID,
         Message_Type => Log_Text_Msg,
         Payload => Payload,
         Result => Message,
         Priority   => Priority);
      Result := Immutable(Message);
      Delete(Message);
      pragma Unused(Last, Payload, Position, Message);
   end Log_Text_Encode;

   procedure Send_Log_Text
      (Sender : Module_Mailbox;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Level : Log_Level_Type;
      Msg_Content : String;
      Priority : System.Priority := System.Default_Priority)
   is
      Message : Message_Record;
   begin
      Log_Text_Encode(
         Sender_Address => (Message_Manager.Domain_ID, Module_ID(Sender)),
         Receiver_Address => Receiver_Address,
         Request_ID => Request_ID,
         Level => Level,
         Msg_Content => Msg_Content,
         Result => Message,
         Priority => Priority);
      Message_Manager.Send_Message(Sender, Message);
   end Send_Log_Text;
   procedure Log_Text_Decode
      (Message : in  Message_Record;
      Level : out Log_Level_Type;
      Msg_Content : out String;
      Msg_Content_Size : out Natural;
      Decode_Status : out Message_Status_Type)
   is
      Position : Data_Index_Type;
      Raw_Level : XDR.XDR_Unsigned;
      Raw_Msg_Content_Size : XDR.XDR_Unsigned;
      Last : Data_Index_Type;
   begin
      Decode_Status := Success;
      Level := Log_Level_Type'First;
      Msg_Content := (others => ' ');
      Position := 0;
      if Decode_Status = Success then
         XDR.Decode(Payload(Message).all, Position, Raw_Level, Last);
         Position := Last + 1;
         if Raw_Level in Log_Level_Type'Pos(Log_Level_Type'First) .. Log_Level_Type'Pos(Log_Level_Type'Last) then
            Level := Log_Level_Type'Val(Raw_Level);
         else
            Decode_Status := Malformed;
            Level := Log_Level_Type'First;
         end if;
      end if;
      if Decode_Status = Success then
         XDR.Decode(Payload(Message).all, Position, Raw_Msg_Content_Size, Last);
         Position := Last + 1;
         if Raw_Msg_Content_Size in XDR.XDR_Unsigned(Natural'First) .. XDR.XDR_Unsigned(Natural'Last) then
            Msg_Content_Size := Natural(Raw_Msg_Content_Size);
         else
            Msg_Content_Size := 0;
         end if;
         if Msg_Content_Size < 1 then
            XDR.Decode(Payload(Message).all, Position, Msg_Content(Msg_Content'First .. Msg_Content'First + (Msg_Content_Size - 1)), Last);
         end if;
      end if;
   end Log_Text_Decode;


end CubedOS.Log_Server.API;
