--------------------------------------------------------------------------------
-- FILE   : cubedos-logger-api.adb
-- SUBJECT: Body of the logger's API package
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);
pragma Profile(Ravenscar);
pragma Partition_Elaboration_Policy(Sequential);

with CubedOS.Lib.XDR;

use CubedOS.Lib;

package body CubedOS.Logger.API is
   use type XDR.XDR_Unsigned;

   procedure Log_Message
     (Sender_Domain : Domain_ID_Type; Sender : in Module_ID_Type; Text : in String) is
   begin
      Message_Manager.Route_Message(Log_Text_Encode(Sender_Domain, Sender, Text));
   end Log_Message;


   function Log_Text_Encode
     (Sender_Domain : Domain_ID_Type;
      Sender   : Module_ID_Type;
      Text     : String;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record :=
        Make_Empty_Message
          (Sender_Domain   => Sender_Domain,
           Receiver_Domain => Domain_ID,
           Sender     => Sender,
           Receiver   => ID,
           Message_ID => Message_Type'Pos(Log_Text),
           Priority   => Priority);
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Text'Length), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(Text, Message.Payload, Position, Last);
      Message.Size := Last + 1;
      return Message;
   end Log_Text_Encode;


   procedure Log_Text_Decode
     (Message : in  Message_Record;
      Text : out String;
      Size : out Log_Message_Size_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position : XDR_Index_Type;
      Last     : XDR_Index_Type;
      Raw_Size : XDR.XDR_Unsigned;
      Raw_Text : String(Log_Message_Index_Type) := (others => ' ');
   begin
      Text := (others => ' ');
      Position := 0;
      XDR.Decode(Message.Payload, Position, Raw_Size, Last);
      Position := Last + 1;
      if Raw_Size > Maximum_Log_Message_Size then
         Size := 0;
         Decode_Status := Malformed;
      else
         Size := Log_Message_Size_Type(Raw_Size);

         pragma Warnings(Off, "unused assignment to ""Last""",
            Reason => "The last value of Last is not needed");
         XDR.Decode(Message.Payload, Position, Raw_Text(1 .. Size), Last);
         Text(Text'First .. (Text'First - 1) + Size) := Raw_Text(1 .. Size);
         Decode_Status := Success;
      end if;
   end Log_Text_Decode;

end CubedOS.Logger.API;
