--------------------------------------------------------------------------------
-- FILE   : cubedos-log_server-api.adb
-- SUBJECT: Body of the logger's API package
-- AUTHOR : (C) Copyright 2022 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);
pragma Profile(Ravenscar);
pragma Partition_Elaboration_Policy(Sequential);

with CubedOS.Lib.XDR;

use CubedOS.Lib;

package body CubedOS.Log_Server.API is
   use type XDR.XDR_Unsigned;

   procedure Log_Message
     (Sender_Address : in Message_Address;
      Log_Level      : in Log_Level_Type;
      Text           : in String)
   is
   begin
      Message_Manager.Route_Message(Log_Text_Encode(Sender_Address, 0, Log_Level, Text));
   end Log_Message;


   function Log_Text_Encode
     (Sender_Address : Message_Address;
      Request_ID     : Request_ID_Type;
      Log_Level      : Log_Level_Type;
      Text           : String;
      Priority       : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record :=
        Make_Empty_Message
          (Sender_Address   => Sender_Address,
           Receiver_Address => Name_Resolver.Log_Server,
           Request_ID       => Request_ID,
           Message_ID       => Message_Type'Pos(Log_Text),
           Priority         => Priority);
      Position : Data_Index_Type;
      Last : Data_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Log_Level_Type'Pos(Log_Level)), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Text'Length), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(Text, Message.Payload, Position, Last);
      Message.Size := Last + 1;
      return Message;
   end Log_Text_Encode;


   procedure Log_Text_Decode
     (Message   : in  Message_Record;
      Log_Level : out Log_Level_Type;
      Text      : out String;
      Size      : out Log_Message_Size_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position      : Data_Index_Type;
      Last          : Data_Index_Type;
      Raw_Log_Level : XDR.XDR_Unsigned;
      Raw_Size      : XDR.XDR_Unsigned;
      Raw_Text      : String(Log_Message_Index_Type) := (others => ' ');
   begin
      Text := (others => ' ');
      Position := 0;
      XDR.Decode(Message.Payload, Position, Raw_Log_Level, Last);
      Position := Last + 1;
      if Raw_Log_Level > Log_Level_Type'Pos(Log_Level_Type'Last) then
         Log_Level     := Critical;
         Size          := 0;
         Decode_Status := Malformed;
      else
         Log_Level := Log_Level_Type'Val(Raw_Log_Level);
         XDR.Decode(Message.Payload, Position, Raw_Size, Last);
         Position := Last + 1;
         if Raw_Size > Maximum_Log_Message_Size then
            Log_Level     := Critical;
            Size          := 0;
            Decode_Status := Malformed;
         else
            Size := Log_Message_Size_Type(Raw_Size);

            pragma Warnings
              (Off, """Last"" is set by ""Decode"" but not used after the call",
                    Reason  => "The last value of Last is not needed");
            XDR.Decode(Message.Payload, Position, Raw_Text(1 .. Size), Last);
            Text(Text'First .. (Text'First - 1) + Size) := Raw_Text(1 .. Size);
            Decode_Status := Success;
         end if;
      end if;
   end Log_Text_Decode;

end CubedOS.Log_Server.API;
