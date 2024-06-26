--------------------------------------------------------------------------------
-- FILE   : domainb_server-api.adb
-- SUBJECT: Body of a package that simplifies use of the module.
-- AUTHOR : (C) Copyright 2024 by Vermont State University
--
--------------------------------------------------------------------------------
with CubedOS.Lib.XDR;
use  CubedOS.Lib;
with Ada.Text_IO;

package body DomainB_Server.API is
   use type XDR.XDR_Unsigned;

   function Ping_Request_Encode
     (Sender_Address : in Message_Address;
      Request_ID     : in Request_ID_Type;
      Priority       : in System.Priority := System.Default_Priority) return Message_Record
   is
      Message : constant Message_Record :=
        Make_Empty_Message
          (Sender_Address   => Sender_Address,
           Receiver_Address => Name_Resolver.DomainB_Server,
           Request_ID       => Request_ID,
           Message_ID       => Message_Type'Pos(Ping_Request),
           Priority         => Priority);
   begin
      return Message;
   end Ping_Request_Encode;


   function Ping_Reply_Encode
     (Receiver_Address : in Message_Address;
      Request_ID : in Request_ID_Type;
      Status     : in Status_Type;
      Priority   : in System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record :=
        Make_Empty_Message
          (Receiver_Address => Receiver_Address,
           Sender_Address   => Name_Resolver.DomainB_Server,
           Request_ID      => Request_ID,
           Message_ID      => Message_Type'Pos(Ping_Reply),
           Priority        => Priority);

      Position : Data_Index_Type;
      Last     : Data_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Status_Type'Pos(Status)), Message.Payload, Position, Last);
      Position := Last + 1;

      Message.Size := Last + 1;
      return Message;
   end Ping_Reply_Encode;


   procedure Ping_Request_Decode
     (Message : in Message_Record;
      Decode_Status : out Message_Status_Type)
   is
   begin
      -- Decoding can never fail in this case because there are no message parameters.
      Decode_Status := Success;
   end Ping_Request_Decode;


   procedure Ping_Reply_Decode
     (Message : in Message_Record;
      Status : out Status_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position  : Data_Index_Type;
      Last      : Data_Index_Type;
      Raw_Value : XDR.XDR_Unsigned;
   begin
      Position := 0;

      XDR.Decode(Message.Payload, Position, Raw_Value, Last);
      Position := Last + 1;

      if Raw_Value > Status_Type'Pos(Status_Type'Last) then
         Ada.Text_IO.Put_Line("Malformed Message");
         Decode_Status := Malformed;
      else
         Status := Status_Type'Val(Raw_Value);
         Decode_Status := Success;
      end if;
   end Ping_Reply_Decode;


end DomainB_Server.API;
