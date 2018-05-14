--------------------------------------------------------------------------------
-- FILE   : cubedos-Positioning_System-api.adb
-- SUBJECT: Body of a package that implements the Positioning_System API
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
-- All the subprograms in this package must be task safe. They can be simultaneously
--called from multiple tasks. If possible, make every subprogram here a pure function.
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.Lib.XDR;
with CubedOS.Lib;
use CubedOS.Lib;
use CubedOS.Lib.XDR;

package body CubedOS.Positioning_System.API is

   function X_Rot_Request_Encode
      (Sender_Domain : Domain_ID_Type;
      Sender : Module_ID_Type;
      --TODO
      Priority : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record;
      Request_ID : Request_ID_Type;
      Position : XDR_Index_Type;
   begin
      Get_Next_Request_ID(Request_ID);
      Message := Make_Empty_Message(
         Sender_Domain   => Sender_Domain,
         Receiver_Domain => Domain_ID,
         Sender     => Sender,
         Receiver   => ID,
         Request_ID   => Request_ID,
         Message_ID => Message_Type'Pos(X_Rot_Request),
         Priority   => Priority);
      Position := 0;
      --TODO
      Message.Size := Position;
      return Message;
   end X_Rot_Request_Encode;


   function X_Rot_Reply_Encode
      (Receiver_Domain : Domain_ID_Type;
      Receiver : Module_ID_Type;
      X_Rot : X_Rotation;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record := Make_Empty_Message(
         Sender_Domain   => Domain_ID,
         Receiver_Domain => Receiver_Domain,
         Sender     => ID,
         Receiver   => Receiver,
         Request_ID   => 0,
         Message_ID => Message_Type'Pos(X_Rot_Reply),
         Priority   => Priority);
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Float(X_Rot), Message.Payload, Position, Last);
      Position := Last + 1;
      Message.Size := Position;
      return Message;
   end X_Rot_Reply_Encode;

   procedure X_Rot_Reply_Decode
      (Message : in  Message_Record;
      X_Rot : out X_Rotation;
      Decode_Status : out Message_Status_Type)
   is
      Position : XDR_Index_Type;
      Raw_X_Rot   : XDR.XDR_Float;
      Last : XDR_Index_Type;
   begin
      pragma Warnings
         (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");
      Decode_Status := Success;
      X_Rot := X_Rotation'First;
      Position := 0;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_X_Rot, Last);
         if Raw_X_Rot in XDR.XDR_Float(X_Rotation'First) .. XDR.XDR_Float(X_Rotation'Last) then
            X_Rot := X_Rotation(Raw_X_Rot);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
   end X_Rot_Reply_Decode;

   function Y_Rot_Request_Encode
      (Sender_Domain : Domain_ID_Type;
      Sender : Module_ID_Type;
      --TODO
      Priority : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record;
      Request_ID : Request_ID_Type;
      Position : XDR_Index_Type;
   begin
      Get_Next_Request_ID(Request_ID);
      Message := Make_Empty_Message(
         Sender_Domain   => Sender_Domain,
         Receiver_Domain => Domain_ID,
         Sender     => Sender,
         Receiver   => ID,
         Request_ID   => Request_ID,
         Message_ID => Message_Type'Pos(Y_Rot_Request),
         Priority   => Priority);
      Position := 0;
      --TODO
      Message.Size := Position;
      return Message;
   end Y_Rot_Request_Encode;


   function Y_Rot_Reply_Encode
      (Receiver_Domain : Domain_ID_Type;
      Receiver : Module_ID_Type;
      Y_Rot : Y_Rotation;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record := Make_Empty_Message(
         Sender_Domain   => Domain_ID,
         Receiver_Domain => Receiver_Domain,
         Sender     => ID,
         Receiver   => Receiver,
         Request_ID   => 0,
         Message_ID => Message_Type'Pos(Y_Rot_Reply),
         Priority   => Priority);
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Float(Y_Rot), Message.Payload, Position, Last);
      Position := Last + 1;
      Message.Size := Position;
      return Message;
   end Y_Rot_Reply_Encode;

   procedure Y_Rot_Reply_Decode
      (Message : in  Message_Record;
      Y_Rot : out Y_Rotation;
      Decode_Status : out Message_Status_Type)
   is
      Position : XDR_Index_Type;
      Raw_Y_Rot   : XDR.XDR_Float;
      Last : XDR_Index_Type;
   begin
      pragma Warnings
         (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");
      Decode_Status := Success;
      Y_Rot := Y_Rotation'First;
      Position := 0;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Y_Rot, Last);
         if Raw_Y_Rot in XDR.XDR_Float(Y_Rotation'First) .. XDR.XDR_Float(Y_Rotation'Last) then
            Y_Rot := Y_Rotation(Raw_Y_Rot);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
   end Y_Rot_Reply_Decode;

   function Z_Rot_Request_Encode
      (Sender_Domain : Domain_ID_Type;
      Sender : Module_ID_Type;
      --TODO
      Priority : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record;
      Request_ID : Request_ID_Type;
      Position : XDR_Index_Type;
   begin
      Get_Next_Request_ID(Request_ID);
      Message := Make_Empty_Message(
         Sender_Domain   => Sender_Domain,
         Receiver_Domain => Domain_ID,
         Sender     => Sender,
         Receiver   => ID,
         Request_ID   => Request_ID,
         Message_ID => Message_Type'Pos(Z_Rot_Request),
         Priority   => Priority);
      Position := 0;
      --TODO
      Message.Size := Position;
      return Message;
   end Z_Rot_Request_Encode;


   function Z_Rot_Reply_Encode
      (Receiver_Domain : Domain_ID_Type;
      Receiver : Module_ID_Type;
      Z_Rot : Z_Rotation;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record := Make_Empty_Message(
         Sender_Domain   => Domain_ID,
         Receiver_Domain => Receiver_Domain,
         Sender     => ID,
         Receiver   => Receiver,
         Request_ID   => 0,
         Message_ID => Message_Type'Pos(Z_Rot_Reply),
         Priority   => Priority);
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Float(Z_Rot), Message.Payload, Position, Last);
      Position := Last + 1;
      Message.Size := Position;
      return Message;
   end Z_Rot_Reply_Encode;

   procedure Z_Rot_Reply_Decode
      (Message : in  Message_Record;
      Z_Rot : out Z_Rotation;
      Decode_Status : out Message_Status_Type)
   is
      Position : XDR_Index_Type;
      Raw_Z_Rot   : XDR.XDR_Float;
      Last : XDR_Index_Type;
   begin
      pragma Warnings
         (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");
      Decode_Status := Success;
      Z_Rot := Z_Rotation'First;
      Position := 0;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Z_Rot, Last);
         if Raw_Z_Rot in XDR.XDR_Float(Z_Rotation'First) .. XDR.XDR_Float(Z_Rotation'Last) then
            Z_Rot := Z_Rotation(Raw_Z_Rot);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
   end Z_Rot_Reply_Decode;


end CubedOS.Positioning_System.API;
