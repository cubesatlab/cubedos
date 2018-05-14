--------------------------------------------------------------------------------
-- FILE   : cubedos-Spiral_Thruster-api.adb
-- SUBJECT: Body of a package that implements the Spiral_Thruster API
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
-- All the subprograms in this package must be task safe. They can be simultaneously
--called from multiple tasks. If possible, make every subprogram here a pure function.
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.Lib.XDR;
with CubedOS.Lib;
use CubedOS.Lib;
use CubedOS.Lib.XDR;

package body CubedOS.Spiral_Thruster.API is

   function Thrust_Command_Request_Encode
      (Sender_Domain : Domain_ID_Type;
      Sender : Module_ID_Type;
      Phi_Degrees : Phi_Degrees_Type;
      Theta_Degrees : Theta_Degrees_Type;
      Thrust_Force : Thrust_Force_Type;
      Time : Time_Type;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record;
      Request_ID : Request_ID_Type;
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
   begin
      Get_Next_Request_ID(Request_ID);
      Message := Make_Empty_Message(
         Sender_Domain   => Sender_Domain,
         Receiver_Domain => Domain_ID,
         Sender     => Sender,
         Receiver   => ID,
         Request_ID   => Request_ID,
         Message_ID => Message_Type'Pos(Thrust_Command_Request),
         Priority   => Priority);
      Position := 0;
      XDR.Encode(XDR.XDR_Float(Phi_Degrees), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Float(Theta_Degrees), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Float(Thrust_Force), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Time), Message.Payload, Position, Last);
      Position := Last + 1;
      Message.Size := Position;
      return Message;
   end Thrust_Command_Request_Encode;

   procedure Thrust_Command_Request_Decode
      (Message : in  Message_Record;
      Phi_Degrees : out Phi_Degrees_Type;
      Theta_Degrees : out Theta_Degrees_Type;
      Thrust_Force : out Thrust_Force_Type;
      Time : out Time_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position : XDR_Index_Type;
      Raw_Phi_Degrees   : XDR.XDR_Float;
      Raw_Theta_Degrees   : XDR.XDR_Float;
      Raw_Thrust_Force   : XDR.XDR_Float;
      Raw_Time   : XDR.XDR_Unsigned;
      Last : XDR_Index_Type;
   begin
      pragma Warnings
         (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");
      Decode_Status := Success;
      Phi_Degrees := Phi_Degrees_Type'First;
      Theta_Degrees := Theta_Degrees_Type'First;
      Thrust_Force := Thrust_Force_Type'First;
      Time := Time_Type'First;
      Position := 0;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Phi_Degrees, Last);
         Position := Last + 1;
         if Raw_Phi_Degrees in XDR.XDR_Float(Phi_Degrees_Type'First) .. XDR.XDR_Float(Phi_Degrees_Type'Last) then
            Phi_Degrees := Phi_Degrees_Type(Raw_Phi_Degrees);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Theta_Degrees, Last);
         Position := Last + 1;
         if Raw_Theta_Degrees in XDR.XDR_Float(Theta_Degrees_Type'First) .. XDR.XDR_Float(Theta_Degrees_Type'Last) then
            Theta_Degrees := Theta_Degrees_Type(Raw_Theta_Degrees);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Thrust_Force, Last);
         Position := Last + 1;
         if Raw_Thrust_Force in XDR.XDR_Float(Thrust_Force_Type'First) .. XDR.XDR_Float(Thrust_Force_Type'Last) then
            Thrust_Force := Thrust_Force_Type(Raw_Thrust_Force);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Time, Last);
         if Raw_Time in XDR.XDR_Unsigned(Time_Type'First) .. XDR.XDR_Unsigned(Time_Type'Last) then
            Time := Time_Type(Raw_Time);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
   end Thrust_Command_Request_Decode;

   function Thrust_Command_Reply_Encode
      (Receiver_Domain : Domain_ID_Type;
      Receiver : Module_ID_Type;
      Thrust_Status : Spiral_Thrust_Status;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record := Make_Empty_Message(
         Sender_Domain   => Domain_ID,
         Receiver_Domain => Receiver_Domain,
         Sender     => ID,
         Receiver   => Receiver,
         Request_ID   => 0,
         Message_ID => Message_Type'Pos(Thrust_Command_Reply),
         Priority   => Priority);
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Spiral_Thrust_Status'Pos(Thrust_Status)), Message.Payload, Position, Last);
      Position := Last + 1;
      Message.Size := Position;
      return Message;
   end Thrust_Command_Reply_Encode;

   procedure Thrust_Command_Reply_Decode
      (Message : in  Message_Record;
      Thrust_Status : out Spiral_Thrust_Status;
      Decode_Status : out Message_Status_Type)
   is
      Position : XDR_Index_Type;
      Raw_Thrust_Status : XDR.XDR_Unsigned;
      Last : XDR_Index_Type;
   begin
      pragma Warnings
         (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");
      Decode_Status := Success;
      Position := 0;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Thrust_Status, Last);
         if Raw_Thrust_Status in Spiral_Thrust_Status'Pos(Spiral_Thrust_Status'First) .. Spiral_Thrust_Status'Pos(Spiral_Thrust_Status'Last) then
            Thrust_Status := Spiral_Thrust_Status'Val(Raw_Thrust_Status);
         else
            Decode_Status := Malformed;
            Thrust_Status := Spiral_Thrust_Status'First;
         end if;
      end if;
   end Thrust_Command_Reply_Decode;

   function Spiral_Thrust_Request_Encode
      (Sender_Domain : Domain_ID_Type;
      Sender : Module_ID_Type;
      Phi_Degrees : Phi_Degrees_Type;
      Theta_Degrees : Theta_Degrees_Type;
      Thrust_Force : Thrust_Force_Type;
      Time : Time_Type;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record;
      Request_ID : Request_ID_Type;
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
   begin
      Get_Next_Request_ID(Request_ID);
      Message := Make_Empty_Message(
         Sender_Domain   => Sender_Domain,
         Receiver_Domain => Domain_ID,
         Sender     => Sender,
         Receiver   => ID,
         Request_ID   => Request_ID,
         Message_ID => Message_Type'Pos(Spiral_Thrust_Request),
         Priority   => Priority);
      Position := 0;
      XDR.Encode(XDR.XDR_Float(Phi_Degrees), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Float(Theta_Degrees), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Float(Thrust_Force), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Time), Message.Payload, Position, Last);
      Position := Last + 1;
      Message.Size := Position;
      return Message;
   end Spiral_Thrust_Request_Encode;

   procedure Spiral_Thrust_Request_Decode
      (Message : in  Message_Record;
      Phi_Degrees : out Phi_Degrees_Type;
      Theta_Degrees : out Theta_Degrees_Type;
      Thrust_Force : out Thrust_Force_Type;
      Time : out Time_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position : XDR_Index_Type;
      Raw_Phi_Degrees   : XDR.XDR_Float;
      Raw_Theta_Degrees   : XDR.XDR_Float;
      Raw_Thrust_Force   : XDR.XDR_Float;
      Raw_Time   : XDR.XDR_Unsigned;
      Last : XDR_Index_Type;
   begin
      pragma Warnings
         (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");
      Decode_Status := Success;
      Phi_Degrees := Phi_Degrees_Type'First;
      Theta_Degrees := Theta_Degrees_Type'First;
      Thrust_Force := Thrust_Force_Type'First;
      Time := Time_Type'First;
      Position := 0;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Phi_Degrees, Last);
         Position := Last + 1;
         if Raw_Phi_Degrees in XDR.XDR_Float(Phi_Degrees_Type'First) .. XDR.XDR_Float(Phi_Degrees_Type'Last) then
            Phi_Degrees := Phi_Degrees_Type(Raw_Phi_Degrees);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Theta_Degrees, Last);
         Position := Last + 1;
         if Raw_Theta_Degrees in XDR.XDR_Float(Theta_Degrees_Type'First) .. XDR.XDR_Float(Theta_Degrees_Type'Last) then
            Theta_Degrees := Theta_Degrees_Type(Raw_Theta_Degrees);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Thrust_Force, Last);
         Position := Last + 1;
         if Raw_Thrust_Force in XDR.XDR_Float(Thrust_Force_Type'First) .. XDR.XDR_Float(Thrust_Force_Type'Last) then
            Thrust_Force := Thrust_Force_Type(Raw_Thrust_Force);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Time, Last);
         if Raw_Time in XDR.XDR_Unsigned(Time_Type'First) .. XDR.XDR_Unsigned(Time_Type'Last) then
            Time := Time_Type(Raw_Time);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
   end Spiral_Thrust_Request_Decode;

   function Spiral_Thrust_Reply_Encode
      (Receiver_Domain : Domain_ID_Type;
      Receiver : Module_ID_Type;
      Thrust_Status : Spiral_Thrust_Status;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record := Make_Empty_Message(
         Sender_Domain   => Domain_ID,
         Receiver_Domain => Receiver_Domain,
         Sender     => ID,
         Receiver   => Receiver,
         Request_ID   => 0,
         Message_ID => Message_Type'Pos(Spiral_Thrust_Reply),
         Priority   => Priority);
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Spiral_Thrust_Status'Pos(Thrust_Status)), Message.Payload, Position, Last);
      Position := Last + 1;
      Message.Size := Position;
      return Message;
   end Spiral_Thrust_Reply_Encode;

   procedure Spiral_Thrust_Reply_Decode
      (Message : in  Message_Record;
      Thrust_Status : out Spiral_Thrust_Status;
      Decode_Status : out Message_Status_Type)
   is
      Position : XDR_Index_Type;
      Raw_Thrust_Status : XDR.XDR_Unsigned;
      Last : XDR_Index_Type;
   begin
      pragma Warnings
         (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");
      Decode_Status := Success;
      Position := 0;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Thrust_Status, Last);
         if Raw_Thrust_Status in Spiral_Thrust_Status'Pos(Spiral_Thrust_Status'First) .. Spiral_Thrust_Status'Pos(Spiral_Thrust_Status'Last) then
            Thrust_Status := Spiral_Thrust_Status'Val(Raw_Thrust_Status);
         else
            Decode_Status := Malformed;
            Thrust_Status := Spiral_Thrust_Status'First;
         end if;
      end if;
   end Spiral_Thrust_Reply_Decode;

   function Standby_Request_Encode
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
         Message_ID => Message_Type'Pos(Standby_Request),
         Priority   => Priority);
      Position := 0;
      --TODO
      Message.Size := Position;
      return Message;
   end Standby_Request_Encode;


   function Standby_Reply_Encode
      (Receiver_Domain : Domain_ID_Type;
      Receiver : Module_ID_Type;
      Thrust_Status : Spiral_Thrust_Status;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record := Make_Empty_Message(
         Sender_Domain   => Domain_ID,
         Receiver_Domain => Receiver_Domain,
         Sender     => ID,
         Receiver   => Receiver,
         Request_ID   => 0,
         Message_ID => Message_Type'Pos(Standby_Reply),
         Priority   => Priority);
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Spiral_Thrust_Status'Pos(Thrust_Status)), Message.Payload, Position, Last);
      Position := Last + 1;
      Message.Size := Position;
      return Message;
   end Standby_Reply_Encode;

   procedure Standby_Reply_Decode
      (Message : in  Message_Record;
      Thrust_Status : out Spiral_Thrust_Status;
      Decode_Status : out Message_Status_Type)
   is
      Position : XDR_Index_Type;
      Raw_Thrust_Status : XDR.XDR_Unsigned;
      Last : XDR_Index_Type;
   begin
      pragma Warnings
         (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");
      Decode_Status := Success;
      Position := 0;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Thrust_Status, Last);
         if Raw_Thrust_Status in Spiral_Thrust_Status'Pos(Spiral_Thrust_Status'First) .. Spiral_Thrust_Status'Pos(Spiral_Thrust_Status'Last) then
            Thrust_Status := Spiral_Thrust_Status'Val(Raw_Thrust_Status);
         else
            Decode_Status := Malformed;
            Thrust_Status := Spiral_Thrust_Status'First;
         end if;
      end if;
   end Standby_Reply_Decode;

   function Telemetry_Value_Request_Encode
      (Sender_Domain : Domain_ID_Type;
      Sender : Module_ID_Type;
      Telem_Addr : Telemetry_Address_Type;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record;
      Request_ID : Request_ID_Type;
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
   begin
      Get_Next_Request_ID(Request_ID);
      Message := Make_Empty_Message(
         Sender_Domain   => Sender_Domain,
         Receiver_Domain => Domain_ID,
         Sender     => Sender,
         Receiver   => ID,
         Request_ID   => Request_ID,
         Message_ID => Message_Type'Pos(Telemetry_Value_Request),
         Priority   => Priority);
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Telem_Addr), Message.Payload, Position, Last);
      Position := Last + 1;
      Message.Size := Position;
      return Message;
   end Telemetry_Value_Request_Encode;

   procedure Telemetry_Value_Request_Decode
      (Message : in  Message_Record;
      Telem_Addr : out Telemetry_Address_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position : XDR_Index_Type;
      Raw_Telem_Addr   : XDR.XDR_Unsigned;
      Last : XDR_Index_Type;
   begin
      pragma Warnings
         (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");
      Decode_Status := Success;
      Telem_Addr := Telemetry_Address_Type'First;
      Position := 0;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Telem_Addr, Last);
         if Raw_Telem_Addr in XDR.XDR_Unsigned(Telemetry_Address_Type'First) .. XDR.XDR_Unsigned(Telemetry_Address_Type'Last) then
            Telem_Addr := Telemetry_Address_Type(Raw_Telem_Addr);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
   end Telemetry_Value_Request_Decode;

   function Telemetry_Value_Reply_Encode
      (Receiver_Domain : Domain_ID_Type;
      Receiver : Module_ID_Type;
      Response_Val : Telemetry_Value_Type;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record := Make_Empty_Message(
         Sender_Domain   => Domain_ID,
         Receiver_Domain => Receiver_Domain,
         Sender     => ID,
         Receiver   => Receiver,
         Request_ID   => 0,
         Message_ID => Message_Type'Pos(Telemetry_Value_Reply),
         Priority   => Priority);
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Float(Response_Val), Message.Payload, Position, Last);
      Position := Last + 1;
      Message.Size := Position;
      return Message;
   end Telemetry_Value_Reply_Encode;

   procedure Telemetry_Value_Reply_Decode
      (Message : in  Message_Record;
      Response_Val : out Telemetry_Value_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position : XDR_Index_Type;
      Raw_Response_Val   : XDR.XDR_Float;
      Last : XDR_Index_Type;
   begin
      pragma Warnings
         (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");
      Decode_Status := Success;
      Response_Val := Telemetry_Value_Type'First;
      Position := 0;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Response_Val, Last);
         if Raw_Response_Val in XDR.XDR_Float(Telemetry_Value_Type'First) .. XDR.XDR_Float(Telemetry_Value_Type'Last) then
            Response_Val := Telemetry_Value_Type(Raw_Response_Val);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
   end Telemetry_Value_Reply_Decode;


end CubedOS.Spiral_Thruster.API;
