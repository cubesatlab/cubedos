--------------------------------------------------------------------------------
-- FILE   : cubedos-test3-api.adb
-- SUBJECT: Body of a package that implements the test3 API
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
-- All the subprograms in this package must be task safe. They can be simultaneously
--called from multiple tasks. If possible, make every subprogram here a pure function.
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.Lib.XDR;
with CubedOS.Lib;
use CubedOS.Lib;
use CubedOS.Lib.XDR;

package body CubedOS.test3.API is

   function ms_Encode
      (Receiver_Domain : Domain_ID_Type;
      Receiver : Module_ID_Type;
      mss1 : s1;
      e3 : e1;
      b2 : b1;
      h1 : Integer;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record := Make_Empty_Message(
         Sender_Domain   => Domain_ID,
         Receiver_Domain => Receiver_Domain,
         Sender     => ID,
         Receiver   => Receiver,
         Request_ID   => 0,
         Message_ID => Message_Type'Pos(ms),
         Priority   => Priority);
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
   begin
      Position := 0;
      for Y in Integer range 0 .. 2 loop
         for I in Integer range 0 .. 2 loop
            XDR.Encode(XDR.XDR_Unsigned(e1'Pos(mss1(Y).e2(I))), Message.Payload, Position, Last);
            Position := Last + 1;
         end loop;
         for I in Integer range 0 .. 2 loop
            XDR.Encode(XDR.XDR_Integer(mss1(Y).iv(I)), Message.Payload, Position, Last);
            Position := Last + 1;
         end loop;
         for I in Integer range 0 .. 9 loop
            XDR.Encode(XDR.XDR_Integer(mss1(Y).i(I)), Message.Payload, Position, Last);
            Position := Last + 1;
         end loop;
         for I in Integer range 0 .. 3 loop
            XDR.Encode(XDR.XDR_Hyper(mss1(Y).hyp1(I)), Message.Payload, Position, Last);
            Position := Last + 1;
         end loop;
      end loop;
      for I in Integer range 0 .. 2 loop
         XDR.Encode(XDR.XDR_Unsigned(e1'Pos(e3(I)), Message.Payload, Position, Last);
         Position := Last + 1;
      end loop;
      for I in Integer range 0 .. 2 loop
         XDR.Encode(XDR.XDR_Integer(h1(I)), Message.Payload, Position, Last);
         Position := Last + 1;
      end loop;
      Message.Size := Position;
      return Message;
   end ms_Encode;

   procedure ms_Decode
      (Message : in  Message_Record;
      mss1 : out s1;
      e3 : out e1;
      b2 : out b1;
      h1 : out Integer;
      Decode_Status : out Message_Status_Type)
   is
      Position : XDR_Index_Type;
      Raw_mss1_fs1   : XDR.XDR_Unsigned;
      Raw_mss1_fs2   : XDR.XDR_Unsigned;
      Raw_mss1_fs3   : XDR.XDR_Unsigned;
      Raw_mss1_fs4   : XDR.XDR_Unsigned;
      Raw_mss1_fs5   : XDR.XDR_Unsigned;
      Raw_mss1_fs6   : XDR.XDR_Unsigned;
      Raw_mss1_e2   : XDR.XDR_Unsigned;
      Raw_mss1_iv   : XDR.XDR_Integer;
      Raw_mss1_i   : XDR.XDR_Integer;
      Raw_mss1_hyp1   : XDR.XDR_Hyper;
      Raw_e3 : XDR.XDR_Unsigned;
      Raw_h1   : XDR.XDR_Integer;
      Last : XDR_Index_Type;
   begin
      pragma Warnings
         (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");
      Decode_Status := Success;
      for Y in Integer range 0 .. 2 loop
         for I in Integer range 0 .. 2 loop
            mss1(Y).iv(I) := int_var'First;
         end loop;
         for I in Integer range 0 .. 9 loop
            mss1(Y).i(I) := Integer(XDR.XDR_Integer'First);
         end loop;
         for I in Integer range 0 .. 3 loop
            mss1(Y).hyp1(I) := Lib.Hyper(XDR.XDR_Hyper'First);
         end loop;
      end loop;
      for I in Integer range 0 .. 2 loop
         h1(I) := Integer(XDR.XDR_Integer'First);
      end loop;
      Position := 0;
      for Y in Integer range 0 .. 2 loop
         for I in Integer range 0 .. 2 loop
            if Decode_Status = Success then
               XDR.Decode(Message.Payload, Position, Raw_mss1_e2, Last);
               Position := Last + 1;
               if Raw_mss1_e2 in e1'Pos(e1'First) .. e1'Pos(e1'Last) then
                  mss1(Y).e2(I) := e1'Val(Raw_mss1_e2);
               else
                  Decode_Status := Malformed;
                  mss1(Y).e2(I) := e1'First;
               end if;
            end if;
         end loop;
         for I in Integer range 0 .. 2 loop
            if Decode_Status = Success then
               XDR.Decode(Message.Payload, Position, Raw_mss1_iv, Last);
               Position := Last + 1;
               if Raw_mss1_iv in XDR.XDR_Integer(Integer'First) .. XDR.XDR_Integer(Integer'Last) then
                  mss1(Y).iv(I) := int_var(Raw_mss1_iv);
                  Decode_Status := Success;
               else
                  Decode_Status := Malformed;
               end if;
            end if;
         end loop;
         for I in Integer range 0 .. 9 loop
            if Decode_Status = Success then
               XDR.Decode(Message.Payload, Position, Raw_mss1_i, Last);
               Position := Last + 1;
               if Raw_mss1_i in XDR.XDR_Integer(Integer'First) .. XDR.XDR_Integer(Integer'Last) then
                  mss1(Y).i(I) := Integer(Raw_mss1_i);
                  Decode_Status := Success;
               else
                  Decode_Status := Malformed;
               end if;
            end if;
         end loop;
         for I in Integer range 0 .. 3 loop
            if Decode_Status = Success then
               XDR.Decode(Message.Payload, Position, Raw_mss1_hyp1, Last);
               Position := Last + 1;
               if Raw_mss1_hyp1 in XDR.XDR_Hyper(Lib.Hyper_Type'First) .. XDR.XDR_Hyper(Lib.Hyper_Type'Last) then
                  mss1(Y).hyp1(I) := Lib.Hyper_Type(Raw_mss1_hyp1);
                  Decode_Status := Success;
               else
                  Decode_Status := Malformed;
               end if;
            end if;
         end loop;
      end loop;
      for I in Integer range 0 .. 2 loop
         if Decode_Status = Success then
            XDR.Decode(Message.Payload, Position, Raw_e3, Last);
            Position := Last + 1;
            if Raw_e3 in e1'Pos(e1'First) .. e1'Pos(e1'Last) then
               e3(I) := e1'Val(Raw_e3);
            else
               Decode_Status := Malformed;
               e3(I) := e1'First;
            end if;
         end if;
      end loop;
      for I in Integer range 0 .. 2 loop
         if Decode_Status = Success then
            XDR.Decode(Message.Payload, Position, Raw_h1, Last);
            Position := Last + 1;
            if Raw_h1 in XDR.XDR_Integer(Integer'First) .. XDR.XDR_Integer(Integer'Last) then
               h1(I) := Integer(Raw_h1);
               Decode_Status := Success;
            else
               Decode_Status := Malformed;
            end if;
         end if;
      end loop;
   end ms_Decode;

   function ms1_Encode
      (Receiver_Domain : Domain_ID_Type;
      Receiver : Module_ID_Type;
      --TODO
      Priority : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record := Make_Empty_Message(
         Sender_Domain   => Domain_ID,
         Receiver_Domain => Receiver_Domain,
         Sender     => ID,
         Receiver   => Receiver,
         Request_ID   => 0,
         Message_ID => Message_Type'Pos(ms1),
         Priority   => Priority);
      Position : XDR_Index_Type;
   begin
      Position := 0;
      --TODO
      Message.Size := Position;
      return Message;
   end ms1_Encode;



end CubedOS.test3.API;
