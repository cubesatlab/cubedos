--------------------------------------------------------------------------------
-- FILE   : cubedos-cfdp-messages.adb
-- SUBJECT: Package containing the CFDP message loop
-- AUTHOR : (C) Copyright 2016 by Vermont Technical College
--
-- This is in it's own child package so that it can be left out of the test program.
-- The message loop doesn't terminate and we want the test program to terminate!
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.CFDP.API;
with CubedOS.CFDP.Internals;
with CubedOS.Lib.XDR;

package body CubedOS.CFDP.Messages is
   use Message_Manager;
   use CubedOS.CFDP.API;
   use CubedOS.CFDP.Internals;
   use CubedOS.Lib;
   use type XDR.XDR_Unsigned;

   -- This is horribly memory inefficient (because Transaction_ID is a 16 bit type).
   Transactions : array(Transaction_ID) of Transaction_Record;

   -------------------
   -- Message Handling
   -------------------

   procedure Get_Free_Transaction(Transaction : out Transaction_ID; Found : out Boolean)
     with
       Global => (In_Out => Transactions),
       Depends => ((Transactions, Transaction, Found) => Transactions)
   is
   begin
      -- Initialize out parameters to the "not found" case.
      Transaction := Transaction_ID'First;
      Found := False;

      -- Look for the first available transaction record.
      for I in Transaction_ID loop
         if not Transactions(I).Is_Used then
            Transactions(I).Is_Used := True;
            Transaction := I;
            Found := True;
            exit;
         end if;
      end loop;
   end Get_Free_Transaction;

   -----------------------------------
   -- Message Decoding and Dispatching
   -----------------------------------

   -- Here is where decoding procedures should be placed for splitting messages into well
   -- typed values according to the message API.

   -- TODO. This should be part of the XDR2OS3 generated API package!
   function Transaction_Indication_Reply
     (Sender : Module_ID_Type; Transaction : Transaction_ID) return Message_Record
   is
      Message  : Message_Record := Make_Empty_Message
        (Sender_Domain => Domain_ID,
         Receiver_Domain => Domain_ID,
         Sender     => Sender,
         Receiver   => ID,
         Request_ID => 0,  -- TODO: This arbitrary value is almost certainly wrong!
         Message_ID => Indication_Type'Pos(Transaction_Indication));
      Position : XDR_Index_Type;
      Last     : XDR_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Transaction), Message.Payload, Position, Last);
      Message.Size := Last + 1;
      return Message;
   end Transaction_Indication_Reply;


   procedure Process_Put_Request(Incoming_Message : in Message_Record) is
      Transaction : Transaction_ID;
      Transaction_Found : Boolean;
   begin
      Get_Free_Transaction(Transaction, Transaction_Found);
      if not Transaction_Found then
         -- TODO: What should happen if we can't honor the put request?
         null;
      else
         -- Send the user a Transaction.indication.
         -- TODO: Generalize this code so that arbitrary domains can be used.
         Message_Manager.Route_Message
           (Transaction_Indication_Reply(Incoming_Message.Sender, Transaction));

         -- TODO Create and send a metadata PDU.
      end if;
   end Process_Put_Request;


   -- This procedure processes exactly one message.
   -- TODO: What should be done about malformed messages? Is ignoring them good enough?
   procedure Process_Message(Incoming_Message : in Message_Record)
     with Global => (In_Out => (Transactions, Mailboxes))
   is
      Message_Kind : XDR.XDR_Unsigned;
      Position     : XDR_Index_Type;
      Last         : XDR_Index_Type;
   begin
      -- All messages must have at least a message kind field.
      if Incoming_Message.Size < 4 then
         return;
      end if;
      Position := 0;
      XDR.Decode(Incoming_Message.Payload, Position, Message_Kind, Last);
      Position := Last + 1;

      -- If the message kind is unrecognized, the message is malformed.
      if Message_Kind > XDR.XDR_Unsigned(Message_Type'Pos(Message_Type'Last))
      then
         return;
      end if;

      -- Deal with the different kinds of messages.
      case Message_Type'Val(Message_Kind) is
         when Put_Request =>
            Process_Put_Request(Incoming_Message);

         when Cancel_Request =>
            null;

         when Suspend_Request =>
            null;

         when Resume_Request =>
            null;

         when Report_Request =>
            null;
      end case;

   end Process_Message;

   ---------------
   -- Message Loop
   ---------------

   task body Message_Loop is
      Incoming_Message : Message_Manager.Message_Record;
   begin
      loop
         Message_Manager.Fetch_Message(ID, Incoming_Message);
         Process_Message(Incoming_Message);
      end loop;
   end Message_Loop;

end CubedOS.CFDP.Messages;
