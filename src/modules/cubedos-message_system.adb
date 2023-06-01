pragma SPARK_Mode (On);

with SPARK.Containers.Formal.Unbounded_Hashed_Maps;
with SPARK.Containers.Types;

pragma Profile (SPARK.Containers.Formal.Unbounded_Hashed_Maps, "gnateDSPARK_BODY_MODE=Off");


package body CubedOS.Message_System
  is

   -------------
   -- Messages
   -------------

   function Sender(Msg: Message_Info_Type) return Message_Address is
      (Msg.m_Sender);

   function Destination(Msg: Message_Info_Type) return Message_Address is
     (Msg.m_Receiver);

   function Has_Request_ID(Msg : Message_Info_Type) return Boolean is
     (Msg.m_Request_ID /= 0);

   function Request_ID(Msg: Message_Info_Type) return Request_ID_Type is
      (Msg.m_Request_ID);

   function Discriminant(Msg: Message_Info_Type) return Message_Discriminant_Type is
     (Msg.Message_Descriminant);

   function To_Byte_Array(Msg : Message_Type'Class) return Data_Array
   is
      Discriminant : constant Data_Array (0 .. 1);
   begin
      XDR.Encode(XDR.XDR_Unsigned(Msg.Message_Info.Message_Descriminant), Discriminant, 0);

      --  Position := 0;
      --  XDR.Encode(XDR.XDR_Unsigned(1000*Interval), Message.Payload, Position, Last);
      --  Position := Last + 1;
      --  XDR.Encode(XDR.XDR_Unsigned(Series_Type'Pos(Request_Type)), Message.Payload, Position, Last);
      --  Position := Last + 1;
      --  XDR.Encode(XDR.XDR_Unsigned(Series_ID), Message.Payload, Position, Last);
      --  Message.Size := Last + 1;
   end To_Byte_Array;




   -------------
   -- Mailboxes
   -------------

   protected body Mailbox_Type is

      procedure Deposit(Msg : access Message_Type'Class) is
      begin
         if Rep.Count = Mailbox_Size then
            Status := Mailbox_Full;
         else
            Rep.Messages(Next_In) := Message;
            if Next_In = Mailbox_Size then
               Next_In := 1;
            else
               Next_In := Next_In + 1;
            end if;
            Count := Count + 1;
            Message_Waiting := True;
            Status := Accepted;
         end if;
      end Deposit;




      entry Receive(Msg : access Message_Type'Class)
      is
      begin
      end;


      --     type Mailbox_Record (Queue_Size : Positive) is
      --  record
      --     Messages : Message_Ptr_Array (1 .. Queue_Size);
      --     Count    : Natural := 0;
      --     Next_In  : Positive := 1;
      --     Next_Out : Positive := 1;
      --  end record;
      --
   end Mailbox_Type;


   ------------
   -- Domains
   ------------

   function HashFn (Domain_ID : Domain_ID_Type) return SPARK.Containers.Types.Hash_Type
   is
   begin
      return SPARK.Containers.Types.Hash_Type(Domain_ID);
   end HashFn;


   package Domain_Maps is new SPARK.Containers.Formal.Unbounded_Hashed_Maps
     (Key_Type => Domain_ID_Type,
      Element_Type => Domain_Type,
     Hash => HashFn);

   Domains_Map : Domain_Maps.Map := Domain_Maps.Empty_Map;

   function Register_Domain (Domain_ID : Domain_ID_Type) return access constant Domain_Type
   is
      New_Domain : Domain_Type;
      Stored : access Domain_Type;
   begin
      Domain_Maps.Insert (Domains_Map, Domain_ID, New_Domain);
      Stored := Domain_Maps.Element(Domains_Map, Domain_ID)'Access;
      return Stored;
   end Register_Domain;

   function Domain_Exists (Domain_ID : Domain_ID_Type) return Boolean is
      (Domain_Maps.Has(Domains_Map, Domain_ID));

   -- Get the domain id of the given domain.
   function ID (Domain : Domain_Type) return Domain_ID_Type is
     (Domain.ID);

   -- Routes the given message to the correct mailbox.
   -- This procedure only goes down the tree to mailboxes
   -- in the given domain.
   procedure Route (Domain : Domain_Ptr; Msg : Message_Type)
     with Pre => Destination(Msg.Message_Info).Domain_ID = Domain.ID;





end CubedOS.Message_System;
