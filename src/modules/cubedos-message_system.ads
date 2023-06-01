pragma SPARK_Mode (On);

with SPARK.Containers.Formal.Unbounded_Vectors;



with CubedOS.Lib.XDR; use CubedOS.Lib.XDR;

-- Package containing the message passing system used by cubedos modules.
package CubedOS.Message_System
with Abstract_State => Domains,
  Initializes => Domains
is
   -- The address of a module.
   -- For safety, this can't be instantiated directly.
   -- Instead, use a Module_Mailbox to send a message.
   type Message_Address is private;

   type Module_ID_Type is new Natural range 0 .. 255;
   type Domain_ID_Type is new Natural range 0 .. 255;

   -- This number is optional and can be used to associated
   -- several messages as belonging to the same conversation.
   -- Set to 0 if not in use.
   type Request_ID_Type is range 0 .. 255;

   -- Tagged only for the convenient prefix notation
   type Message_Info_Type is private;

   ---------------
   -- Messages
   ---------------

   -- Base class for all messages.
   type Message_Type is abstract tagged
      record
         Message_Info : Message_Info_Type;
      end record;

   -- Used to determine what Message_Info_Type type a message is.
   -- Then we can decode it.
   type Message_Discriminant_Type is range 0 .. 255;


   function Sender(Msg: Message_Info_Type) return Message_Address;
   function Destination(Msg: Message_Info_Type) return Message_Address;
   function Has_Request_ID(Msg : Message_Info_Type) return Boolean;
   function Request_ID(Msg: Message_Info_Type) return Request_ID_Type
     with Pre => Has_Request_ID(Msg);
   function Discriminant(Msg: Message_Info_Type) return Message_Discriminant_Type;

   subtype Data_Array is XDR_Array;


   -- Reads a byte array, producing the appropriate message object.
   -- Implementer may assume the given data was encoded by the implementing
   -- message type.
   function From_Byte_Array(Data : Data_Array) return Message_Type is abstract;

   -- Implemented by Message_Types.
   -- Implementers should convert only the data held in the message.
   -- The message descriminant will be prepended to this function's result.
   function To_Byte_Array_Impl(Msg : Message_Type) return Data_Array is abstract;

   ---------------
   -- Mailboxes
   ---------------

   -- Public view of a Mailbox_Type, seen by all modules.
   type Public_Mailbox_Interface is protected interface;

   procedure Deposite (Box : access Public_Mailbox_Interface; Msg: access Message_Type'Class) is abstract;

   function Can_Receive (Box : access Public_Mailbox_Interface; Discriminant : Message_Discriminant_Type) return Boolean is abstract;


   -- Module private view of a Mailbox_Type, seen only by the owning module.
   type Module_Mailbox_Interface is protected interface;

   -- Constructs a Message_Info_Type object with a destination and source address.
   function Prepare_Message(From : access Module_Mailbox_Interface; To : access Public_Mailbox_Interface'Class) return Message_Info_Type is abstract;

   procedure Send_Message(From : Module_Mailbox_Interface; Msg : Message_Type'Class) is abstract;


    -- tagged only so that we can use the OO syntax
   type Domain_Type is tagged private;

   -- Registers a domain with the messaging system system,
   -- returning an observable access to the domain.
   function Register_Domain (Domain_ID : Domain_ID_Type) return access constant Domain_Type
     with Pre => not Domain_Exists (Domain_ID);

   -- True if a domain with that id has already been registered.
   function Domain_Exists (Domain_ID : Domain_ID_Type) return Boolean
     with Global => (Input => Domains);

   -- Get the domain id of the given domain.
   function ID (Domain : Domain_Type) return Domain_ID_Type;

private

   -------------
   -- Messages
   -------------

   function To_Byte_Array(Msg : Message_Type'Class) return Data_Array;

   type Message_Address is
      record
         Module_ID : Module_ID_Type;
         Domain_ID : Domain_ID_Type;
      end record;

   type Message_Info_Type is
      record
         m_Sender : Message_Address;
         m_Receiver: Message_Address;
         m_Request_ID : Request_ID_Type;
         Message_Descriminant: Message_Discriminant_Type;
      end record;

   --------------
   -- Mailboxes
   --------------

   function Equality (X, Y : Message_Type'Class) return Boolean;

   -- It's not a queue because there is no queue in the formal spark lib
   package Message_Vector is new SPARK.Containers.Formal.Unbounded_Vectors
     (Index_Type => Natural,
      Element_Type => Message_Type'Class,
     "=" => Equality);

   -- The array that holds pointers to the actual messages
   type Message_Ptr_Array is array (Positive range <>) of access Message_Info_Type;

   type Mailbox_Record (Queue_Size : Positive) is
      record
         Messages : Message_Ptr_Array (1 .. Queue_Size);
         Count    : Natural := 0;
         Next_In  : Positive := 1;
         Next_Out : Positive := 1;
      end record;

   protected type Mailbox_Type (Queue_Size : Positive) is new Public_Mailbox_Interface and Module_Mailbox_Interface with

      -- Send the indicated Message_Info_Type. This procedure returns at once without waiting for the
      -- Message_Info_Type to be received. If the Mailbox_Type is full the returned status indicates this.
      procedure Deposit(Msg : access Message_Type'Class);

      -- Receive a Message_Info_Type. This entry waits indefinitely for a message to be available.
      entry Receive(Msg : access Message_Type'Class);

   private
      Rep : Mailbox_Record (Queue_Size);
   end Mailbox_Type;

   overriding
   procedure Deposite (Box : access Mailbox_Type; Msg: access Message_Type'Class);

   overriding
   function Can_Receive (Box : access Mailbox_Type; Discriminant : Message_Discriminant_Type) return Boolean;

   overriding
   function Prepare_Message(From : access Mailbox_Type; To : access Public_Mailbox_Interface'Class) return Message_Info_Type;

   overriding
   procedure Send_Message(From : Mailbox_Type; Msg : Message_Type'Class);

   type Domain_Type is tagged
      record
         ID : Domain_ID_Type;
      end record;


   type Domain_Ptr is not null access Domain_Type'Class;

   -- Adds the given mailbox to the given domain.
   procedure Add_Mailbox (Domain : Domain_Ptr; Box : not null access Public_Mailbox_Interface'Class);


end CubedOS.Message_System;
