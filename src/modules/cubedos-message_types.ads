--------------------------------------------------------------------------------
-- FILE   : cubedos-message_types.ads
-- SUBJECT: Specification of a package for message passing in CubedOS.
-- AUTHOR : (C) Copyright 2023 by Vermont Technical College
--
-- This package defines several types used in the message passing system.
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);

package CubedOS.Message_Types is
   -- Definition of domain ID numbers. Domain #0 is special; it means the "current" domain.
   -- There is a limit to the number of domains that can be used. Make this a generic parameter?
   Maximum_Domain_Count : constant := 256;
   type Domain_ID_Type is new Natural range 0 .. Maximum_Domain_Count;

   -- Definition of module ID numbers. Full IDs are qualified by the domain ID.
   type Module_ID_Type is new Positive range 1 .. 256;

   -- Definition of message IDs. Full IDs are qualified by the module ID.
   -- There is a limit to how many messages a module can define. Make this a generic parameter?
   Maximum_Message_Count : constant := 256;
   type Message_ID_Type is range 0 .. Maximum_Message_Count - 1;

   -- Message Addresses hold the Domain_ID and Module_ID for Modules in a CubedOS Application
   type Message_Address is
      record
         Domain_ID : Domain_ID_Type := 0;
         Module_ID : Module_ID_Type := 1;
      end record;

   type Module_ID_Set is array (Natural range <>) of Module_ID_Type;

   -- Describes a domain
   type Domain_Declaration(Module_Count : Natural) is
      record
         ID : Domain_ID_Type;
         Module_IDs : Module_ID_Set(1 .. Module_Count);
      end record;

   function Declare_Domain(Module_Count : Natural; Domain_ID : Domain_ID_Type; Module_IDs : Module_ID_Set) return Domain_Declaration
     with Pre => Module_Count = Module_IDs'Length,
       Post => (for all M of Module_IDs => Has_Module(Declare_Domain'Result, M));

   function Has_Module(Domain : Domain_Declaration; Module_ID : Module_ID_Type) return Boolean;


   -- The union of a module ID and message ID. Forms a
   -- unique identifier for this type of message across
   -- all domains in this project.
   type Universal_Message_Type is
      record
         Module_ID : Module_ID_Type;
         Message_ID : Message_ID_Type;
      end record;

   type Message_Type_Array is array (Natural range <>) of Universal_Message_Type;
   type Msg_Type_Array_Ptr is access Message_Type_Array;
   type Const_Msg_Type_Array_Ptr is access constant Message_Type_Array;

   type Module_Metadata is
      record
         Module_ID : Module_ID_Type;
         Receive_Types : Const_Msg_Type_Array_Ptr;
      end record;

   function Receives(Receiver : Module_Metadata; Msg_Type : Universal_Message_Type) return Boolean
     with Ghost;

   function Declare_Receives(This_Module : Module_ID_Type; This_Receives : Const_Msg_Type_Array_Ptr) return Module_Metadata
     with Pre => This_Receives /= null,
     Post => (for all T of This_Receives.all => Receives(Declare_Receives'Result, T))
     and Declare_Receives'Result.Module_ID = This_Module;

private
   function Declare_Domain(Module_Count : Natural; Domain_ID : Domain_ID_Type; Module_IDs : Module_ID_Set) return Domain_Declaration
     is (Module_Count, Domain_ID, Module_IDs);

   function Declare_Receives(This_Module : Module_ID_Type; This_Receives : Const_Msg_Type_Array_Ptr) return Module_Metadata
     is (This_Module, This_Receives);

   function Has_Module(Domain : Domain_Declaration; Module_ID : Module_ID_Type) return Boolean
     is (for some M of Domain.Module_IDs => M = Module_ID);

   function Receives(Receiver : Module_Metadata; Msg_Type : Universal_Message_Type) return Boolean
     is (for some T of Receiver.Receive_Types.all => T = Msg_Type);

end CubedOS.Message_Types;
