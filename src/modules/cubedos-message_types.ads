--------------------------------------------------------------------------------
-- FILE   : cubedos-message_types.ads
-- SUBJECT: Specification of a package for message passing in CubedOS.
-- AUTHOR : (C) Copyright 2023 by Vermont Technical College
--
-- This package defines several types used in the message passing system.
--------------------------------------------------------------------------------

package CubedOS.Message_Types is
   -- Definition of domain ID numbers. Domain #0 is special; it means the "current" domain.
   -- There is a limit to the number of domains that can be used. Make this a generic parameter?
   Maximum_Domain_Count : constant := 256;
   subtype Domain_ID_Type is Natural range 0 .. Maximum_Domain_Count;

   -- Definition of module ID numbers. Full IDs are qualified by the domain ID.
   subtype Module_ID_Type is Positive range 1 .. 256;

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

private
   function Declare_Domain(Module_Count : Natural; Domain_ID : Domain_ID_Type; Module_IDs : Module_ID_Set) return Domain_Declaration
     is (Module_Count, Domain_ID, Module_IDs);


   function Has_Module(Domain : Domain_Declaration; Module_ID : Module_ID_Type) return Boolean
     is (for some M of Domain.Module_IDs => M = Module_ID);

end CubedOS.Message_Types;
