--------------------------------------------------------------------------------
-- FILE   : cubedos-lib-generic_rid_map.ads
-- SUBJECT: Specification of a package for maps of Request IDs to some other data type
--          used to map Request IDs to senders, for instance.
-- AUTHOR : (C) Copyright 2018 by Vermont Technical College
--
--------------------------------------------------------------------------------

generic
   type Value_Type is private;

package CubedOS.Lib.Generic_rID_Map
with SPARK_Mode => On
is
   type Num_Entries_Type is new Natural range 0 .. 10000;
   type Key_Array_Type is array(Num_Entries_Type)
     of Natural; --NOTE: this only works because the
                 --CubedOS.Lib.Generic_Message_Manager's Request IDs are an alias for Natural.
                 --TODO: FIX THIS
   type Value_Array_Type is array(Num_Entries_Type)
     of Value_Type;
   type Used_Indices_Array_Type is array(Num_Entries_Type)
     of Boolean;

   --the map itself. This program maintains the next free index to speed up inserts (sort
   --of an amortized deal where because we maintain the next pointer at insert, we speed
   --up execution overall)
   type RID_Map_Record is
      record
         Key_Array : Key_Array_Type := (others => 0);
         Value_Array : Value_Array_Type;
         Used_Indices_Array : Used_Indices_Array_Type := (others => False);
         Next_Free_Index : Num_Entries_Type := 0;
      end record;

   --insert into the next available space
   procedure Insert(Map : in out RID_Map_Record; Key : in Natural;
                    Value : in Value_Type);

   --purge entries up to this index
   procedure Purge(Map : in out RID_Map_Record; Index : in Num_Entries_Type);

   --get entry for this ID (not sure yet what to do if there isn't an entry)
   function Get_At(Map : RID_Map_Record; Key : Natural) return Value_Type;

end CubedOS.Lib.Generic_rID_Map;
