--------------------------------------------------------------------------------
-- FILE   : cubedos-lib-generic_rid_map.adb
-- SUBJECT: Implementation of a package for maps of Request IDs to some other data type
--          used to map Request IDs to senders, for instance.
-- AUTHOR : (C) Copyright 2018 by Vermont Technical College
--
--------------------------------------------------------------------------------

package body CubedOS.Lib.Generic_rID_Map is

   --insert into the next available space
   procedure Insert(Map : in out RID_Map_Record; Key : in Natural;
                    Value : in Value_Type)
   is
      Next_Found : Boolean;
      Temp_Index : Num_Entries_Type;
   begin
      Next_Found := False;
      Temp_Index := 0;

      --TODO: out of bounds checking everywhere in here

      --add the new value
      Map.Key_Array(Map.Next_Free_Index) := Key;
      Map.Value_Array(Map.Next_Free_Index) := Value;
      Map.Used_Indices_Array(Map.Next_Free_Index) := True;

      --TODO: possible to get stuck in here forever
      --figure out the next free index
      while not Next_Found loop
         if not Map.Used_Indices_Array(Temp_Index) then
            Map.Next_Free_Index := Temp_Index;
            Next_Found := True;
         else
            if Temp_Index + 1 > Num_Entries_Type'Last then
               Temp_Index := 0;
            else
               Temp_Index := Temp_Index + 1;
            end if;
         end if;
      end loop;

   end Insert;

   --purge entries up to this index
   procedure Purge(Map : in out RID_Map_Record; Index : in Num_Entries_Type)
   is
   begin
      --reset the used_index array and key
      for I in 0 .. Index loop
         if Map.Used_Indices_Array(I) then
            Map.Used_Indices_Array(I) := False;
            Map.Key_Array(I) := 0;
         end if;
      end loop;

      --we know that at least 0 is free now for sure, so..
      Map.Next_Free_Index := 0;
   end Purge;

   --get entry for this ID (not sure yet what to do if there isn't an entry)
   function Get_At(Map : RID_Map_Record; Key : Natural) return Value_Type
   is
      Value : Value_Type;
      Found_Index : Boolean;
      Index : Num_Entries_Type;
   begin
      Found_Index := False;
      Index := 0;

      --set index to the index of the key
      while not Found_Index loop
         if Map.Key_Array(Index) = Key then
            Found_Index := True;
         else
            Index := Index + 1;
         end if;
      end loop;

      if Map.Used_Indices_Array(Index) then
         Value := Map.Value_Array(Index);
      else
         null; --This is a bad place to reach. need to do something about this error.
      end if;

      return Value;
   end Get_At;

end CubedOS.Lib.Generic_rID_Map;
