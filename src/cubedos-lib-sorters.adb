--------------------------------------------------------------------------------
-- FILE   : cubedos-lib-sorters.adb
-- SUBJECT: Body of a package containing utility sorting procedures.
-- AUTHOR : (C) Copyright 2018 by Vermont Technical College
--
--------------------------------------------------------------------------------

package body CubedOS.Lib.Sorters is

   -- This placeholder implementation allows the test program to build when the assertion
   -- policy is set to 'Check'. Otherwise the uses of Perm in assertions cause an undefined
   -- external reference error.
   --
   function Perm(A : in Array_Type; B : in Array_Type) return Boolean is
      pragma Unreferenced(A, B);
   begin
      -- TODO: Write a real version of Perm (without sorting?)
      return True;
   end Perm;


   --function Perm_Transitive(A, B, C : Array_Type) return Boolean
   --  with
   --    Global => null,
   --    Post   => (if Perm_Transitive'Result and Perm(A, B) and Perm(B, C) then Perm (A, C)),
   --    Ghost  => True,
   --    Import => True;


   -- Is this procedure needed? It seems too useful to just delete!
   procedure Swap(Values : in out Array_Type; X : in Positive; Y : in Positive)
     with
       Depends => (Values =>+ (X, Y)),
       Pre =>
         (X in Values'Range and
          Y in Values'Range and
          X /= Y),
       Post =>
         Perm(Values'Old, Values) and
         (Values(X) = Values'Old(Y) and
          Values(Y) = Values'Old(X) and
          (for all J in Values'Range =>
            (if J /= X and J /= Y then Values(J) = Values'Old(J))))
   is
      Values_Old : constant Array_Type := Values
        with Ghost => True;

      Temp : Element_Type;
   begin
      Temp      := Values(X);
      Values(X) := Values(Y);
      Values(Y) := Temp;
      pragma Assume(Perm(Values_Old, Values));
   end Swap;


   procedure Insert_Single(Values : in out Array_Type; Place : in Positive)
     with
       Global => null,
       Depends => (Values => (Values, Place)),
       Pre =>
         Values'Length >= 1   and
         Values'First < Place and
         Place <= Values'Last and
         (for all J in Values'First .. Place - 2 => Values(J) <= Values(J + 1)),
       Post =>
         (for all J in Values'First .. Place - 1 => Values(J) <= Values(J + 1))
   is
      N : Positive;
      Holder : Element_Type;
   begin
      Holder := Values(Place);
      N := Place;
      while N > Values'First and then Holder < Values(N - 1)  loop

         pragma Loop_Invariant(N - 1 >= Values'First and N <= Values'Last);
         Values(N) := Values(N - 1);
         N := N - 1;
      end loop;

      Values(N) := Holder;
   end Insert_Single;


   procedure Insertion_Sort(Values : in out Array_Type) is
   begin
      if Values'Length = 1 then
         return;
      end if;

      for I in Values'First + 1 .. Values'Last loop
         Insert_Single(Values, I);

         pragma Loop_Invariant
           (for all J in Values'First .. I - 1=> Values(J) <= Values(J + 1));
      end loop;
   end Insertion_Sort;


end CubedOS.Lib.Sorters;
