--------------------------------------------------------------------------------
-- FILE   : cubedos-lib-sorters.adb
-- SUBJECT: Body of a package containing utility sorting procedures.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------

package body CubedOS.Lib.Sorters is

   -- Is this procedure needed? It seems too useful to just delete!
   procedure Swap(Values : in out Array_Type; X : in Positive; Y : in Positive)
     with
       Depends => (Values =>+ (X, Y)),
       Pre =>
         (X in Values'Range and
          Y in Values'Range and
          X /= Y),
       Post =>
         Values(X) = Values'Old(Y) and
         Values(Y) = Values'Old(X) and
         (for all J in Values'Range =>
            (if J /= X and J /= Y then Values(J) = Values'Old(J)))
   is
      Temp : Element_Type;
   begin
      Temp      := Values(X);
      Values(X) := Values(Y);
      Values(Y) := Temp;
   end Swap;
   pragma Unreferenced(Swap);


   procedure Insert_Single(Values : in out Array_Type; Place : in Positive)
     with
       Global => null,
       Depends => (Values => (Values, Place)),
       Pre =>
         (Values'Length >= 1   and
          Values'First < Place and
          Place <= Values'Last) and then
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


   procedure Push_Heap(Values : in out Array_Type; Last : in Positive) is
   begin
      null;
   end Push_Heap;


   procedure Pop_Heap(Values : in out Array_Type; Last : in Positive; Result : out Element_Type) is
   begin
      null;
   end Pop_Heap;

end CubedOS.Lib.Sorters;
