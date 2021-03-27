--------------------------------------------------------------------------------
-- FILE   : cubedos-lib-sorters.ads
-- SUBJECT: Specification of a package containing utility sorting procedures.
-- AUTHOR : (C) Copyright 2018 by Vermont Technical College
--
--------------------------------------------------------------------------------
generic
   type Element_Type is private;
   type Array_Type is array(Positive range <>) of Element_Type;
   with function "<"(Left, Right : Element_Type) return Boolean is <>;
package CubedOS.Lib.Sorters is

   -- Convenience function.
   function "<="(Left, Right : Element_Type) return Boolean is
      (Left = Right or Left < Right);

   -- Sorts the elements in the array Values in ascending order
   procedure Insertion_Sort(Values : in out Array_Type)
     with
       Global => null,
       Depends => (Values => Values),
       Pre => Values'Length >= 1,
       Post => (for all J in Values'First .. Values'Last - 1 => Values(J) <= Values(J + 1));

   -- Organizes the array from positions Values'First .. Last into a heap.
   procedure Make_Heap(Values : in out Array_Type; Last : in Positive)
     with
       Global => null,
       Depends => (Values => (Values, Last)),
       Pre => Values'Length >= 1 and Last in Values'Range,
       Post => (for all J in Last + 1 .. Values'Last => Values(J) = Values'Old(J));


   -- Given a heap from positions Values'First .. Last - 1, add the item at position Last to the heap.
   -- TODO: We could add a precondition that asserts the validity of the existing heap.
   procedure Push_Heap(Values : in out Array_Type; Last : in Positive)
     with
       Global => null,
       Depends => (Values => (Values, Last)),
       Pre => Values'Length >= 1 and Last in Values'Range,
     Post => (for all J in Last + 1 .. Values'Last => Values(J) = Values'Old(J));

   -- Given a heap from positions Values'First .. Last, sort it in ascending order.
   -- TODO: We could add a precondition that asserts the validity of the existing heap.
   procedure Sort_Heap(Values : in out Array_Type; Last : in Positive)
     with
       Global => null,
       Depends => (Values => (Values, Last)),
       Pre => Values'Length >= 1 and Last in Values'Range,
       Post =>
         (for all J in Values'First .. Last - 1 => Values(J) <= Values(J + 1)) and
         (for all J in Last + 1 .. Values'Last => Values(J) = Values'Old(J));

end CubedOS.Lib.Sorters;
