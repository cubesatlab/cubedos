--------------------------------------------------------------------------------
-- FILE   : cubedos-lib-sorters.ads
-- SUBJECT: Specification of a package containing utility sorting procedures.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
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

   -- Returns True if A is a permuation of B
   function Perm(A : in Array_Type; B : in Array_Type) return Boolean
     with
       Global => null,
       Ghost  => True;

   -- Sorts the elements in the array Values in ascending order
   procedure Insertion_Sort(Values : in out Array_Type)
     with
       Depends => (Values => Values),
       Pre => Values'Length >= 1,
       Post => (for all J in Values'First .. Values'Last - 1 => Values(J) <= Values(J + 1));

end CubedOS.Lib.Sorters;
