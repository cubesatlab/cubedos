with CubedOS.Lib.Sorters;

package Test_Sorters is

   type Integer_Array is array (Positive range <>) of Integer;

   package Integer_Sorters is new CubedOS.Lib.Sorters
     (Element_Type => Integer,
      Array_Type => Integer_Array);

end Test_Sorters;
