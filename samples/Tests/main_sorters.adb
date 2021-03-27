pragma SPARK_Mode(On);

with Ada.Text_IO;
with Ada.Integer_Text_IO;
with CubedOS.Lib.Sorters;
use Ada.Text_Io;
use Ada.Integer_Text_Io;

procedure Main_Sorters is


   type Integer_Array is array(Positive range <>) of Integer;
   package Integer_Sorters is new CubedOS.Lib.Sorters
     (Element_Type => Integer, Array_Type => Integer_Array);

   -- Create a test array.
   Test_Array1 : Integer_Array(1 .. 10) := (9, 12, 7, 3, 5, 4, 3, 0, 1, 0);
   Test_Array2 : Integer_Array(1 .. 10) := (7, 2, 3, 5, 6, 7, 8, 12, 13, 15);
begin
   -- Sort the test array.
   Integer_Sorters.Insertion_Sort(Test_Array1);
   Integer_Sorters.Insertion_Sort(Test_Array2);

   -- Print the results.
   for I in Test_Array1'Range loop
      Put("Test_Array1("); Put(I); Put(") = "); Put(Test_Array1(I)); New_Line;
   end loop;
   New_Line;
   for I in Test_Array2'Range loop
      Put("Test_Array2("); Put(I); Put(") = "); Put(Test_Array2(I)); New_Line;
   end loop;

end Main_Sorters;
