with Ada.Numerics.Discrete_Random;

package body PNumbers is
   package Random_Integer is new Ada.Numerics.Discrete_Random (Random_Range);
   function Get_Random_Number return Positive is
      Number_Generator : Random_Integer.Generator;
      Value            : Random_Range;
   begin
      Random_Integer.Reset (Number_Generator);
      Value := Random_Integer.Random (Number_Generator);
      return Positive (Value);
   end Get_Random_Number;
end PNumbers;
