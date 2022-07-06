package body Fibonacci is
   function Gen_Recursive (N : Fib_Seed) return Natural is
   begin
      if N = 0 then
         return 0;
      end if;
      if N = 1 then
         return 1;
      end if;

      return Gen_Recursive (N - 1) + Gen_Recursive (N - 2);
   end Gen_Recursive;

   function Gen_Dynamic (N : Fib_Seed) return Natural is
      Fib : array (0 .. N + 1) of Natural;
   begin
      Fib (0) := 0;
      Fib (1) := 1;

      for I in 2 .. N loop
         Fib (I) := Fib (I - 1) + Fib (I - 2);
      end loop;

      return Fib (N);
   end Gen_Dynamic;

   function Gen_Slowest (N : Fib_Seed) return Natural is
      Fib_Num : Natural;
      Curr_FS : Fib_Seed;
   begin
      for I in 0 .. Natural (N) loop
         Curr_FS := Fib_Seed (I);
         Fib_Num := Gen_Recursive (Curr_FS);
      end loop;

      return Fib_Num;
   end Gen_Slowest;
end Fibonacci;
