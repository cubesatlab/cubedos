package Fibonacci is
   type Fib_Seed is range 0 .. 46;
   function Gen_Recursive (N : Fib_Seed) return Natural;
   function Gen_Dynamic (N : Fib_Seed) return Natural;
   function Gen_Slowest (N : Fib_Seed) return Natural;
end Fibonacci;
