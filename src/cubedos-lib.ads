--------------------------------------------------------------------------------
-- FILE   : cubedos-lib.ads
-- SUBJECT: Specification of a package serving as a parent to the CubedOS runtime library.
-- AUTHOR : (C) Copyright 2016 by Vermont Technical College
--
-- This package also defines a number of useful, library-wide data types.
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

package CubedOS.Lib is
   pragma Pure;

   type Octet is mod 2**8;
   type Double_Octet is mod 2**16;
   type Quadruple_Octet is mod 2**32;
   type Octuple_Octet is mod 2**64;

   -- Limiting the maximum size of arrays simplfies proving freedom from overflow in array index
   -- computations. There is no other particular reason for this limit; it is arbitrary. The
   -- limit should be high enough to satisfy any reasonable application.
   --
   Maximum_Array_Size : constant := 2**16;

   -- Starting the index type at 0 is consistent with low level expectations.
   subtype Octet_Array_Index is Natural range 0 .. Maximum_Array_Size - 1;
   subtype Octet_Array_Count is Natural range 0 .. Maximum_Array_Size;
   type Octet_Array is array (Octet_Array_Index range <>) of Octet
     with Component_Size => 8;


   -- Declare intrinsic shift functions for these types.

   function Shift_Left(Value : Octet; Count : Natural) return Octet
     with
       Import,
       Convention => Intrinsic,
       Global     => null;

   function Shift_Right(Value : Octet; Count : Natural) return Octet
     with
       Import,
       Convention => Intrinsic,
       Global     => null;

   function Shift_Left(Value : Double_Octet; Count : Natural) return Double_Octet
     with
       Import,
       Convention => Intrinsic,
       Global     => null;

   function Shift_Right(Value : Double_Octet; Count : Natural) return Double_Octet
     with
       Import,
       Convention => Intrinsic,
       Global     => null;

   function Shift_Left(Value : Quadruple_Octet; Count : Natural) return Quadruple_Octet
     with
       Import,
       Convention => Intrinsic,
       Global     => null;

   function Shift_Right(Value : Quadruple_Octet; Count : Natural) return Quadruple_Octet
     with
       Import,
       Convention => Intrinsic,
       Global     => null;

end CubedOS.Lib;
