--------------------------------------------------------------------------------
-- FILE   : cubedos-lib-bounded_strings-check.adb
-- SUBJECT: Package containing unit tests of bounded_strings
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
with Ada.Assertions;
with Ada.Characters.Latin_1;
with Ada.Text_IO;

use Ada.Assertions;
use Ada.Text_IO;

package body CubedOS.Lib.Bounded_Strings.Check is
   Nul : Character renames Ada.Characters.Latin_1.Nul;

   procedure Test_Make_1 is
      B1 : constant Bounded_String := Make(1, "");          -- Minimum size, empty intializer.
      B2 : constant Bounded_String := Make(1, "1");         -- Minimum size, fully initialized.
      B3 : constant Bounded_String := Make(8, "");          -- Typical case, empty initializer.
      B4 : constant Bounded_String := Make(8, "1234");      -- Typical case, partially initialized.
      B5 : constant Bounded_String := Make(8, "12345678");  -- Typical case, fully initialized.
   begin
      -- Don't need to check lengths. They are covered by the postcondition.

      Assert(B1.Text(1) = Nul, "Invalid content");
      Assert(B2.Text(1) = '1', "Invalid content");
      Assert(B3.Text = (Nul, Nul, Nul, Nul, Nul, Nul, Nul, Nul), "Invalid content");
      Assert(B4.Text = ('1', '2', '3', '4', Nul, Nul, Nul, Nul), "Invalid content");
      Assert(B5.Text = ('1', '2', '3', '4', '5', '6', '7', '8'), "Invalid content");
   end Test_Make_1;


   procedure Test_Make_2 is
      B1 : constant Bounded_String := Make(3, "123");
      B2 : constant Bounded_String := Make(8, B1);
      B3 : constant Bounded_String := Make(8, B2);
   begin
      Assert(B1.Text = ('1', '2', '3'), "Invalid content");
      Assert(B2.Text = ('1', '2', '3', Nul, Nul, Nul, Nul, Nul), "Invalid content");
      Assert(B3.Text = ('1', '2', '3', Nul, Nul, Nul, Nul, Nul), "Invalid content");
   end Test_Make_2;


   procedure Test_Make_3 is
      C1 : constant Character := '1';
      B1 : constant Bounded_String := Make(1, C1);           -- Make taking character, minimum bound.
      B2 : constant Bounded_String := Make(8, '1');          -- Make taking character normal bound.
   begin
      Assert(B1.Text(1) = '1', "Invalid content");
      Assert(B2.Text = ('1', Nul, Nul, Nul, Nul, Nul, Nul, Nul), "Invalid content");
   end Test_Make_3;


   procedure Test_Element is
      B1 : constant Bounded_String := Make(1, "1");
      B2 : constant Bounded_String := Make(3, "123");
      C1 : constant Character := Element(B1, 1);       -- Test for Index = Size.
      C2 : constant Character := Element(B2, 3);       -- Test for Index < Size.
   begin
      -- No need to check for Index > Size. This is covered by the precondition.
      Assert(C1 = '1', "Invalid content");
      Assert(C2 = '3', "Invalid content");
   end Test_Element;


   procedure Test_Replace_Element is
      B1 : Bounded_String := Make(1, "1");
      B2 : Bounded_String := Make(3, "123");
   begin
      Replace_Element(B1, 1, '2');
      Replace_Element(B2, 3, '4');
      Assert(B1.Text(1) = '2', "Invalid content");
      Assert(B2.Text = ('1', '2', '4'), "Invalid content");
   end Test_Replace_Element;


   procedure Test_To_String is
      B1 : constant Bounded_String := Make(1, "1");
      B2 : constant Bounded_String := Make(5, "1234");
      S1 : constant String := To_String(B1);
      S2 : constant String := To_String(B2);
   begin
      Assert(S1 = "1", "Invalid string");
      Assert(S2 = "1234", "Invalid string");
   end Test_To_String;


   procedure Test_Append_1 is
      B1 : Bounded_String := Make(4,"1");
      B2 : constant Bounded_String := Make(3, "234");
   begin
      Append(B1, B2);
      Assert(B1.Text = ('1', '2', '3', '4'), "Invalid content");
   end Test_Append_1;


   procedure Test_Append_2 is
      B1 : Bounded_String := Make(4, "1");
      S1 : constant String := "234";
      B2 : Bounded_String := Make(8, "");
      S2 : constant String := "123";
   begin
      Append(B1, S1);
      Assert(B1.Text = ('1', '2', '3', '4'), "Invalid content");
      Append(B2, S2);
      Assert(B2.Text = ('1', '2', '3', Nul, Nul, Nul, Nul, Nul), "Invalid content");
   end Test_Append_2;


   procedure Test_Append_3 is
      B1 : Bounded_String := Make(4, "123");
      C1 : constant Character := '4';
      B2 : Bounded_String := Make(8, "");
      C2 : constant Character := '1';
   begin
      Append(B1, C1);
      Assert(B1.Text = ('1', '2', '3', '4'), "Invalid content");
      Append(B2, C2);
      Assert(B2.Text = ('1', Nul, Nul, Nul, Nul, Nul, Nul, Nul), "Invalid content");
   end Test_Append_3;


   procedure Test_Clear is
      B1 : Bounded_String := Make(1, "");
      B2 : Bounded_String := Make(8, "12345678");
   begin
      Clear(B1);
      Clear(B2);
      Assert(B1.Text(1) = Nul, "Invalid content");
      Assert(B2.Text = (Nul, Nul, Nul, Nul, Nul, Nul, Nul, Nul), "Invalid content");
   end Test_Clear;


   procedure Run_Tests is
   begin
      Put("Bounded_Strings: Make 1");   Test_Make_1;   Put_Line(" (Ok)");
      Put("Bounded_Strings: Make 2");   Test_Make_2;   Put_Line(" (Ok)");
      Put("Bounded_Strings: Make 3");   Test_Make_3;   Put_LIne(" (Ok)");
      Put("Bounded_Strings: Element");  Test_ELement;  Put_Line(" (Ok)");
      Put("Bounded_Strings: Replace_Element"); Test_Replace_Element; Put_Line(" (Ok)");
      Put("Bounded_Strings: To_String"); Test_To_String; Put_Line(" (Ok)");
      Put("Bounded_Strings: Append 1"); Test_Append_1; Put_Line(" (Ok)");
      Put("Bounded_Strings: Append 2"); Test_Append_2; Put_Line(" (Ok)");
      Put("Bounded_Strings: Append 3"); Test_Append_3; Put_Line(" (Ok)");
      Put("Bounded_Strings: Clear");    Test_Clear;    Put_Line(" (Ok)");
   end Run_Tests;


end CubedOS.Lib.Bounded_Strings.Check;
