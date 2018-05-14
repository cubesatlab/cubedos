--------------------------------------------------------------------------------
-- FILE   : cubedos-lib-bounded_strings.adb
-- SUBJECT: Body of a package for manipulating general purpose bounded strings.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

package body CubedOS.Lib.Bounded_Strings is

   function Make(Upper_Bound : Index_Type; Initializer : Bounded_String) return Bounded_String is
      Result : Bounded_String(Upper_Bound);
   begin
      Result.Text := (others => Ada.Characters.Latin_1.Nul);
      Result.Text(1 .. Initializer.Length) := Initializer.Text(1 .. Initializer.Length);
      Result.Length := Initializer.Length;
      return Result;
   end Make;


   function Make(Upper_Bound : Index_Type; Initializer : String) return Bounded_String is
      Result : Bounded_String(Upper_Bound);
   begin
      Result.Text := (others => Ada.Characters.Latin_1.Nul);
      Result.Text(1 .. Initializer'Length) := Initializer;
      Result.Length := Initializer'Length;
      return Result;
   end Make;


   function Make(Upper_Bound : Index_Type; Initializer : Character) return Bounded_String is
      Result : Bounded_String(Upper_Bound);
   begin
      Result.Text := (others => Ada.Characters.Latin_1.Nul);
      Result.Text(1) := Initializer;
      Result.Length := 1;
      return Result;
   end Make;


   procedure Replace_Element
     (Target : in out Bounded_String; Index : in Index_Type; Item : Character) is
   begin
      Target.Text(Index) := Item;
   end Replace_Element;


   function To_String(Source : Bounded_String) return String is
      Result : constant String(1 .. Source.Length) := Source.Text(1 .. Source.Length);
   begin
      return Result;
   end To_String;


   procedure Append(Target : in out Bounded_String; Item : in Bounded_String) is
   begin
      Target.Text(Target.Length + 1 .. (Target.Length + Item.Length)) :=
        Item.Text(1 .. Item.Length);
      Target.Length := Target.Length + Item.Length;
   end Append;


   procedure Append(Target : in out Bounded_String; Item : in String) is
   begin
      Target.Text(Target.Length + 1 .. (Target.Length + Item'Length)) :=
        Item(Item'First .. Item'Last);
      Target.Length := Target.Length + Item'Length;
   end Append;


   procedure Append(Target : in out Bounded_String; Item : in Character) is
   begin
      Target.Text(Target.Length + 1) := Item;
      Target.Length := Target.Length + 1;
   end Append;


   procedure Clear(Target : out Bounded_String) is
   begin
      Target.Text := (others => Ada.Characters.Latin_1.Nul);
      Target.Length := 0;
   end Clear;

end CubedOS.Lib.Bounded_Strings;
