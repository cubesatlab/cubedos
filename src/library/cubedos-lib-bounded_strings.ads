--------------------------------------------------------------------------------
-- FILE   : cubedos-lib-bounded_strings.ads
-- SUBJECT: Specification of a package for manipulating general purpose bounded strings.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Ada.Characters.Latin_1;

package CubedOS.Lib.Bounded_Strings is

   Maximum_Bound : constant := 2**16;
   subtype Index_Type is Positive range 1 .. Maximum_Bound;
   subtype Length_Type is Natural range 0 .. Maximum_Bound;

   type Bounded_String(Bound : Index_Type) is private
     with
       Default_Initial_Condition => Is_Empty(Bounded_String);

   -- As with all private types, bounded strings can be assigned and compared for equality and
   -- inequality (with the usual rules for discriminated types). IMPLEMENTORS: See the notes in
   -- the private part for important information regarding these operations.

   -- Returns the number of characters stored in this bounded string.
   function Length(Source : Bounded_String) return Length_Type
     with
       Global => null,
       Inline;

   function Is_Empty(Source: Bounded_String) return Boolean
     with
       Global => null,
       Inline;

   -- A convenient alias.
   function Size(Source : Bounded_String) return Length_Type renames Length;

   -- This function expresses the Bounded_String invariant. It is called explicitly at the
   -- interface of the various operations below. Once type invariants are supported by SPARK
   -- for child units, it should be possible to remove this function and use a type invariant.
   function BSI(BS : Bounded_String) return Boolean is
      (Length(BS) <= BS.Bound);

   -- Returns a bounded string with the given upper bound and set to the initializer.
   function Make(Upper_Bound : Index_Type; Initializer : Bounded_String) return Bounded_String
     with
       Global => null,
       Pre => BSI(Initializer) and Length(Initializer) <= Upper_Bound,
       Post =>
         BSI(Make'Result) and then
         (Make'Result.Bound = Upper_Bound and Length(Make'Result) = Length(Initializer)) and then
         (for all I in 1 .. Length(Make'Result) =>
            Element(Make'Result, I) = Element(Initializer, I));

   -- Returns a bounded string with the given upper bound and set to the initializer string.
   function Make(Upper_Bound : Index_Type; Initializer : String) return Bounded_String
     with
       Global => null,
       Pre => Initializer'Length <= Upper_Bound,
       Post =>
         BSI(Make'Result) and then
         (Make'Result.Bound = Upper_Bound and Length(Make'Result) = Initializer'Length) and then
         (for all I in 1 .. Length(Make'Result) =>
            Element(Make'Result, I) = Initializer(Initializer'First - 1 + I));

   -- Returns a bounded string with the given upper bound and set to the initializer character.
   function Make(Upper_Bound : Index_Type; Initializer : Character) return Bounded_String
     with
       Global => null,
       Post =>
         BSI(Make'Result) and then
         (Make'Result.Bound = Upper_Bound and Length(Make'Result) = 1) and then
           Element(Make'Result, 1) = Initializer;

   -- Returns the character at position 'Index' in the given bounded string.
   function Element(Source : Bounded_String; Index : Index_Type) return Character
     with
       Global => null,
       Pre => BSI(Source) and Index <= Length(Source);

   -- Replaces the character at position 'Index' with 'Item' in the given bounded string.
   procedure Replace_Element
     (Target : in out Bounded_String; Index : in Index_Type; Item : Character)
     with
       Global => null,
       Depends => (Target =>+ (Index, Item)),
       Pre => BSI(Target) and Index <= Length(Target),
       Post =>
         BSI(Target) and
         Length(Target) = Length(Target)'Old and Element(Target, Index) = Item;

   function To_String(Source : Bounded_String) return String
     with
       Global => null,
       Pre => BSI(Source),
       Post =>
         To_String'Result'Length = Length(Source) and then
         (for all I in 1 .. Length(Source) =>
            Element(Source, I) = To_String'Result(To_String'Result'First - 1 + I));

   procedure Append(Target : in out Bounded_String; Item : in Bounded_String)
     with
       Global => null,
       Depends => (Target =>+ Item),
       Pre => BSI(Target) and BSI(Item) and Length(Target) + Length(Item) <= Target.Bound,
       Post =>
         BSI(Target) and
         Length(Target) = Length(Target)'Old + Length(Item) and
         (for all I in 1 .. Length(Target)'Old => Element(Target, I) = Element(Target'Old, I)) and
         (for all I in 1 .. Length(Item) =>
            Element(Target, Length(Target'Old) + I) = Element(Item, I));

   procedure Append(Target : in out Bounded_String; Item : in String)
     with
       Global => null,
       Depends => (Target =>+ Item),
       Pre => BSI(Target) and Item'Length <= Target.Bound - Length(Target),
       Post =>
         BSI(Target) and
         Length(Target) = Length(Target)'Old + Item'Length and
         (for all I in 1 .. Length(Target)'Old => Element(Target, I) = Element(Target'Old, I)) and
         (for all I in 1 .. Item'Length =>
            Element(Target, Length(Target'Old) + I) = Item(Item'First - 1 + I));

   procedure Append(Target : in out Bounded_String; Item : in Character)
     with
       Global => null,
       Depends => (Target =>+ Item),
       Pre => BSI(Target) and Length(Target) + 1 <= Target.Bound,
       Post =>
         BSI(Target) and
         Length(Target) = Length(Target)'Old + 1 and
         (for all I in 1 .. Length(Target)'Old => Element(Target, I) = Element(Target'Old, I)) and
         Element(Target, Length(Target)'Old + 1) = Item;

   -- Erases the contents of the given bounded string.
   procedure Clear(Target : out Bounded_String)
     with
       Global => null,
       Depends => (Target =>+ null),
       Post => BSI(Target) and Length(Target) = 0;

private

   type Bounded_String(Bound : Index_Type) is
      record
         Text : String(1 .. Bound) := (others => Ada.Characters.Latin_1.NUL);
         Length : Length_Type := 0;
      end record;
   -- Type_Invariants are not yet supported in child units.
   -- with Type_Invariant => Bounded_String.Length <= Bounded_String.Bound;


   -- NOTES
   --
   -- To ensure that the default equality and inequality work as intended, all unused characters
   -- in a bounded string should be set to Ada.Characters.Latin_1.Nul. Otherwise two bounded
   -- strings that are logically the same might appear different if they have different "junk"
   -- characters. Thus care is needed in any operation that might reduce the length of a bounded
   -- string.

   function Length(Source : Bounded_String) return Length_Type is
     (Source.Length);

   function Is_Empty(Source : Bounded_String) return Boolean is
     (Source.Length = 0);

   function Element(Source : Bounded_String; Index : Index_Type) return Character is
     (Source.Text(Index));

end CubedOS.Lib.Bounded_Strings;
