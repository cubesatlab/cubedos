--------------------------------------------------------------------------------
-- FILE   : cubedos-lib-bounded_queues.adb
-- SUBJECT: Implementation of a package serving as a parent to the CubedOS runtime library.
-- AUTHOR : (C) Copyright 2023 by Vermont Technical College
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

package body CubedOS.Lib.Bounded_Queues is

   procedure Move(Src : in out Data_Owner; Dest : in out Data_Owner)
     with Pre => Src /= null and Dest = null,
     Post => Src = null and Dest /= null
   is
   begin
      Dest := Src;
      Src := null;
   end Move;

   --  type Bounded_Queue (Size : Count_Type) is
   --     record
   --        Storage : Data_Array(0 .. Size-1);
   --        Next_In : Index := 0;
   --        Next_Out : Index := 0;
   --        Num_Items : Count_Type := 0;
   --     end record;

   function Make (Capacity : Positive) return Bounded_Queue is
      (Capacity - 1, (others => null), 0, 0, 0);

   function Count(Q: in Bounded_Queue) return Count_Type is
     (Q.Num_Items)
     with Refined_Post => Count'Result = Q.Num_Items;

   function Size (Q: in Bounded_Queue) return Count_Type is
      (Q.Max_Index + 1);

   -- Takes the next item off the queue.
   procedure Next(Q: in out Bounded_Queue; Item : in out Data_Owner) is
   begin
      Move(Q.Storage(Q.Next_Out), Item);
      Q.Next_Out := (Q.Next_Out + 1) mod (Q.Max_Index + 1);
      Q.Num_Items := Q.Num_Items - 1;
   end Next;

   -- Adds an item to the queue.
   procedure Put(Q: in out Bounded_Queue; Item : in out Data_Owner) is
   begin
      Move(Item, Q.Storage(Q.Next_In));
      Q.Next_In := (Q.Next_In + 1) mod (Q.Max_Index + 1);
      Q.Num_Items := Q.Num_Items + 1;
   end Put;

   function Valid(Q : Bounded_Queue) return Boolean
   is (
       Q.Next_In in Q.Storage'Range
       and then Q.Next_Out in Q.Storage'Range
       and then (Q.Storage(Q.Next_In) = null or Q.Num_Items - 1 = Q.Max_Index)
       and then (Q.Storage(Q.Next_Out) /= null or Q.Num_Items = 0)
       and then Q.Max_Index < Natural'Last - 1
      );

end CubedOS.Lib.Bounded_Queues;
