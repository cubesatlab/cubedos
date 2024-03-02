--------------------------------------------------------------------------------
-- FILE   : cubedos-lib-bounded_queues.adb
-- SUBJECT: Implementation of a package serving as a parent to the CubedOS runtime library.
-- AUTHOR : (C) Copyright 2024 by Vermont State University
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

   function Make (Capacity : in Positive) return Bounded_Queue is
      (Capacity - 1, (others => null), 0, 0, 0);

   function Count(Q: in Bounded_Queue) return Count_Type is
     (Q.Num_Items)
     with Refined_Post => Count'Result = Q.Num_Items;

   function Size (Q: in Bounded_Queue) return Count_Type is
     (Q.Max_Index + 1);

   function Is_Empty (Q: in Bounded_Queue) return Boolean
     is (Q.Num_Items = 0);

   function Is_Full (Q: in Bounded_Queue) return Boolean
     is (Q.Num_Items - 1 = Q.Max_Index);

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

end CubedOS.Lib.Bounded_Queues;
