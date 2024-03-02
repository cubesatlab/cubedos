--------------------------------------------------------------------------------
-- FILE   : cubedos-lib-bounded_queues.ads
-- SUBJECT: SPARK verified bounded queue implementation.
-- AUTHOR : (C) Copyright 2024 by Vermont State University
--
-- First in first out queue of a fixed size implemented in SPARK.
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

generic
   type Data is private;
   type Data_Owner is access Data;
package CubedOS.Lib.Bounded_Queues is

   subtype Count_Type is Natural;

   type Bounded_Queue(Max_Index : Natural) is private;

   -- Creates a new queue.
   function Make(Capacity : in Positive) return Bounded_Queue
     with Pre => Capacity < Natural'Last - 1;

   -- Return the number of items currently in the queue.
   function Count(Q: in Bounded_Queue) return Count_Type;

   -- Return the total capacity of the queue
   function Size(Q: in Bounded_Queue) return Count_Type;

   function Is_Empty(Q: in Bounded_Queue) return Boolean
     with Post => Is_Empty'Result = (Count(Q) = 0);

   function Is_Full(Q: in Bounded_Queue) return Boolean
     with Post => Is_Full'Result = (Count(Q) = Size(Q));

   -- Takes the next item off the queue.
   procedure Next(Q: in out Bounded_Queue; Item : in out Data_Owner)
     -- Mention Valid the first time so that Count can pass, the second time
     -- to let spark now that Count(Q) > 0.
     with
       Pre => not Is_Empty(Q) and Item = null,
       Post => Item /= null;

   -- Adds an item to the queue.
   procedure Put(Q: in out Bounded_Queue; Item : in out Data_Owner)
     with
       Pre => not Is_Full(Q) and Item /= null,
       Post => Item = null;

private
   subtype Index is Natural;
   type Data_Array is array (Natural range <>) of Data_Owner;
   function Valid(Q : Bounded_Queue) return Boolean
     with Ghost;

   type Bounded_Queue(Max_Index : Natural) is
      record
         Storage : Data_Array(0 .. Max_Index) := (others => null);
         Next_In : Index := 0;
         Next_Out : Index := 0;
         Num_Items : Count_Type := 0;
      end record
     with Type_Invariant => Valid(Bounded_Queue);

   function Valid(Q : Bounded_Queue) return Boolean
    is (
        Q.Next_In in Q.Storage'Range
        and then Q.Next_Out in Q.Storage'Range
        and then (Q.Storage(Q.Next_In) = null or Q.Num_Items - 1 = Q.Max_Index)
        and then (Q.Storage(Q.Next_Out) /= null or Q.Num_Items = 0)
        and then Q.Max_Index < Natural'Last - 1
       );

end CubedOS.Lib.Bounded_Queues;
