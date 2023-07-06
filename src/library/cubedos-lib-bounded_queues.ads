--------------------------------------------------------------------------------
-- FILE   : cubedos-lib-bounded_queues.ads
-- SUBJECT: SPARK verified bounded queue implementation.
-- AUTHOR : (C) Copyright 2023 by Vermont Technical College
--
-- First in first out queue of a fixed size implemented in SPARK.
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

generic
   type Data is private;
   type Data_Owner is access Data;
package CubedOS.Lib.Bounded_Queues is

   subtype Count_Type is Natural;

   type Bounded_Queue (Max_Index : Natural) is private;

   function Valid(Q : Bounded_Queue) return Boolean
     with Ghost;

   -- Creates a new queue.
   function Make (Capacity : Positive) return Bounded_Queue
     with Pre => Capacity < Natural'Last - 1,
     Post => Valid(Make'Result);

   -- Return the number of items currently in the queue.
   function Count (Q: in Bounded_Queue) return Count_Type
     with Pre => Valid(Q);

   -- Return the total capacity of the queue
   function Size (Q: in Bounded_Queue) return Count_Type
     with Pre => Valid(Q);

   function Is_Empty (Q: in Bounded_Queue) return Boolean
     with Pre => Valid(Q),
       Post => Is_Empty'Result = (Count(Q) = 0);

   function Is_Full (Q: in Bounded_Queue) return Boolean
     with Pre => Valid(Q),
       Post => Is_Full'Result = (Count(Q) = Size(Q));

   -- Takes the next item off the queue.
   procedure Next(Q: in out Bounded_Queue; Item : in out Data_Owner)
     -- Mention Valid the first time so that Count can pass, the second time
     -- to let spark now that Count(Q) > 0.
     with Pre =>
       Valid(Q)
       and then not Is_Empty(Q)
       and then Valid(Q)
       and then Item = null,
       Post => Item /= null and Valid(Q);

   -- Adds an item to the queue.
   procedure Put(Q: in out Bounded_Queue; Item : in out Data_Owner)
     with Pre => Valid(Q)
     and then not Is_Full(Q)
     and then Item /= null,
     Post => Item = null and Valid(Q);

private
   subtype Index is Natural;
   type Data_Array is array (Natural range <>) of Data_Owner;

   type Bounded_Queue (Max_Index : Natural) is
      record
         Storage : Data_Array(0 .. Max_Index);
         Next_In : Index := 1;
         Next_Out : Index := 1;
         Num_Items : Count_Type := 0;
      end record;
   -- Not using a type invariant because SPARK doesn't support them in this situation

end CubedOS.Lib.Bounded_Queues;
