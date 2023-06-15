--------------------------------------------------------------------------------
-- FILE   : cubedos-tick_server-messages.adb
-- SUBJECT: Body of a package that implements the main part of the time server.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);

with Name_Resolver;
with CubedOS.Time_Server.API;
use CubedOS.Time_Server.API;

package body CubedOS.Time_Server.Messages with
   Refined_State => (Tick_Database => (Series_Database, Send_Tick_Messages), Own_Mailbox => Mailbox)
is

   use Message_Manager;
   use type Ada.Real_Time.Time;
   use type Ada.Real_Time.Time_Span;

   Mailbox : Message_Manager.Module_Mailbox;

   -- Stores all persistent info about a series.
   type Series_Record is record
      -- Doesn't matter what default values are, they should all be overriden.
      -- We only have them because SPARK can't cope with uninitialized things.
      Address  : Message_Address         := (0, 1);
      ID       : Series_ID_Type          := Series_ID_Type'First;
      Kind     : Series_Type             := One_Shot;
      Next     : Ada.Real_Time.Time      := Ada.Real_Time.Time_First;
      Interval : Ada.Real_Time.Time_Span := Ada.Real_Time.Time_Span_First;
      Count    : Series_Count_Type       := 1;
      Is_Used  : Boolean                 := False;
   end record;

   -- Holds information on the active series.
   Series_Maximum : constant := 16;
   subtype Series_Array_Index_Type is Positive range 1 .. Series_Maximum;
   subtype Series_Array_Count_Type is Natural range 0 .. Series_Maximum;
   type Series_Array_Type is array (Series_Array_Index_Type) of Series_Record;

   -- Number of milliseconds to wait for the next poll (see task Send_Tick_Messages)
   Release_Interval : constant Ada.Real_Time.Time_Span :=
     Ada.Real_Time.Milliseconds (5);

   -------------------
   -- Message Handling
   -------------------

   protected Series_Database is

      -- Add a series record to the database.
      procedure Unchecked_Add_Series_Record (Series : in Series_Record) with
         Global => null;

         -- Remove a series record from the database.
      procedure Remove_Series_Record
        (Sender_Address : in Message_Address;
         Series_ID      : in Series_ID_Type) with
         Global => null;

         -- Send tick message(s) to the core logic as required.
      procedure Next_Ticks with
         Global => (Input => Ada.Real_Time.Clock_Time, In_Out => Mailboxes);

   private
      Series_Array : Series_Array_Type;
      Count        : Series_Array_Count_Type := 0;
   end Series_Database;

   -- This must be declared before the body of Series_Database.
   task Send_Tick_Messages;

   protected body Series_Database is

      procedure Unchecked_Add_Series_Record (Series : in Series_Record) is
      begin
         -- If there is insufficient space, there is no effect.
         if Count < Series_Maximum then

            -- If the caller is attempting to add two identical series IDs, there is no effect.
            for I in Series_Array'Range loop
               if Series_Array (I).Is_Used
                 and then Series_Array (I).ID = Series.ID
               then
                  return;
               end if;
            end loop;

            Install_Loop :
            for I in Series_Array'Range loop
               pragma Loop_Invariant (Count < Series_Maximum);

               if not Series_Array (I).Is_Used then
                  Series_Array (I) := Series;
                  exit Install_Loop;
               end if;
            end loop Install_Loop;
            Count := Count + 1;
         end if;
      end Unchecked_Add_Series_Record;

      procedure Remove_Series_Record
        (Sender_Address : in Message_Address; Series_ID : in Series_ID_Type)
      is
      begin
         -- If there is nothing in the database, there is no effect.
         if Count > 0 then

            Search_Loop :
            for I in Series_Array'Range loop
               pragma Loop_Invariant (Count > 0);

               if Series_Array (I).Is_Used
                 and then
                 (Series_Array (I).Address = Sender_Address and
                  Series_Array (I).ID = Series_ID)
               then
                  Series_Array (I).Is_Used := False;
                  Count                    := Count - 1;
                  exit Search_Loop;
               end if;
            end loop Search_Loop;
         end if;
      end Remove_Series_Record;

      procedure Next_Ticks is
         Current_Time : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
      begin
         -- Iterate through the array to see who needs a tick message.
         for I in Series_Array'Range loop
            declare
               Current_Series : Series_Record renames Series_Array (I);
            begin
               -- If we need to send a tick from this series...
               if Current_Series.Is_Used
                 and then Current_Series.Next <= Current_Time
               then
                  Message_Manager.Send_Message
                    (Mailbox, Tick_Reply_Encode
                       (Receiver_Address => Current_Series.Address,
                        Request_ID       => 0, Series_ID => Current_Series.ID,
                        Count            => Current_Series.Count));

                  -- Update the current record.
                  case Current_Series.Kind is
                     when One_Shot =>
                        -- TODO: Should we reinitialize the rest of the series record?
                        Current_Series.Is_Used := False;

                     when Periodic =>
                        -- Advance the counter. If we exhaust the counter's range, stop.
                        if Current_Series.Count = Series_Count_Type'Last then
                           -- TODO: Should we reinitialize the rest of the series record?
                           Current_Series.Is_Used := False;
                        else
                           Current_Series.Count := Current_Series.Count + 1;
                           Current_Series.Next  :=
                             Current_Series.Next + Current_Series.Interval;
                        end if;
                  end case;
               end if;
            end;
         end loop;
      end Next_Ticks;

   end Series_Database;

   task body Send_Tick_Messages is
      Next_Release : Ada.Real_Time.Time := Ada.Real_Time.Clock;
   begin
      loop
         delay until Next_Release;
         Next_Release := Next_Release + Release_Interval;
         Series_Database.Next_Ticks;
      end loop;
   end Send_Tick_Messages;

   procedure Initialize is
   begin
      Message_Manager.Register_Module(Name_Resolver.Time_Server.Module_ID, 8, Mailbox, Empty_Type_Array);
   end Initialize;

   -----------------------------------
   -- Message Decoding and Dispatching
   -----------------------------------

   procedure Process_Relative_Request
     (Incoming_Message : in Message_Record) with
      Pre => API.Is_Relative_Request (Incoming_Message)
   is
      Tick_Interval : Ada.Real_Time.Time_Span;
      Request_Type  : Series_Type;
      Series_ID     : Series_ID_Type;
      Status        : Message_Status_Type;
      Series        : Series_Record;
      Current_Time  : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
   begin
      API.Relative_Request_Decode
        (Incoming_Message, Tick_Interval, Request_Type, Series_ID, Status);
      if Status = Success then
         Series :=
           (Address => Sender_Address(Incoming_Message), ID => Series_ID,
            Kind    => Request_Type, Interval => Tick_Interval,
            Next => Current_Time + Tick_Interval, Count => 1, Is_Used => True);
         Series_Database.Unchecked_Add_Series_Record (Series);

      end if;
   end Process_Relative_Request;

   procedure Process_Absolute_Request
     (Incoming_Message : in Message_Record) with
      Pre => API.Is_Absolute_Request (Incoming_Message)
   is
      Tick_Time : Ada.Real_Time.Time;
      Series_ID : API.Series_ID_Type;
      Status    : Message_Status_Type;
      Series    : Series_Record;
   begin
      API.Absolute_Request_Decode
        (Incoming_Message, Tick_Time, Series_ID, Status);
      if Status = Success then

         Series :=
           (Address => Sender_Address(Incoming_Message), ID => Series_ID,
            Kind    => One_Shot, Interval => Ada.Real_Time.Time_Span_Zero,
            Next    => Tick_Time, Count => 1, Is_Used => True);
         Series_Database.Unchecked_Add_Series_Record (Series);

      end if;
   end Process_Absolute_Request;

   procedure Process_Cancel_Request (Incoming_Message : in Message_Record) with
      Pre => API.Is_Cancel_Request (Incoming_Message)
   is
      Series_ID : API.Series_ID_Type;
      Status    : Message_Status_Type;
   begin
      API.Cancel_Request_Decode (Incoming_Message, Series_ID, Status);

      if Status = Success then
         Series_Database.Remove_Series_Record
           (Sender_Address(Incoming_Message), Series_ID);
      end if;
   end Process_Cancel_Request;

   procedure Process (Incoming_Message : in Message_Record) with
      Global => (Input => Ada.Real_Time.Clock_Time, In_Out => Series_Database),
      Depends =>
      (Series_Database =>+ (Ada.Real_Time.Clock_Time, Incoming_Message))
   is
   begin
      if API.Is_Relative_Request (Incoming_Message) then
         Process_Relative_Request (Incoming_Message);
      elsif API.Is_Absolute_Request (Incoming_Message) then
         Process_Absolute_Request (Incoming_Message);
      elsif API.Is_Cancel_Request (Incoming_Message) then
         Process_Cancel_Request (Incoming_Message);
      else
         --$ TODO: What should be done about malformed/unrecognized messages?
         null;
      end if;
   end Process;

   ---------------
   -- Message Loop
   ---------------

   task body Message_Loop with
      Refined_Global => (Input => Ada.Real_Time.Clock_Time,
       In_Out => (Series_Database, Mailbox))
   is
      Incoming_Message : Message_Manager.Message_Record;
   begin
      Initialize;

      loop
         Read_Next(Mailbox, Incoming_Message);

         -- This module should never receive a message from itself.
         -- We check that here because technically any module can
         -- send a message to and from anywhere.
         if Sender_Address(Incoming_Message) /= Name_Resolver.Time_Server then
            Process (Incoming_Message);
         end if;
      end loop;
   end Message_Loop;

end CubedOS.Time_Server.Messages;
