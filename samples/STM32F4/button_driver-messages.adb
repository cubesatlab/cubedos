--------------------------------------------------------------------------------
-- FILE   : SAMPLE_MODULE-messages.adb
-- SUBJECT: Body of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2024 by Vermont State University
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Button; use Button;
with Name_Resolver;
--with LEDs; -- Used for testing

with CubedOS.Lib; use CubedOS.Lib;
with CubedOS.Publish_Subscribe_Server.API; use CubedOS.Publish_Subscribe_Server.API;

package body Button_Driver.Messages is
   use Message_Manager;

   procedure Button_Pressed_Message is
      Outgoing_Message : Message_Record;
      Message_Data : CubedOS.Lib.Octet_Array(1 .. 2);
   begin
       -- Create the message data
       Message_Data := [1, 2];

       -- Craft a publish request message
       Outgoing_Message := Publish_Request_Encode
        (Sender_Address => Name_Resolver.Button_Driver,
         Request_ID    => 0,
         Channel       => 1,
         Message_Data  => Message_Data,
         Priority      => System.Default_Priority);
       Route_Message(Outgoing_Message);

   end Button_Pressed_Message;


   -- As of right now, button releases don't send anything.
   -- Template for if release messages are needed.
   procedure Button_Released_Message is
     -- Outgoing_Message : Message_Record;
     -- Message_Data : CubedOS.Lib.Octet_Array (1 .. 2);

   begin

       -- Create the message data
       --  Message_Data := (1, 2);

       -- Craft a publish request message
       -- Outgoing_Message := Publish_Request_Encode
       -- (Sender_Domain => Domain_ID,
       --  Sender        => Button_Driver.ID,
       --  Request_ID    => 0,
       --  Channel       => 1,
       --  Message_Data  => Message_Data,
       --  Priority      => System.Default_Priority);
       --Route_Message(Outgoing_Message);
       null;
   end Button_Released_Message;



   procedure Process_Release (State_Test : in Button_State_Type)
     with
       Global => null,
       Pre => State_Test = Released
   is
   begin
     -- LEDs.All_On; -- Indicates that the button has been released with LED flash (test)
     -- Implement Button_Released_Message if release message is needed
     Button_Released_Message;
   end Process_Release;


   procedure Process_Press(State_Test : in out Button_State_Type)
     with
       Global => null,
       Pre => State_Test = Pressed
   is
   begin
      if State_Test = Pressed then
         Button_Pressed_Message;
         State_Test := Released;

         -- The button has been released
         Process_Release(State_Test);
     end if;
   end Process_Press;


   task body Message_Loop is
      State_Test : Button_State_Type;
   begin
      -- Nothing to initialize;
      loop
         -- The only "message" that arrives is if the button has been pressed
         State_Test := Button.Button_Pressed;
         Process_Press(State_Test);
     end loop;
   end Message_Loop;

end Button_Driver.Messages;
