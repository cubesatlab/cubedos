--------------------------------------------------------------------------------
-- FILE   : line_rider-states.adb
-- SUBJECT: State machine of the Line Rider application.
-- AUTHOR : (C) Copyright 2024 by Vermont State University
--
--------------------------------------------------------------------------------
with Message_Manager;
with Motor_Driver.API;
with Name_Resolver;
with Sensor_Driver.API;

package body Line_Rider.States is

   package Motor_API renames Motor_Driver.API;
   package Sensor_API renames Sensor_Driver.API;

   task body Controller is
      State_Opt1 : Sensor_API.State_Type := Sensor_API.Off;
      State_Opt2 : Sensor_API.State_Type := Sensor_API.Off;
      use type Sensor_API.State_Type;

      type State_Type is (Idle, Straight, Left, Right, Stop_Or_Turn);
      State : State_Type := Idle;

      Incoming_Message : Message_Manager.Message_Record;
   begin
      -- Execute the state machine in the loop below.
      loop
         Message_Manager.Fetch_Message(Name_Resolver.Line_Rider.Module_ID, Incoming_Message);

         -- The two sensor values below should be extracted from the incoming message.
         -- Sensors.Sense_1(State_Opt1);
         -- Sensors.Sense_2(State_Opt2);

         case State is
            when Idle =>
               null;

            when Straight =>
               Message_Manager.Mailboxes(Motor_Driver.ID).Unchecked_Send
                 (Motor_API.Drive_Straight_Message(Name_Resolver.Line_Rider.Module_ID));

               -- TODO: What if both sensors are off?
               if State_Opt1 = Sensor_API.Off and State_Opt2 = Sensor_API.On then
                  State := Right;
               elsif State_Opt1 = Sensor_API.On and State_Opt2 = Sensor_API.Off then
                  State := Left;
               elsif State_Opt1 = Sensor_API.On and State_Opt2 = Sensor_API.On then
                  State := Stop_Or_Turn;
               end if;

            when Left =>
               Message_Manager.Mailboxes(Motor_Driver.ID).Unchecked_Send
                 (Motor_API.Turn_Left_Message(ID));

               -- TODO: What if one sensor is on and the other is off?
               if State_Opt1 = Sensor_API.Off and State_Opt2 = Sensor_API.Off then
                  State := Straight;
               elsif State_Opt1 = Sensor_API.On and State_Opt1 = Sensor_API.On then
                  State := Stop_Or_Turn;
               end if;

            when Right =>
               Message_Manager.Mailboxes(Motor_Driver.ID).Unchecked_Send
                 (Motor_API.Turn_Right_Message(ID));

               -- TODO: What if one sensor is on and the other is off?
               if State_Opt1 = Sensor_API.Off and State_Opt2 = Sensor_API.Off then
                  State := Straight;
               elsif State_Opt1 = Sensor_API.On and State_Opt2 = Sensor_API.On then
                  State := Stop_Or_Turn;
               end if;

            when Stop_Or_Turn =>
               null;
         end case;
      end loop;
   end Controller;

end Line_Rider.States;
