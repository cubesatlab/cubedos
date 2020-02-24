--------------------------------------------------------------------------------
-- FILE   : led_driver-internals.adb
-- SUBJECT: Body of a package that implements the main part of the LED driver.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Registers;     use Registers;
with STM32F4;       use STM32F4;
with STM32F4.GPIO;  use STM32F4.GPIO;

package body LED_Driver.Internals is

   subtype LED_Code_Type is Octet range 0 .. 3;

   -- The order of values in this table must match the order of the LED_Type enumeration in the
   -- API. They correspond to Green (LED4), Orange (LED3), Red (LED5), and Blue (LED6) LEDs.
   Lookup_Table : constant array(LED_Code_Type) of Word :=
     (16#1000#, 16#2000#, 16#4000#, 16#8000#);

   procedure On(LED : LED_Code_Type) is
   begin
      GPIOD.BSRR := Lookup_Table(LED);
   end On;


   procedure Off(LED : LED_Code_Type) is
   begin
      GPIOD.BSRR := Shift_Left(Lookup_Table(LED), 16);
   end Off;


   All_LEDs_On : constant Word := 16#F000#;
   All_LEDs_Off : constant Word := Shift_Left(All_LEDs_On, 16);


   procedure All_Off is
   begin
      GPIOD.BSRR := All_LEDs_Off;
   end All_Off;


   procedure All_On is
   begin
      GPIOD.BSRR := All_LEDs_On;
   end All_On;


   procedure Process_Message(Incoming_Message : in Message_Record) is
   begin
      -- Ignore messages without an op-code.
      if Incoming_Message.Size > 0 then
         case Incoming_Message.Data(1) is
            when On_Message =>
               -- Just ignore malformed messages.
               if Incoming_Message.Size = 2 and then Incoming_Message.Data(2) in LED_Code_Type then
                  On(Incoming_Message.Data(2));
               end if;

            when Off_Message =>
               -- Just ignore malformed messages.
               if Incoming_Message.Size = 2 and then Incoming_Message.Data(2) in LED_Code_Type then
                  Off(Incoming_Message.Data(2));
               end if;

            when All_On_Message =>
               -- Just ignore malformed messages.
               if Incoming_Message.Size = 1 then
                  All_On;
               end if;

            when All_Off_Message =>
               -- Just ignore malformed messages.
               if Incoming_Message.Size = 1 then
                  All_Off;
               end if;

            -- No other op-code values should ever occur. If one does, ignore it.
            when others =>
               null;
         end case;
      end if;
   end Process_Message;


   procedure Initialize is
      RCC_AHB1ENR_GPIOD : constant Word := 16#08#;
      RCC_AHB1ENR : constant Word := RCC.AHB1ENR;  -- Copy volatile value into local variable.
      GPIO_Temp : GPIO_Register;
   begin
      --  Enable clock for GPIO-D
      RCC.AHB1ENR := RCC_AHB1ENR or RCC_AHB1ENR_GPIOD;

      --  Configure PD12-15
      GPIO_Temp := GPIOD;
      GPIO_Temp.MODER   (12 .. 15) := (others => Mode_OUT);
      GPIO_Temp.OTYPER  (12 .. 15) := (others => Type_PP);
      GPIO_Temp.OSPEEDR (12 .. 15) := (others => Speed_100MHz);
      GPIO_Temp.PUPDR   (12 .. 15) := (others => No_Pull);
      GPIOD := GPIO_Temp;
   end Initialize;

end LED_Driver.Internals;

