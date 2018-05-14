--------------------------------------------------------------------------------
-- FILE   : spi-messages.ads
-- SUBJECT: Specification of the message handler package for the SPI driver module.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
with System;

package SPI.Messages is
   
   task Message_Loop is
      pragma Storage_Size(4 * 1024);
      pragma Priority(System.Default_Priority);
   end Message_Loop;

end SPI.Messages;
