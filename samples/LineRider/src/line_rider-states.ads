--------------------------------------------------------------------------------
-- FILE   : line_rider-states.ads
-- SUBJECT: Specification of state machine package.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
with System;

package Line_Rider.States is

   task Controller is
      pragma Storage_Size(4 * 1024);
      pragma Priority(System.Default_Priority);
   end Controller;

end Line_Rider.States;
