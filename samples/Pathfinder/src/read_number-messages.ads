--------------------------------------------------------------------------------
--
--  FILE   : Read_Number-messages.ads
--  SUBJECT: Specification of a package that implements the main part
--  of the module.
--  AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);
with Message_Manager; use Message_Manager;
with Read_Number.API;

package Read_Number.Messages is

   --  Every module contains a message loop that receives messages
   --  from the module's mailbox and processes them.  That loop is
   --  declared here with its attributes.  Be mindful of priorities.
   --  We recommend giving every module a unique priority; the correct
   --  assignments need to be considered carefully to meet timing
   --  requirements.  Similar comments apply to the stack size
   --  setting.  We recommend analyzing the code in the module to find
   --  the most appropriate stack size.
   --
   --  This package is a public child because users of the module need
   --  to 'with' this package to ensure it is part of the program
   --  (otherwise messages sent to a module will never be processed!
   --  Module users also need to 'with' the API package.  However, if
   --  the API package tries to bring in this package circular
   --  elaboration loops result.  We recommend that the compilation
   --  unit containing the main procedure 'with' the message loop
   --  packages of all modules the program is trying to use.  That
   --  also serves as documentation about the modules required by the
   --  program.  For example in the main compilation unit include
   --  something like:
   --
   --  with CubedOS.Sample_Module.Messages;
   --  pragma Unreferenced(CubedOS.Sample_Module.Messages);
   task type Message_Loop with
      Priority => Pri,
      CPU      => CPU_Num
   is
      pragma Storage_Size (4 * 1_024);
   end Message_Loop;

   --  This justifcation is needed to silence a SPARK error related
   --  tasking.  Each CubedOS module reads from exactly one mailbox.
   --  However, since the mailboxes are stored in an array, SPARK
   --  can't tell for sure which mailbox is being read by which
   --  task. SPARK therefor conservatively assumes two tasks might be
   --  reading from the same mailbox, which is a violation of SPARK
   --  rules.  This justification should be satisfied as long as every
   --  module has a unique module ID (since module IDs are used as
   --  mailbox identifiers).
   pragma Annotate
     (GNATprove, Intentional,
      "multiple tasks might suspend on protected object",
      "Every module has a unique ID");

private
   procedure Initialize;
   procedure Process (Message : in Message_Record);
   procedure Handle_Read_Number_Reply (Message : in Message_Record) with
      Pre => Read_Number.API.Is_Read_Number_Reply (Message);

end Read_Number.Messages;
