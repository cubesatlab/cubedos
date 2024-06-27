package edu.vermontstate.merc

/**
  * Instances of this case class are used to hold expected messages during testing.
  *
  * @param line The line number where the message is expected.
  * @param column The column position where the message is expected.
  * @param message The text of the expected message.
  */
case class ExpectedMessage(line: Int, column: Int, message: String)
