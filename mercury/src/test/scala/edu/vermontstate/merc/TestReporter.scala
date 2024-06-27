package edu.vermontstate.merc

class TestReporter(
  expectedErrors  : Array[ExpectedMessage],
  expectedWarnings: Array[ExpectedMessage]) extends Reporter {

  var nextError = 0
  var nextWarning = 0

  def reportError(line: Int, column: Int, message: String): Unit = {
    assert(
      nextError < expectedErrors.length,
      s"Unexpected error generated. Line: $line, Column: $column, Message: $message")

    assert(
      ExpectedMessage(line, column, message) == expectedErrors(nextError),
      s"Got: ${ExpectedMessage(line, column, message)}, Expected: ${expectedErrors(nextError)}")
    nextError += 1
  }


  def reportWarning(line: Int, column: Int, message: String): Unit = {
    assert(
      nextWarning < expectedWarnings.length,
      s"Unexpected warning generated. Line: $line, Column: $column, Message: $message")

    assert(
      ExpectedMessage(line, column, message) == expectedWarnings(nextWarning),
      s"Got: ${ExpectedMessage(line, column, message)}, Expected: ${expectedWarnings(nextWarning)}")
    nextWarning += 1
  }

}
