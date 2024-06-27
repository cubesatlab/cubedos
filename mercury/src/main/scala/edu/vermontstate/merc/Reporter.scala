package edu.vermontstate.merc

/**
 * This trait describes the interface to error reporting objects. Different implementations
 * report errors in different ways. By centralizing the way errors are reported, changes can be
 * made to this important function without modifying the bulk of the compiler where errors are
 * detected.
 */
trait Reporter {

  /**
   * Used to report an error condition. If an error is detected code can not be generated.
   *
   * @param line The line in the source file where the error was detected.
   * @param column The column on the line where the error was detected.
   * @param message The human readable message describing the error.
   */
  def reportError(line: Int, column: Int, message: String): Unit

  /**
   * Used to report a warning condition. Warnings do not prevent code from being generated.
   *
   * @param line The line in the source file where the warning was detected.
   * @param column THe column on hte line where the warning was detected.
   * @param message THe human readable message describing the warning.
   */
  def reportWarning(line: Int, column: Int, message: String): Unit
}


object Reporter {

  /**
   * Internal compiler errors are handled by throwing instances of this exception. They are not
   * reported in the usual way. Instead the compiler is intended to abort.
   *
   * @param message A human readable message describing the error.
   */
  class InternalErrorException(message: String) extends Exception(message)
}
