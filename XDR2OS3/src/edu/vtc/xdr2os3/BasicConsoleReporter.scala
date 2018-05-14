package edu.vtc.xdr2os3

class BasicConsoleReporter extends Reporter {

  var errorCount = 0
  var warningCount = 0

  def reportError(line: Int, column: Int, message: String) {
    errorCount += 1
    printf("ERR: [Line: %3d, Column: %3d] %s\n", line, column, message)
  }


  def reportWarning(line: Int, column: Int, message: String) {
    warningCount += 1
    printf("WRN: [Line: %3d, Column: %3d] %s\n", line, column, message)
  }

}
