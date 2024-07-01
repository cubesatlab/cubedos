
# This is just a sample of how to use the ANTLR generated MXDR parser.
# It is assumed this material will get moved elsewhere at some point.

import sys       # For later
import tkinter   # For later
from antlr4 import *
from parser.MXDRLexer import MXDRLexer
from parser.MXDRParser import MXDRParser
from parser.MXDRVisitor import MXDRVisitor

data = InputStream(input(">>> "))   # Get from a file eventually.
lexer = MXDRLexer(data)             # Create a lexer object.
stream = CommonTokenStream(lexer)   # Create a token stream using the lexer.
parser = MXDRParser(stream)         # Create a parser from the token stream.
tree = parser.specification()       # Parse using "specification" as the start symbol.
visitor = MXDRVisitor()             # Create a visitor object.
output = visitor.visit(tree)        # Vist the parse tree to process it.
print(output)                       # Print the overall result.
