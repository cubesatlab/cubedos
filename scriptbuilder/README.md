ScriptBuilder
==============

A program to build command sequences for execution by the on-board Interpreter Service.

The `venv` folder is the Python virtual environment used for this project. Required dependencies
are in `requirements.txt`. The `scripter` folder contains the source code of the project. The
`docs` and `tests` foldes have the obvious meaning. The `bin` folder contains some supporting
scripts used by the project.

To set up a new development system do the following:

+ Create a virtual environment folder named `venv` off the root folder of the project. This can
  be done using the command: `python -m venv venv`. This runs the venv module as a script and
  builds the new virtual environment in the indicated folder (which happens to have the same
  name). Be sure to use a Python3 interpreter. Python2 is not supported by ScriptBuilder.
  
+ Be sure to activate the virtual environment before you do any of the operations described
  below. From the root of the project folder do something like:
  
      $ source venv/bin/activate
      
  This causes all further Python manipulations to use the virtual environment created above.
  
+ Next, populate the virtual environment with the required packages using pip as follows:

      $ python -m pip install -r requirements.txt.
      
  This runs the pip module as a script and installs all packages mentioned in requirements.txt
  into the virtual environment created in the previous step. These are packages required by
  ScriptBuilder. Use `pip freeze > requirements.txt` when the requirements file needs to be
  updated after installing a new package into the virtual environment.
  
+ Be sure the ANTLR jar file is downloaded and installed on your system. On Linux systems us a
  command such as:
  
      $ curl -O https://www.antlr.org/download/antlr-4.13.1-complete.jar
      
  Copy the file to a suitable location (such as `/usr/local/lib`).
  
+ Finally, generate the MXDR parser using the following command in the project root:

      $ bin/generate-parser
      
  This populates scripter/parser with the Python files comprising the parser. If you install the
  ANTLR jar file somewhere other than `/usr/local/lib` you may have to edit the generate-parser
  script (and the grun script also).
  
After completing the steps above you should be ready to start PyCharm on the ScripBuilder folder
and begin work on the project.
