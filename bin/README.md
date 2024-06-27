
README
======

The script `mercury.sh` runs the Mercury tool. It is written in a way that it will run even when
launched via a symbolic link somewhere else. Thus, you can add a symbolic link to the script in,
for example, a personal `bin` directory to run Mercury conveniently from anywhere.

The script assumes Mercury has already been built via:

    $ sbt assembly
    
from inside the `mercury` folder.

The `jenkins.sh` script is used by Jenkins-CI to build and test the CubedOS system and its
various components, test & sample programs, and documentation. It builds Mercury as well, since
Mercury is a prerequisite for the CubedOS build.

See the comments in `run-gnatcheck.sh` for an explaination of what it does and why it exists.
