#
# This shell script is executed by Jenkins. It defines a "build."

# This script will exit immediately if any command returns a failing exit status.
set -e

# Make sure we have the environment we need.
export PATH=/opt/gnat/bin:/opt/spark/bin:/opt/codepeer/bin:/opt/gnatstudio/bin:$PATH

# Build the system.
alr build

# Run the unit tests.
obj/development/cubedos_check

# Run the test programs.
# ... can't actually do this because they are infinite loops.
#obj/development/main
#obj/development/main_file
#obj/development/main_time

# Build the sample programs.
# ... needs to be updated for the message refactor.
#gprbuild -P samples/Echo/echo.gpr samples/Echo/main.adb
gprbuild -P samples/Networking/domain_a.gpr samples/Networking/DomainA/main.adb
gprbuild -P samples/Networking/domain_b.gpr samples/Networking/DomainB/main.adb
# ... need to be updated for the message refactor.
#gprbuild -P samples/PubSub/pubsub.gpr samples/PubSub/main.adb
#gprbuild -P samples/STM32F4/stmdemo.gpr samples/STM32F4/main.adb

# Do a style check using GNATcheck using a helper shell script.
# See the comments in the script file for an explanation.
bin/run-gnatcheck.sh

# Build the API documentation. This has to be done after a successful build.
gnatdoc -P cubedos.gpr --output=html

# Build the main documentation.
cd doc
pdflatex -file-line-error -halt-on-error CubedOS.tex
bibtex CubedOS
pdflatex -file-line-error -halt-on-error CubedOS.tex > /dev/null
pdflatex -file-line-error -halt-on-error CubedOS.tex > /dev/null
cd ..

# Do SPARK analysis.
gnatprove -P cubedos.gpr --clean
gnatprove -P cubedos.gpr --level=2 --mode=silver -j2

# Do CodePeer analysis.
codepeer -P ./cubedos.gpr -level 2 -j2 -output-msg -quiet

# TODO: Copy documentation to the web site for public review.
