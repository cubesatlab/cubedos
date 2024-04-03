#
# This shell script is executed by Jenkins. It defines a "build."

# This script will exit immediately if any command returns a failing exit status.
set -e

# Make sure we have the environment we need.
export PATH=/opt/gnat/bin:/opt/spark/bin:/opt/gnatsas/bin:/opt/gnatstudio/bin:$PATH

# Build and run the unit tests.
gprbuild -P cubedos.gpr src/check/cubedos_check.adb
src/check/build/cubedos_check

# Build the test programs.
# We can't run them right now because they are infinite loops, but we can at least build them.
gprbuild -P cubedos.gpr src/check/main.adb
gprbuild -P cubedos.gpr src/check/main_file.adb
gprbuild -P cubedos.gpr src/check/main_message_manager.adb
gprbuild -P cubedos.gpr src/check/main_time.adb

# Build the sample programs.
gprbuild -P samples/Echo/echo.gpr samples/Echo/main.adb
gprbuild -P samples/Networking/networking.gpr -XBUILD=DomainA samples/Networking/DomainA/main.adb
gprbuild -P samples/Networking/networking.gpr -XBUILD=DomainB samples/Networking/DomainB/main.adb
gprbuild -P samples/PubSub/pubsub.gpr samples/PubSub/main.adb
#gprbuild -P samples/STM32F4/stmdemo.gpr samples/STM32F4/main.adb

# Do a style check using GNATcheck using a helper shell script.
# See the comments in the script file for an explanation.
bin/run-gnatcheck.sh

# Build the API documentation. This has to be done after a successful build.
gnatdoc -P cubedos.gpr

# Build the main documentation.
cd doc
pdflatex -file-line-error -halt-on-error CubedOS.tex
bibtex CubedOS
pdflatex -file-line-error -halt-on-error CubedOS.tex > /dev/null
pdflatex -file-line-error -halt-on-error CubedOS.tex > /dev/null
cd ..

# Do SPARK analysis.
gnatprove -P cubedos.gpr --level=2 --mode=silver -j2

# Do CodePeer analysis.
gnatsas analyze -P cubedos.gpr --quiet -j2 --mode=deep --no-gnat -- inspector -quiet
gnatsas report text -P cubedos.gpr --quiet -j2 --mode=deep

# TODO: Copy documentation to the web site for public review.
