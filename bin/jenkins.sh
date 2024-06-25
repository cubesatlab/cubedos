#
# This shell script is executed by Jenkins. It defines a "build."

# This script will exit immediately if any command returns a failing exit status.
set -e

# Make sure we have the environment we need.
export PATH=/opt/gnat/bin:/opt/spark/bin:/opt/gnatsas/bin:/opt/gnatstudio/bin:$PATH

echo "BUILD & RUN UNIT TEST PROGRAM"
echo "============================="
gprbuild -P cubedos.gpr src/check/cubedos_check.adb
src/check/build/cubedos_check

echo -e "\nBUILD TEST PROGRMS"
echo      "=================="
# We can't run them right now because they are infinite loops, but we can at least build them.
echo -e "\nAll-Modules"
echo      "-----------"
gprbuild -P cubedos.gpr src/check/main.adb

echo -e "\nFile Server"
echo      "-----------"
gprbuild -P cubedos.gpr src/check/main_file.adb

echo -e "\nMessage Manager"
echo      "---------------"
gprbuild -P cubedos.gpr src/check/main_message_manager.adb

echo -e "\nTime Server"
echo      "-----------"
gprbuild -P cubedos.gpr src/check/main_time.adb

echo -e "\nBUILD SAMPLE PROGRAMS"
echo      "====================="
echo -e "\nEcho"
echo      "----"
gprbuild -P samples/Echo/echo.gpr samples/Echo/main.adb

echo -e "\nMulti-Domain"
echo      "------------"
gprbuild -P samples/Networking/networking.gpr -XBUILD=DomainA samples/Networking/DomainA/main.adb
gprbuild -P samples/Networking/networking.gpr -XBUILD=DomainB samples/Networking/DomainB/main.adb

echo -e "\nPub/Sub Server"
echo      "--------------"
gprbuild -P samples/PubSub/pubsub.gpr samples/PubSub/main.adb

echo -e "\nSTM32F4"
echo      "-------"
gprbuild -P samples/STM32F4/stmdemo.gpr samples/STM32F4/main.adb

echo -e "\nSTYLE CHECKING"
echo      "=============="
# Do a style check using GNATcheck using a helper shell script.
# See the comments in the script file for an explanation.
bin/run-gnatcheck.sh

echo -e "\nAPI DOCUMENTATION"
echo      "================="
# This has to be done after a successful build.
gnatdoc -P cubedos.gpr

echo -e "\nLaTeX DOCUMENTATION"
echo      "==================="
cd doc
pdflatex -file-line-error -halt-on-error CubedOS.tex
bibtex CubedOS
pdflatex -file-line-error -halt-on-error CubedOS.tex > /dev/null
pdflatex -file-line-error -halt-on-error CubedOS.tex > /dev/null
cd ..

echo -e "\nSPARK ANALYSIS"
echo      "=============="
gnatprove -P cubedos.gpr --level=2 --mode=silver -j2

echo -e "\nCodePeer ANALYSIS"
echo      "================="
gnatsas analyze -P cubedos.gpr --quiet -j2 --mode=deep --no-gnat -- inspector -quiet
gnatsas report text -P cubedos.gpr --quiet -j2 --mode=deep

# TODO: Copy documentation to the web site for public review.
