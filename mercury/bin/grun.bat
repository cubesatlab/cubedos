@echo off
REM
REM This is the testing tool described in "Definitive ANTRL 4."
REM

cd build\production\Merc
java -cp .;..\..\..\..\..\..\lib\antlr4-4.5.3.jar org.antlr.v4.runtime.misc.TestRig edu.vermontstate.merc.Main %1 %2 %3 %4
cd ..\..\..\..
