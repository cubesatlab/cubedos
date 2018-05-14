@echo off

cd src\edu\vtc\xdr2os3
java -Xmx2048M -cp ..\..\..\..\lib\antlr4-4.5.3.jar org.antlr.v4.Tool -visitor XDR.g4
cd ..\..\..\..

