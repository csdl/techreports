@echo off
rem RunLeap.bat: a Batch file for invoking the LEAP toolset.

rem Edit the following lines to point to LEAP and the JDK.
set LEAP=p:\Distributions\Leap\3.2.0\
set JDK=p:\jdk1.1.6native\bin

rem The following lines do not need to be modified.
set PATH=%JDK%;%PATH%;

set CLASSPATH=%LEAP%\swingall.jar;%LEAP%\jcbwt220S-classes.zip;%LEAP%\jcchart220S-classes.zip;%LEAP%\jccontrib.zip;%LEAP%\jctable220S-classes.zip;%LEAP%\jscape.zip;%LEAP%\leap-3.2.0.zip;

java csdl.java.leap.tool.LeapTool review.defs -honu

