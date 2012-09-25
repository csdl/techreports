rem RunLeap.bat: Runs LEAP with pre-installed Java.

rem Edit if LEAP is not installed in default location.
set LEAPVER=5.8.4
set LEAP=s:\leap\dist\%LEAPVER%
set CLASSPATH=%LEAP%\bin\leap-%LEAPVER%.jar;%LEAP%\bin\swingall.jar;%LEAP%\bin\jclass-362J.jar;%LEAP%\bin\jscape.zip;%LEAP%\extensions;

rem p:\jdk1.1.8native\bin\java csdl.leap.tool.LeapTool u:\Leap\DRLs\CSDL.defs u:\Leap\cam.drl u:\Leap\cam-check.leap

c:\jdk1.2.2\bin\java csdl.leap.tool.LeapTool u:\Leap\DRLs\CSDL.defs u:\Leap\cam.drl u:\Leap\cam-check.leap
rem c:\jdk1.2.2\bin\java csdl.leap.tool.LeapTool 

