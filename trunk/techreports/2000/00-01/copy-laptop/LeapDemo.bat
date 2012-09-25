rem RunLeap.bat: Runs LEAP with pre-installed Java.

rem Edit if LEAP is not installed in default location.
set LEAP=c:\PROGRA~1\Leap

set CLASSPATH=%LEAP%\bin\leap-5.8.4.jar;%LEAP%\bin\swingall.jar;%LEAP%\bin\jclass-362J.jar;%LEAP%\bin\jscape.zip;%LEAP%\extensions\;

D:\jdk1.2.2\bin\java csdl.leap.tool.LeapTool demo.leap

