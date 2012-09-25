rem RunLeap.bat: Runs LEAP with pre-installed Java.

rem Edit if LEAP is not installed in default location.
set LEAP=p:\Leap

set CLASSPATH=%LEAP%\bin\swingall.jar;%LEAP%\bin\jclass-300S.jar;%LEAP%\bin\jscape.zip;%LEAP%\bin\leap-4.0.4.jar

p:\jdk1.1.6native\bin\java csdl.java.leap.tool.LeapTool review.defs -honu
