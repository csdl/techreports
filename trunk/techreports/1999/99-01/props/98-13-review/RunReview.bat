rem RunReview.bat: Runs Honu with review.defs

rem Edit if LEAP is not installed in default location.
set LEAP=c:\PROGRA~1\Leap

rem Edit 'review.defs' at end of command if necessary
%LEAP%\jre\bin\jre -cp %LEAP%\bin\swingall.jar;%LEAP%\bin\jclass-360S.jar;%LEAP%\bin\jscape.zip;%LEAP%\bin\leap-5.0.3.jar csdl.java.leap.tool.LeapTool %LEAP%\demo\review.defs -honu
