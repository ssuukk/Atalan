REM =======================================================
REM Delete all temporary files generated during compilation
REM =======================================================


REM _Output folder contains only the temporary files

if exist Debug rmdir /S /Q Debug
if exist Release rmdir /S /Q Release
REM if exist ipch rmdir /S /Q ipch
REM del /q *.ncb *.opt *.plg *.sdf
REM del /q /AH *.suo

