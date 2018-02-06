@echo off
REM ============================================================================
REM Build conZ80 using Visual Studio 2010.
REM Debug or release version can be build depending on specified argument.
REM Specify 'release' to build release version, 'debug' (default) to build debug. 
REM ============================================================================

set CONFIG="Debug"
if /i "%1"=="release" set CONFIG="Release"

REM ----------------------------------------------------------------------------
REM Setup path to compilers
REM ----------------------------------------------------------------------------

set OLDPATH=%PATH%
call "%VS100COMNTOOLS%vsvars32.bat"

msbuild /nologo conZ80.vcxproj /p:Configuration=%CONFIG% 
if errorlevel 1 pause

