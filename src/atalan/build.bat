@echo off
REM ============================================================================
REM Build ATALAN using Visual Studio 2010.
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

REM if exist "%ProgramFiles%\Microsoft Visual Studio\VB98" set PATH=%PATH%;%ProgramFiles%\Microsoft Visual Studio\VB98
REM if exist "%ProgramFiles%\VS6_0\VB98" SET PATH=%PATH%;%ProgramFiles%\VS6_0\VB98
REM if exist "%ProgramFiles%\VS60\VB98" SET PATH=%PATH%;%ProgramFiles%\VS60\VB98

REM ------------------------------------------------------------------------------
REM Build the Compiler
REM ------------------------------------------------------------------------------

msbuild /nologo atalan.vcxproj /p:Configuration=%CONFIG% 
REM /p:Configuration=%CONFIG%

REM vcbuild /nologo /r atalan.vcxproj %CONFIGURATION%
if errorlevel 1 pause

