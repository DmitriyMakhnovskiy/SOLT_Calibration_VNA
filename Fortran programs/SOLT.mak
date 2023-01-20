# Microsoft Developer Studio Generated NMAKE File, Format Version 4.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

!IF "$(CFG)" == ""
CFG=SOLT - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to SOLT - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "SOLT - Win32 Release" && "$(CFG)" != "SOLT - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "SOLT.mak" CFG="SOLT - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "SOLT - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "SOLT - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 
################################################################################
# Begin Project
# PROP Target_Last_Scanned "SOLT - Win32 Debug"
RSC=rc.exe
F90=fl32.exe

!IF  "$(CFG)" == "SOLT - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
OUTDIR=.\Release
INTDIR=.\Release

ALL : "$(OUTDIR)\SOLT.exe" "$(OUTDIR)\SOLT.bsc"

CLEAN : 
	-@erase ".\Release\SOLT.bsc"
	-@erase ".\Release\Scenario3.sbr"
	-@erase ".\Release\Scenario2.sbr"
	-@erase ".\Release\NLine.sbr"
	-@erase ".\Release\SOLCorrection.sbr"
	-@erase ".\Release\Scenario1.sbr"
	-@erase ".\Release\SOLTCorrection.sbr"
	-@erase ".\Release\SOLErrors.sbr"
	-@erase ".\Release\SOLTErrors.sbr"
	-@erase ".\Release\Scenario4.sbr"
	-@erase ".\Release\Main.sbr"
	-@erase ".\Release\SOLT.exe"
	-@erase ".\Release\SOLTErrors.obj"
	-@erase ".\Release\Scenario3.obj"
	-@erase ".\Release\Scenario2.obj"
	-@erase ".\Release\NLine.obj"
	-@erase ".\Release\SOLCorrection.obj"
	-@erase ".\Release\Scenario1.obj"
	-@erase ".\Release\SOLTCorrection.obj"
	-@erase ".\Release\SOLErrors.obj"
	-@erase ".\Release\Scenario4.obj"
	-@erase ".\Release\Main.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE F90 /Ox /I "Release/" /c /nologo
# ADD F90 /Ox /WX /FR /Zi /I "Release/" /c /nologo
F90_PROJ=/Ox /WX /FR"Release/" /Zi /I "Release/" /c /nologo /Fo"Release/"\
 /Fd"Release/SOLT.pdb" 
F90_OBJS=.\Release/
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/SOLT.bsc" 
BSC32_SBRS= \
	"$(INTDIR)/Scenario3.sbr" \
	"$(INTDIR)/Scenario2.sbr" \
	"$(INTDIR)/NLine.sbr" \
	"$(INTDIR)/SOLCorrection.sbr" \
	"$(INTDIR)/Scenario1.sbr" \
	"$(INTDIR)/SOLTCorrection.sbr" \
	"$(INTDIR)/SOLErrors.sbr" \
	"$(INTDIR)/SOLTErrors.sbr" \
	"$(INTDIR)/Scenario4.sbr" \
	"$(INTDIR)/Main.sbr"

"$(OUTDIR)\SOLT.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)/SOLT.pdb" /machine:I386 /out:"$(OUTDIR)/SOLT.exe" 
LINK32_OBJS= \
	"$(INTDIR)/SOLTErrors.obj" \
	"$(INTDIR)/Scenario3.obj" \
	"$(INTDIR)/Scenario2.obj" \
	"$(INTDIR)/NLine.obj" \
	"$(INTDIR)/SOLCorrection.obj" \
	"$(INTDIR)/Scenario1.obj" \
	"$(INTDIR)/SOLTCorrection.obj" \
	"$(INTDIR)/SOLErrors.obj" \
	"$(INTDIR)/Scenario4.obj" \
	"$(INTDIR)/Main.obj"

"$(OUTDIR)\SOLT.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "SOLT - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
OUTDIR=.\Debug
INTDIR=.\Debug

ALL : "$(OUTDIR)\SOLT.exe" "$(OUTDIR)\SOLT.bsc"

CLEAN : 
	-@erase ".\Debug\SOLT.bsc"
	-@erase ".\Debug\SOLCorrection.sbr"
	-@erase ".\Debug\NLine.sbr"
	-@erase ".\Debug\SOLTCorrection.sbr"
	-@erase ".\Debug\Scenario3.sbr"
	-@erase ".\Debug\SOLErrors.sbr"
	-@erase ".\Debug\Scenario2.sbr"
	-@erase ".\Debug\Scenario1.sbr"
	-@erase ".\Debug\SOLTErrors.sbr"
	-@erase ".\Debug\Scenario4.sbr"
	-@erase ".\Debug\Main.sbr"
	-@erase ".\Debug\SOLT.exe"
	-@erase ".\Debug\SOLTErrors.obj"
	-@erase ".\Debug\SOLCorrection.obj"
	-@erase ".\Debug\NLine.obj"
	-@erase ".\Debug\SOLTCorrection.obj"
	-@erase ".\Debug\Scenario3.obj"
	-@erase ".\Debug\SOLErrors.obj"
	-@erase ".\Debug\Scenario2.obj"
	-@erase ".\Debug\Scenario1.obj"
	-@erase ".\Debug\Scenario4.obj"
	-@erase ".\Debug\Main.obj"
	-@erase ".\Debug\SOLT.ilk"
	-@erase ".\Debug\SOLT.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE F90 /Zi /I "Debug/" /c /nologo
# ADD F90 /Ox /WX /FR /Zi /I "Debug/" /c /nologo
F90_PROJ=/Ox /WX /FR"Debug/" /Zi /I "Debug/" /c /nologo /Fo"Debug/"\
 /Fd"Debug/SOLT.pdb" 
F90_OBJS=.\Debug/
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/SOLT.bsc" 
BSC32_SBRS= \
	"$(INTDIR)/SOLCorrection.sbr" \
	"$(INTDIR)/NLine.sbr" \
	"$(INTDIR)/SOLTCorrection.sbr" \
	"$(INTDIR)/Scenario3.sbr" \
	"$(INTDIR)/SOLErrors.sbr" \
	"$(INTDIR)/Scenario2.sbr" \
	"$(INTDIR)/Scenario1.sbr" \
	"$(INTDIR)/SOLTErrors.sbr" \
	"$(INTDIR)/Scenario4.sbr" \
	"$(INTDIR)/Main.sbr"

"$(OUTDIR)\SOLT.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)/SOLT.pdb" /debug /machine:I386 /out:"$(OUTDIR)/SOLT.exe" 
LINK32_OBJS= \
	"$(INTDIR)/SOLTErrors.obj" \
	"$(INTDIR)/SOLCorrection.obj" \
	"$(INTDIR)/NLine.obj" \
	"$(INTDIR)/SOLTCorrection.obj" \
	"$(INTDIR)/Scenario3.obj" \
	"$(INTDIR)/SOLErrors.obj" \
	"$(INTDIR)/Scenario2.obj" \
	"$(INTDIR)/Scenario1.obj" \
	"$(INTDIR)/Scenario4.obj" \
	"$(INTDIR)/Main.obj"

"$(OUTDIR)\SOLT.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.for{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f90{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

################################################################################
# Begin Target

# Name "SOLT - Win32 Release"
# Name "SOLT - Win32 Debug"

!IF  "$(CFG)" == "SOLT - Win32 Release"

!ELSEIF  "$(CFG)" == "SOLT - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\SOLTCorrection.for

"$(INTDIR)\SOLTCorrection.obj" : $(SOURCE) "$(INTDIR)"

"$(INTDIR)\SOLTCorrection.sbr" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\SOLCorrection.for

"$(INTDIR)\SOLCorrection.obj" : $(SOURCE) "$(INTDIR)"

"$(INTDIR)\SOLCorrection.sbr" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\SOLErrors.for

"$(INTDIR)\SOLErrors.obj" : $(SOURCE) "$(INTDIR)"

"$(INTDIR)\SOLErrors.sbr" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\SOLTErrors.for

"$(INTDIR)\SOLTErrors.obj" : $(SOURCE) "$(INTDIR)"

"$(INTDIR)\SOLTErrors.sbr" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Scenario1.for

"$(INTDIR)\Scenario1.obj" : $(SOURCE) "$(INTDIR)"

"$(INTDIR)\Scenario1.sbr" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\NLine.for

"$(INTDIR)\NLine.obj" : $(SOURCE) "$(INTDIR)"

"$(INTDIR)\NLine.sbr" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Scenario2.for

"$(INTDIR)\Scenario2.obj" : $(SOURCE) "$(INTDIR)"

"$(INTDIR)\Scenario2.sbr" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Scenario3.for

"$(INTDIR)\Scenario3.obj" : $(SOURCE) "$(INTDIR)"

"$(INTDIR)\Scenario3.sbr" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Scenario4.for

"$(INTDIR)\Scenario4.obj" : $(SOURCE) "$(INTDIR)"

"$(INTDIR)\Scenario4.sbr" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Main.for

"$(INTDIR)\Main.obj" : $(SOURCE) "$(INTDIR)"

"$(INTDIR)\Main.sbr" : $(SOURCE) "$(INTDIR)"


# End Source File
# End Target
# End Project
################################################################################
