; newlisp-gs.nsi
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The name of the installer
Name "newLISP-GS"

; The file to write
OutFile "c:\WinDev\newlisp\newlisp-10603-win-gs-163.exe"

; The default installation directory
InstallDir $PROGRAMFILES\newlisp

; The text to prompt the user to enter a directory
DirText "This will install newLISP v.10.6.3 and Guiserver 1.63 on your computer. IF NOT INSTALLING IN THE DEFAULT DIRECTORY REBOOT AFTER INSTALL."

;;;;;;; subroutines for PATH change in MS Windows environment ;;;;;;;;;
!verbose 3
!include "WinMessages.NSH"
!verbose 4

; AddToPath - Adds the given dir to the search path.
;        Input - head of the stack
;        Note - Win9x systems requires reboot

Function AddToPath
  Exch $0
  Push $1
  Push $2
  Push $3

  # don't add if the path doesn't exist
  IfFileExists "$0\*.*" "" AddToPath_done

  ReadEnvStr $1 PATH
  Push "$1;"
  Push "$0;"
  Call StrStr
  Pop $2
  StrCmp $2 "" "" AddToPath_done
  Push "$1;"
  Push "$0\;"
  Call StrStr
  Pop $2
  StrCmp $2 "" "" AddToPath_done
  GetFullPathName /SHORT $3 $0
  Push "$1;"
  Push "$3;"
  Call StrStr
  Pop $2
  StrCmp $2 "" "" AddToPath_done
  Push "$1;"
  Push "$3\;"
  Call StrStr
  Pop $2
  StrCmp $2 "" "" AddToPath_done

  Call IsNT
  Pop $1
  StrCmp $1 1 AddToPath_NT
    ; Not on NT
    StrCpy $1 $WINDIR 2
    FileOpen $1 "$1\autoexec.bat" a
    FileSeek $1 -1 END
    FileReadByte $1 $2
    IntCmp $2 26 0 +2 +2 # DOS EOF
      FileSeek $1 -1 END # write over EOF
    FileWrite $1 "$\r$\nSET PATH=%PATH%;$3$\r$\n"
    FileClose $1
    SetRebootFlag true
    Goto AddToPath_done

  AddToPath_NT:
    ReadRegStr $1 HKCU "Environment" "PATH"
    StrCpy $2 $1 1 -1 # copy last char
    StrCmp $2 ";" 0 +2 # if last char == ;
      StrCpy $1 $1 -1 # remove last char
    StrCmp $1 "" AddToPath_NTdoIt
      StrCpy $0 "$1;$0"
    AddToPath_NTdoIt:
      WriteRegExpandStr HKCU "Environment" "PATH" $0
      SendMessage ${HWND_BROADCAST} ${WM_WININICHANGE} 0 "STR:Environment" /TIMEOUT=5000

  AddToPath_done:
    Pop $3
    Pop $2
    Pop $1
    Pop $0
FunctionEnd

; RemoveFromPath - Remove a given dir from the path
;     Input: head of the stack

Function un.RemoveFromPath
  Exch $0
  Push $1
  Push $2
  Push $3
  Push $4
  Push $5
  Push $6

  IntFmt $6 "%c" 26 # DOS EOF

  Call un.IsNT
  Pop $1
  StrCmp $1 1 unRemoveFromPath_NT
    ; Not on NT
    StrCpy $1 $WINDIR 2
    FileOpen $1 "$1\autoexec.bat" r
    GetTempFileName $4
    FileOpen $2 $4 w
    GetFullPathName /SHORT $0 $0
    StrCpy $0 "SET PATH=%PATH%;$0"
    Goto unRemoveFromPath_dosLoop

    unRemoveFromPath_dosLoop:
      FileRead $1 $3
      StrCpy $5 $3 1 -1 # read last char
      StrCmp $5 $6 0 +2 # if DOS EOF
        StrCpy $3 $3 -1 # remove DOS EOF so we can compare
      StrCmp $3 "$0$\r$\n" unRemoveFromPath_dosLoopRemoveLine
      StrCmp $3 "$0$\n" unRemoveFromPath_dosLoopRemoveLine
      StrCmp $3 "$0" unRemoveFromPath_dosLoopRemoveLine
      StrCmp $3 "" unRemoveFromPath_dosLoopEnd
      FileWrite $2 $3
      Goto unRemoveFromPath_dosLoop
      unRemoveFromPath_dosLoopRemoveLine:
        SetRebootFlag true
        Goto unRemoveFromPath_dosLoop

    unRemoveFromPath_dosLoopEnd:
      FileClose $2
      FileClose $1
      StrCpy $1 $WINDIR 2
      Delete "$1\autoexec.bat"
      CopyFiles /SILENT $4 "$1\autoexec.bat"
      Delete $4
      Goto unRemoveFromPath_done

  unRemoveFromPath_NT:
    ReadRegStr $1 HKCU "Environment" "PATH"
    StrCpy $5 $1 1 -1 # copy last char
    StrCmp $5 ";" +2 # if last char != ;
      StrCpy $1 "$1;" # append ;
    Push $1
    Push "$0;"
    Call un.StrStr ; Find $0; in $1
    Pop $2 ; pos of our dir
    StrCmp $2 "" unRemoveFromPath_done
      ; else, it is in path
      # $0 - path to add
      # $1 - path var
      StrLen $3 "$0;"
      StrLen $4 $2
      StrCpy $5 $1 -$4 # $5 is now the part before the path to remove
      StrCpy $6 $2 "" $3 # $6 is now the part after the path to remove
      StrCpy $3 $5$6

      StrCpy $5 $3 1 -1 # copy last char
      StrCmp $5 ";" 0 +2 # if last char == ;
        StrCpy $3 $3 -1 # remove last char

      WriteRegExpandStr HKCU "Environment" "PATH" $3
      SendMessage ${HWND_BROADCAST} ${WM_WININICHANGE} 0 "STR:Environment" /TIMEOUT=5000

  unRemoveFromPath_done:
    Pop $6
    Pop $5
    Pop $4
    Pop $3
    Pop $2
    Pop $1
    Pop $0
FunctionEnd

###########################################
#            Utility Functions            #
###########################################

; IsNT
; no input
; output, top of the stack = 1 if NT or 0 if not
;
; Usage:
;   Call IsNT
;   Pop $R0
;  ($R0 at this point is 1 or 0)

!macro IsNT un
Function ${un}IsNT
  Push $0
  ReadRegStr $0 HKLM "SOFTWARE\Microsoft\Windows NT\CurrentVersion" CurrentVersion
  StrCmp $0 "" 0 IsNT_yes
  ; we are not NT.
  Pop $0
  Push 0
  Return

  IsNT_yes:
    ; NT!!!
    Pop $0
    Push 1
FunctionEnd
!macroend
!insertmacro IsNT ""
!insertmacro IsNT "un."

; StrStr
; input, top of stack = string to search for
;        top of stack-1 = string to search in
; output, top of stack (replaces with the portion of the string remaining)
; modifies no other variables.
;
; Usage:
;   Push "this is a long ass string"
;   Push "ass"
;   Call StrStr
;   Pop $R0
;  ($R0 at this point is "ass string")

!macro StrStr un
Function ${un}StrStr
Exch $R1 ; st=haystack,old$R1, $R1=needle
  Exch    ; st=old$R1,haystack
  Exch $R2 ; st=old$R1,old$R2, $R2=haystack
  Push $R3
  Push $R4
  Push $R5
  StrLen $R3 $R1
  StrCpy $R4 0
  ; $R1=needle
  ; $R2=haystack
  ; $R3=len(needle)
  ; $R4=cnt
  ; $R5=tmp
  loop:
    StrCpy $R5 $R2 $R3 $R4
    StrCmp $R5 $R1 done
    StrCmp $R5 "" done
    IntOp $R4 $R4 + 1
    Goto loop
done:
  StrCpy $R1 $R2 "" $R4
  Pop $R5
  Pop $R4
  Pop $R3
  Pop $R2
  Exch $R1
FunctionEnd
!macroend
!insertmacro StrStr ""
!insertmacro StrStr "un."

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; end subroutines for PATH change ====================

LicenseText License
LicenseData c:\newlisp\COPYING

ComponentText Select components

ShowInstDetails show

; The stuff to install
Section "newLISP program (required) and DLL"
  CreateDirectory $INSTDIR\guiserver
  CreateDirectory $INSTDIR\modules
  CreateDirectory $INSTDIR\util
  ; Set output path to the installation directories.
  ; sincd 10.3.5 newlisp.dll is back in the newlisp directory

  SetOutPath $INSTDIR
  File "c:\newlisp\newlisp.exe"
  File "c:\newlisp\newlisp.dll"  
  File "c:\newlisp\md5-checksums.txt"
  File "c:\newlisp\newlisp_manual.html"
  File "c:\newlisp\newlisp_index.html"
  File "c:\newlisp\manual_frame.html"
  File "c:\newlisp\CodePatterns.html"
  File "c:\newlisp\newLISPdoc.html"
  File "c:\newlisp\newLISP-10.6.3-Release.html"
  File "c:\newlisp\COPYING"
  File "c:\newlisp\guiserver.lsp"
  File "c:\newlisp\guiserver.jar"
  File "c:\newlisp\newlisp-edit.lsp"
  File "c:\newlisp\newlisp-win.ico"

  SetOutPath $INSTDIR\guiserver
  File "c:\newlisp\guiserver\index-gs.html"
  File "c:\newlisp\guiserver\guiserver.lsp.html"
  File "c:\newlisp\guiserver\newlispdoc.css"
  File "c:\newlisp\guiserver\allfonts-demo.lsp"
  File "c:\newlisp\guiserver\animation-demo.lsp"
  File "c:\newlisp\guiserver\border-layout-demo.lsp"
  File "c:\newlisp\guiserver\button-demo.lsp"
  File "c:\newlisp\guiserver\clipboard-demo.lsp"
  File "c:\newlisp\guiserver\cursor-demo.lsp"
  File "c:\newlisp\guiserver\drag-demo.lsp"
  File "c:\newlisp\guiserver\font-demo.lsp"
  File "c:\newlisp\guiserver\frameless-demo.lsp"
  File "c:\newlisp\guiserver\html-demo.lsp"
  File "c:\newlisp\guiserver\image-demo.lsp"
  File "c:\newlisp\guiserver\midi-demo.lsp"
  File "c:\newlisp\guiserver\midi2-demo.lsp"
  File "c:\newlisp\guiserver\mouse-demo.lsp"
  File "c:\newlisp\guiserver\move-resize-demo.lsp"
  File "c:\newlisp\guiserver\pinballs-demo.lsp"
  File "c:\newlisp\guiserver\properties-demo.lsp"
  File "c:\newlisp\guiserver\rotation-demo.lsp"
  File "c:\newlisp\guiserver\scroll-pane-demo.lsp"
  File "c:\newlisp\guiserver\shapes-demo.lsp"
  File "c:\newlisp\guiserver\scroll-pane-demo.lsp"
  File "c:\newlisp\guiserver\sound-demo.lsp"
  File "c:\newlisp\guiserver\stroke-demo.lsp"
  File "c:\newlisp\guiserver\tabs-demo.lsp"
  File "c:\newlisp\guiserver\table-demo.lsp"
  File "c:\newlisp\guiserver\textrot-demo.lsp"
  File "c:\newlisp\guiserver\widgets-demo.lsp"
  File "c:\newlisp\guiserver\word-count.lsp"
  File "c:\newlisp\guiserver\uppercase.lsp"

  SetOutPath $INSTDIR\modules
  File "c:\newlisp\modules\canvas.lsp"
  File "c:\newlisp\modules\cgi.lsp"
  File "c:\newlisp\modules\crypto.lsp"
  File "c:\newlisp\modules\ftp.lsp"
  File "c:\newlisp\modules\gsl.lsp"
  File "c:\newlisp\modules\infix.lsp"
  File "c:\newlisp\modules\mysql.lsp"
  File "c:\newlisp\modules\odbc.lsp"
  File "c:\newlisp\modules\plot.lsp"
  File "c:\newlisp\modules\pop3.lsp"
  File "c:\newlisp\modules\postgres.lsp"
  File "c:\newlisp\modules\postscript.lsp"
  File "c:\newlisp\modules\smtp.lsp"
  File "c:\newlisp\modules\smtpx.lsp"
  File "c:\newlisp\modules\sqlite3.lsp"
  File "c:\newlisp\modules\stat.lsp"
  File "c:\newlisp\modules\unix.lsp"
  File "c:\newlisp\modules\xmlrpc-client.lsp" 
  File "c:\newlisp\modules\zlib.lsp"

  SetOutPath $INSTDIR\util
  File "c:\newlisp\util\newlispdoc"
  File "c:\newlisp\util\newlisp.vim"
  File "c:\newlisp\util\syntax.cgi"

  SetOutPath $INSTDIR

  ; Write the installation path into the registry
  WriteRegStr HKLM SOFTWARE\newLISP "Install_Dir" "$INSTDIR"

  ; Write envionment variable intor registry
  WriteRegStr HKLM "SYSTEM\CurrentControlSet\Control\Session Manager\Environment" "NEWLISPDIR" "$INSTDIR"

  ; Write the uninstall keys for Windows
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\newLISP" "DisplayName" "newLISP (remove only)"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\newLISP" "UninstallString" '"$INSTDIR\uninstall.exe"'

  WriteUninstaller "uninstall.exe"
  ; make menu in startmenus
  CreateDirectory "$SMPROGRAMS\newLISP"
  CreateShortCut "$SMPROGRAMS\newLISP\newLISP.lnk" "$INSTDIR\newlisp.exe" "" "$INSTDIR\newlisp.exe" 0
  CreateShortCut "$SMPROGRAMS\newLISP\Release Notes.lnk" "$INSTDIR\newLISP-10.6.3-Release.html" "" "$INSTDIR\newLISP-10.6.3-Release.html" 0
  CreateShortCut "$SMPROGRAMS\newLISP\Manual and Reference.lnk" "$INSTDIR\manual_frame.html" "" "$INSTDIR\manual_frame.html" 0
  CreateShortCut "$SMPROGRAMS\newLISP\Code Patterns.lnk" "$INSTDIR\CodePatterns.html" "" "$INSTDIR\CodePatterns.html" 0
  CreateShortCut "$SMPROGRAMS\newLISP\GS Manual and Reference.lnk" "$INSTDIR\guiserver\index-gs.html" "" "$INSTDIR\guiserver\index-gs.html" 0
  CreateShortCut "$SMPROGRAMS\newLISP\newLISP-GS.lnk" "$INSTDIR\guiserver.jar" "64001 newlisp-edit.lsp /local/newLISPsplash.png" "$INSTDIR\newlisp-win.ico" 0
  CreateShortCut "$SMPROGRAMS\newLISP\Uninstall.lnk" "$INSTDIR\uninstall.exe" "" "$INSTDIR\uninstall.exe" 0

SectionEnd ; end the section

Section "Add to path"
  Push $INSTDIR
  Call AddToPath
SectionEnd

Section "Create desktop shortcut"
  CreateShortCut "$DESKTOP\newLISP-GS.lnk" "$INSTDIR\guiserver.jar" "64001 newlisp-edit.lsp /local/newLISPsplash.png" "$INSTDIR\newlisp-win.ico" 0
SectionEnd

; uninstall stuff
UninstallText "This will uninstall newLISP. Hit next to continue."

; special uninstall section.
Section "Uninstall"

  ; remove registry keys
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\newLISP"
  DeleteRegKey HKLM SOFTWARE\newLISP

  ; remove files
  Delete $INSTDIR\newlisp.exe
  Delete $INSTDIR\newlisp.dll
  Delete $INSTDIR\editor.txt
  Delete $INSTDIR\util\newlispdoc
  Delete $INSTDIR\util\newlisp.vim
  Delete $INSTDIR\util\syntax.cgi
  Delete $INSTDIR\md5-checksums.txt
  Delete $INSTDIR\newlisp_manual.html
  Delete $INSTDIR\newlisp_index.html
  Delete $INSTDIR\manual_frame.html
  Delete $INSTDIR\CodePatterns.html
  Delete $INSTDIR\newLISPdoc.html
  Delete $INSTDIR\newLISP-10.6.3-Release.html
  Delete $INSTDIR\COPYING
  Delete $INSTDIR\guiserver.lsp
  Delete $INSTDIR\guiserver.jar
  Delete $INSTDIR\newlisp-edit.lsp
  Delete $INSTDIR\newlisp-win.ico

  Delete $INSTDIR\guiserver\index-gs.html
  Delete $INSTDIR\guiserver\guiserver.lsp.html
  Delete $INSTDIR\guiserver\newlispdoc.css
  Delete $INSTDIR\guiserver\allfonts-demo.lsp
  Delete $INSTDIR\guiserver\animation-demo.lsp
  Delete $INSTDIR\guiserver\border-layout-demo.lsp
  Delete $INSTDIR\guiserver\button-demo.lsp
  Delete $INSTDIR\guiserver\clipboard-demo.lsp
  Delete $INSTDIR\guiserver\cursor-demo.lsp
  Delete $INSTDIR\guiserver\drag-demo.lsp
  Delete $INSTDIR\guiserver\font-demo.lsp
  Delete $INSTDIR\guiserver\frameless-demo.lsp
  Delete $INSTDIR\guiserver\html-demo.lsp
  Delete $INSTDIR\guiserver\image-demo.lsp
  Delete $INSTDIR\guiserver\midi-demo.lsp
  Delete $INSTDIR\guiserver\midi2-demo.lsp
  Delete $INSTDIR\guiserver\mouse-demo.lsp
  Delete $INSTDIR\guiserver\move-resize-demo.lsp
  Delete $INSTDIR\guiserver\pinballs-demo.lsp
  Delete $INSTDIR\guiserver\properties-demo.lsp
  Delete $INSTDIR\guiserver\rotation-demo.lsp
  Delete $INSTDIR\guiserver\shapes-demo.lsp
  Delete $INSTDIR\guiserver\scroll-pane-demo.lsp
  Delete $INSTDIR\guiserver\sound-demo.lsp
  Delete $INSTDIR\guiserver\stroke-demo.lsp
  Delete $INSTDIR\guiserver\tabs-demo.lsp
  Delete $INSTDIR\guiserver\table-demo.lsp
  Delete $INSTDIR\guiserver\textrot-demo.lsp
  Delete $INSTDIR\guiserver\widgets-demo.lsp
  Delete $INSTDIR\guiserver\word-count.lsp
  Delete $INSTDIR\guiserver\uppercase.lsp

  Delete $INSTDIR\modules\canvas.lsp
  Delete $INSTDIR\modules\cgi.lsp
  Delete $INSTDIR\modules\crypto.lsp
  Delete $INSTDIR\modules\ftp.lsp
  Delete $INSTDIR\modules\gsl.lsp
  Delete $INSTDIR\modules\infix.lsp
  Delete $INSTDIR\modules\mysql.lsp
  Delete $INSTDIR\modules\odbc.lsp
  Delete $INSTDIR\modules\plot.lsp
  Delete $INSTDIR\modules\pop3.lsp
  Delete $INSTDIR\modules\postgres.lsp
  Delete $INSTDIR\modules\postscript.lsp
  Delete $INSTDIR\modules\smtp.lsp
  Delete $INSTDIR\modules\smtpx.lsp
  Delete $INSTDIR\modules\sqlite3.lsp
  Delete $INSTDIR\modules\stat.lsp
  Delete $INSTDIR\modules\unix.lsp
  Delete $INSTDIR\modules\xmlrpc-client.lsp 
  Delete $INSTDIR\modules\zlib.lsp
  ; remove start menu entries
  Delete "$DESKTOP\newLISP-GS.lnk"
  Delete "$SMPROGRAMS\newLISP-GS.lnk"
  Delete "$SMPROGRAMS\newLISP\*.*"
  RMDir "$SMPROGRAMS\newLISP"

  ; remove uninstaller, links and install dir
  Delete $INSTDIR\uninstall.exe
  RMDir "$INSTDIR\guiserver"
  RMDir "$INSTDIR\modules"
  RMDir "$INSTDIR\util"
  RMDir "$INSTDIR"

  Push $INSTDIR
  Call un.RemoveFromPath

SectionEnd

; eof
