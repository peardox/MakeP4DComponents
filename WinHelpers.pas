unit WinHelpers; 

interface
{$IFDEF MSWINDOWS}
 
uses Registry, ShlObj, SysUtils, Windows; 

procedure RegisterFileType(cMyExt, cMyFileType, cMyDescription, ExeName: string; IcoIndex: integer; DoUpdate: boolean = false); 
{$ENDIF}

implementation 

{$IFDEF MSWINDOWS}
procedure RegisterFileType(cMyExt, cMyFileType, cMyDescription, ExeName: string; IcoIndex: integer; DoUpdate: boolean = false); 
var 
   Reg: TRegistry; 
begin 
  Reg := TRegistry.Create; 
  try 
    Reg.RootKey := HKEY_CLASSES_ROOT; 
    Reg.OpenKey(cMyExt, True); 
    // Write my file type to it. 
    // This adds HKEY_CLASSES_ROOT\.abc\(Default) = 'Project1.FileType' 
    Reg.WriteString('', cMyFileType); 
    Reg.CloseKey; 
    // Now create an association for that file type 
    Reg.OpenKey(cMyFileType, True); 
    // This adds HKEY_CLASSES_ROOT\Project1.FileType\(Default) 
    //   = 'Project1 File' 
    // This is what you see in the file type description for 
    // the a file's properties. 
    Reg.WriteString('', cMyDescription); 
    Reg.CloseKey;    // Now write the default icon for my file type 
    // This adds HKEY_CLASSES_ROOT\Project1.FileType\DefaultIcon 
    //  \(Default) = 'Application Dir\Project1.exe,0' 
    Reg.OpenKey(cMyFileType + '\DefaultIcon', True); 
    Reg.WriteString('', ExeName + ',' + IntToStr(IcoIndex)); 
    Reg.CloseKey; 
    // Now write the open action in explorer 
    Reg.OpenKey(cMyFileType + '\Shell\Open', True); 
    Reg.WriteString('', '&Open'); 
    Reg.CloseKey; 
    // Write what application to open it with 
    // This adds HKEY_CLASSES_ROOT\Project1.FileType\Shell\Open\Command 
    //  (Default) = '"Application Dir\Project1.exe" "%1"' 
    // Your application must scan the command line parameters 
    // to see what file was passed to it. 
    Reg.OpenKey(cMyFileType + '\Shell\Open\Command', True); 
    Reg.WriteString('', '"' + ExeName + '" "%1"'); 
    Reg.CloseKey; 
    // Finally, we want the Windows Explorer to realize we added 
    // our file type by using the SHChangeNotify API. 
    if DoUpdate then SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil); 
  finally 
    Reg.Free; 
  end; 
end; 
{$ENDIF}

end.
