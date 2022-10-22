unit OSBrowser;

// Pinched from https://stackoverflow.com/questions/7443264/how-to-open-an-url-with-the-default-browser-with-firemonkey-cross-platform-appli

interface

uses
{$IFDEF MSWINDOWS}
  Winapi.ShellAPI, Winapi.Windows;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  Posix.Stdlib;
{$ENDIF POSIX}

type
  TOSBrowser = class
    class procedure Open(sCommand: string);
  end;

implementation

class procedure TOSBrowser.Open(sCommand: string);
begin
{$IFDEF MSWINDOWS}
  ShellExecute(0, 'OPEN', PChar(sCommand), '', '', SW_SHOWNORMAL);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  _system(PAnsiChar('open ' + AnsiString(sCommand)));
{$ENDIF POSIX}
end;

end.
