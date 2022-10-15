(*****************************************************
 Part of Python Packages for Delphi

 Copyright © 2022 Embarcadero Technologies
 Licensed under MIT License

 https://github.com/peardox/MakePythonComponents
*****************************************************)
unit __COMPONENT__Reg;

interface

procedure Register();

implementation

uses
  Classes, __COMPONENT__;

procedure Register();
begin
  RegisterComponents('__PALETTE_PAGE__', [T__COMPONENT__]);
end;

end.
