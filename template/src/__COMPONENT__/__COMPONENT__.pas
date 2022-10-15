(*****************************************************
 Part of Python Packages for Delphi

 Copyright © 2022 Embarcadero Technologies
 Licensed under MIT License

 https://github.com/peardox/MakePythonComponents
*****************************************************)
unit __COMPONENT__;

interface

uses
  System.Classes, PyPackage, PyPackage.Model, PythonEngine;

type
  [ComponentPlatforms(pidAllPlatforms)]
  T__COMPONENT__ = class(TPyManagedPackage)
  private
    function AsVariant: variant;
  protected
    procedure Prepare(const AModel: TPyPackageModel); override;
    procedure ImportModule; override;
  public
    property __PUBLIC_PROPERTY__: variant read AsVariant;
  end;

implementation

uses
  System.Variants,
  PyPackage.Manager.ManagerKind,
  PyPackage.Manager.Pip,
  PyPackage.Manager.Conda;

{ T__COMPONENT__ }

function T__COMPONENT__.AsVariant: variant;
begin
  Result := inherited;
end;

procedure T__COMPONENT__.ImportModule;
begin
  MaskFPUExceptions(true);
  try
    inherited;
  finally
    MaskFPUExceptions(false);
  end;
end;

procedure T__COMPONENT__.Prepare(const AModel: TPyPackageModel);
begin
  inherited;
  with AModel do begin
    PackageName := '__PACKAGE_NAME__';
    //NumPy from PIP
    PackageManagers.Add(
      TPyPackageManagerKind.pip,
      TPyPackageManagerPip.Create('__PACKAGE_NAME_PIP__'));
    //NumPy from Conda
    PackageManagers.Add(
      TPyPackageManagerKind.conda,
      TPyPackageManagerConda.Create('__PACKAGE_NAME_CONDA__'));
  end;
end;

end.
