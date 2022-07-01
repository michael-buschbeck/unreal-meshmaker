program MeshMaker;


uses
  Forms, Localization, Registry,
  MeshMaker_FormMain in 'MeshMaker_FormMain.pas' {FormMain},
  MeshMaker_FormCompile in 'MeshMaker_FormCompile.pas' {FormCompile};


{$R *.RES}


procedure Localization;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;

  if Registry.OpenKey('Software\Phase\MeshMaker', False) then
  begin
    if Registry.ValueExists('Language') then SetLanguage(Registry.ReadString('Language'));
    Registry.CloseKey;
  end;

  Registry.Free;
end;


begin
  Localization;

  Application.Initialize;
  Application.Title := 'MeshMaker';
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormCompile, FormCompile);
  Application.Run;
end.
