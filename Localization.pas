unit Localization;


interface


uses
  Classes;


function  GetLanguage(CodeLanguage: Byte = 0): string; forward;
procedure SetLanguage(TextLanguageNew: string); forward;

procedure Localize (Component: TComponent); forward;
function  Localized(Component: TComponent; TextIdentifier: string; TextDefault: string = ''): string; forward;

procedure SaveLocalizationTemplate(Component: TComponent; TextFileTemplate: string); forward;


implementation


uses
  TypInfo, Windows, SysUtils;
                                        
var
  TextLanguage: string = '';
  StringListLocalized: TStringList = nil;


procedure SetLanguage(TextLanguageNew: string);
begin
  TextLanguage := TextLanguageNew;
end;


function GetLanguage(CodeLanguage: Byte): string;
begin
  if CodeLanguage = 0 then
  begin
    if Length(TextLanguage) > 0 then
    begin
      Result := TextLanguage;
      Exit;
    end;

    CodeLanguage := GetUserDefaultLangID and $ff;
  end;

  Result := 'int';

  case CodeLanguage of
    LANG_AFRIKAANS:  Result := 'af';
    LANG_ALBANIAN:   Result := 'sq';
    LANG_ARABIC:     Result := 'ar';
    LANG_BASQUE:     Result := 'eu';
    LANG_BELARUSIAN: Result := 'be';
    LANG_BULGARIAN:  Result := 'bg';
    LANG_CATALAN:    Result := 'ca';
    LANG_CHINESE:    Result := 'zh';
    LANG_CROATIAN:   Result := 'hr';
    LANG_CZECH:      Result := 'cs';
    LANG_DANISH:     Result := 'da';
    LANG_DUTCH:      Result := 'nl';
    LANG_ESTONIAN:   Result := 'et';
    LANG_FAEROESE:   Result := 'fo';
    LANG_FARSI:      Result := 'fa';
    LANG_FINNISH:    Result := 'fi';
    LANG_FRENCH:     Result := 'fr';
    LANG_GERMAN:     Result := 'de';
    LANG_GREEK:      Result := 'el';
    LANG_HEBREW:     Result := 'he';
    LANG_HUNGARIAN:  Result := 'hu';
    LANG_ICELANDIC:  Result := 'is';
    LANG_INDONESIAN: Result := 'in';
    LANG_ITALIAN:    Result := 'it';
    LANG_JAPANESE:   Result := 'ja';
    LANG_KOREAN:     Result := 'ko';
    LANG_LATVIAN:    Result := 'lv';
    LANG_LITHUANIAN: Result := 'lt';
    LANG_NORWEGIAN:  Result := 'no';
    LANG_POLISH:     Result := 'pl';
    LANG_PORTUGUESE: Result := 'pt';
    LANG_ROMANIAN:   Result := 'ro';
    LANG_RUSSIAN:    Result := 'ru';
    LANG_SLOVAK:     Result := 'sk';
    LANG_SLOVENIAN:  Result := 'sl';
    LANG_SPANISH:    Result := 'es';
    LANG_SWEDISH:    Result := 'sv';
    LANG_THAI:       Result := 'th';
    LANG_TURKISH:    Result := 'tr';
    LANG_UKRAINIAN:  Result := 'uk';
    LANG_VIETNAMESE: Result := 'vi';
  end;
end;


function GetPropertyText(Component: TComponent; TextNameProperty: ShortString): string;
var
  PointerInfo: PPropInfo;
begin
  if not IsPublishedProp(Component, TextNameProperty) then
  begin
    Result := '';
    Exit;
  end;

  PointerInfo := GetPropInfo(Component.ClassInfo, TextNameProperty);

  if Assigned(PointerInfo) and (PointerInfo.PropType^.Kind in [tkString, tkLString]) then
  begin
    Result := GetStrProp(Component, PointerInfo);
  end
  else begin
    Result := '';
  end;
end;


procedure SetPropertyText(Component: TComponent; TextNameProperty: ShortString; TextProperty: string);
var
  PointerInfo: PPropInfo;
begin
  if not IsPublishedProp(Component, TextNameProperty) then Exit;

  PointerInfo := GetPropInfo(Component.ClassInfo, TextNameProperty);

  if Assigned(PointerInfo) and (PointerInfo.PropType^.Kind in [tkString, tkLString]) then
  begin
    SetStrProp(Component, PointerInfo, TextProperty);
  end;
end;


function IsDescendant(Component: TComponent; TextNameClass: ShortString): Boolean;
var
  ClassCurrent: TClass;
begin
  ClassCurrent := Component.ClassType;

  while Assigned(ClassCurrent) do
  begin
    if AnsiSameText(ClassCurrent.ClassName, TextNameClass) then
    begin
      Result := True;
      Exit;
    end;

    ClassCurrent := ClassCurrent.ClassParent;
  end;

  Result := False;
end;


procedure Localize(Component: TComponent);
var
  ComponentCurrent: TComponent;
  FileLocalization: TextFile;
  IndexCharSeparator: Integer;
  IndexComponent: Integer;
  TextFileLocalization: string;
  TextIdentifier: string;
  TextLine: string;


  procedure LocalizeComponent(ComponentCurrent: TComponent; TextIdentifier: string; TextNameProperty: ShortString);
  begin
    SetPropertyText(ComponentCurrent, TextNameProperty, Localized(Component, TextIdentifier, GetPropertyText(ComponentCurrent, TextNameProperty)));
  end;


begin
  if not Assigned(StringListLocalized) then
  begin
    StringListLocalized := TStringList.Create;
  end;

  try
    TextFileLocalization := ChangeFileExt(ParamStr(0), '-' + GetLanguage + '.lang');

    if not FileExists(TextFileLocalization) then
    begin
      TextFileLocalization := ChangeFileExt(ParamStr(0), '-int.lang');
    end;

    if not FileExists(TextFileLocalization) then Exit;

    AssignFile(FileLocalization, TextFileLocalization);
    Reset(FileLocalization);

    try
      while not Eof(FileLocalization) do
      begin
        ReadLn(FileLocalization, TextLine);
        TextLine := Trim(TextLine);
        if (Length(TextLine) = 0) or (TextLine[1] = ';') then Continue;
        if TextLine = '[' + Component.Name + ']' then Break;
      end;

      while not Eof(FileLocalization) do
      begin
        ReadLn(FileLocalization, TextLine);
        TextLine := Trim(TextLine);
        if (Length(TextLine) = 0) or (TextLine[1] = ';') then Continue;
        if TextLine[1] = '[' then Break;

        IndexCharSeparator := Pos('=', TextLine);
        if IndexCharSeparator <= 1 then Continue;

        TextIdentifier := TrimRight(Copy(TextLine, 1, IndexCharSeparator - 1));

        Delete(TextLine, 1, IndexCharSeparator);
        TextLine := TrimLeft(TextLine);

        if (Length(TextLine) > 0) and (TextLine[1] = '"') then
        begin
          Delete(TextLine, 1, 1);
          if (Length(TextLine) > 0) and (TextLine[Length(TextLine)] = '"') then
          begin
            Delete(TextLine, Length(TextLine), 1);
          end;
        end;

        StringListLocalized.Values[LowerCase(Component.Name + '.' + TextIdentifier)] := TextLine;
      end;

    finally
      CloseFile(FileLocalization);
    end;

  except
    on Exception do;
  end;

  for IndexComponent := Component.ComponentCount - 1 downto 0 do
  begin
    ComponentCurrent := Component.Components[IndexComponent];
    if (Length(ComponentCurrent.Name) > 6) and AnsiSameText(Copy(ComponentCurrent.Name, Length(ComponentCurrent.Name) - 5, 6), '_NoLoc') then Continue;

    if IsDescendant(ComponentCurrent, 'TCommonDialog') then
    begin
      LocalizeComponent(ComponentCurrent, ComponentCurrent.Name + '(Title)',  'Title');
      LocalizeComponent(ComponentCurrent, ComponentCurrent.Name + '(Filter)', 'Filter');
      Continue;
    end;

    if IsDescendant(ComponentCurrent, 'TControl') or IsDescendant(ComponentCurrent, 'TMenuItem') then
    begin
      LocalizeComponent(ComponentCurrent, ComponentCurrent.Name, 'Caption');
      LocalizeComponent(ComponentCurrent, ComponentCurrent.Name + '(Hint)', 'Hint');
      Continue;
    end;
  end;
end;


function Localized(Component: TComponent; TextIdentifier: string; TextDefault: string): string;
begin
  Result := StringListLocalized.Values[LowerCase(Component.Name + '.' + TextIdentifier)];
  if Length(Result) = 0 then Result := TextDefault;

  Result := StringReplace(Result, '\n', #13#10, [rfReplaceAll]);
  Result := StringReplace(Result, '\t', #9,     [rfReplaceAll]);
  Result := StringReplace(Result, '\\', '\',    [rfReplaceAll]);
end;


procedure SaveLocalizationTemplate(Component: TComponent; TextFileTemplate: string);
var
  ComponentCurrent: TComponent;
  FileTemplate: TextFile;
  IndexComponent: Integer;
begin
  AssignFile(FileTemplate, TextFileTemplate);
  if FileExists(TextFileTemplate)
    then Append(FileTemplate)
    else Rewrite(FileTemplate);

  try
    WriteLn(FileTemplate, '[' + Component.Name + ']');

    for IndexComponent := Component.ComponentCount - 1 downto 0 do
    begin
      ComponentCurrent := Component.Components[IndexComponent];
      if (Length(ComponentCurrent.Name) > 6) and AnsiSameText(Copy(ComponentCurrent.Name, Length(ComponentCurrent.Name) - 5, 6), '_NoLoc') then Continue;

      if IsDescendant(ComponentCurrent, 'TCommonDialog') then
      begin
        WriteLn(FileTemplate, ComponentCurrent.Name + '(Title)='  + GetPropertyText(ComponentCurrent, 'Title'));
        WriteLn(FileTemplate, ComponentCurrent.Name + '(Filter)=' + GetPropertyText(ComponentCurrent, 'Filter'));
        Continue;
      end;

      if IsDescendant(ComponentCurrent, 'TControl') then
      begin
        if GetPropertyText(ComponentCurrent, 'Caption') <> '' then WriteLn(FileTemplate, ComponentCurrent.Name +       '=' + GetPropertyText(ComponentCurrent, 'Caption'));
        if GetPropertyText(ComponentCurrent, 'Hint')    <> '' then WriteLn(FileTemplate, ComponentCurrent.Name + '(Hint)=' + GetPropertyText(ComponentCurrent, 'Hint'));
        Continue;
      end;

      if IsDescendant(ComponentCurrent, 'TMenuItem') and (GetPropertyText(ComponentCurrent, 'Caption') <> '-') then
      begin
        if GetPropertyText(ComponentCurrent, 'Caption') <> '' then WriteLn(FileTemplate, ComponentCurrent.Name +       '=' + GetPropertyText(ComponentCurrent, 'Caption'));
        if GetPropertyText(ComponentCurrent, 'Hint')    <> '' then WriteLn(FileTemplate, ComponentCurrent.Name + '(Hint)=' + GetPropertyText(ComponentCurrent, 'Hint'));
        Continue;
      end;
    end;

    WriteLn(FileTemplate);

  finally
    CloseFile(FileTemplate);
  end;
end;


end.
