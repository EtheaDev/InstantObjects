unit OFExpert;

interface

uses
  Classes, MMIOAPI, OFOptions, SysUtils, MMToolsAPI, OFDefs;

type
  TObjectFoundryExpert = class(TInterfacedObject, IUnknown, IMMExpert, IInstantObjectsExpert)
  private
    FOptions: TOFOptions;
    procedure AttributeEditorLoadClasses(Sender: TObject; Items: TStrings);
    //procedure AttributeEditorLoadClassAttrs(Sender: TObject;
    //  const ClassName: String; Items: TStrings);
    function GetOptions: TOFOptions;
    function IsInstantObjectClass(AClass: IMMClassBase): Boolean;
  protected
    procedure Destroyed; safecall;
    function EditAttribute(const P: IMMProperty): Boolean; stdcall;
    procedure EraseMetaData(const C: IMMClass); stdcall;
    procedure Execute(Index: Integer); safecall;
    procedure ExecuteClassRegWizard;
    function ExpertID: WideString; safecall;
    function GetAttributeAccessCode(const P: IMMProperty;
      MethodType: TIOAccessMethodType; Buf: PChar; BufLen: Integer): Integer; stdcall;
    function GetAttributeType(const P: IMMProperty): TIOAttributeType; stdcall;
    function GetAvailable: Boolean; stdcall;
    function GetMenuPositions(Index: Integer): TMMMenuPosition; safecall;
    function GetMenuShortCuts(Index: Integer): TShortCut; safecall;
    function GetIOUsesClause(Buf: PChar; BufLen: Integer): Integer; stdcall;
    function GetVerbCount: Integer; safecall;
    function GetVerbs(Index: Integer): WideString; safecall;
    function GetMetaData(const C: IMMClass; Buf: PChar; BufLen: Integer): Integer; stdcall;
    procedure SetMetaData(const C: IMMClass; const MetaData: PChar); stdcall;
    procedure UpdateAttributeMembers(const P: IMMProperty); stdcall;

    { ModelMaker 7 IMMExpert specific!
      Bad Gerrit!. Extend interfaces instead of modifying them to ensure
      backwards compatibility. }
    procedure ExecuteAction(Index: Integer); safecall;
    function GetActionCount: Integer; safecall;
    procedure GetActionData(Index: Integer; var Data: TMMActionData); safecall;
    function GetCustomImages(ImageIndexOffset: Integer; var Images: THandle): Boolean; safecall;
    function GetCustomMenuData: WideString; safecall;
    function GetCustomToolbarData: WideString; safecall;

  public
    procedure EditOptions;
    function HandleException(E: Exception; const Text: string = '';
      Entity: IUnknown = nil): Boolean;
    property Options: TOFOptions read GetOptions;
  end;

implementation

uses
  Contnrs, Windows, OFClasses, OFUtils, OFCritic, InstantAttributeEditor,
  InstantPersistence, InstantCode, Forms, Controls, Menus, MMEngineDefs,
  OFClassRegWizard, InstantDesignUtils;

const
  SObjectFoundry = 'ObjectFoundry';

// SRM - 01 Oct 2004: begin
// Function externalised from AttributeEditorLoadClasses function.
function TObjectFoundryExpert.IsInstantObjectClass(AClass: IMMClassBase):
  Boolean;
begin
  Result := SameText(AClass.Name, TInstantObject.ClassName) or
    (Assigned(AClass.Ancestor) and IsInstantObjectClass(AClass.Ancestor));
end;

(*procedure TObjectFoundryExpert.AttributeEditorLoadClassAttrs(
  Sender: TObject; const ClassName: String; Items: TStrings);
var
  i, j: Integer;
  Attr: IMMIOAttribute;
  Prop: IMMProperty;
  CodeModel: IMMCodeModel;
  AClass: IMMClassBase;
begin
  CodeModel := MMToolServices.CodeModel;
  if Assigned(CodeModel) then begin
    for i := 0 to Pred(CodeModel.ClassCount) do
    begin
      AClass := CodeModel.Classes[i];
      if IsInstantObjectClass(AClass) and
              SameText(AClass.Name, ClassName) then begin
        for j := 0 to Pred(AClass.MemberCount) do begin
          Attr := MemberAsAttribute(AClass.Members[j]);
          if Assigned(Attr) and Attr.IsIOAttribute then begin
            Prop := AttributeAsProperty(Attr);
            if Assigned(Prop) then
              Items.Add(Prop.Name);
          end;    { if }
        end;    { for }
        Break;
      end;    { if }
    end;
  end;    { if }
end; *)

procedure TObjectFoundryExpert.AttributeEditorLoadClasses(Sender: TObject;
  Items: TStrings);
var
  I: Integer;
  CodeModel: IMMCodeModel;
  AClass: IMMClassBase;
begin
  CodeModel := MMToolServices.CodeModel;
  if Assigned(CodeModel) then begin
    for I := 0 to Pred(CodeModel.ClassCount) do
    begin
      AClass := CodeModel.Classes[I];
      if IsInstantObjectClass(AClass) then
        Items.Add(AClass.Name);
    end;
  end;    { if }
end;
// SRM - 01 Oct 2004: end

procedure TObjectFoundryExpert.Destroyed;
begin
end;

function TObjectFoundryExpert.EditAttribute(const P: IMMProperty): Boolean;
var
  Attribute: TMMCodeAttribute;
  lClass: IMMClassifier;

  function GetBaseClassStorageName: String;
  begin
    Assert(Assigned(lClass), 'lClass is not assigned!');
    if lClass.TaggedValues[IOClassStorageName] <> '' then
      Result := lClass.TaggedValues[IOClassStorageName]
    else
      Result := lClass.Name;

    Result := Remove_T_FromClassName(Result);
  end;

begin
  if Assigned(P) and P.Valid then
  begin
    lClass := P.ClassBase;
    Attribute := TMMCodeAttribute.Create(P);
    try
      with TInstantAttributeEditorForm.Create(nil) do
      try
        BaseClassStorageName := GetBaseClassStorageName;
        OnLoadClasses := AttributeEditorLoadClasses;
        Subject := Attribute;
        Result := ShowModal = mrOK;
        if Result then
          Attribute.ApplyChanges;
      finally
        Free;
      end;
    finally
      Attribute.Free;
    end;
  end else
    Result := False;
end;

procedure TObjectFoundryExpert.EditOptions;
begin
  with TOFOptionsForm.Create(nil) do
  try
    Subject := Options;
    if ShowModal = mrOk then
      Options.Save;
  finally
    Free;
  end;
end;

procedure TObjectFoundryExpert.EraseMetaData(const C: IMMClass);
var
  AClass: IMMV9ClassBase;
begin
  AClass := ClassAsV9ClassBase(C);
  if Assigned(AClass) then
    AClass.Persistency := cpAutoDetect;
end;

procedure TObjectFoundryExpert.Execute(Index: Integer);
begin
  case Index of
    0: ExecuteClassRegWizard;
  end;
end;

procedure TObjectFoundryExpert.ExecuteAction(Index: Integer);
begin
end;

procedure TObjectFoundryExpert.ExecuteClassRegWizard;
begin
  with TClassRegWizardForm.Create(nil) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

function TObjectFoundryExpert.ExpertID: WideString;
begin
  Result := 'Seleqt.ObjectFoundryExpert';
end;

function TObjectFoundryExpert.GetActionCount: Integer;
begin
  Result := 0;
end;

procedure TObjectFoundryExpert.GetActionData(Index: Integer;
  var Data: TMMActionData);
begin
end;

function TObjectFoundryExpert.GetAttributeAccessCode(const P: IMMProperty;
  MethodType: TIOAccessMethodType; Buf: PChar; BufLen: Integer): Integer;
var
  Code: string;
begin
  with TMMCodeAttribute.Create(P) do
  try
    case MethodType of
      ioamGetter:
        Code := ValueGetterCode;
      ioamSetter:
        Code := ValueSetterCode;
    else
      Code := '';
    end;
  finally
    Free;
  end;
  if Length(Code) = 0 then
    Result := -1
  else if Length(Code) >= BufLen then
    Result := Length(Code) + 1
  else begin
    StrPCopy(Buf, Code);
    Result := 0;
  end;
end;

function TObjectFoundryExpert.GetAttributeType(
  const P: IMMProperty): TIOAttributeType;
begin
  if Assigned(P) and P.Valid then
    with TMMCodeAttribute.Create(P) do
    try
      case AttributeType of
        atPart:
          Result := ioaPart;
        atParts:
          Result := ioaParts;
        atReference:
          Result := ioaReference;
        atReferences:
          Result := ioaReferences;
      else
        Result := ioaSimple;
      end;
    finally
      Free;
    end
  else
    Result := ioaNoAttribute;
end;

function TObjectFoundryExpert.GetAvailable: Boolean;
begin
  Result := True;
end;

function TObjectFoundryExpert.GetCustomImages(ImageIndexOffset: Integer;
  var Images: THandle): Boolean;
begin
  Result := False;      
end;

function TObjectFoundryExpert.GetCustomMenuData: WideString;
begin
  Result := '';
end;

function TObjectFoundryExpert.GetCustomToolbarData: WideString;
begin
  Result := '';
end;

function TObjectFoundryExpert.GetIOUsesClause(Buf: PChar;
  BufLen: Integer): Integer;
const
  UnitList = 'InstantPersistence';
begin
  if Length(UnitList) + 1 > BufLen then
    Result := Length(UnitList) + 1
  else begin
    Result := 0;
    StrCopy(Buf, UnitList);
  end;
end;

function TObjectFoundryExpert.GetMenuPositions(Index: Integer): TMMMenuPosition;
begin
  Result := mpToolsMenu;
end;

function TObjectFoundryExpert.GetMenuShortCuts(Index: Integer): TShortCut;
begin
  Result := 0;
  {
  case Index of
    0: Result := ShortCut(Ord('R'), [ssCtrl, ssAlt]);
  else
    Result := 0;
  end;
  }
end;

function TObjectFoundryExpert.GetMetaData(const C: IMMClass; Buf: PChar;
  BufLen: Integer): Integer;

  function ClassMetadataText(AClass: IMMV9ClassBase): string;
  var
    I: Integer;
    MetadataInfo: TInstantCodeMetadataInfo;
    Attr: IMMIOAttribute;
    Prop: IMMProperty;
    Attributes: TList;
    Attribute: TMMCodeAttribute;
  begin
    Attributes := TObjectList.Create;
    MetadataInfo := TInstantCodeMetadataInfo.Create(nil);
    try
      if AClass.Persistency = cpEmbedded then
        MetadataInfo.Persistence := peEmbedded
      else begin
        MetadataInfo.Persistence := peStored;
        MetadataInfo.StorageName := AClass.TaggedValues[IOClassStorageName];
      end;
      for I := 0 to Pred(AClass.MemberCount) do
      begin
        Attr := MemberAsAttribute(AClass.Members[I]);
        if Assigned(Attr) and Attr.IsIOAttribute then
        begin
          Prop := AttributeAsProperty(Attr);
          if Assigned(Prop) then
          begin
            Attribute := TMMCodeAttribute.Create(Prop);
            Attributes.Add(Attribute);
            MetadataInfo.InsertAttribute(Attribute);
          end;
        end;
      end;
      Result := MetadataInfo.AsString;
    finally
      MetadataInfo.Free;
      Attributes.Free;
    end;
  end;

var
  S: string;
  AClass: IMMV9ClassBase;
begin
  AClass := ClassAsV9ClassBase(C);
  if Assigned(AClass) then
    S := ClassMetadataText(AClass)
  else
    S := '';
  if BufLen <= Length(S) then
    Result := Length(S) + 1
  else begin
    StrCopy(Buf, PChar(S));
    Result := 0;
  end;
end;

function TObjectFoundryExpert.GetOptions: TOFOptions;
begin
  if not Assigned(FOptions) then
    FOptions := TOFOptions.Create;
  Result := FOptions;
end;

function TObjectFoundryExpert.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TObjectFoundryExpert.GetVerbs(Index: Integer): WideString;
begin
  case Index of
    0: Result := 'ObjectFoundry Class &Registration';
  else
    Result := '';
  end;
end;

function TObjectFoundryExpert.HandleException(E: Exception; const Text: string;
  Entity: IUnknown): Boolean;
const
  MessageTypes: array[TInstantCodeErrorSeverity] of TMMMessageType =
    (mmtWarning, mmtError);
var
  Msg: IMMMessage;
  S: string;
begin
  Msg := MMToolServices.MessageServer.CreateMessage(SObjectFoundry,
    MMMessagesContainer);
  Msg.Category := SObjectFoundry;
  if Text = '' then
    S := Msg.Category
  else
    S := Text;
  Msg.HeadLine := Format('%s: %s', [S, E.Message]);
  if Assigned(Entity) then
    Msg.ReferToEntity(Entity);
  if E is EInstantCodeError then
    with EInstantCodeError(E) do
    begin
      Msg.MsgType := MessageTypes[Severity];
      //Msg.ReferToSourceFile(FileName, Position.Line, Position.Column);
    end
  else
    Msg.MsgType := mmtError;
  Result := True;
end;

procedure TObjectFoundryExpert.SetMetaData(const C: IMMClass; const MetaData: PChar);

  procedure LinkProperty(AClass: IMMClass; Attribute: TInstantCodeAttribute);
  var
    Index: Integer;
    MMAttr: IMMIOAttribute;
    MMField: IMMField;
  begin
    if AClass.FindMember(Attribute.Name, Index) then
    begin
      MMAttr := MemberAsAttribute(AClass.Members[Index]);
      if Assigned(MMAttr) then
      begin
        if AClass.FindMember(Attribute.FieldName, Index) then
        begin
          MMField := MemberAsField(AClass.Members[Index]);
          if Assigned(MMField) then
            MMAttr.LinkIOStateField(MMField);
        end;
        with TMMCodeAttribute.Create(AttributeAsProperty(MMAttr)) do
        try
          Metadata.Assign(Attribute.Metadata);
          IsDefault := Attribute.IsDefault;
          ApplyData;
          LinkMembers;
        finally
          Free;
        end;
      end;
    end;
  end;

var
  MetadataInfo: TInstantCodeMetadataInfo;
  Attribute: TInstantCodeAttribute;
  I: Integer;
  AClass: IMMV9ClassBase;
  S: string;
begin
  MetadataInfo := TInstantCodeMetadataInfo.Create(nil);
  try
    try
      { The Metadata PChar needs to be copied to a local string to
        avoid using the original PChar which may become invalid.
        MetadataInfo.AsString := Metadata causes problems. }
      S := Metadata;
      S := '{' + S + '}';
      MetadataInfo.AsString := S;
    except
      on E: Exception do
      begin
        S := Format('Syntax error in metadata for class %s', [C.Name]);
        if not HandleException(E, S, C) then
          raise;
      end;
    end;
    AClass := ClassAsV9ClassBase(C);
    if Assigned(AClass) then
      if MetadataInfo.Persistence = peStored then
      begin
        AClass.TaggedValues[IOClassStorageName] := MetadataInfo.StorageName;
        AClass.Persistency := cpPersistent;
      end else
        AClass.Persistency := cpEmbedded;
    for I := 0 to Pred(MetadataInfo.AttributeCount) do
    begin
      Attribute := MetadataInfo.Attributes[I];
      LinkProperty(C, Attribute);
    end;
  finally
    MetadataInfo.Free;
  end;
end;

procedure TObjectFoundryExpert.UpdateAttributeMembers(const P: IMMProperty);
begin
  if Assigned(P) and P.Valid then
    with TMMCodeAttribute.Create(P) do
    try
      ApplyAttribute;
    finally
      Free;
    end;
end;

end.
