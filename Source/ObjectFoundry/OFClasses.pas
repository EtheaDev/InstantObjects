(*
 *   InstantObjects
 *   Object Foundry Expert
 *)

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is: Seleqt InstantObjects/Object Foundry Expert
 *
 * The Initial Developer of the Original Code is: Seleqt
 *
 * Portions created by the Initial Developer are Copyright (C) 2001-2003
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Steven Mitchell
 *
 * ***** END LICENSE BLOCK ***** *)

unit OFClasses;

{ 18 Sep 2004 - Steven Mitchell
  Modified for use in MM7.25 -
  Use V9Visibility property inplace of Visibility
  property in MMToolsAPI V10 IMMMember interface.
  30 Sep 2004 - Steven Mitchell
  Added tags for part(s) external storage params
}

interface

uses
  Classes, MMToolsAPI, MMIOAPI, InstantCode;

type
  TMMCodeAttribute = class(TInstantCodeAttribute)
  private
    FOldCount: Integer;
    FOldName: string;
    FOldSingularName: string;
    FProp: IMMProperty;
    procedure ApplyArray;
    procedure ApplyContainerMethods;
    procedure ApplyReadOnly;
    procedure DetectContainerMethods;
    function GetAttribute: IMMIOAttribute;
    function GetTaggedBoolean(const Name: string): Boolean;
    function GetTaggedIntegers(const Name: string): Integer;
    function GetTaggedStrings(const Name: string): string;
    procedure SetTaggedBooleans(const Name: string; const Value: Boolean);
    procedure SetTaggedIntegers(const Name: string; const Value: Integer);
    procedure SetTaggedStrings(const Name, Value: string);
  protected
    procedure BeginOld;
    procedure EndOld;
    function GetIsDefault: Boolean; override;
    function GetName: string; override;
    function GetSingularName: string; override;
    function GetValueGetterCode: string; override;
    function GetValueSetterCode: string; override;
    function HasMethod(const Name: string): Boolean;
    function IsOld: Boolean;
    procedure Load;
    procedure Save;
    procedure SetIsDefault(const Value: Boolean); override;
    property Attribute: IMMIOAttribute read GetAttribute;
    property Prop: IMMProperty read FProp;
    property TaggedBooleans[const Name: string]: Boolean read GetTaggedBoolean write SetTaggedBooleans;
    property TaggedIntegers[const Name: string]: Integer read GetTaggedIntegers write SetTaggedIntegers;
    property TaggedStrings[const Name: string]: string read GetTaggedStrings write SetTaggedStrings;
  public
    constructor Create(AProp: IMMProperty); reintroduce;
    procedure ApplyAttribute;
    procedure ApplyChanges;
    procedure ApplyData;
    procedure LinkMembers;
    procedure Refresh;
  end;

implementation

uses
  SysUtils, MMEngineDefs, InstantPersistence, InstantMetadata, InstantTypes,
  TypInfo, OFUtils;

const
  IOTagPrefix = 'IO.';

{ TMMCodeAttribute }

procedure TMMCodeAttribute.ApplyArray;

  function FindCountProp: IMMProperty;
  var
    Index: Integer;
    Member: IMMMember;
    OldCountPropName: string;
  begin
    BeginOld;
    try
      OldCountPropName := CountPropName;
    finally
      EndOld;
    end;
    if Prop.ClassBase.FindMember(OldCountPropName, Index) then
    begin
      Member := Prop.ClassBase.Members[Index];
      Result := MemberAsProperty(Member);
    end else
      Result := nil;
  end;

  function AddCountProp: IMMProperty;
  var
    I: Integer;
    Getter: IMMMethod;
    OldBody, NewBody: string;
  begin
    Result := FindCountProp;
    if not Assigned(Result) then
    begin
      Result := Prop.ClassBase.AddProperty;
      Attribute.LinkMember(Result.Id);
    end;
    Result.Name := CountPropName;
{$IFDEF MM7+}                                      // SRM begin - 16 Mar 2005
    Result.V9Visibility := TV9Visibility(Visibility);
{$ELSE}
    Result.Visibility := TVisibility(Visibility);
{$ENDIF}                                          // SRM end - 16 Mar 2005
    Result.SetAccessSpec(rwMethod, rwNone);
    Getter := MemberAsMethod(Result.ReadMember);
    if Assigned(Getter) then
    begin
      NewBody := Tailor.CountGetterCode;
      if Getter.SectionCount = 0 then
        Getter.AddSection(NewBody)
      else begin
        BeginOld;
        try
          OldBody := Tailor.CountGetterCode;
        finally
          EndOld;
        end;
        if not SameCode(NewBody, OldBody) then
          for I := 0 to Pred(Getter.SectionCount) do
            if SameCode(Getter.Sections[I], OldBody) then
            begin
              Getter.Sections[I] := NewBody;
              Break;
            end;
      end;
    end;
  end;

  procedure RemoveCountProp;
  var
    CountProp: IMMProperty;
  begin
    CountProp := FindCountProp;
    if Assigned(CountProp) then
    begin
      Attribute.UnlinkMember(CountProp.Id);
      CountProp.Delete;
    end;
  end;

begin
  if IsContainer then
  begin
    AddCountProp;
    Prop.ArraySpec := 'Index: Integer'
  end else
  begin
    RemoveCountProp;
    Prop.ArraySpec := '';
  end;
  ApplyContainerMethods;
end;

procedure TMMCodeAttribute.ApplyAttribute;
begin
  Attribute.UpdateIOStateField(PChar(FieldName), PChar(AttributeClassName));
  if Attribute.IsIOAttribute then
    Attribute.UpdateMethodAccessCode(PChar(ValueGetterCode),
      PChar(ValueSetterCode));
end;

procedure TMMCodeAttribute.ApplyChanges;
begin
  Save;
end;

procedure TMMCodeAttribute.ApplyContainerMethods;

  function FindMethod(MT: TInstantCodeContainerMethodType): IMMMethod;
  begin
    BeginOld;
    try
      Result := Prop.ClassBase.FindMethod(GetMethodName(MT));
    finally
      EndOld;
    end;
  end;

  function GetOldBody(MT: TInstantCodeContainerMethodType): string;
  var
    CodeMethod: TInstantCodeMethod;
  begin
    BeginOld;
    try
      CodeMethod := Tailor.CreateMethod(MT);
      Result := CodeMethod.Proc.Body.Text;
    finally
      EndOld;
    end;
  end;

  function AddMethod(MT: TInstantCodeContainerMethodType;
    CodeMethod: TInstantCodeMethod): IMMMethod;
  var
    I: Integer;
    OldBody, NewBody: string;
  begin
    Result := FindMethod(MT);
    if not Assigned(Result) then
      Result := Prop.ClassBase.AddMethod;
    Attribute.LinkMember(Result.Id);
    Result.Name := CodeMethod.Name;
    Result.Parameters := CodeMethod.Proc.Parameters.AsString;
    if CodeMethod.Proc.ResultTypeName = '' then
      Result.MethodKind := MMEngineDefs.mkProcedure
    else begin
      Result.MethodKind := MMEngineDefs.mkFunction;
      Result.DataName := CodeMethod.Proc.ResultTypeName;
    end;
{$IFDEF MM7+}                                      // SRM begin - 16 Mar 2005
    Result.V9Visibility := TV9Visibility(Visibility);
{$ELSE}
    Result.Visibility := TVisibility(Visibility);
{$ENDIF}                                          // SRM end - 16 Mar 2005
    NewBody := CodeMethod.Proc.Body.AsString;
    if Result.SectionCount = 0 then
      Result.AddSection(NewBody)
    else begin
      OldBody := GetOldBody(MT);
      if not SameCode(NewBody, OldBody) then
        for I := 0 to Pred(Result.SectionCount) do
          if SameCode(Result.Sections[I], OldBody) then
          begin
            Result.Sections[I] := NewBody;
            Break;
          end;
    end;
  end;

  procedure RemoveMethod(MT: TInstantCodeContainerMethodType);
  var
    Method: IMMMethod;
  begin
    Method := FindMethod(MT);
    if Assigned(Method) then
    begin
      Attribute.UnlinkMember(Method.Id);
      Method.Delete;
    end;
  end;

var
  MT: TInstantCodeContainerMethodType;
begin
  Tailor.Apply;
  for MT := Low(MT) to High(MT) do begin
    if IsContainer and (MT in MethodTypes) then
      AddMethod(MT, Tailor.MethodByType[MT])
    else
      RemoveMethod(MT);
  end;    { for }
end;

procedure TMMCodeAttribute.ApplyData;
const
  DefaultSpecs: array[Boolean] of TDefaultSpecifier = (dsUnspecified, dsDefault);
var
  WasReadonly: Boolean;
begin
  Prop.Name := Name;
  Prop.DataName := PropTypeName;
{$IFDEF MM7+}                                      // SRM begin - 16 Mar 2005
  Prop.V9Visibility := TV9Visibility(Visibility);
{$ELSE}
  Prop.Visibility := TVisibility(Visibility);
{$ENDIF}                                          // SRM end - 16 Mar 2005
  TaggedStrings['StorageName'] := StorageName;
  // External part(s) options
  TaggedStrings['ExternalStorageName'] := ExternalStorageName;
  TaggedIntegers['StorageKind'] := Ord(StorageKind);

  TaggedIntegers['Size'] := Metadata.Size;
  TaggedBooleans['IsDefault'] := IsDefault;
  TaggedBooleans['IsIndexed'] := IsIndexed;
  TaggedBooleans['IsRequired'] := IsRequired;
  TaggedStrings['SingularName'] := SingularName;
  TaggedStrings['EditMask'] := Metadata.EditMask;
  TaggedStrings['ValidChars'] := Metadata.ValidCharsString;
  TaggedIntegers['DisplayWidth'] := Metadata.DisplayWidth;
  TaggedStrings['DefaultValue'] := Metadata.DefaultValue;
  WasReadOnly := Prop.WriteAccess = rwNone;
  if ReadOnly <> WasReadOnly then
    ApplyReadOnly;
end;

procedure TMMCodeAttribute.ApplyReadOnly;
begin
  if ReadOnly then
    Prop.SetAccessSpec(rwMethod, rwNone)
  else
    Prop.SetAccessSpec(rwMethod, rwMethod);
end;

procedure TMMCodeAttribute.BeginOld;
begin
  Inc(FOldCount);
end;

constructor TMMCodeAttribute.Create(AProp: IMMProperty);
begin
  inherited Create(nil);
  FProp := AProp;
  Load;
end;

procedure TMMCodeAttribute.DetectContainerMethods;
var
  MT: TInstantCodeContainerMethodType;
  MTs: TInstantCodeContainerMethodTypes;
begin
  MTs := [];
  for MT := Low(MT) to High(MT) do
    if HasMethod(GetMethodName(MT)) then
      Include(MTs, MT);
  MethodTypes := MTs;
end;

procedure TMMCodeAttribute.EndOld;
begin
  if IsOld then
    Dec(FOldCount);
end;

function TMMCodeAttribute.GetAttribute: IMMIOAttribute;
begin
  Result := Prop as IMMIOAttribute;
end;

function TMMCodeAttribute.GetIsDefault: Boolean;
begin
  Result := TaggedBooleans['IsDefault'] or inherited GetIsDefault;
end;

function TMMCodeAttribute.GetName: string;
begin
  if IsOld then
    Result := FOldName
  else
    Result := inherited GetName;
end;

function TMMCodeAttribute.GetSingularName: string;
begin
  if IsOld then
    Result := FOldSingularName
  else
    Result := inherited GetSingularName;
end;

function TMMCodeAttribute.GetTaggedBoolean(const Name: string): Boolean;
begin
  Result := TaggedStrings[Name] = GetEnumName(TypeInfo(Boolean), Ord(True));
end;

function TMMCodeAttribute.GetTaggedIntegers(const Name: string): Integer;
begin
  Result := StrToIntDef(TaggedStrings[Name], 0);
end;

function TMMCodeAttribute.GetTaggedStrings(const Name: string): string;
begin
  Result := Prop.TaggedValues[IOTagPrefix + Name];
end;

function TMMCodeAttribute.GetValueGetterCode: string;
begin
  if TaggedBooleans[IOAttrGetterEmpty] then
    Result := ''
  else
    Result := inherited GetValueGetterCode;
end;

function TMMCodeAttribute.GetValueSetterCode: string;
begin
  if TaggedBooleans[IOAttrSetterEmpty] then
    Result := ''
  else
    Result := inherited GetValueSetterCode;
end;

function TMMCodeAttribute.HasMethod(const Name: string): Boolean;
begin
  Result := Assigned(Prop.ClassBase.FindMethod(Name));
end;

function TMMCodeAttribute.IsOld: Boolean;
begin
  Result := FOldCount > 0;
end;

procedure TMMCodeAttribute.LinkMembers;
var
  Index: Integer;
  MT: TInstantCodeContainerMethodType;
  Method: IMMMethod;
  Member: IMMMember;
  CountProp: IMMProperty;
begin
  if not IsContainer then
    Exit;
  for MT := Low(MT) to High(MT) do
    if MT in MethodTypes then
    begin
      Method := Prop.ClassBase.FindMethod(GetMethodName(MT));
      if Assigned(Method) then
        Attribute.LinkMember(Method.Id);
    end;
  if Prop.ClassBase.FindMember(CountPropName, Index) then
  begin
    Member := Prop.ClassBase.Members[Index];
    CountProp := MemberAsProperty(Member);
    Attribute.LinkMember(CountProp.Id);
  end;
end;

procedure TMMCodeAttribute.Load;
var
  FieldTypeName: string;
begin
  Name := Prop.Name;
  Visibility := TInstantCodeVisibility(Prop.V9Visibility);  // SRM - 18 Sep 2004
  if Attribute.IsIOAttribute then
  begin
    { If the type of attribute field is Integer (which is considered
      an invalid type since types should be TInstant*) the attribute
      is assumed to be new. If the type of the associated property
      is also Integer the attribute type is TInstantInteger, otherwise
      the attribute defaults to a TInstantPart of an instance of the
      class specified as the property type. }

    FieldTypeName := Attribute.IOStateField.DataName;
    if SameText(FieldTypeName, 'Integer') then
      if Prop.DataName = 'Integer' then
        FieldTypeName := 'TInstantInteger'
      else
        FieldTypeName := 'TInstantPart';
    AttributeClassName := FieldTypeName;
  end;
  if Assigned(AttributeClass) and
      AttributeClass.InheritsFrom(TInstantComplex) then
    ObjectClassName := Prop.DataName;
  { TODO: Do we need this? (causes Memo to become String)
  else
    PropTypeName := Prop.DataName;
  }
  Tailor.IsArray := Prop.Options[PropArray];
  ReadOnly := Prop.WriteAccess = rwNone;
  StorageName := TaggedStrings['StorageName'];

  // External part(s) options
  ExternalStorageName := TaggedStrings['ExternalStorageName'];
  StorageKind := TInstantStorageKind(TaggedIntegers['StorageKind']);

  IsDefault := TaggedBooleans['IsDefault'];
  IsIndexed := TaggedBooleans['IsIndexed'];
  IsRequired := TaggedBooleans['IsRequired'];
  SingularName := TaggedStrings['SingularName'];
  Metadata.Size := TaggedIntegers['Size'];
  Metadata.EditMask := TaggedStrings['EditMask'];
  Metadata.ValidCharsString := TaggedStrings['ValidChars'];
  Metadata.DisplayWidth := TaggedIntegers['DisplayWidth'];
  Metadata.DefaultValue := TaggedStrings['DefaultValue'];
  DetectContainerMethods;
  FOldName := Name;
  FOldSingularName := SingularName;
end;

procedure TMMCodeAttribute.Refresh;
begin
  Load;
end;

procedure TMMCodeAttribute.Save;
begin
  Tailor.Apply;
  ApplyData;
  ApplyAttribute;
  ApplyArray;
end;

procedure TMMCodeAttribute.SetIsDefault(const Value: Boolean);
begin
  inherited;
  TaggedBooleans['IsDefault'] := Value;
end;

procedure TMMCodeAttribute.SetTaggedBooleans(const Name: string;
  const Value: Boolean);
begin
  TaggedStrings[Name] := GetEnumName(TypeInfo(Boolean), Ord(Value));
end;

procedure TMMCodeAttribute.SetTaggedIntegers(const Name: string;
  const Value: Integer);
begin
  TaggedStrings[Name] := IntToStr(Value);
end;

procedure TMMCodeAttribute.SetTaggedStrings(const Name, Value: string);
begin
  Prop.TaggedValues[IOTagPrefix + Name] := Value;
end;

end.
