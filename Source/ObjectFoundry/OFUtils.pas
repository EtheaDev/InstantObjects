unit OFUtils;

interface

uses
  MMToolsAPI, MMIOAPI, OFDefs;

function ClassAsV9ClassBase(AClass: IMMClassBase): IMMV9ClassBase;
function CodeModelAsV9CodeModel(ACodeModel: IMMCodeModel): IMMV9CodeModel;
function GetRootClass: IMMClassBase;
function IsPersistentClass(AClass: IMMClassBase): Boolean;
function MemberAsAttribute(Member: IMMMember): IMMIOAttribute;
function MemberAsProperty(Member: IMMMember): IMMProperty;
function MemberAsField(Member: IMMMember): IMMField;
function MemberAsMethod(Member: IMMMember): IMMMethod;
function AttributeAsProperty(Attribute: IMMIOAttribute): IMMProperty;
function MemberAsAttributeProperty(Member: IMMMember): IMMProperty;
function SameCode(const Code1, Code2: string): Boolean;

implementation

uses
  Windows, SysUtils;

function ClassAsV9ClassBase(AClass: IMMClassBase): IMMV9ClassBase;
begin
  if not Assigned(AClass) or not AClass.Valid or
    (AClass.QueryInterface(IMMV9ClassBase, Result) <> S_OK) then
    Result := nil;
end;

function CodeModelAsV9CodeModel(ACodeModel: IMMCodeModel): IMMV9CodeModel;
begin
  if ACodeModel.QueryInterface(IMMV9CodeModel, Result) <> S_OK then
    Result := nil;
end;

function GetRootClass: IMMClassBase;
var
  CodeModel: IMMV9CodeModel;
begin
  CodeModel := CodeModelAsV9CodeModel(MMToolServices.CodeModel);
  if Assigned(CodeModel) then
    Result := CodeModel.ClassPersistencyRoot
  else
    Result := nil;
end;

function IsPersistentClass(AClass: IMMClassBase): Boolean;
var
  Root: IMMClassBase;
begin
  Root := GetRootClass;
  Result := Assigned(Root) and Assigned(AClass) and AClass.IsClass(Root);
end;

function MemberAsAttribute(Member: IMMMember): IMMIOAttribute;
begin
  if not Assigned(Member) or not Member.Valid or
    (Member.QueryInterface(IMMIOAttribute, Result) <> S_OK) then
    Result := nil;
end;

function MemberAsProperty(Member: IMMMember): IMMProperty;
begin
  if not Assigned(Member) or not Member.Valid or
    (Member.QueryInterface(IMMProperty, Result) <> S_OK) then
    Result := nil;
end;

function MemberAsField(Member: IMMMember): IMMField;
begin
  if not Assigned(Member) or not Member.Valid or
    (Member.QueryInterface(IMMField, Result) <> S_OK) then
    Result := nil;
end;

function MemberAsMethod(Member: IMMMember): IMMMethod;
begin
  if not Assigned(Member) or not Member.Valid or
    (Member.QueryInterface(IMMMember, Result) <> S_OK) then
    Result := nil;
end;

function AttributeAsProperty(Attribute: IMMIOAttribute): IMMProperty;
begin
  if not Assigned(Attribute) or
    (Attribute.QueryInterface(IMMProperty, Result) <> S_OK) then
    Result := nil;
end;

function MemberAsAttributeProperty(Member: IMMMember): IMMProperty;
var
  Attribute: IMMIOAttribute;
begin
  Attribute := MemberAsAttribute(Member);
  if Assigned(Attribute) then
    Result := AttributeAsProperty(Attribute)
  else
    Result := nil;
end;

function SameCode(const Code1, Code2: string): Boolean;
begin
  Result := SameText(Trim(Code1), Trim(Code2));
end;

end.
