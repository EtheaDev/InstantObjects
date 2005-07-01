unit Model;

interface

uses
  InstantPersistence;

type
  TEmbeddedPartClass = class;
  TEmbeddedPartsClass = class;
  TEmbeddedReferencesClass = class;
  TExternalPartClass = class;
  TExternalPartsClass = class;
  TExternalReferencesClass = class;
  TMasterClass = class;
  TReferenceClass = class;

  TMasterClass = class(TInstantObject)
  {IOMETADATA stored;
    StringAttribute: String(10) stored 'StringAttr';
    BlobAttribute: Blob stored 'BlobAttr';
    BooleanAttribute: Boolean stored 'BooleanAttr';
    CurrencyAttribute: Currency stored 'CurrencyAttr';
    DateTimeAttribute: DateTime stored 'DateTimeAttr';
    FloatAttribute: Float stored 'FloatAttr';
    GraphicAttribute: Graphic stored 'GraphicAttr';
    IntegerAttribute: Integer stored 'IntegerAttr';
    MemoAttribute: Memo stored 'MemoAttr';
    ReferenceAttribute: Reference(TReferenceClass);
    EmbeddedPartsAtttribute: Parts(TEmbeddedPartsClass);
    ExternalPartsAttribute: Parts(TExternalPartsClass) external 'MC_ExtPartsAttr';
    EmbeddedPartAttribute: Part(TEmbeddedPartClass);
    ExternalPartAttribute: Part(TExternalPartClass) external;
    EmbeddedReferencesAtttribute: References(TEmbeddedReferencesClass) stored 'EmbRefsAtttr';
    ExternalReferencesAttributes: References(TExternalReferencesClass) external 'MC_ExtRefsAttr'; }
    _BlobAttribute: TInstantBlob;
    _BooleanAttribute: TInstantBoolean;
    _CurrencyAttribute: TInstantCurrency;
    _DateTimeAttribute: TInstantDateTime;
    _EmbeddedReferencesAtttribute: TInstantReferences;
    _ExternalPartAttribute: TInstantPart;
    _ExternalPartsAttribute: TInstantParts;
    _ExternalReferencesAttributes: TInstantReferences;
    _FloatAttribute: TInstantFloat;
    _GraphicAttribute: TInstantGraphic;
    _IntegerAttribute: TInstantInteger;
    _EmbeddedPartAttribute: TInstantPart;
    _EmbeddedPartsAtttribute: TInstantParts;
    _MemoAttribute: TInstantMemo;
    _StringAttribute: TInstantString;
    _ReferenceAttribute: TInstantReference;
  private
    function GetBlobAttribute: string;
    function GetBooleanAttribute: Boolean;
    function GetCurrencyAttribute: Currency;
    function GetDateTimeAttribute: TDateTime;
    function GetEmbeddedReferencesAtttribute(Index: Integer): TEmbeddedReferencesClass;
    function GetEmbeddedReferencesAtttributeCount: Integer;
    function GetExternalPartAttribute: TExternalPartClass;
    function GetExternalPartsAttribute(Index: Integer): TExternalPartsClass;
    function GetExternalPartsAttributeCount: Integer;
    function GetExternalReferencesAttributeCount: Integer;
    function GetExternalReferencesAttributes(Index: Integer): TExternalReferencesClass;
    function GetFloatAttribute: Double;
    function GetGraphicAttribute: string;
    function GetIntegerAttribute: Integer;
    function GetInternalPartsAtttributeCount: Integer;
    function GetEmbeddedPartAttribute: TEmbeddedPartClass;
    function GetEmbeddedPartsAtttribute(Index: Integer): TEmbeddedPartsClass;
    function GetMemoAttribute: string;
    function GetStringAttribute: string;
    function GetReferenceAttribute: TReferenceClass;
    procedure SetBlobAttribute(const Value: string);
    procedure SetBooleanAttribute(Value: Boolean);
    procedure SetCurrencyAttribute(Value: Currency);
    procedure SetDateTimeAttribute(Value: TDateTime);
    procedure SetEmbeddedReferencesAtttribute(Index: Integer; Value: TEmbeddedReferencesClass);
    procedure SetExternalPartAttribute(Value: TExternalPartClass);
    procedure SetExternalPartsAttribute(Index: Integer; Value: TExternalPartsClass);
    procedure SetExternalReferencesAttributes(Index: Integer; Value: TExternalReferencesClass);
    procedure SetFloatAttribute(Value: Double);
    procedure SetGraphicAttribute(const Value: string);
    procedure SetIntegerAttribute(Value: Integer);
    procedure SetEmbeddedPartAttribute(Value: TEmbeddedPartClass);
    procedure SetEmbeddedPartsAtttribute(Index: Integer; Value: TEmbeddedPartsClass);
    procedure SetMemoAttribute(const Value: string);
    procedure SetStringAttribute(const Value: string);
    procedure SetReferenceAttribute(Value: TReferenceClass);
  public
    function AddEmbeddedReferencesAtttribute(EmbeddedReferencesAtttribute: TEmbeddedReferencesClass): Integer;
    function AddExternalPartsAttribute(ExternalPartsAttribute: TExternalPartsClass): Integer;
    function AddExternalReferencesAttribute(ExternalReferencesAttribute: TExternalReferencesClass): Integer;
    function AddInternalPartsAtttribute(InternalPartsAtttribute: TEmbeddedPartsClass): Integer;
    procedure ClearEmbeddedPartsAtttribute;
    procedure ClearEmbeddedReferencesAtttribute;
    procedure ClearExternalPartsAttribute;
    procedure ClearExternalReferencesAttributes;
    procedure DeleteEmbeddedReferencesAtttribute(Index: Integer);
    procedure DeleteExternalPartsAttribute(Index: Integer);
    procedure DeleteExternalReferencesAttribute(Index: Integer);
    procedure DeleteInternalPartsAtttribute(Index: Integer);
    function IndexOfEmbeddedReferencesAtttribute(EmbeddedReferencesAtttribute: TEmbeddedReferencesClass): Integer;
    function IndexOfExternalPartsAttribute(ExternalPartsAttribute: TExternalPartsClass): Integer;
    function IndexOfExternalReferencesAttribute(ExternalReferencesAttribute: TExternalReferencesClass): Integer;
    function IndexOfInternalPartsAtttribute(InternalPartsAtttribute: TEmbeddedPartsClass): Integer;
    procedure InsertEmbeddedReferencesAtttribute(Index: Integer; EmbeddedReferencesAtttribute: TEmbeddedReferencesClass);
    procedure InsertExternalPartsAttribute(Index: Integer; ExternalPartsAttribute: TExternalPartsClass);
    procedure InsertExternalReferencesAttribute(Index: Integer; ExternalReferencesAttribute: TExternalReferencesClass);
    procedure InsertInternalPartsAtttribute(Index: Integer; InternalPartsAtttribute: TEmbeddedPartsClass);
    function RemoveEmbeddedReferencesAtttribute(EmbeddedReferencesAtttribute: TEmbeddedReferencesClass): Integer;
    function RemoveExternalPartsAttribute(ExternalPartsAttribute: TExternalPartsClass): Integer;
    function RemoveExternalReferencesAttribute(ExternalReferencesAttribute: TExternalReferencesClass): Integer;
    function RemoveInternalPartsAtttribute(InternalPartsAtttribute: TEmbeddedPartsClass): Integer;
    property EmbeddedReferencesAtttribute[Index: Integer]: TEmbeddedReferencesClass read GetEmbeddedReferencesAtttribute write SetEmbeddedReferencesAtttribute;
    property EmbeddedReferencesAtttributeCount: Integer read GetEmbeddedReferencesAtttributeCount;
    property ExternalPartsAttribute[Index: Integer]: TExternalPartsClass read GetExternalPartsAttribute write SetExternalPartsAttribute;
    property ExternalPartsAttributeCount: Integer read GetExternalPartsAttributeCount;
    property ExternalReferencesAttributeCount: Integer read GetExternalReferencesAttributeCount;
    property ExternalReferencesAttributes[Index: Integer]: TExternalReferencesClass read GetExternalReferencesAttributes write SetExternalReferencesAttributes;
    property InternalPartsAtttributeCount: Integer read GetInternalPartsAtttributeCount;
    property EmbeddedPartsAtttribute[Index: Integer]: TEmbeddedPartsClass read GetEmbeddedPartsAtttribute write SetEmbeddedPartsAtttribute;
  published
    property BlobAttribute: string read GetBlobAttribute write SetBlobAttribute;
    property BooleanAttribute: Boolean read GetBooleanAttribute write SetBooleanAttribute;
    property CurrencyAttribute: Currency read GetCurrencyAttribute write SetCurrencyAttribute;
    property DateTimeAttribute: TDateTime read GetDateTimeAttribute write SetDateTimeAttribute;
    property EmbeddedPartAttribute: TEmbeddedPartClass read GetEmbeddedPartAttribute write SetEmbeddedPartAttribute;
    property ExternalPartAttribute: TExternalPartClass read GetExternalPartAttribute write SetExternalPartAttribute;
    property FloatAttribute: Double read GetFloatAttribute write SetFloatAttribute;
    property GraphicAttribute: string read GetGraphicAttribute write SetGraphicAttribute;
    property IntegerAttribute: Integer read GetIntegerAttribute write SetIntegerAttribute;
    property MemoAttribute: string read GetMemoAttribute write SetMemoAttribute;
    property StringAttribute: string read GetStringAttribute write SetStringAttribute;
    property ReferenceAttribute: TReferenceClass read GetReferenceAttribute write SetReferenceAttribute;
  end;

  TReferenceClass = class(TInstantObject)
  {IOMETADATA stored;
    DescriptionAtttribute: String(20); }
    _DescriptionAtttribute: TInstantString;
  private
    function GetDescriptionAtttribute: string;
    procedure SetDescriptionAtttribute(const Value: string);
  published
    property DescriptionAtttribute: string read GetDescriptionAtttribute write SetDescriptionAtttribute;
  end;

  TEmbeddedPartsClass = class(TInstantObject)
  {IOMETADATA DescriptionAtttributes: String(20); }
    _DescriptionAtttributes: TInstantString;
  private
    function GetDescriptionAtttributes: string;
    procedure SetDescriptionAtttributes(const Value: string);
  published
    property DescriptionAtttributes: string read GetDescriptionAtttributes write SetDescriptionAtttributes;
  end;

  TExternalPartsClass = class(TInstantObject)
  {IOMETADATA stored;
    Description: String(20); }
    _Description: TInstantString;
  private
    function GetDescription: string;
    procedure SetDescription(const Value: string);
  published
    property Description: string read GetDescription write SetDescription;
  end;

  TEmbeddedPartClass = class(TInstantObject)
  {IOMETADATA Description: String(20); }
    _Description: TInstantString;
  private
    function GetDescription: string;
    procedure SetDescription(const Value: string);
  published
    property Description: string read GetDescription write SetDescription;
  end;

  TExternalPartClass = class(TInstantObject)
  {IOMETADATA stored;
    Description: String(20); }
    _Description: TInstantString;
  private
    function GetDescription: string;
    procedure SetDescription(const Value: string);
  published
    property Description: string read GetDescription write SetDescription;
  end;

  TEmbeddedReferencesClass = class(TInstantObject)
  {IOMETADATA stored;
    Description: String(20); }
    _Description: TInstantString;
  private
    function GetDescription: string;
    procedure SetDescription(const Value: string);
  published
    property Description: string read GetDescription write SetDescription;
  end;

  TExternalReferencesClass = class(TInstantObject)
  {IOMETADATA stored;
    Description: String(20); }
    _Description: TInstantString;
  private
    function GetDescription: string;
    procedure SetDescription(const Value: string);
  published
    property Description: string read GetDescription write SetDescription;
  end;

implementation

{ TMasterClass }

function TMasterClass.AddEmbeddedReferencesAtttribute(EmbeddedReferencesAtttribute: TEmbeddedReferencesClass): Integer;
begin
  Result := _EmbeddedReferencesAtttribute.Add(EmbeddedReferencesAtttribute);
end;

function TMasterClass.AddExternalPartsAttribute(ExternalPartsAttribute: TExternalPartsClass): Integer;
begin
  Result := _ExternalPartsAttribute.Add(ExternalPartsAttribute);
end;

function TMasterClass.AddExternalReferencesAttribute(ExternalReferencesAttribute: TExternalReferencesClass): Integer;
begin
  Result := _ExternalReferencesAttributes.Add(ExternalReferencesAttribute);
end;

function TMasterClass.AddInternalPartsAtttribute(InternalPartsAtttribute: TEmbeddedPartsClass): Integer;
begin
  Result := _EmbeddedPartsAtttribute.Add(InternalPartsAtttribute);
end;

procedure TMasterClass.ClearEmbeddedPartsAtttribute;
begin
  _EmbeddedPartsAtttribute.Clear;
end;

procedure TMasterClass.ClearEmbeddedReferencesAtttribute;
begin
  _EmbeddedReferencesAtttribute.Clear;
end;

procedure TMasterClass.ClearExternalPartsAttribute;
begin
  _ExternalPartsAttribute.Clear;
end;

procedure TMasterClass.ClearExternalReferencesAttributes;
begin
  _ExternalReferencesAttributes.Clear;
end;

procedure TMasterClass.DeleteEmbeddedReferencesAtttribute(Index: Integer);
begin
  _EmbeddedReferencesAtttribute.Delete(Index);
end;

procedure TMasterClass.DeleteExternalPartsAttribute(Index: Integer);
begin
  _ExternalPartsAttribute.Delete(Index);
end;

procedure TMasterClass.DeleteExternalReferencesAttribute(Index: Integer);
begin
  _ExternalReferencesAttributes.Delete(Index);
end;

procedure TMasterClass.DeleteInternalPartsAtttribute(Index: Integer);
begin
  _EmbeddedPartsAtttribute.Delete(Index);
end;

function TMasterClass.GetBlobAttribute: string;
begin
  Result := _BlobAttribute.Value;
end;

function TMasterClass.GetBooleanAttribute: Boolean;
begin
  Result := _BooleanAttribute.Value;
end;

function TMasterClass.GetCurrencyAttribute: Currency;
begin
  Result := _CurrencyAttribute.Value;
end;

function TMasterClass.GetDateTimeAttribute: TDateTime;
begin
  Result := _DateTimeAttribute.Value;
end;

function TMasterClass.GetEmbeddedReferencesAtttribute(Index: Integer): TEmbeddedReferencesClass;
begin
  Result := _EmbeddedReferencesAtttribute[Index] as TEmbeddedReferencesClass;
end;

function TMasterClass.GetEmbeddedReferencesAtttributeCount: Integer;
begin
  Result := _EmbeddedReferencesAtttribute.Count;
end;

function TMasterClass.GetExternalPartAttribute: TExternalPartClass;
begin
  Result := _ExternalPartAttribute.Value as TExternalPartClass;
end;

function TMasterClass.GetExternalPartsAttribute(Index: Integer): TExternalPartsClass;
begin
  Result := _ExternalPartsAttribute[Index] as TExternalPartsClass;
end;

function TMasterClass.GetExternalPartsAttributeCount: Integer;
begin
  Result := _ExternalPartsAttribute.Count;
end;

function TMasterClass.GetExternalReferencesAttributeCount: Integer;
begin
  Result := _ExternalReferencesAttributes.Count;
end;

function TMasterClass.GetExternalReferencesAttributes(Index: Integer): TExternalReferencesClass;
begin
  Result := _ExternalReferencesAttributes[Index] as TExternalReferencesClass;
end;

function TMasterClass.GetFloatAttribute: Double;
begin
  Result := _FloatAttribute.Value;
end;

function TMasterClass.GetGraphicAttribute: string;
begin
  Result := _GraphicAttribute.Value;
end;

function TMasterClass.GetIntegerAttribute: Integer;
begin
  Result := _IntegerAttribute.Value;
end;

function TMasterClass.GetInternalPartsAtttributeCount: Integer;
begin
  Result := _EmbeddedPartsAtttribute.Count;
end;

function TMasterClass.GetEmbeddedPartAttribute: TEmbeddedPartClass;
begin
  Result := _EmbeddedPartAttribute.Value as TEmbeddedPartClass;
end;

function TMasterClass.GetEmbeddedPartsAtttribute(Index: Integer): TEmbeddedPartsClass;
begin
  Result := _EmbeddedPartsAtttribute[Index] as TEmbeddedPartsClass;
end;

function TMasterClass.GetMemoAttribute: string;
begin
  Result := _MemoAttribute.Value;
end;

function TMasterClass.GetStringAttribute: string;
begin
  Result := _StringAttribute.Value;
end;

function TMasterClass.GetReferenceAttribute: TReferenceClass;
begin
  Result := _ReferenceAttribute.Value as TReferenceClass;
end;

function TMasterClass.IndexOfEmbeddedReferencesAtttribute(EmbeddedReferencesAtttribute: TEmbeddedReferencesClass): Integer;
begin
  Result := _EmbeddedReferencesAtttribute.IndexOf(EmbeddedReferencesAtttribute);
end;

function TMasterClass.IndexOfExternalPartsAttribute(ExternalPartsAttribute: TExternalPartsClass): Integer;
begin
  Result := _ExternalPartsAttribute.IndexOf(ExternalPartsAttribute);
end;

function TMasterClass.IndexOfExternalReferencesAttribute(ExternalReferencesAttribute: TExternalReferencesClass): Integer;
begin
  Result := _ExternalReferencesAttributes.IndexOf(ExternalReferencesAttribute);
end;

function TMasterClass.IndexOfInternalPartsAtttribute(InternalPartsAtttribute: TEmbeddedPartsClass): Integer;
begin
  Result := _EmbeddedPartsAtttribute.IndexOf(InternalPartsAtttribute);
end;

procedure TMasterClass.InsertEmbeddedReferencesAtttribute(Index: Integer; EmbeddedReferencesAtttribute: TEmbeddedReferencesClass);
begin
  _EmbeddedReferencesAtttribute.Insert(Index, EmbeddedReferencesAtttribute);
end;

procedure TMasterClass.InsertExternalPartsAttribute(Index: Integer; ExternalPartsAttribute: TExternalPartsClass);
begin
  _ExternalPartsAttribute.Insert(Index, ExternalPartsAttribute);
end;

procedure TMasterClass.InsertExternalReferencesAttribute(Index: Integer; ExternalReferencesAttribute: TExternalReferencesClass);
begin
  _ExternalReferencesAttributes.Insert(Index, ExternalReferencesAttribute);
end;

procedure TMasterClass.InsertInternalPartsAtttribute(Index: Integer; InternalPartsAtttribute: TEmbeddedPartsClass);
begin
  _EmbeddedPartsAtttribute.Insert(Index, InternalPartsAtttribute);
end;

function TMasterClass.RemoveEmbeddedReferencesAtttribute(EmbeddedReferencesAtttribute: TEmbeddedReferencesClass): Integer;
begin
  Result := _EmbeddedReferencesAtttribute.Remove(EmbeddedReferencesAtttribute);
end;

function TMasterClass.RemoveExternalPartsAttribute(ExternalPartsAttribute: TExternalPartsClass): Integer;
begin
  Result := _ExternalPartsAttribute.Remove(ExternalPartsAttribute);
end;

function TMasterClass.RemoveExternalReferencesAttribute(ExternalReferencesAttribute: TExternalReferencesClass): Integer;
begin
  Result := _ExternalReferencesAttributes.Remove(ExternalReferencesAttribute);
end;

function TMasterClass.RemoveInternalPartsAtttribute(InternalPartsAtttribute: TEmbeddedPartsClass): Integer;
begin
  Result := _EmbeddedPartsAtttribute.Remove(InternalPartsAtttribute);
end;

procedure TMasterClass.SetBlobAttribute(const Value: string);
begin
  _BlobAttribute.Value := Value;
end;

procedure TMasterClass.SetBooleanAttribute(Value: Boolean);
begin
  _BooleanAttribute.Value := Value;
end;

procedure TMasterClass.SetCurrencyAttribute(Value: Currency);
begin
  _CurrencyAttribute.Value := Value;
end;

procedure TMasterClass.SetDateTimeAttribute(Value: TDateTime);
begin
  _DateTimeAttribute.Value := Value;
end;

procedure TMasterClass.SetEmbeddedReferencesAtttribute(Index: Integer; Value: TEmbeddedReferencesClass);
begin
  _EmbeddedReferencesAtttribute[Index] := Value;
end;

procedure TMasterClass.SetExternalPartAttribute(Value: TExternalPartClass);
begin
  _ExternalPartAttribute.Value := Value;
end;

procedure TMasterClass.SetExternalPartsAttribute(Index: Integer; Value: TExternalPartsClass);
begin
  _ExternalPartsAttribute[Index] := Value;
end;

procedure TMasterClass.SetExternalReferencesAttributes(Index: Integer; Value: TExternalReferencesClass);
begin
  _ExternalReferencesAttributes[Index] := Value;
end;

procedure TMasterClass.SetFloatAttribute(Value: Double);
begin
  _FloatAttribute.Value := Value;
end;

procedure TMasterClass.SetGraphicAttribute(const Value: string);
begin
  _GraphicAttribute.Value := Value;
end;

procedure TMasterClass.SetIntegerAttribute(Value: Integer);
begin
  _IntegerAttribute.Value := Value;
end;

procedure TMasterClass.SetEmbeddedPartAttribute(Value: TEmbeddedPartClass);
begin
  _EmbeddedPartAttribute.Value := Value;
end;

procedure TMasterClass.SetEmbeddedPartsAtttribute(Index: Integer; Value: TEmbeddedPartsClass);
begin
  _EmbeddedPartsAtttribute[Index] := Value;
end;

procedure TMasterClass.SetMemoAttribute(const Value: string);
begin
  _MemoAttribute.Value := Value;
end;

procedure TMasterClass.SetStringAttribute(const Value: string);
begin
  _StringAttribute.Value := Value;
end;

procedure TMasterClass.SetReferenceAttribute(Value: TReferenceClass);
begin
  _ReferenceAttribute.Value := Value;
end;

{ TReferenceClass }

function TReferenceClass.GetDescriptionAtttribute: string;
begin
  Result := _DescriptionAtttribute.Value;
end;

procedure TReferenceClass.SetDescriptionAtttribute(const Value: string);
begin
  _DescriptionAtttribute.Value := Value;
end;

{ TEmbeddedPartsClass }

function TEmbeddedPartsClass.GetDescriptionAtttributes: string;
begin
  Result := _DescriptionAtttributes.Value;
end;

procedure TEmbeddedPartsClass.SetDescriptionAtttributes(const Value: string);
begin
  _DescriptionAtttributes.Value := Value;
end;

{ TExternalPartsClass }

function TExternalPartsClass.GetDescription: string;
begin
  Result := _Description.Value;
end;

procedure TExternalPartsClass.SetDescription(const Value: string);
begin
  _Description.Value := Value;
end;

{ TEmbeddedPartClass }

function TEmbeddedPartClass.GetDescription: string;
begin
  Result := _Description.Value;
end;

procedure TEmbeddedPartClass.SetDescription(const Value: string);
begin
  _Description.Value := Value;
end;

{ TExternalPartClass }

function TExternalPartClass.GetDescription: string;
begin
  Result := _Description.Value;
end;

procedure TExternalPartClass.SetDescription(const Value: string);
begin
  _Description.Value := Value;
end;

{ TEmbeddedReferencesClass }

function TEmbeddedReferencesClass.GetDescription: string;
begin
  Result := _Description.Value;
end;

procedure TEmbeddedReferencesClass.SetDescription(const Value: string);
begin
  _Description.Value := Value;
end;

{ TExternalReferencesClass }

function TExternalReferencesClass.GetDescription: string;
begin
  Result := _Description.Value;
end;

procedure TExternalReferencesClass.SetDescription(const Value: string);
begin
  _Description.Value := Value;
end;

initialization
  InstantRegisterClasses([
    TEmbeddedPartClass,
    TEmbeddedPartsClass,
    TEmbeddedReferencesClass,
    TExternalPartClass,
    TExternalPartsClass,
    TExternalReferencesClass,
    TMasterClass,
    TReferenceClass
  ]);

end.
