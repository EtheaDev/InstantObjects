unit Model;

interface

{$IFDEF LINUX64}
{$I '../InstantDefines.inc'}
{$ELSE}
{$I '..\InstantDefines.inc'}
{$ENDIF}

uses
  System.Classes,
  InstantPersistence,
  InstantTypes,
  InstantClasses;

type
  TSimpleClass = class(TInstantObject)
  {IOMETADATA stored 'SIMPLE';
    StringProperty: String(10) stored 'STRING'; }
    _StringProperty: TInstantString;
  private
    function GetStringProperty: string;
    procedure SetStringProperty(const Value: string);
  public
    {$IFDEF WINLINUX64}
    class function Retrieve(const AObjectId: string; CreateIfMissing: Boolean = False;
      ARefresh: Boolean = False; AConnector: TComponent = nil;
      const AObjectData: TInstantAbstractObjectData = nil): TSimpleClass; reintroduce; virtual;
    {$ENDIF}
  published
    property StringProperty: string read GetStringProperty write SetStringProperty;
  end;

procedure CreateInstantModel;

implementation

uses
  InstantMetadata;

procedure CreateInstantModel;
var
  InstantClassMetadata : TInstantClassMetadata;
  InstantAttributeMetadata : TInstantAttributeMetadata;
begin
(*
<TInstantClassMetadatas>
  <TInstantClassMetadata>
    <Name>TSimpleClass</Name>
    <Persistence>peStored</Persistence>
    <StorageName>SIMPLE</StorageName>
    <AttributeMetadatas>
      <TInstantAttributeMetadatas>
        <TInstantAttributeMetadata>
          <Name>StringProperty</Name>
          <AttributeType>atString</AttributeType>
          <IsIndexed>FALSE</IsIndexed>
          <IsRequired>FALSE</IsRequired>
          <Size>10</Size>
          <StorageName>STRING</StorageName>
        </TInstantAttributeMetadata>
      </TInstantAttributeMetadatas>
    </AttributeMetadatas>
  </TInstantClassMetadata>
</TInstantClassMetadatas>
*)
  InstantModel.ClassMetadatas.Create(InstantModel);
  InstantClassMetadata := InstantModel.ClassMetadatas.Add;
  InstantClassMetadata.Name := 'TSimpleClass';
  InstantClassMetadata.Persistence := peStored;
  InstantClassMetadata.StorageName := 'SIMPLE';
  InstantAttributeMetadata := InstantClassMetadata.AttributeMetadatas.Add;
  InstantAttributeMetadata.Name := 'StringProperty';
  InstantAttributeMetadata.AttributeType := atString;
  InstantAttributeMetadata.IsIndexed := FALSE;
  InstantAttributeMetadata.IsRequired := FALSE;
  InstantAttributeMetadata.Size := 10;
  InstantAttributeMetadata.StorageName := 'STRING';
end;

{ TSimpleClass }

{$IFDEF WINLINUX64}
class function TSimpleClass.Retrieve(const AObjectId: string; CreateIfMissing: Boolean = False;
  ARefresh: Boolean = False; AConnector: TComponent = nil;
  const AObjectData: TInstantAbstractObjectData = nil): TSimpleClass;
begin
  Result := inherited Retrieve(AObjectId,
    CreateIfMissing, ARefresh, AConnector, AObjectData) as TSimpleClass;
end;
{$ENDIF}

function TSimpleClass.GetStringProperty: string;
begin
  Result := _StringProperty.Value;
end;

procedure TSimpleClass.SetStringProperty(const Value: string);
begin
  _StringProperty.Value := Value;
end;

initialization
  InstantRegisterClasses([
    TSimpleClass
  ]);

end.
