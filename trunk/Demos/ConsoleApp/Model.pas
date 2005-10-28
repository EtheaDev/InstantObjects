unit Model;

interface

uses
  InstantPersistence;

type
  TSimpleClass = class(TInstantObject)
  {IOMETADATA stored 'SIMPLE';
    StringProperty: String(10) stored 'STRING'; }
    _StringProperty: TInstantString;
  private
    function GetStringProperty: string;
    procedure SetStringProperty(const Value: string);
  published
    property StringProperty: string read GetStringProperty write SetStringProperty;
  end;

procedure CreateInstantModel;

implementation

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
