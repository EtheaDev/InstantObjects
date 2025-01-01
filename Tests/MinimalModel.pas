(*
 *   InstantObjects Test Suite
 *   MinimalModel
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
 * The Original Code is: InstantObjects Test Suite/MinimalModel
 *
 * The Initial Developer of the Original Code is: Uberto Barbini
 *
 * Portions created by the Initial Developer are Copyright (C) 2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Steven Mitchell
 *
 * ***** END LICENSE BLOCK ***** *)

unit MinimalModel;

interface

uses
  InstantPersistence;

type
  TSimpleClass = class(TInstantObject)
  {IOMETADATA stored 'SIMPLE';
    StringProperty: String(20) stored 'STRING'; }
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
    property Id;
    property StringProperty: string read GetStringProperty write SetStringProperty;
  end;

procedure CreateMinimalModel;

implementation

uses InstantMetadata, InstantTypes;

procedure CreateMinimalModel;
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
  // An empty InstantModel.ClassMetadatas should already be available
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
