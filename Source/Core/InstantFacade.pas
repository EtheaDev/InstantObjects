(*
 *   InstantObjects(tm)
 *   Facade class beta
 *
 *   Copyright (c) Seleqt
 *
 *)

unit InstantFacade;

interface

uses
  System.Classes,
 InstantClasses, InstantPersistence;

type
  TInstantFacade = class(TPersistent)
  private
    FOwner: TInstantObject;
    procedure InitAttributes;
  public
    constructor Create(AOwner: TInstantObject);
    property Owner: TInstantObject read FOwner;
  end;

implementation

{ TInstantFacade }

constructor TInstantFacade.Create(AOwner: TInstantObject);
begin
  inherited Create;
  FOwner := AOwner;
  InitAttributes;
end;

procedure TInstantFacade.InitAttributes;
var
  ClassMetadata: TInstantClassMetadata;
  I: Integer;
  AttribMeta: TInstantAttributeMetadata;
  AttribPtr: PInstantAttribute;
  OwnerAttrib: TInstantAttribute;
begin
  ClassMetadata := InstantModel.ClassMetadatas.Find(ClassName);
  if Assigned(ClassMetadata) then
  begin
    for I := 0 to Pred(ClassMetadata.AttributeMetadatas.Count) do
    begin
      AttribMeta := ClassMetadata.AttributeMetadatas[I];
      AttribPtr := FieldAddress(InstantAttributePrefix + AttribMeta.Name);
      if Assigned(AttribPtr) and not Assigned(AttribPtr^) then
      begin
        if Assigned(Owner) then
        begin
          OwnerAttrib := InstantFindAttribute(AttribMeta.FieldName, Owner);
          if Assigned(OwnerAttrib) then
            if (OwnerAttrib.Metadata.AttributeType =
              AttribMeta.AttributeType) then
              AttribPtr^ := OwnerAttrib
            else
              raise EInstantError.CreateFmt('Unable to facade attribute: %s',
                [AttribMeta.Name]);
        end;
        if not Assigned(AttribPtr^) then
          AttribPtr^ := AttribMeta.CreateAttribute(nil);
      end;
    end;
  end;
end;

end.
