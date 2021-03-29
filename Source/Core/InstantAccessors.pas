(*
 *   InstantObjects
 *   Accessor Classes
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
 * The Original Code is: Seleqt InstantObjects
 *
 * The Initial Developer of the Original Code is: Seleqt
 *
 * Portions created by the Initial Developer are Copyright (C) 2001-2003
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Nando Dessena, Steven Mitchell, Joao Morais
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantAccessors;

{$IFDEF LINUX}
{$I '../InstantDefines.inc'}
{$ELSE}
{$I '..\InstantDefines.inc'}
{$ENDIF}

interface

uses
  InstantPersistence, InstantPresentation, Classes;

type
  TInstantObjectAccessor = class(TInstantAccessor)
  private
    function CreateObjectReference(AObject: TObject): TInstantObjectReference;
    function GetContainer: TInstantContainer;
    function GetSubject: TInstantObject;
  protected
    function GetConnector: TInstantConnector; override;
    function GetObjectCount: Integer; override;
    function GetView: TList; override;
    function InternalAddObject(AObject: TObject): Integer; override;
    function InternalAddToView(AObject: TObject): Integer; override;
    procedure InternalApplyChanges; override;
    procedure InternalClear; override;
    function InternalGetIsChanged: Boolean; override;
    function InternalGetObjectClassName: string; override;
    function InternalGetObjects(Index: Integer): TObject; override;
    function InternalGetObjectCount: Integer; override;
    function InternalGetViewObjects(Index: Integer): TObject; override;
    function InternalIndexOfInstance(Instance: Pointer): Integer; override;
    function InternalIndexOfObject(AObject: TObject): Integer; override;
    procedure InternalInsertInView(Index: Integer; AObject: TObject); override;
    function InternalInsertObject(Index: Integer; AObject: TObject): Integer; override;
    function InternalRemoveFromView(AObject: TObject): Integer; override;
    function InternalRemoveObject(AObject: TObject): Integer; override;
    function InternalViewIndexOfInstance(Instance: Pointer): Integer; override;
    function InternalViewIndexOfObject(AObject: TObject): Integer; override;
    property Container: TInstantContainer read GetContainer;
  public
    constructor Create(ASubject: TObject); override;
    destructor Destroy; override;
    class function SubjectClass: TClass; override;
    property Subject: TInstantObject read GetSubject;
  end;

  TInstantListAccessor = class(TInstantAccessor)
  private
    function GetSubject: TList;
  protected
    function InternalAddObject(AObject: TObject): Integer; override;
    procedure InternalClear; override;
    function InternalGetObjects(Index: Integer): TObject; override;
    function InternalGetObjectCount: Integer; override;
    function InternalIndexOfObject(AObject: TObject): Integer; override;
    function InternalInsertObject(Index: Integer; AObject: TObject): Integer; override;
    function InternalRemoveObject(AObject: TObject): Integer; override;
  public
    class function SubjectClass: TClass; override;
    property Subject: TList read GetSubject;
  end;

  TInstantCollectionAccessor = class(TInstantAccessor)
  private
    function GetSubject: TCollection;
  protected
    function InternalAddObject(AObject: TObject): Integer; override;
    procedure InternalClear; override;
    function InternalCreateObject: TObject; override;
    function InternalGetObjectCount: Integer; override;
    function InternalGetObjectClass: TClass; override;
    function InternalGetObjects(Index: Integer): TObject; override;
    function InternalIndexOfObject(AObject: TObject): Integer; override;
    function InternalInsertObject(AIndex: Integer; AObject: TObject): Integer; override;
    function InternalRemoveObject(AObject: TObject): Integer; override;
  public
    class function SubjectClass: TClass; override;
    property Subject: TCollection read GetSubject;
  end;

implementation

uses
  SysUtils,
{$IFDEF D17+}
  System.Types,
{$ENDIF}
  Contnrs, InstantClasses, InstantConsts;

{ TInstantObjectAccessor }

constructor TInstantObjectAccessor.Create(ASubject: TObject);
begin
  inherited;
  if Assigned(Subject) then
    Subject.AddRef;
end;

destructor TInstantObjectAccessor.Destroy;
begin
  if Assigned(Subject) then
    Subject.Free;
  inherited;
end;

function TInstantObjectAccessor.CreateObjectReference(AObject: TObject):
    TInstantObjectReference;
begin
  if AObject is TInstantObject then
    Result := TInstantObjectReference.Create(TInstantObject(AObject), True)
  else
    raise EInstantError.CreateFmt(SInvalidClass,
            [AObject.ClassName, InternalGetObjectClassName]);
end;

function TInstantObjectAccessor.GetConnector: TInstantConnector;
begin
  Result := Subject.Connector;
end;

function TInstantObjectAccessor.GetContainer: TInstantContainer;
begin
  Result := Subject.ContainerByName(ContainerName);
end;

function TInstantObjectAccessor.GetObjectCount: Integer;
begin
  if Altered then
    Result := View.Count
  else
    Result := InternalObjectCount;
end;

function TInstantObjectAccessor.GetSubject: TInstantObject;
begin
  Result := inherited Subject as TInstantObject;
end;

function TInstantObjectAccessor.GetView: TList;
var
  I: Integer;
  Continue:Boolean;
begin
  if not Assigned(FView) then
  begin
    Continue:=True;
    FView := TObjectList.Create;
    FView.Capacity := InternalObjectCount;
    for I := 0 to Pred(InternalObjectCount) do
    begin
      DoProgress(InternalObjects[I], I+1, Continue);
      if not Continue then Break;
      AddToView(InternalObjects[I]);
    end;
  end;
  Result := FView;
end;

function TInstantObjectAccessor.InternalAddObject(AObject: TObject): Integer;
begin
  if InContent and (AObject is TInstantObject) then
  begin
    Result := Container.Add(TInstantObject(AObject));
    if Container is TInstantParts then
      TInstantObject(AObject).AddRef;
  end else
    Result := -1;
end;

function TInstantObjectAccessor.InternalAddToView(AObject: TObject): Integer;
begin
  Result := View.Add(CreateObjectReference(AObject));
end;

procedure TInstantObjectAccessor.InternalApplyChanges;
begin
  Subject.Store;
end;

procedure TInstantObjectAccessor.InternalClear;
begin
  if InContent then
    Container.Clear;
end;

function TInstantObjectAccessor.InternalGetIsChanged: Boolean;
begin
  Result := Subject.IsChanged;
end;

function TInstantObjectAccessor.InternalGetObjectClassName: string;
begin
  if InContent then
    Result := Container.RequiredClassName
  else
    Result := inherited InternalGetObjectClassName;
end;

function TInstantObjectAccessor.InternalGetObjectCount: Integer;
begin
  if InContent then
    Result := Container.Count
  else
    Result := inherited InternalGetObjectCount;
end;

function TInstantObjectAccessor.InternalGetObjects(
  Index: Integer): TObject;
begin
  if InContent then
    Result := Container[Index]
  else
    Result := inherited InternalGetObjects(Index);
end;

function TInstantObjectAccessor.InternalGetViewObjects(Index: Integer): TObject;
begin
  Result := TInstantObjectReference(View[Index]).Dereference(Connector);
end;

function TInstantObjectAccessor.InternalIndexOfInstance(
  Instance: Pointer): Integer;
begin
  if InContent then
    Result := Container.IndexOfInstance(Instance)
  else
    Result := -1;
end;

function TInstantObjectAccessor.InternalIndexOfObject(
  AObject: TObject): Integer;
begin
  if InContent and (AObject is TInstantObject) then
    Result := Container.IndexOf(TInstantObject(AObject))
  else
    Result := -1;
end;

procedure TInstantObjectAccessor.InternalInsertInView(Index: Integer; AObject:
    TObject);
begin
  View.Insert(Index, CreateObjectReference(AObject));
end;

function TInstantObjectAccessor.InternalInsertObject(Index: Integer;
  AObject: TObject): Integer;
begin
  if InContent and (AObject is TInstantObject) then
  begin
    Container.Insert(Index, TInstantObject(AObject));
    if Container is TInstantParts then
      TInstantObject(AObject).AddRef;
    Result := Index;
  end else
    Result := -1;
end;

function TInstantObjectAccessor.InternalRemoveFromView(AObject: TObject):
    Integer;
begin
  Result := InternalViewIndexOfObject(AObject);
  if Result > -1 then
    View.Delete(Result);
end;

function TInstantObjectAccessor.InternalRemoveObject(
  AObject: TObject): Integer;
begin
  if InContent and (AObject is TInstantObject) then
    Result := Container.Remove(TInstantObject(AObject))
  else
    Result := -1;
end;

function TInstantObjectAccessor.InternalViewIndexOfInstance(Instance: Pointer):
    Integer;
begin
  Result := InternalViewIndexOfObject(Instance);
end;

function TInstantObjectAccessor.InternalViewIndexOfObject(AObject: TObject):
    Integer;
var
  Ref: TInstantObjectReference;
begin
  for Result := 0 to Pred(View.Count) do
  begin
    Ref := TInstantObjectReference(View[Result]);
    if Ref.Equals(AObject as TInstantObject) then
      Exit;
  end;
  Result := -1;
end;

class function TInstantObjectAccessor.SubjectClass: TClass;
begin
  Result := TInstantObject;
end;

{ TInstantListAccessor }

function TInstantListAccessor.GetSubject: TList;
begin
  Result := inherited Subject as TList;
end;

function TInstantListAccessor.InternalAddObject(AObject: TObject): Integer;
begin
  if InContent then
  begin
    Subject.Add(AObject);
    Result := InternalIndexOfObject(AObject);
  end else
    Result := -1;
end;

procedure TInstantListAccessor.InternalClear;
begin
  if InContent then
    Subject.Clear;
end;

function TInstantListAccessor.InternalGetObjectCount: Integer;
begin
  if InContent then
    Result := Subject.Count
  else
    Result := 1;
end;

function TInstantListAccessor.InternalGetObjects(Index: Integer): TObject;
begin
  if InContent then
    Result := Subject[Index]
  else
    Result := Subject;
end;

function TInstantListAccessor.InternalIndexOfObject(AObject: TObject): Integer;
begin
  if InContent then
    Result := Subject.IndexOf(AObject)
  else
    Result := -1;
end;

function TInstantListAccessor.InternalInsertObject(Index: Integer;
  AObject: TObject): Integer;
begin
  if InContent then
  begin
    Subject.Insert(Index, AObject);
    Result := Index;
  end else
    Result := -1;
end;

function TInstantListAccessor.InternalRemoveObject(AObject: TObject): Integer;
begin
  if InContent then
    Result := Subject.Remove(AObject)
  else
    Result := -1;
end;

class function TInstantListAccessor.SubjectClass: TClass;
begin
  Result := TList;
end;

{ TInstantCollectionAccessor }

function TInstantCollectionAccessor.GetSubject: TCollection;
begin
  Result := inherited Subject as TCollection;
end;

function TInstantCollectionAccessor.InternalAddObject(
  AObject: TObject): Integer;
begin
  if AObject is TCollectionItem then
    with TCollectionItem(AObject) do
    begin
      Collection := Subject;
      Result := Index;
    end
  else
    Result := -1;
end;

procedure TInstantCollectionAccessor.InternalClear;
begin
  Subject.Clear;
end;

function TInstantCollectionAccessor.InternalCreateObject: TObject;
begin
  if ObjectClass.InheritsFrom(TCollectionItem) then
    Result := TCollectionItemClass(ObjectClass).Create(Subject)
  else
    Result := inherited InternalCreateObject;
end;

function TInstantCollectionAccessor.InternalGetObjectClass: TClass;
begin
  Result := Subject.ItemClass;
end;

function TInstantCollectionAccessor.InternalGetObjectCount: Integer;
begin
  Result := Subject.Count;
end;

function TInstantCollectionAccessor.InternalGetObjects(Index: Integer): TObject;
begin
  Result := Subject.Items[Index];
end;

function TInstantCollectionAccessor.InternalIndexOfObject(
  AObject: TObject): Integer;
begin
  if AObject is TCollectionItem then
    Result := TCollectionItem(AObject).Index
  else
    Result := -1;
end;

function TInstantCollectionAccessor.InternalInsertObject(AIndex: Integer;
  AObject: TObject): Integer;
begin
  if AObject is TCollectionItem then
    with TCollectionItem(AObject) do
    begin
      Collection := Subject;
      Index := AIndex;
      Result := Index;
    end
  else
    Result := -1;
end;

function TInstantCollectionAccessor.InternalRemoveObject(
  AObject: TObject): Integer;
begin
  Result := InternalIndexOfObject(AObject);
  if Result <> -1 then
    Subject.Delete(Result);
end;

class function TInstantCollectionAccessor.SubjectClass: TClass;
begin
  Result := TCollection;
end;

end.
