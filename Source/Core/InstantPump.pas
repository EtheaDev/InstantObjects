(*
 *   InstantObjects
 *   Database Pump
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
 * The Original Code is: InstantObjects Pump
 *
 * The Initial Developer of the Original Code is: Nando Dessena
 *
 * Portions created by the Initial Developer are Copyright (C) 2004
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantPump;

{$IFDEF LINUX}
{$I '../InstantDefines.inc'}
{$ELSE}
{$I '..\InstantDefines.inc'}
{$ENDIF}

interface

uses
  SysUtils, Classes, InstantPersistence, InstantMetadata;

type
  EInstantPumpError = class(Exception);

  TInstantPumpOption = (poEmptyDestBeforePump);
  TInstantPumpOptions = set of TInstantPumpOption;

const
  DefaultInstantPumpOptions = [poEmptyDestBeforePump];

type
  TInstantPump = class(TComponent)
  private
    FDestConnector: TInstantConnector;
    FSourceConnector: TInstantConnector;
    FOptions: TInstantPumpOptions;
    FBeforePump: TInstantSchemeEvent;
    FAfterPump: TInstantSchemeEvent;
    procedure SetDestConnector(const Value: TInstantConnector);
    procedure SetSourceConnector(const Value: TInstantConnector);
    procedure InternalPump(const Model: TInstantModel);
    procedure PumpAllObjects(const ClassMetadata: TInstantClassMetadata);
    procedure DeleteAllDestObjects(const ClassMetadata: TInstantClassMetadata);
  protected
    procedure CheckSourceConnector;
    procedure CheckDestConnector;
    procedure CheckConnectors;
    procedure PumpError(const ErrorMsg: string);
    procedure DoBeforePump(Scheme: TInstantScheme);
    procedure DoAfterPump(Scheme: TInstantScheme);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Pump(Model: TInstantModel = nil);
  published
    property SourceConnector: TInstantConnector
      read FSourceConnector
      write SetSourceConnector;
    property DestConnector: TInstantConnector
      read FDestConnector
      write SetDestConnector;
    property Options: TInstantPumpOptions
      read FOptions
      write FOptions
      default DefaultInstantPumpOptions;
    property BeforePump: TInstantSchemeEvent
      read FBeforePump
      write FBeforePump;
    property AfterPump: TInstantSchemeEvent
      read FAfterPump
      write FAfterPump;
  end;                           

implementation

resourcestring
  SUnassignedSourceConnector = 'SourceConnector is not assigned';
  SUnassignedDestConnector = 'DestConnector is not assigned';
  SConnectorsMustBeDifferent = 'SourceConnector cannot be equal to DestConnector';

{ TInstantPump }

constructor TInstantPump.Create(AOwner: TComponent);
begin
  inherited;
  FOptions := DefaultInstantPumpOptions;
end;

procedure TInstantPump.SetDestConnector(const Value: TInstantConnector);
begin
  if Value <> FDestConnector then
  begin
    if Assigned(FDestConnector) then
      FDestConnector.RemoveFreeNotification(Self);
    FDestConnector := Value;
    if Assigned(FDestConnector) then
      FDestConnector.FreeNotification(Self);
  end;
end;

procedure TInstantPump.SetSourceConnector(const Value: TInstantConnector);
begin
  if Value <> FSourceConnector then
  begin
    if Assigned(FSourceConnector) then
      FSourceConnector.RemoveFreeNotification(Self);
    FSourceConnector := Value;
    if Assigned(FSourceConnector) then
      FSourceConnector.FreeNotification(Self);
  end;
end;

procedure TInstantPump.CheckSourceConnector;
begin
  if not Assigned(FSourceConnector) then
    PumpError(SUnassignedSourceConnector);
end;

procedure TInstantPump.CheckDestConnector;
begin
  if not Assigned(FDestConnector) then
    PumpError(SUnassignedDestConnector);
end;

procedure TInstantPump.CheckConnectors;
begin
  CheckSourceConnector;
  CheckDestConnector;
  if FSourceConnector = FDestConnector then
    PumpError(SConnectorsMustBeDifferent);
end;

procedure TInstantPump.PumpError(const ErrorMsg: string);
begin
  raise EInstantPumpError.Create(ErrorMsg);
end;

procedure TInstantPump.Pump(Model: TInstantModel = nil);
var
  Scheme: TInstantScheme;
begin
  CheckConnectors;
  SourceConnector.Connect;
  try
    DestConnector.Connect;
    try
      if Model = nil then
        Model := InstantModel;
      Scheme := FSourceConnector.CreateScheme(Model);
      try
        DoBeforePump(Scheme);
        InternalPump(Model);
        DoAfterPump(Scheme);
      finally
        Scheme.Free;
      end;
    finally
      DestConnector.Disconnect;
    end;
  finally
    SourceConnector.Disconnect;
  end;
end;

procedure TInstantPump.DoBeforePump(Scheme: TInstantScheme);
begin
  if Assigned(FBeforePump) then
    FBeforePump(Self, Scheme);
end;

procedure TInstantPump.DoAfterPump(Scheme: TInstantScheme);
begin
  if Assigned(FAfterPump) then
    FAfterPump(Self, Scheme);
end;

procedure TInstantPump.InternalPump(const Model: TInstantModel);
var
  I: Integer;
  ClassMetadata: TInstantClassMetadata;
begin
  FDestConnector.StartTransaction;
  try
    for I := 0 to Pred(Model.ClassMetadatas.Count) do
    begin
      ClassMetadata := Model.ClassMetadatas[I];
      if ClassMetadata.IsStored then
      begin
        if poEmptyDestBeforePump in FOptions then
          DeleteAllDestObjects(ClassMetadata);
        PumpAllObjects(ClassMetadata);
      end;
    end;
    FDestConnector.CommitTransaction;
  except
    FDestConnector.RollbackTransaction;
    raise;
  end;
end;

procedure TInstantPump.PumpAllObjects(const ClassMetadata: TInstantClassMetadata);
var
  Query : TInstantQuery;
  SourceObject, DestObject: TInstantObject;
  i : integer;
begin
  Query := FSourceConnector.CreateQuery;
  try
    Query.Command := 'select * from ' + ClassMetadata.Name + ' order by Id';
    Query.Open;
    try
      for i := 0 to Query.ObjectCount -1 do
      begin
        SourceObject :=  Query.Objects[i] as TInstantObject;
        DestObject := TInstantObjectClass(Query.ObjectClass).Clone(SourceObject, FDestConnector);
        DestObject.Store;
      end;
    finally
      Query.Close;
    end;
  finally
    Query.Free;
  end;
end;

procedure TInstantPump.DeleteAllDestObjects(const ClassMetadata: TInstantClassMetadata);
var
  Query: TInstantQuery;
  i : integer;
begin
  Query := FDestConnector.CreateQuery;
  try
    Query.Command := 'select * from ' + ClassMetadata.Name + ' order by Id';
    Query.Open;
    try
      for i := 0 to Query.ObjectCount -1 do
      begin
        if (Query.Objects[i] is TInstantObject) then
          TInstantObject(Query.Objects[i]).Dispose;
      end;
    finally
      Query.Close;
    end;
  finally
    Query.Free;
  end;
end;

procedure TInstantPump.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FSourceConnector then
      FSourceConnector := nil;
    if AComponent = FDestConnector then
      FDestConnector := nil;
  end;
end;

end.
