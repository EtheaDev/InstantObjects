(*
 *   InstantObjects Test Suite
 *   InstantMock
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
 * The Original Code is: InstantObjects Test Suite/InstantMock
 *
 * The Initial Developer of the Original Code is: Uberto Barbini
 *
 * Portions created by the Initial Developer are Copyright (C) 2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Steven Mitchell, Nando Dessena
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantMock;

{$IFDEF LINUX64}
{$I '../InstantDefines.inc'}
{$ELSE}
{$I '..\InstantDefines.inc'}
{$ENDIF}

interface

uses
  SysUtils, Classes, DB, InstantPersistence, InstantBrokers, InstantMetadata,
  InstantTypes, InstantClasses, UbMockObject;

type
  TInstantBrokerClass = class of TInstantBroker;

  TInstantMockConnector = class(TInstantConnector)
  private
    FMock: TUbMockObject;
    FBrokerClass: TInstantBrokerClass;
    procedure SetBrokerClass(const Value: TInstantBrokerClass);
  protected
    procedure SetMock(const Value: TUbMockObject);
    function CreateBroker: TInstantBroker; override;
    procedure InternalConnect; override;
    function InternalCreateScheme(Model: TInstantModel): TInstantScheme; override;
    procedure InternalDisconnect; override;
    procedure InternalStartTransaction; override;
    procedure InternalCommitTransaction; override;
    procedure InternalRollbackTransaction; override;
  public
    property MockManager: TUbMockObject read FMock write SetMock;
    property BrokerClass: TInstantBrokerClass read FBrokerClass write SetBrokerClass;
    class function ConnectionDefClass: TInstantConnectionDefClass; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;
  
  TInstantMockConnectionDef = class(TInstantConnectionDef)
    class function ConnectionTypeName: String; override;
    class function ConnectorClass: TInstantConnectorClass; override;
    function Edit: Boolean; override;
  end;

  TInstantMockBroker = class(TInstantBroker)
  private
    FMock: TUbMockObject;
    procedure SetMock(const Value: TUbMockObject);
  protected
    function InternalDisposeObject(AObject: TInstantObject;
      ConflictAction: TInstantConflictAction): Boolean; override;
    function InternalRetrieveObject(AObject: TInstantObject;
      const AObjectId: string;
      ConflictAction: TInstantConflictAction;
      const AObjectData: TInstantAbstractObjectData = nil): Boolean;  override;
    function InternalStoreObject(AObject: TInstantObject;
      ConflictAction: TInstantConflictAction): Boolean;  override;
  public
    property MockManager: TUbMockObject read FMock write SetMock;
    constructor Create(AConnector: TInstantConnector); override;
    destructor Destroy; override;
  end;

  TInstantMockCRBroker = class(TInstantCustomRelationalBroker)
  private
    FMock: TUbMockObject;
    procedure SetMock(const Value: TUbMockObject);
  protected
    function EnsureResolver(Map: TInstantAttributeMap): TInstantCustomResolver; override;
    function InternalDisposeObject(AObject: TInstantObject;
      ConflictAction: TInstantConflictAction): Boolean; override;
    function InternalRetrieveObject(AObject: TInstantObject;
      const AObjectId: String;
      ConflictAction: TInstantConflictAction;
      const AObjectData: TInstantAbstractObjectData = nil): Boolean;  override;
    function InternalStoreObject(AObject: TInstantObject;
      ConflictAction: TInstantConflictAction): Boolean;  override;
  public
    property MockManager: TUbMockObject read FMock write SetMock;
    constructor Create(AConnector: TInstantConnector); override;
    destructor Destroy; override;
  end;

  TInstantMockRelationalConnector = class(TInstantRelationalConnector)
  private
    FBrokerClass: TInstantBrokerClass;
    FMock: TUbMockObject;
    procedure SetBrokerClass(const Value: TInstantBrokerClass);
  protected
    function CreateBroker: TInstantBroker; override;
    procedure InternalCommitTransaction; override;
    procedure InternalConnect; override;
    function InternalCreateScheme(Model: TInstantModel): TInstantScheme; override;
    procedure InternalDisconnect; override;
    procedure InternalRollbackTransaction; override;
    procedure InternalStartTransaction; override;
    procedure SetMock(const Value: TUbMockObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function ConnectionDefClass: TInstantConnectionDefClass; override;
    property BrokerClass: TInstantBrokerClass read FBrokerClass write
        SetBrokerClass;
    property MockManager: TUbMockObject read FMock write SetMock;
  end;

  TInstantMockSQLBroker = class(TInstantSQLBroker)
  private
    FMock: TUbMockObject;
    procedure SetMock(const Value: TUbMockObject);
  protected
    procedure AssignDataSetParams(DataSet : TDataSet; AParams: TParams;
      OnAssignParamValue: TAssignParamValue = nil); override;
    function CreateDataSet(const AStatement: string; AParams: TParams = nil;
      OnAssignParamValue: TAssignParamValue = nil):
        TDataSet; override;
    function CreateResolver(Map: TInstantAttributeMap): TInstantSQLResolver;
        override;
    function EnsureResolver(Map: TInstantAttributeMap): TInstantCustomResolver;
        override;
    function InternalDisposeObject(AObject: TInstantObject; ConflictAction:
        TInstantConflictAction): Boolean; override;
    function InternalRetrieveObject(AObject: TInstantObject; const AObjectId:
        String; ConflictAction: TInstantConflictAction;
        const AObjectData: TInstantAbstractObjectData = nil): Boolean; override;
    function InternalStoreObject(AObject: TInstantObject; ConflictAction:
        TInstantConflictAction): Boolean; override;
  public
    constructor Create(AConnector: TInstantConnector); override;
    destructor Destroy; override;
    function DataTypeToColumnType(DataType: TInstantDataType; Size: Integer):
        string; override;
    property MockManager: TUbMockObject read FMock write SetMock;
  end;

  TInstantMockDataset = class(TDataSet)
  private
    FMock: TUbMockObject;
    procedure SetMock(const Value: TUbMockObject);
  protected
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean):
        TGetResult; override;
    procedure InternalClose; override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    function IsCursorOpen: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property MockManager: TUbMockObject read FMock write SetMock;
  end;

implementation

function CaToStr(ConflictAction: TInstantConflictAction): string;
begin
  if ConflictAction = caIgnore then
    Result := 'caIgnore'
  else if ConflictAction = caFail then
    Result := 'caFail'
  else
    Result := '???';
end;

{ TMockIConnector }

class function TInstantMockConnector.ConnectionDefClass: TInstantConnectionDefClass;
begin
  result := TInstantMockConnectionDef;
end;

constructor TInstantMockConnector.Create(AOwner: TComponent);
begin
  inherited;
  FMock := TUbMockObject.Create;
end;

function TInstantMockConnector.CreateBroker: TInstantBroker;
begin
  if not Assigned(FBrokerClass) then
     raise  Exception.Create('Undefined BrokerClass');
  FMock.AddExpectation('CreateBroker ' + FBrokerClass.ClassName);
  Result := FBrokerClass.Create(Self);
end;

{ TInstantMockConnectionDef }

class function TInstantMockConnectionDef.ConnectionTypeName: String;
begin
  result := 'Mock';
end;

class function TInstantMockConnectionDef.ConnectorClass: TInstantConnectorClass;
begin
  result := TInstantMockConnector;
end;

function TInstantMockConnectionDef.Edit: Boolean;
begin
  result := True; //boh??
end;

{ TInstantMockBroker }

destructor TInstantMockConnector.Destroy;
begin
  FMock.Free;
  inherited;
end;

procedure TInstantMockConnector.InternalCommitTransaction;
begin
  inherited;
  FMock.AddExpectation('InternalCommitTransaction');
end;

procedure TInstantMockConnector.InternalConnect;
begin
  FMock.AddExpectation('InternalConnect');
end;

function TInstantMockConnector.InternalCreateScheme(
  Model: TInstantModel): TInstantScheme;
begin
  FMock.AddExpectation('InternalCreateScheme');
  Result := TInstantScheme.Create;
  Result.Catalog := TInstantModelCatalog.Create(Result, Model);
end;

procedure TInstantMockConnector.InternalDisconnect;
begin
  FMock.AddExpectation('InternalDisconnect');
end;

procedure TInstantMockBroker.SetMock(const Value: TUbMockObject);
begin
  FMock := Value;
end;

constructor TInstantMockBroker.Create(AConnector: TInstantConnector);
begin
  inherited;
  FMock := TUbMockObject.Create;
end;

destructor TInstantMockBroker.Destroy;
begin
  FMock.Free;
  inherited;
end;

{ TInstantMockBroker }

function TInstantMockBroker.InternalDisposeObject(AObject: TInstantObject;
  ConflictAction: TInstantConflictAction): Boolean;
begin
  Result := True;
  FMock.AddExpectation('InternalDisposeObject ' + AObject.Id);
end;

function TInstantMockBroker.InternalRetrieveObject(AObject: TInstantObject;
  const AObjectId: string;
  ConflictAction: TInstantConflictAction;
  const AObjectData: TInstantAbstractObjectData = nil): Boolean;
begin
  Result := True;
  FMock.AddExpectation('InternalRetrieveObject ' + AObjectId);
end;

function TInstantMockBroker.InternalStoreObject(AObject: TInstantObject;
  ConflictAction: TInstantConflictAction): Boolean;
begin
  Result := True;
  FMock.AddExpectation('InternalStoreObject ' + AObject.Id);
end;

procedure TInstantMockConnector.InternalRollbackTransaction;
begin
  inherited;
  FMock.AddExpectation('InternalRollbackTransaction');
end;

procedure TInstantMockConnector.InternalStartTransaction;
begin
  inherited;
  FMock.AddExpectation('InternalStartTransaction');
end;

procedure TInstantMockConnector.SetBrokerClass(
  const Value: TInstantBrokerClass);
begin
  FBrokerClass := Value;
end;

procedure TInstantMockConnector.SetMock(const Value: TUbMockObject);
begin
  FMock := Value;
end;

{ TInstantMockCRBroker }

constructor TInstantMockCRBroker.Create(AConnector: TInstantConnector);
begin
  inherited;
  FMock := TUbMockObject.Create;
end;

destructor TInstantMockCRBroker.Destroy;
begin
  FMock.Free;
  inherited;
end;

function TInstantMockCRBroker.EnsureResolver(
  Map: TInstantAttributeMap): TInstantCustomResolver;
begin
  MockManager.AddExpectation('EnsureResolver');
  Result := nil;
end;

function TInstantMockCRBroker.InternalDisposeObject(
  AObject: TInstantObject;
  ConflictAction: TInstantConflictAction): Boolean;
begin
  Result := True;
  MockManager.AddExpectation('InternalDisposeObject ' + CaToStr(ConflictAction) + ' ' + AObject.Id);
end;

function TInstantMockCRBroker.InternalRetrieveObject(
  AObject: TInstantObject; const AObjectId: String;
  ConflictAction: TInstantConflictAction;
  const AObjectData: TInstantAbstractObjectData = nil): Boolean;
begin
  Result := True;
  MockManager.AddExpectation('InternalRetrieveObject ' + CaToStr(ConflictAction) + ' ' + AObjectId);
end;

function TInstantMockCRBroker.InternalStoreObject(AObject: TInstantObject;
  ConflictAction: TInstantConflictAction): Boolean;
begin
  Result := True;
  MockManager.AddExpectation('InternalStoreObject ' + CaToStr(ConflictAction) + ' ' + AObject.Id);
end;

procedure TInstantMockCRBroker.SetMock(const Value: TUbMockObject);
begin
  FMock := Value;
end;

constructor TInstantMockRelationalConnector.Create(AOwner: TComponent);
begin
  inherited;
  FMock := TUbMockObject.Create;
end;

{ TInstantMockBroker }

destructor TInstantMockRelationalConnector.Destroy;
begin
  FMock.Free;
  inherited;
end;

{ TMockIConnector }

class function TInstantMockRelationalConnector.ConnectionDefClass:
    TInstantConnectionDefClass;
begin
  result := TInstantMockConnectionDef;
end;

function TInstantMockRelationalConnector.CreateBroker: TInstantBroker;
begin
  if not Assigned(FBrokerClass) then
     raise  Exception.Create('Undefined BrokerClass');
  FMock.AddExpectation('CreateBroker ' + FBrokerClass.ClassName);
  Result := FBrokerClass.Create(Self);
end;

procedure TInstantMockRelationalConnector.InternalCommitTransaction;
begin
  inherited;
  FMock.AddExpectation('InternalCommitTransaction');
end;

procedure TInstantMockRelationalConnector.InternalConnect;
begin
  FMock.AddExpectation('InternalConnect');
end;

function TInstantMockRelationalConnector.InternalCreateScheme(Model:
    TInstantModel): TInstantScheme;
begin
  FMock.AddExpectation('InternalCreateScheme');
  Result := TInstantScheme.Create;
  Result.Catalog := TInstantModelCatalog.Create(Result, Model);
end;

procedure TInstantMockRelationalConnector.InternalDisconnect;
begin
  FMock.AddExpectation('InternalDisconnect');
end;

procedure TInstantMockRelationalConnector.InternalRollbackTransaction;
begin
  inherited;
  FMock.AddExpectation('InternalRollbackTransaction');
end;

procedure TInstantMockRelationalConnector.InternalStartTransaction;
begin
  inherited;
  FMock.AddExpectation('InternalStartTransaction');
end;

procedure TInstantMockRelationalConnector.SetBrokerClass(const Value:
    TInstantBrokerClass);
begin
  FBrokerClass := Value;
end;

procedure TInstantMockRelationalConnector.SetMock(const Value: TUbMockObject);
begin
  FMock := Value;
end;

{ TInstantMockCRBroker }

constructor TInstantMockSQLBroker.Create(AConnector: TInstantConnector);
begin
  inherited;
  FMock := TUbMockObject.Create;
end;

destructor TInstantMockSQLBroker.Destroy;
begin
  FMock.Free;
  inherited;
end;

procedure TInstantMockSQLBroker.AssignDataSetParams(DataSet : TDataSet;
  AParams: TParams; OnAssignParamValue: TAssignParamValue);
begin
  MockManager.AddExpectation('AssignDataSetParams');
end;

function TInstantMockSQLBroker.CreateDataSet(const AStatement: string; AParams: TParams = nil;
  OnAssignParamValue: TAssignParamValue = nil): TDataSet;
begin
  MockManager.AddExpectation('CreateDataSet');
  Result := TInstantMockDataset.Create(nil);
end;

function TInstantMockSQLBroker.CreateResolver(Map: TInstantAttributeMap):
    TInstantSQLResolver;
begin
  MockManager.AddExpectation('CreateResolver');
  Result := nil;
end;

function TInstantMockSQLBroker.DataTypeToColumnType(DataType: TInstantDataType;
    Size: Integer): string;
begin
  MockManager.AddExpectation('DataTypeToColumnType');
  Result := '';
end;

function TInstantMockSQLBroker.EnsureResolver(Map: TInstantAttributeMap):
    TInstantCustomResolver;
begin
  MockManager.AddExpectation('EnsureResolver');
  Result := nil;
end;

function TInstantMockSQLBroker.InternalDisposeObject(AObject: TInstantObject;
    ConflictAction: TInstantConflictAction): Boolean;
begin
  Result := True;
  MockManager.AddExpectation('InternalDisposeObject ' + CaToStr(ConflictAction) + ' ' + AObject.Id);
end;

function TInstantMockSQLBroker.InternalRetrieveObject(AObject: TInstantObject;
    const AObjectId: String; ConflictAction: TInstantConflictAction;
    const AObjectData: TInstantAbstractObjectData = nil): Boolean;
begin
  Result := True;
  MockManager.AddExpectation('InternalRetrieveObject ' + CaToStr(ConflictAction) + ' ' + AObjectId);
end;

function TInstantMockSQLBroker.InternalStoreObject(AObject: TInstantObject;
    ConflictAction: TInstantConflictAction): Boolean;
begin
  Result := True;
  MockManager.AddExpectation('InternalStoreObject ' + CaToStr(ConflictAction) + ' ' + AObject.Id);
end;

procedure TInstantMockSQLBroker.SetMock(const Value: TUbMockObject);
begin
  FMock := Value;
end;

{ TInstantMockCRBroker }

constructor TInstantMockDataset.Create(AOwner: TComponent);
begin
  inherited;
  FMock := TUbMockObject.Create;
end;

destructor TInstantMockDataset.Destroy;
begin
  FMock.Free;
  inherited;
end;

function TInstantMockDataset.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
    DoCheck: Boolean): TGetResult;
begin
  MockManager.AddExpectation('GetRecord');
  Result := grError;
end;

procedure TInstantMockDataset.InternalClose;
begin
  MockManager.AddExpectation('InternalClose');
end;

procedure TInstantMockDataset.InternalHandleException;
begin
  raise Exception.Create('Exception raised in InternalHandleException');
end;

procedure TInstantMockDataset.InternalInitFieldDefs;
begin
  raise Exception.Create('Exception raised in InternalInitFieldDefs');
  MockManager.AddExpectation('InternalInitFieldDefs');
end;

procedure TInstantMockDataset.InternalOpen;
begin
  MockManager.AddExpectation('InternalOpen');
end;

function TInstantMockDataset.IsCursorOpen: Boolean;
begin
  MockManager.AddExpectation('IsCursorOpen');
  Result := False;
end;

procedure TInstantMockDataset.SetMock(const Value: TUbMockObject);
begin
  FMock := Value;
end;

initialization
  RegisterClass(TInstantMockConnectionDef);
  TInstantMockConnector.RegisterClass;

finalization
  TInstantMockConnector.UnregisterClass;

end.

