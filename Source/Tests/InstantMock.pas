unit InstantMock;

interface

uses
  InstantPersistence, Classes, UbMockObject;

type
  TInstantMockConnector = class(TInstantConnector, IUbMockObject)
  private
    FMock: TUbMockObject;
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
    property MockManager: TUbMockObject read FMock write SetMock implements IUbMockObject;
    class function ConnectionDefClass: TInstantConnectionDefClass; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;
  
  TInstantMockConnectionDef = class(TInstantConnectionDef)
    class function ConnectionTypeName: String; override;
    class function ConnectorClass: TInstantConnectorClass; override;
    function Edit: Boolean; override;
  end;

  TInstantMockBroker = class(TInstantBroker, IUbMockObject)
  private
    FMock: TUbMockObject;
    procedure SetMock(const Value: TUbMockObject);
  protected
    function InternalDisposeObject(AObject: TInstantObject;
      ConflictAction: TInstantConflictAction): Boolean; override;
    function InternalRetrieveObject(AObject: TInstantObject;
      const AObjectId: string;
      ConflictAction: TInstantConflictAction): Boolean;  override;
    function InternalStoreObject(AObject: TInstantObject;
      ConflictAction: TInstantConflictAction): Boolean;  override;
  public
    property MockManager: TUbMockObject read FMock write SetMock implements IUbMockObject;
    constructor Create(AConnector: TInstantConnector); override;
    destructor Destroy; override;
  end;

implementation

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
  FMock.AddExpectation('CreateBroker');
  Result := TInstantMockBroker.Create(Self);
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
  result := TInstantScheme.Create(Model);
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
  ConflictAction: TInstantConflictAction): Boolean;
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

procedure TInstantMockConnector.SetMock(const Value: TUbMockObject);
begin
  FMock := Value;
end;

initialization
  RegisterClass(TInstantMockConnectionDef);
  TInstantMockConnector.RegisterClass;

finalization
  TInstantMockConnector.UnregisterClass;

end.

