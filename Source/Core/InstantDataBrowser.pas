unit InstantDataBrowser;

interface

uses
  SysUtils, System.Classes,
 DB
  , Data.DBClient,
  InstantPersistence;

type
  TInstantDataBrowser = class(TCustomClientDataSet)
  private
    FParamCheck: Boolean;
    FMaxCount: Integer;
    FCommand: TStringList;
    FConnector: TInstantConnector;
    FParams: TParams;
    FQuery: TInstantQuery;
    FObjectClassName: string;
    procedure CommandChanged(Sender: TObject);
    function GetCommand: TStringList;
    function GetConnector: TInstantConnector;
    function GetParams: TParams;
    function GetQuery: TInstantQuery;
    procedure SetCommand(const Value: TStringList);
    procedure SetConnector(const Value: TInstantConnector);
    procedure SetMaxCount(const Value: Integer);
    procedure SetParams(const Value: TParams);
    procedure SetObjectClassName(const Value: string);
    procedure DestroyQuery;
    function HasConnector: Boolean;
  protected
    procedure InternalReset; virtual;
    procedure UpdateParams; virtual;
    property Query: TInstantQuery read GetQuery;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Reset;
  published
    property Command: TStringList read GetCommand write SetCommand;
    property Connector: TInstantConnector read GetConnector write SetConnector;
    property MaxCount: Integer read FMaxCount write SetMaxCount default 0;
    property ObjectClassName: string read FObjectClassName write SetObjectClassName;
    property ParamCheck: Boolean read FParamCheck write FParamCheck default True;
    property Params: TParams read GetParams write SetParams stored False;
  end;

implementation
  
{ TInstantDataBrowser }

procedure TInstantDataBrowser.CommandChanged(Sender: TObject);
begin
  if csReading in ComponentState then
    Exit;
  Query.Command := Command.Text;
  if ParamCheck or (csDesigning in ComponentState) then
    UpdateParams;
  Reset;
end;

constructor TInstantDataBrowser.Create(AOwner: TComponent);
begin
  inherited;
  ParamCheck := True;
end;

destructor TInstantDataBrowser.Destroy;
begin
  DestroyQuery;
  FParams.Free;
  FCommand.Free;
  inherited;
end;

procedure TInstantDataBrowser.DestroyQuery;
begin
  FreeAndNil(FQuery);
end;

function TInstantDataBrowser.GetCommand: TStringList;
begin
  if not Assigned(FCommand) then
  begin
    FCommand := TStringList.Create;
    FCommand.OnChange := CommandChanged;
  end;
  Result := FCommand;
end;

function TInstantDataBrowser.GetConnector: TInstantConnector;
begin
  if Assigned(FConnector) then
    Result := FConnector
  else
  {$IFNDEF MARS_FIREDAC}
    Result := InstantDefaultConnector;
  {$ELSE}
    Result := nil;
  {$ENDIF}
end;

function TInstantDataBrowser.GetParams: TParams;
begin
  if not Assigned(FParams) then
    FParams := TParams.Create(Self);
  Result := FParams;
end;

function TInstantDataBrowser.GetQuery: TInstantQuery;
begin
  if not Assigned(FQuery) then
  begin
    if (csDesigning in ComponentState) or not HasConnector then
      FQuery := TInstantQuery.Create(nil)
    else
      FQuery := Connector.CreateQuery;
    FQuery.MaxCount := MaxCount;
    if not (csReading in ComponentState) then
      FQuery.Command := Command.Text;
  end;
  Result := FQuery;
end;

function TInstantDataBrowser.HasConnector: Boolean;
begin
  Result := Assigned(Connector);
end;

procedure TInstantDataBrowser.InternalReset;
var
  WasActive: Boolean;
begin
  WasActive := Active;
  Close;
  try
    FieldDefs.Clear;
    FieldDefs.Updated := False;
  finally
    if WasActive then
      Open;
  end;
end;

procedure TInstantDataBrowser.Reset;
begin
  DisableControls;
  try
    InternalReset;
  finally
    EnableControls;
  end;
end;

procedure TInstantDataBrowser.SetCommand(const Value: TStringList);
begin
  if Value.Text <> Command.Text then
    Command.Assign(Value);
end;

procedure TInstantDataBrowser.SetConnector(const Value: TInstantConnector);
begin
  if Value <> FConnector then
  begin
    if Assigned(FConnector) then
      FConnector.RemoveFreeNotification(Self);
    FConnector := Value;
    if Assigned(FConnector) then
      FConnector.FreeNotification(Self);
    DestroyQuery;
  end;
end;

procedure TInstantDataBrowser.SetMaxCount(const Value: Integer);
begin
  if Value <> MaxCount then
  begin
    FMaxCount := Value;
    Query.MaxCount := FMaxCount;
  end;
end;

procedure TInstantDataBrowser.SetObjectClassName(const Value: string);
begin
  FObjectClassName := Value;
end;

procedure TInstantDataBrowser.SetParams(const Value: TParams);
begin
  Params.AssignValues(Value);
end;

procedure TInstantDataBrowser.UpdateParams;
var
  AParams: TParams;
begin
  AParams := TParams.Create(Self);
  try
    Query.FetchParams(Command.Text, AParams);
    AParams.AssignValues(Params);
    Params.Clear;
    Params.Assign(AParams);
  finally
    AParams.Free;
  end;
end;

end.
