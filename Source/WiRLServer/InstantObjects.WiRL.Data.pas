(*
 *   InstantObject with WiRL REST Library
 *   InstantConnector Context
 *)

unit InstantObjects.WiRL.Data;

{$I WiRL.inc}

interface

uses
  System.Classes
  , System.SysUtils
  , System.Rtti
  , System.Types
  , FireDAC.Comp.Client
  , FireDAC.Stan.Def
  , WiRL.Core.Injection
  , WiRL.Core.Context
  , WiRL.Core.GarbageCollector
  , InstantObjects.WiRL.Server.Exceptions
  , InstantPersistence
  , InstantFireDAC
;

type
  //Class to be Injected
  TWiRLInstantObjects = class
  private
    FConnectionName: string;
    FInstantFireDACConnector: TInstantFireDACConnector;
    function GetInstantFireDACConnector: TInstantFireDACConnector;
  public
    constructor Create(AConnection: TFDConnection);
    procedure ConnectToDatabase;
    property ConnectionName: string read FConnectionName;
    property Connector: TInstantFireDACConnector read GetInstantFireDACConnector;
  end;

  // The class factory is responsable to create the context.
  // It will be released by the system unless it's annotated
  // with the Singleton attribute
  TInstantObjectInjectionFactory = class(TInterfacedObject, IContextHttpFactory)
  public
    function CreateContextObject(const AObject: TRttiObject;
      AContext: TWiRLContextHttp): TValue;
  end;

implementation

uses
  WiRL.Rtti.Utils
  , WiRL.Data.FireDAC
  , InstantClasses
;

{ TWiRLInstantObjects }

procedure TWiRLInstantObjects.ConnectToDatabase;
begin
  Assert(Assigned(Connector));
  try
    Connector.Connect;
  except
    on E: Exception do
      raise EWiRLServerException.CreateError(
        http_401_Unauthorized,
        Format('Connection to database "%s" failed with error: %s',
          [FConnectionName, E.Message]));
  end;
end;

constructor TWiRLInstantObjects.Create(AConnection: TFDConnection);
begin
  inherited Create;
  FInstantFireDACConnector := TInstantFireDACConnector.Create(nil);
  try
    FInstantFireDACConnector.BlobStreamFormat := sfXML;
    FInstantFireDACConnector.Connection := AConnection;
    FInstantFireDACConnector.loginPrompt := False;
  except
    FInstantFireDACConnector.Free;
    raise;
  end;
end;

function TWiRLInstantObjects.GetInstantFireDACConnector: TInstantFireDACConnector;
begin
  Result := FInstantFireDACConnector;
end;

{ TInstantObjectInjectionFactory }

function TInstantObjectInjectionFactory.CreateContextObject(
  const AObject: TRttiObject; AContext: TWiRLContextHttp): TValue;
var
  LConnectionName: string;
  LInstance: TWiRLInstantObjects;
  LFDConnection: TFDConnection;
  LGarbageCollector: TWiRLGarbageCollector;
begin
  LGarbageCollector := AContext.GetContextDataAs<TWiRLGarbageCollector>;
  //Create a FireDAC Connection
  LConnectionName := AContext.Request.Headers['environment'];
  if LConnectionName = '' then
    LConnectionName := 'test';

  LFDConnection := TFDConnection.Create(nil);
  LGarbageCollector.AddGarbage(LFDConnection);
  LFDConnection.ConnectionDefName := LConnectionName;
  try
    if Assigned(LFDConnection) then
    begin
      LInstance := TWiRLInstantObjects.Create(LFDConnection);
      Result := LInstance;
    end
    else
    begin
      raise EWiRLServerException.CreateError(
        http_406_NotAcceptable,
        Format('Connection for %s environment not configured',[LConnectionName]));
    end;
  except
    LFDConnection.Free;
    raise;
  end;
end;

initialization
  TWiRLContextInjectionRegistry.Instance.RegisterFactory<TWiRLInstantObjects>(TInstantObjectInjectionFactory);

end.
