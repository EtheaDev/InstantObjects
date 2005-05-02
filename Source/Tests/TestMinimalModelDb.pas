unit TestMinimalModelDb;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  InstantPersistence, InstantUIBConnection, MinimalModel,
  InstantUIB, InstantXML;

type

  { TestMinimalModel }

  TTestMinimalModel=class(TTestCase)
  private
    _Connector : TInstantConnector;
    function GetApplicationPath: string;
    function CreateObject : TSimpleClass;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function CreateConnector : TInstantConnector; virtual; abstract;
  public
    property ApplicationPath : string read GetApplicationPath;
  published
    procedure TestAll;
    procedure TestConnected;
    procedure TestStoring;
  end;

  { TestMinimalModelUIB }

  TTestMinimalModelUIB=class(TTestMinimalModel)
  private
    _Connection: TInstantUIBConnection;
  protected
    function CreateConnector : TInstantConnector; override;
  end;

  { TestMinimalModelXML }

  TTestMinimalModelXML=class(TTestMinimalModel)
  private
    _ApplicationPath : string;
    _Connection: TXMLFilesAccessor;
  protected
    function CreateConnector : TInstantConnector; override;
  end;

implementation

uses
  InstantClasses;

{ TestMinimalModel }
function TTestMinimalModel.GetApplicationPath : string;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

procedure TTestMinimalModel.Setup;
begin
//  InstantModel.LoadFromFile(_ApplicationPath+'MinimalModel.xml');
  Randomize;
  CreateMinimalModel;
  _Connector := CreateConnector;
  _Connector.BlobStreamFormat := sfXML;
  _Connector.IsDefault := True;
  _Connector.Connect;
end;

function TTestMinimalModel.CreateObject : TSimpleClass;
begin
  // Create and Store Object.
  Result := TSimpleClass.Create;
  AssertEquals('', Result.Id);
  Result.StringProperty := IntToStr(Random(MaxInt));
end;

procedure TTestMinimalModel.TearDown;
begin
  if assigned(_Connector) and _Connector.Connected then
    _Connector.Disconnect;
  FreeAndNil(_Connector); //free the connection too
end;

procedure TTestMinimalModel.TestAll;
var
  SimpleObject : TSimpleClass;
  Id : string;
  i : integer;
  s: string;
begin
  for i := 0 to 10 do
  begin
// Storing Object.
   SimpleObject := CreateObject;
   Try
     SimpleObject.Store;
     Id := SimpleObject.Id;
     s := SimpleObject.StringProperty;
   Finally
     SimpleObject.Free;
   End;

// Retrieving and changing Object.
    SimpleObject := TSimpleClass.Retrieve(Id);
    Try
      AssertEquals(s, SimpleObject.StringProperty);
      SimpleObject.StringProperty := IntToStr(Random(MaxInt));
      s := SimpleObject.StringProperty;
      SimpleObject.Store;
    Finally
      SimpleObject.Free;
    End;

// Retrieving and deleting Object.
    SimpleObject := TSimpleClass.Retrieve(Id);
    Try
      AssertEquals(s, SimpleObject.StringProperty);
      SimpleObject.Dispose;
    Finally
      SimpleObject.Free;
    End;


// Trying to retrive deleted object
    SimpleObject := nil;
    SimpleObject := TSimpleClass.Retrieve(Id);
    AssertNull(SimpleObject);

  end;
end;

procedure TTestMinimalModel.TestConnected;
begin
  AssertTrue(_Connector.Connected);
  _Connector.Disconnect;
  AssertFalse(_Connector.Connected);
end;

procedure TTestMinimalModel.TestStoring;
var
  i: integer;
  SimpleObject : TSimpleClass;
begin
  for i := 0 to 10 do
  begin
    // Storing Object.
    SimpleObject := CreateObject;
    Try
     SimpleObject.Store;
    Finally
     SimpleObject.Free;
    End;
  end;
end;

{ TTestMinimalModelUIB }
function TTestMinimalModelUIB.CreateConnector : TInstantConnector;
var
  Connector : TInstantUIBConnector;
begin
  //Connect to database
  _Connection := TInstantUIBConnection.Create(nil);
  _Connection.Database.DatabaseName :=  ApplicationPath + 'MINIMAL.FDB';
  _Connection.Database.UserName := 'SYSDBA';
  _Connection.Database.Password := 'a'; //'a';
  Connector := TInstantUIBConnector.Create(nil);
  Connector.Connection := _Connection;
  Connector.LoginPrompt := False;
  Result := Connector;
end;

{ TTestMinimalModelXML }
function TTestMinimalModelXML.CreateConnector : TInstantConnector;
var
  Connector : TInstantXMLConnector;
begin
  _Connection :=  TXMLFilesAccessor.Create(nil);
  _Connection.RootFolder :=  ApplicationPath + 'XMLMinimalModel'+PathDelim;
//  _Connection.XMLFileFormat := xffIso;
  Connector := TInstantXMLConnector.Create(nil);
  Connector.Connection := _Connection;
  Connector.BuildDatabase;
  Result := Connector;
end;

initialization

  RegisterTest(TTestMinimalModelUIB);
  RegisterTest(TTestMinimalModelXML);

end.

