unit TestModelDb;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  InstantPersistence, Model,
  InstantUIBConnection, InstantUIB, InstantXML;

type

  { TTestModel }
  TTestModel=class(TTestCase)
  private
    _Connector : TInstantConnector;
    function GetApplicationPath: string;
    function CreateContact : TContact;
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
    procedure TestselectAll;
  end;

  { TTestModelUIB }
  TTestModelUIB=class(TTestModel)
  private
    _Connection: TInstantUIBConnection;
  protected
    function CreateConnector : TInstantConnector; override;
  end;

  { TTestModelXML }
  TTestModelXML=class(TTestModel)
  private
    _Connection: TXMLFilesAccessor;
  protected
    function CreateConnector : TInstantConnector; override;
  end;

implementation

uses
  InstantClasses;

function TTestModel.CreateContact: TContact;
var
  t: TPhone;
begin
  Result := TContact.Create;
  try
//      AssertEquals('', Result.Id); why Id is yet defined?
    Result.Name := 'Mike';
    Result.Address.City := 'Milan';
    t := TPhone.Create;
    t.Name := 'Home';
    t.Number := '012 12345678';
    Result.AddPhone(t);
    AssertEquals(1, Result.PhoneCount);

    t := TPhone.Create;
    t.Name := 'Office';
    t.Number := '012 23456781';
    Result.AddPhone(t);
    AssertEquals(2, Result.PhoneCount);
  except
    FreeAndNil(Result);
  end;
end;

function TTestModel.GetApplicationPath: string;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

procedure TTestModel.SetUp;
begin
  InstantModel.LoadFromFile(ChangeFileExt(ParamStr(0),'.mdx'));
  //Connect to database
  _Connector := CreateConnector;
  _Connector.BlobStreamFormat := sfXml;
  _Connector.IsDefault := True;
//  _Connector.BuildDatabase(InstantModel);
  _Connector.Connect;
end;

procedure TTestModel.TearDown;
begin
  if assigned(_Connector) and _Connector.Connected then
    _Connector.Disconnect;
  FreeAndNil(_Connector);  //free the connection too
end;

procedure TTestModel.TestAll;
var
  c: TContact;
  id, old_id: string;
  i : integer;
  s: string;
  t : TPhone;
begin
  old_id := '';
  for i := 0 to 10 do
  begin
    c := CreateContact;
    try
      c.Store;
      id := c.id;
      AssertFalse(old_id = id);
      old_id := id; //different from previous one
    finally
      FreeAndNil(c);
    end;
    AssertNull(c);

    c := TContact.Retrieve(id);
    try
      AssertEquals(id, c.Id);
      AssertNotNull(c.Address);
      AssertEquals('Milan', c.Address.City);
      AssertEquals(2, c.PhoneCount);
      AssertEquals('012 12345678', c.MainPhoneNumber);
      c.Phones[0].Number := '012 12345679';
      c.Address.City := 'Rome';
      t := TPhone.Create;
      t.Name := 'Mobile';
      t.Number := '333-445556666';
      c.AddPhone(t);
      AssertEquals(3, c.PhoneCount);
      c.Store();
    finally
      FreeAndNil(c);
    end;
    AssertNull(c);

// Retrieving and deleting Object.
    c := TContact.Retrieve(id);
    try
      AssertEquals('Rome', c.Address.City);
      AssertEquals(3, c.PhoneCount);
      c.Dispose;
    finally
      FreeAndNil(c);
    end;
    AssertNull(c);

// Trying to retrieve deleted object
    c := TContact.Retrieve(id);
    AssertNull(c);
  end;
end;

procedure TTestModel.TestConnected;
begin
  AssertTrue(_Connector.Connected);
  _Connector.Disconnect;
  AssertFalse(_Connector.Connected);
end;

function TTestModelUIB.CreateConnector : TInstantConnector;
var
  Connector : TInstantUIBConnector;
begin
  //Connect to database
  _Connection := TInstantUIBConnection.Create(nil);
  _Connection.Database.DatabaseName :=  ChangeFileExt(ParamStr(0),'.FDB');
  _Connection.Database.UserName := 'SYSDBA';
  _Connection.Database.Password := 'a';
  Connector := TInstantUIBConnector.Create(nil);
  Connector.Connection := _Connection;
  Connector.LoginPrompt := False;
  Result := Connector;
end;

{ TTestModelXML }

function TTestModelXML.CreateConnector: TInstantConnector;
var
  Connector : TInstantXMLConnector;
begin
  _Connection :=  TXMLFilesAccessor.Create(nil);
  _Connection.RootFolder :=  ApplicationPath + 'XMLModel'+PathDelim;
//  _Connection.XMLFileFormat := xffIso;
  Connector := TInstantXMLConnector.Create(nil);
  Connector.Connection := _Connection;
  Connector.BuildDatabase;
  Result := Connector;
end;

procedure TTestModel.TestselectAll;
var
  InstantQuery : TInstantQuery;
  IO, IO2 : TInstantObject;
  Id : string;
  I : integer;
begin
  InstantQuery := _Connector.CreateQuery;
  Try
    InstantQuery.Command := 'SELECT * FROM ANY TContact';
    InstantQuery.Open;
    Try
      for i := 0 to InstantQuery.ObjectCount-1 do
      begin
        IO := InstantQuery.Objects[i] as TInstantObject;
        AssertEquals(IO.RefCount,1);
        IO2 := TContact.Retrieve(IO.Id);
        Try
          AssertEquals(IO.RefCount,2);
        Finally
          IO2.Free;
        End;
        AssertEquals(IO.RefCount,1);
      end;
    Finally
      Instantquery.Close;
    End;
  Finally
    InstantQuery.Free;
  End;
end;

procedure TTestModel.TestStoring;
var
  i : integer;
  c : TContact;
  old_id, id : string;
begin
  old_id := '';
  for i := 0 to 10 do
  begin
    c := CreateContact;
    try
      c.Store;
      id := c.id;
      AssertFalse(old_id = id);
      old_id := id; //different from previous one
    finally
      FreeAndNil(c);
    end;
    AssertNull(c);
  end;
end;

initialization

  RegisterTest(TTestModelUIB);
  RegisterTest(TTestModelXML);
end.

