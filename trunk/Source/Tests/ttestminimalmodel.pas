(*
 *   InstantObjects
 *   TTestMinimalModel
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
 * Uberto Barbini
 *
 * ***** END LICENSE BLOCK ***** *)

unit TTestMinimalModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, InstantMock;

type

  { TTestSimpleModel }

  TTestMinimalModel = class(TTestCase)
  protected
    FConn: TInstantMockConnector;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreateModelMdx;
    procedure TestStoreSimpleClass;
    procedure TestRetrieveSimpleClass;
  end;

implementation

uses
  InstantPersistence, testregistry, MinimalModel;

{ TTestMinimalModel }

procedure TTestMinimalModel.SetUp;
begin
  inherited;
  FConn := TInstantMockConnector.Create(nil);
end;

procedure TTestMinimalModel.TearDown;
begin
  FConn.Free;
  inherited;
end;

procedure TTestMinimalModel.TestCreateModelMdx;
var
  InstantClassMetadata : TInstantClassMetadata;
  InstantAttributeMetadata : TInstantAttributeMetadata;
begin
//codice da  CreateMinimalModel;
  InstantModel.ClassMetadatas.Create(InstantModel);
  AssertNotNull('!',InstantModel);
  InstantClassMetadata := InstantModel.ClassMetadatas.Add;
  AssertNotNull('!!',InstantClassMetadata);
  InstantClassMetadata.Name := 'TSimpleClass';
  InstantClassMetadata.Persistence := peStored;
  InstantClassMetadata.StorageName := 'SIMPLE';
  InstantAttributeMetadata := InstantClassMetadata.AttributeMetadatas.Add;
  AssertNotNull('!!!',InstantAttributeMetadata);
  InstantAttributeMetadata.Name := 'StringProperty';
  InstantAttributeMetadata.AttributeType := atString;
  InstantAttributeMetadata.IsIndexed := FALSE;
  InstantAttributeMetadata.IsRequired := FALSE;
  InstantAttributeMetadata.Size := 10;
  InstantAttributeMetadata.StorageName := 'STRING';


  AssertNotNull('1',InstantModel);
  AssertNotNull('11',InstantModel.ClassMetadatas);
  AssertNotNull('111',InstantModel.ClassMetadatas.Find('TSimpleClass'));
end;

procedure TTestMinimalModel.TestStoreSimpleClass;
var
  conn: TInstantMockConnector;
  i: integer;
  SimpleClass : TSimpleClass;
begin
  CreateMinimalModel;
  conn := TInstantMockConnector.Create(nil);
  conn.IsDefault := True;
  conn.BrokerClass := TInstantMockBroker;
  conn.Connect;
  for i := 0 to 100 do
  begin
    SimpleClass := TSimpleClass.Create;
    try
      SimpleClass.StringProperty := IntToStr(Random(MaxInt));
      SimpleClass.Store;
      AssertTrue('Id ' + SimpleClass.Id, SimpleClass.Id <> '');
    finally
      SimpleClass.Free;
    end;
  end;
end;

procedure TTestMinimalModel.TestRetrieveSimpleClass;
var
  conn: TInstantMockConnector;
  i: integer;
  SimpleClass : TSimpleClass;
  Id : string;
begin
  CreateMinimalModel;
  conn := TInstantMockConnector.Create(nil);
  conn.IsDefault := True;
  conn.BrokerClass := TInstantMockBroker;
  conn.Connect;
  Id := '';
  for i := 0 to 100 do
  begin
    SimpleClass := TSimpleClass.Create;
    try
      SimpleClass.StringProperty := IntToStr(Random(MaxInt));
      SimpleClass.Store;
      Id := SimpleClass.Id;
    finally
      SimpleClass.Free;
    end;

    SimpleClass := TSimpleClass.Create;
    try
      SimpleClass.StringProperty := IntToStr(Random(MaxInt));
      SimpleClass.Retrieve(Id);
      AssertEquals(Id, SimpleClass.Id);
    finally
      SimpleClass.Free;
    end;
  end;
end;

(*
program IOMinimal;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Model in 'Model.pas',
  InstantPersistence;
  //,InstantUIBConnection, InstantUIB;

//{$R *.mdr} {Model}

var
  ApplicationPath : string;
  Connection : TInstantUIBConnection;
  Connector : TInstantUIBConnector;
  SimpleClass : TSimpleClass;
  Id : string;
  i : integer;
begin
  ApplicationPath := ExtractFilePath(ParamStr(0));
  Try
//    InstantModel.LoadFromFile(ApplicationPath+'MinimalModel.xml');
    CreateInstantModel;

    //Connect to database
    Connection := nil;
    Connector := nil;
    Try
      Connection := TInstantUIBConnection.Create(nil);
      Connection.Database.DatabaseName := ApplicationPath+'MINIMAL.FDB';
      Connection.Database.UserName := 'SYSDBA';
      Connection.Database.Password := 'a';
      Connector := TInstantUIBConnector.Create(nil);
      Connector.Connection := Connection;
      Connector.LoginPrompt := False;
      Connector.IsDefault := True;
      WriteLn('Connecting to Database.');
      Connector.Connect;

      for i := 0 to 100 do
      begin
        WriteLn('Storing Object.');
        SimpleClass := TSimpleClass.Create;
        Try
          SimpleClass.StringProperty := IntToStr(Random(MaxInt));
          SimpleClass.Store;
          Id := SimpleClass.Id;
        Finally
          SimpleClass.Free;
        End;

        WriteLn('Retrieving and changing Object.');
        SimpleClass := TSimpleClass.Retrieve(Id);
        Try
          SimpleClass.StringProperty := IntToStr(Random(MaxInt));
          SimpleClass.Store;
        Finally
          SimpleClass.Free;
        End;

        WriteLn('Retrieving and deleting Object.');
        SimpleClass := TSimpleClass.Retrieve(Id);
        Try
          SimpleClass.Dispose;
        Finally
          SimpleClass.Free;
        End;
      end;

      WriteLn('Disconnecting from Database.');
      Connector.Disconnect;
    Finally
      Connector.Free;
      Connection.Free;
    End;
    WriteLn('Done!');
  Except
    on E: Exception do WriteLn(E.Message);
  End;
end.

*)


initialization
  RegisterTests([TTestMinimalModel]);

end.


