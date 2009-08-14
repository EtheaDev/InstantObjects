(*
 *   InstantObjects Test Suite
 *   TestMinimalModel
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
 * The Original Code is: InstantObjects Test Suite/TestMinimalModel
 *
 * The Initial Developer of the Original Code is: Uberto Barbini
 *
 * Portions created by the Initial Developer are Copyright (C) 2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Steven Mitchell
 *
 * ***** END LICENSE BLOCK ***** *)

unit TestMinimalModel;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

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
    procedure TestSaveModelMdx;
    procedure TestRestoreModelMdx;
    procedure TestStoreSimpleClass;
    procedure TestRetrieveSimpleClass;
  end;

implementation

uses
  InstantPersistence, testregistry, MinimalModel, InstantClasses,
  InstantMetadata, InstantTypes, InstantConsts;

{ TTestMinimalModel }

procedure TTestMinimalModel.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
end;

procedure TTestMinimalModel.TearDown;
begin
  InstantModel.ClassMetadatas.Clear;
  FConn.Free;
end;

procedure TTestMinimalModel.TestCreateModelMdx;
var
  InstantClassMetadata : TInstantClassMetadata;
  InstantAttributeMetadata : TInstantAttributeMetadata;
begin
//code from CreateMinimalModel;

  AssertNotNull('InstantModel',InstantModel);
  AssertNotNull('ClassMetadatas',InstantModel.ClassMetadatas);
  InstantClassMetadata := InstantModel.ClassMetadatas.Add;
  AssertNotNull('InstantClassMetadata',InstantClassMetadata);

  InstantClassMetadata.Name := 'TSimpleClass';
  InstantClassMetadata.Persistence := peStored;
  InstantClassMetadata.StorageName := 'SIMPLE';
  InstantAttributeMetadata := InstantClassMetadata.AttributeMetadatas.Add;
  AssertNotNull('InstantAttributeMetadata',InstantAttributeMetadata);

  InstantAttributeMetadata.Name := 'StringProperty';
  InstantAttributeMetadata.AttributeType := atString;
  InstantAttributeMetadata.IsIndexed := FALSE;
  InstantAttributeMetadata.IsRequired := FALSE;
  InstantAttributeMetadata.Size := 10;
  InstantAttributeMetadata.StorageName := 'STRING';

  AssertNotNull('ClassMetadatas.Find',InstantModel.ClassMetadatas.Find('TSimpleClass'));
end;

procedure TTestMinimalModel.TestSaveModelMdx;
var
  Stream: TStringStream;
  s, r: string;
  LSaveIndentation: Byte;
begin
  CreateMinimalModel;
{$IFNDEF FPC}
//delphi and fpc differs only in property order
  r := '<TInstantClassMetadatas><TInstantClassMetadata><Name>TSimpleClass</Name>';
  r := r + '<Persistence>peStored</Persistence><StorageName>SIMPLE</StorageName>';
  r := r + '<AttributeMetadatas><TInstantAttributeMetadatas><TInstantAttributeMetadata>';
  r := r + '<Name>StringProperty</Name><AttributeType>atString</AttributeType>';
  r := r + '<IsIndexed>FALSE</IsIndexed><IsRequired>FALSE</IsRequired><Size>10</Size>';
  r := r + '<StorageName>STRING</StorageName></TInstantAttributeMetadata>';
  r := r + '</TInstantAttributeMetadatas></AttributeMetadatas></TInstantClassMetadata></TInstantClassMetadatas>';
{$ELSE}
  r := '<TInstantClassMetadatas><TInstantClassMetadata><Persistence>peStored</Persistence>';
  r := r + '<StorageName>SIMPLE</StorageName><Name>TSimpleClass</Name><AttributeMetadatas>';
  r := r + '<TInstantAttributeMetadatas><TInstantAttributeMetadata><AttributeType>atString</AttributeType>';
  r := r + '<ClassMetadata.Persistence>peStored</ClassMetadata.Persistence>';
  r := r + '<ClassMetadata.StorageName>SIMPLE</ClassMetadata.StorageName>';
  r := r + '<ClassMetadata.Name>TSimpleClass</ClassMetadata.Name><Size>10</Size>';
  r := r + '<StorageName>STRING</StorageName><Name>StringProperty</Name></TInstantAttributeMetadata>';
  r := r + '</TInstantAttributeMetadatas></AttributeMetadatas></TInstantClassMetadata></TInstantClassMetadatas>';
{$ENDIF}
  s := '';

  LSaveIndentation := InstantXMLIndentationSize;
  InstantXMLIndentationSize := 0;
  try
  Stream := TStringStream.Create(s);
  try
    InstantWriteObject(Stream, sfXML, InstantModel.ClassMetadatas);
    AssertEquals(r, Stream.DataString);
  finally
    Stream.Free;
  end;
  finally
    InstantXMLIndentationSize := LSaveIndentation;
  end;
end;

procedure TTestMinimalModel.TestRestoreModelMdx;
var
  Stream: TStringStream;
  s: string;
begin
  CreateMinimalModel;
  s := '';
  Stream := TStringStream.Create(s);
  try
    InstantWriteObject(Stream, sfXML, InstantModel.ClassMetadatas);
    Stream.Position := 0;
    InstantModel.ClassMetadatas.Clear;
    AssertEquals(0, InstantModel.ClassMetadatas.Count);

    InstantReadObject(Stream, sfXML, InstantModel.ClassMetadatas);
    AssertEquals(1, InstantModel.ClassMetadatas.Count);
    AssertNotNull('ClassMetadatas.Find',InstantModel.ClassMetadatas.Find('TSimpleClass'));
  finally
    Stream.Free;
  end;
end;

procedure TTestMinimalModel.TestStoreSimpleClass;
var
  i: integer;
  SimpleClass : TSimpleClass;
begin
  CreateMinimalModel;
  FConn.IsDefault := True;
  FConn.Connect;
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
  i: integer;
  SimpleClass : TSimpleClass;
  Id : string;
begin
  CreateMinimalModel;
  FConn.IsDefault := True;
  FConn.Connect;
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
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([TTestMinimalModel]);
{$ENDIF}

end.


