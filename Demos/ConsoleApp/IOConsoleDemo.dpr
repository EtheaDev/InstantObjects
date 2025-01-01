(*
 *   InstantObjects - IOConsoleDemo
 *   Using InstantObjects without user interface
 *
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
 * The Initial Developer of the Original Code is: Carlo Barazzetta
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

program IOConsoleDemo;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Model in 'Model.pas',
  InstantPersistence,
  InstantXML;

{$R *.mdr} {Model}

var
  ApplicationPath : string;
  Connection : TXMLFilesAccessor;
  Connector : TInstantXMLConnector;
  SimpleClass : TSimpleClass;
  Id : string;
  i : integer;
begin
  ApplicationPath := ExtractFilePath(ParamStr(0));
  Try
    //In every application the mdr file is normally included into the applications
    //by InstantObjects.

    //You can generate it at run-time calling, for example:
    //CreateInstantModel;

    //You read it from disk, for example:
    //InstantModel.LoadFromFile(ApplicationPath+'MinimalModel.xml');

    //Connect to database
    Connection := nil;
    Connector := nil;
    Try
      Connection := TXMLFilesAccessor.Create(nil);
      Connection.RootFolder := ApplicationPath+'XMLStorage';
      Connector := TInstantXMLConnector.Create(nil);
      Connector.Connection := Connection;
      Connector.LoginPrompt := False;
      Connector.IsDefault := True;
      WriteLn('Building Database structure');
      Connector.BuildDatabase;
      WriteLn('Connecting to Database.');
      Connector.Connect;

      for i := 0 to 100 do
      begin
        SimpleClass := TSimpleClass.Create;
        Try
          SimpleClass.StringProperty := IntToStr(Random(MaxInt));
          SimpleClass.Store;
          Id := SimpleClass.Id;
        WriteLn(Format('Stoed Object: Id="%s"', [Id]));
        Finally
          SimpleClass.Free;
        End;

        SimpleClass := TSimpleClass.Retrieve(Id);
        Try
        WriteLn(Format('Retrieving and changing Object: Id="%s"', [Id]));
          SimpleClass.StringProperty := IntToStr(Random(MaxInt));
          SimpleClass.Store;
        Finally
          SimpleClass.Free;
        End;
(*
        WriteLn('Retrieving and deleting Object.');
        SimpleClass := TSimpleClass.Retrieve(Id);
        Try
          SimpleClass.Dispose;
        Finally
          SimpleClass.Free;
        End;
*)
      end;
      WriteLn('Disconnecting from Database.');
      Connector.Disconnect;

      WriteLn('Console Demo ended. Press "Enter" to exit.');
      readLn;
    Finally
      Connector.Free;
      Connection.Free;
    End;
    WriteLn('Done!');
  Except
    on E: Exception do WriteLn(E.Message);
  End;
end.
