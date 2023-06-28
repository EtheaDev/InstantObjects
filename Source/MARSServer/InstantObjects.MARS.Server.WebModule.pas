(*
 *   InstantObject with MARS Curiosity REST Library
 *   Server.WebModule
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
 * Carlo Barazzetta, Nicola Tambascia
 *
 * ***** END LICENSE BLOCK ***** *)
unit InstantObjects.MARS.Server.WebModule;

{$I MARS.inc}

interface

uses
  System.SysUtils
  , System.Classes
  , Web.HTTPApp;

type

  TServerWebModule = class(TWebModule)
    procedure ServerWebModuleDefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    //FdmConnectionManager: TdmConnectionManager;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TServerWebModule;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

uses
  MARS.http.Server.Indy
  , Server.Ignition;

procedure TServerWebModule.ServerWebModuleDefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  inherited;
  if not TServerEngine.Default.HandleRequest(TMARSWebRequest.Create(Request), TMARSWebResponse.Create(Response)) then
  begin
    Response.ContentType := 'application/json';
    Response.Content :=
      '{"success": false, "details": '
      + '{'
        + '"error": "Request not found",'
        + '"pathinfo": "' + Request.PathInfo + '"'
      + '}'
    + '}';
  end
  else
    Handled := True;
end;

procedure TServerWebModule.WebModuleCreate(Sender: TObject);
begin
  ;
end;

procedure TServerWebModule.WebModuleDestroy(Sender: TObject);
begin
  ;
end;

end.
