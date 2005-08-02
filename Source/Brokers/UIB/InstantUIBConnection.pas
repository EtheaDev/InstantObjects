(*
 *   InstantObjects
 *   UIB Connection
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
 * The Original Code is: Andrea Petrelli
 *
 * The Initial Developer of the Original Code is: Andrea Petrelli
 *
 * Contributor(s):
 * Carlo Barazzetta, Nando Dessena
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantUIBConnection;

{$I '../../InstantDefines.inc'}

interface

uses
  Classes, DB, dbLogDlg, jvuib;

const
  UIBLoginDialog : function (const ADatabaseName: string; var AUserName, APassword: string; NameReadOnly: Boolean): Boolean = nil;

resourcestring
  SLoginPromptFailure = 'Can not find default login prompt dialog.  Please add DBLogDlg to the uses section of your main file.';

type
  TInstantUIBConnection = class(TCustomConnection)
  private
    FDatabase: TJvUIBDataBase;
    function Login: Boolean;
  protected
    procedure DoConnect; override;
    procedure DoDisconnect; override;
    function GetConnected: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Database: TJvUIBDataBase read FDatabase;
    property LoginPrompt;
    property OnLogin;
    property Connected;
    property AfterConnect;
    property AfterDisconnect;
    property BeforeConnect;
    property BeforeDisconnect;
  end;

implementation

{ TInstantUIBConnection }

constructor TInstantUIBConnection.Create(AOwner: TComponent);
begin
  inherited;
  FDatabase := TJvUIBDataBase.Create(Self);
  FDatabase.Name := 'Database';
{$IFDEF D5+}
  //oif FDatabase.SetSubComponent(True);
{$ENDIF}
end;

destructor TInstantUIBConnection.Destroy;
begin
  FDatabase.Free;
  FDatabase := nil;
  inherited;
end;

procedure TInstantUIBConnection.DoConnect;
begin
  inherited;
  if LoginPrompt then
    Login;
  FDatabase.Connected := True;
end;

procedure TInstantUIBConnection.DoDisconnect;
begin
  inherited;
  FDatabase.Connected := False;
end;

function TInstantUIBConnection.GetConnected: Boolean;
begin
  Result := assigned(FDatabase) and FDatabase.Connected;
end;

function TInstantUIBConnection.Login: Boolean;
var
  Username, Password: String;
begin
  Username := FDatabase.UserName;
  Password := FDatabase.PassWord;
  if Assigned(OnLogin) then
  begin
    result := True;
    OnLogin(Self, UserName, Password);
  end
  else
  begin
    if Assigned(UIBLoginDialog) then
      result := UIBLoginDialog(FDatabase.DatabaseName, Username, Password, False)
    else
    begin
      raise EDatabaseError.Create(SLoginPromptFailure);
    end;
    if result then
    begin
      FDatabase.UserName := Username;
      FDatabase.PassWord := Password;
    end;
  end;
end;

initialization
  UIBLoginDialog := LoginDialogEx;

end.
