(*
 *   InstantObject with WiRL REST Library
 *   Primer WiRL Console Demo
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
program Primer.WiRL.Console;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  InstantObjects.Neon.MessageBodyProvider in '..\..\Source\WiRLServer\InstantObjects.Neon.MessageBodyProvider.pas',
  Instant.Neon.Serializers in '..\..\Source\Core\Instant.Neon.Serializers.pas',
  Primer.WiRL.Server.Console in 'Primer.WiRL.Server.Console.pas',
  Primer.WiRL.Server.Listener in 'Primer.WiRL.Server.Listener.pas',
  Primer.WiRL.Server.Filters.Logger in 'Primer.WiRL.Server.Filters.Logger.pas',
  Primer.WiRL.Server.Filters.Environment in 'Primer.WiRL.Server.Filters.Environment.pas',
  Primer.WiRL.Server.Claims in 'Primer.WiRL.Server.Claims.pas',
  Primer.WiRL.Server.Resources.User in 'Primer.WiRL.Server.Resources.User.pas',
  Model in '..\PrimerCross\Model\Model.pas',
  InstantObjects.WiRL.Data in '..\..\Source\WiRLServer\InstantObjects.WiRL.Data.pas',
  InstantObjects.WiRL.Server.Consts in '..\..\Source\WiRLServer\InstantObjects.WiRL.Server.Consts.pas',
  InstantObjects.WiRL.Server.Exceptions in '..\..\Source\WiRLServer\InstantObjects.WiRL.Server.Exceptions.pas',
  InstantObjects.WiRL.Server.Resources.Base in '..\..\Source\WiRLServer\InstantObjects.WiRL.Server.Resources.Base.pas',
  Primer.WiRL.Server.Resources.Config in 'Primer.WiRL.Server.Resources.Config.pas',
  InstantObjects.WiRL.Server.Resources in '..\..\Source\WiRLServer\InstantObjects.WiRL.Server.Resources.pas',
  InstantObjects.WiRL.Server.Resources.Utils in '..\..\Source\WiRLServer\InstantObjects.WiRL.Server.Resources.Utils.pas',
  Delphi.Mocks.Helpers in '..\..\Source\WiRLServer\Delphi.Mocks.Helpers.pas';

{$R *.mdr} {Model}

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  TConsole.Run;
end.
