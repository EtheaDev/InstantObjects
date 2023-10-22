(*
 *   InstantObject with MARS Curiosity REST Library
 *   Primer MARS Console Demo
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
program Primer.MARS.Console;

{$APPTYPE CONSOLE}

{$I MARS.inc}

uses
  MidasLib,
  System.SysUtils,
  Web.WebReq,

  //LoggerPro Support
  LoggerPro,
  //InstantObjects Units
  Instant.Neon.Serializers in '..\..\Source\Core\Instant.Neon.Serializers.pas',

  //InstantObjects MARS Server Units
  InstantObjects.MARS.Server.Console in '..\..\Source\MARSServer\InstantObjects.MARS.Server.Console.pas',
  InstantObjects.MARS.Server.Consts in '..\..\Source\MARSServer\InstantObjects.MARS.Server.Consts.pas',
  InstantObjects.MARS.Server.WebModule in '..\..\Source\MARSServer\InstantObjects.MARS.Server.WebModule.pas' {ServerWebModule: TWebModule},
  InstantObjects.MARS.Server.Resources.Base in '..\..\Source\MARSServer\InstantObjects.MARS.Server.Resources.Base.pas',
  InstantObjects.MARS.Server.Resources in '..\..\Source\MARSServer\InstantObjects.MARS.Server.Resources.pas',
  InstantObjects.MARS.Server.Exceptions in '..\..\Source\MARSServer\InstantObjects.MARS.Server.Exceptions.pas',
  InstantObjects.MARS.Server.Resources.Utils in '..\..\Source\MARSServer\InstantObjects.MARS.Server.Resources.Utils.pas',
  InstantObjects.MARS.Server.Resources.Config in '..\..\Source\MARSServer\InstantObjects.MARS.Server.Resources.Config.pas',
  InstantObjects.MARS.Server.LoggerPro.Config in '..\..\Source\MARSServer\InstantObjects.MARS.Server.LoggerPro.Config.pas',
  InstantObjects.Neon.MessageBodyWriters in '..\..\Source\MARSServer\InstantObjects.Neon.MessageBodyWriters.pas',
  InstantObjects.Neon.MessageBodyReaders in '..\..\Source\MARSServer\InstantObjects.Neon.MessageBodyReaders.pas',
  InstantObjects.MARS.Data in '..\..\Source\MARSServer\InstantObjects.MARS.Data.pas',
  InstantObjects.MARS.InjectionService in '..\..\Source\MARSServer\InstantObjects.MARS.InjectionService.pas',
  Delphi.Mocks.Helpers in '..\..\Source\MARSServer\Delphi.Mocks.Helpers.pas',

  //Primer MARS Server Units
  Primer.MARS.Server.Resources.User in 'Primer.MARS.Server.Resources.User.pas',
  Primer.MARS.Server.Resources.Token in 'Primer.MARS.Server.Resources.Token.pas',
  Server.Ignition in 'Server.Ignition.pas',

  //Primer Model
  Model in '..\PrimerCross\Model\Model.pas';

{$R *.res}

{$R *.mdr} {Model}

begin
  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    RunServer();
  except
    on E:Exception do
      Log.ErrorFmt('%s (%s)',[E.Message, E.Classname], '');
  end;
end.
