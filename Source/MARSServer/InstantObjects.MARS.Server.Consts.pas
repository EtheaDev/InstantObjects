(*
 *   InstantObject with MARS Curiosity REST Library
 *   Server.Resources.Base
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
 unit InstantObjects.MARS.Server.Consts;

interface

const
  PARAM_SERVICEKEY   = 'servicekey';
  PARAM_ENVIRONMENT   = 'environment';

resourcestring
{$IFDEF CBLIB_ITA}
  ID_DONT_MATCH_JSON = 'Id dell''oggetto:"%s" non corrisponde al Body.Id: "%s"';
  EMPTY_CLASSNAME = 'ClassName vuoto';
  WRONG_CLASSNAME = 'ClassName "%s" non corretta';
  MISSED_OBJECT_IN_BODY = 'Nessun oggetto presente nel Body della Request';
  OBJECT_ALREADY_EXISTS = 'L''oggetto Class: "%s" e Id: "%s" esiste già';
  OBJECT_OF_TYPE_NOT_FOUND = 'Oggetto "%s" di tipo "%s" non trovato';
  OBJECT_NOT_FOUND = 'Oggetto Classe:"%s" e Id:"%s" non trovato';
  CLASS_NOT_FOUND = 'Classe "%s" non trovata';
  INVALID_CLASS = '"%s" non è un ClassName valido';
  INCOMPATIBLE_IOCLASS = 'InstantObject Class %s incompatibile';
  IP_ADDR_NOT_WELLFORMED = 'L''indirizzo IP "%s" non è formattato correttamente';
  FIELD_NOT_SPECIFIED = 'Il campo "%s" non è stato specificato';
  FIELD_VALUE_TOO_LONG = 'Valore "%s" più lungo di %d caratteri ammessi';
  WRONG_NUMBER_FIELD = 'Il campo "%s" non è un numero';
  WRONG_INTEGER_FIELD = 'Il campo "%s" non è un numero intero';
  WRONG_AUTH = 'UserName o Password errati';
  CONNECT_OK = 'Connesso con successo!';
  CONNECT_KO = 'Errore: Impossibile connetteri al Database: "%s"';
  NOT_CONNECTED = 'Non connesso!';

  PWD_NOT_CHANGED = 'Password non modificata!';
  AUTH_FAILED = 'Autenticazione fallita';
  PWD_CHANGED = 'Password modificata con successo!';
  PWD_MIN_8_CHARS ='La password deve avere almeno 8 caratteri';
  PWD_MUST_CONTAIN_LOWER_CHAR = 'La password deve contenere almeno un carattere minuscolo';
  PWD_MUST_CONTAIN_UPPER_CHAR = 'La password deve contenere almeno un carattere maiuscolo';
  PWD_MUST_CONTAIN_NUM_CHAR = 'La password deve contenere almeno un carattere numerico';
  PWD_MUST_CONTAIN_SPECIAL_CHAR = 'La password deve contenere almeno uno tra questi caratteri speciali: !,#,%,&,*,@';

  sPortInUse = '- Errore: Porta %s già in uso';
  sPortSet = '- Porta impostata su %s';
  sServerRunning = '- Il Server è già in esecuzione';
  sStartingServer = '- Avviamento HTTP Server sulla porta %d - HTTPS Server sulla porta %d';
  sStoppingServer = '- Arresto del Server';
  sServerStopped = '- Server arrestato';
  sServerNotRunning = '- Il Server non è in esecuzione';
  sInvalidCommand = '- Errore: Comando non valido';
{$ELSE}
  ID_DONT_MATCH_JSON = 'Object Id:"%s" don''t match Body.Id: "%s"';
  EMPTY_CLASSNAME = 'Empty ClassName';
  WRONG_CLASSNAME = 'ClassName "%s" not acceptable';
  MISSED_OBJECT_IN_BODY = 'No Object present into Request Body';
  OBJECT_ALREADY_EXISTS = 'The object Class: "%s" and Id: "%s" already exists';
  OBJECT_OF_TYPE_NOT_FOUND = 'Object "%s" of type "%s" not found';
  OBJECT_NOT_FOUND = 'Object Class:"%s" and Id:"%s" not found';
  CLASS_NOT_FOUND = 'Class "%s" not found';
  INVALID_CLASS = '"%s" is not a valid ClassName';
  INCOMPATIBLE_IOCLASS = 'Incompatible InstantObject Class %s';
  IP_ADDR_NOT_WELLFORMED = 'IP Addredd "%s" is not well formed';
  FIELD_NOT_SPECIFIED = 'The field "%s" was not specified';
  FIELD_VALUE_TOO_LONG = 'Value "%s" longer than %d characters';
  WRONG_NUMBER_FIELD = 'The field "%s" is not a number';
  WRONG_INTEGER_FIELD = 'The field "%s" is not an integer number';
  WRONG_AUTH = 'Wrong UserName or Password';
  CONNECT_OK = 'Connected succesfully!';
  CONNECT_KO = 'Error: Cannot connect to Database: "%s"';
  NOT_CONNECTED = 'Not connected!';

  PWD_NOT_CHANGED = 'Password not changed!';
  AUTH_FAILED = 'Authentication failed';
  PWD_CHANGED = 'Password succesfully changed!';
  PWD_MIN_8_CHARS ='The password must have at least 8 characters';
  PWD_MUST_CONTAIN_LOWER_CHAR = 'Password must contain at least one lowercase character';
  PWD_MUST_CONTAIN_UPPER_CHAR = 'Password must contain at least one uppercase character';
  PWD_MUST_CONTAIN_NUM_CHAR = 'Password must contain at least one numeric character';
  PWD_MUST_CONTAIN_SPECIAL_CHAR = 'Password must contain at least one of these special characters: !,#,%,&,*,@';

  sPortInUse = '- Error: Port %s already in use';
  sPortSet = '- Port set to %s';
  sServerRunning = '- The Server is already running';
  sStartingServer = '- Starting HTTP Server on port %d - HTTPS Server on port %d';
  sStoppingServer = '- Stopping Server';
  sServerStopped = '- Server Stopped';
  sServerNotRunning = '- The Server is not running';
  sInvalidCommand = '- Error: Invalid Command';
{$ENDIF}

  sIndyVersion = '- Indy Version: ';
  sActive = '- Active: ';
  sPort = '- Port: ';
  sPortSSL = '- SSL Port: ';
  sSessionID = '- Session ID CookieName: ';
  sCommands = 'Enter a Command: ' + slineBreak +
    '   - "start" to start the server'+ slineBreak +
    '   - "stop" to stop the server'+ slineBreak +
    '   - "set port" to change the default port'+ slineBreak +
    '   - "status" for Server status'+ slineBreak +
    '   - "help" to show commands'+ slineBreak +
    '   - "exit" to close the application';
  sEngineParams = 'Engine parameters';

const
  cArrow = '->';
  cCommandStart = 'start';
  cCommandStop = 'stop';
  cCommandStatus = 'status';
  cCommandHelp = 'help';
  cCommandSetPort = 'set port';
  cCommandSetSSLPort = 'set sslport';
  cCommandExit = 'exit';

implementation

end.
