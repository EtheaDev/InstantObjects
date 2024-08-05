(*
 *   InstantObjects
 *   Constants
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
 * Nando Dessena, Steven Mitchell, Joao Morais, Andrea Magni
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantConsts;

{$IFDEF LINUX64}
{$I '../InstantDefines.inc'}
{$ELSE}
{$I '..\InstantDefines.inc'}
{$ENDIF}

interface

const
  InstantAttributeMetadatasTagName = 'AttributeMetadatas';
  InstantClassFieldName = 'Class';
  InstantClassIdPropName = InstantClassFieldName + 'Id';
  InstantDateFormat = 'yyyymmdd';
  InstantDateJFormat = 'yyyy-mm-dd';
  InstantDateString = 'DATE';
  InstantDefaultFieldSize = 32;
  InstantTimeFormat = 'hhnnsszzz';
  InstantTimeJFormat = 'hh:nn:ss:zzz';
  InstantDateTimeFormat = InstantDateFormat + InstantTimeFormat;
  InstantDateTimeJFormat = InstantDateJFormat +'T' + InstantTimeJFormat + 'Z';
  InstantFloatJFormat = '0.0';
  InstantDot = '.';
  InstantPkSeparator = '|';
  InstantFalseString = 'FALSE';
  InstantFalseJString = 'false';
  InstantNowString = 'NOW';
  InstantTagEnd = '>';
  InstantTagStart = '<';
  JsonObjectEnd = '}';
  JsonObjectStart = '{';
  JsonArrayEnd = ']';
  JsonArrayStart = '[';
  InstantProcessingInstructionStart = '?';
  InstantTimeString = 'TIME';
  InstantTrueString = 'TRUE';
  InstantTrueJString = 'true';
  InstantEndTagFormat= InstantTagStart + '/%s' + InstantTagEnd;
  InstantIdFieldName = 'Id';
  InstantDescriptionAttrName = 'Description';
  InstantIdPropName = InstantIdFieldName;
  InstantIndexFieldNames = InstantClassFieldName + ';' + InstantIdFieldName;
  InstantModelResourceName = 'InstantObjects.Model';
  InstantPaletteName = 'InstantObjects';
  InstantStartTagFormat= InstantTagStart + '%s' + InstantTagEnd;
  InstantUpdateCountFieldName = 'UpdateCount';
  InstantClassMetadatasTagName = 'ClassMetadatas';

  InstantParentIdFieldName = 'ParentId';
  InstantParentClassFieldName = 'ParentClass';
  InstantChildIdFieldName = 'ChildId';
  InstantSequenceNoFieldName = 'SequenceNo';
  InstantChildClassFieldName = 'ChildClass';
  InstantLogStatementBefore = 'Before: ';
  InstantLogStatementSelect = 'Select: ';
  InstantLogStatementExecute = 'Execute: ';
  InstantPrologue = '<?xml version="1.0" encoding="UTF-8"?>';
var
  InstantXMLIndentationSize: Byte = 2;
  InstantJSONIndentationSize: Byte = 2;

resourcestring
{$IFDEF CBLIB_ITA}
  SAccessError = 'Non è possibile accedere all''attributo %s(''%s'') come tipo: %s';
  SAccessorClassNotFoundFor = 'Classe di accesso non trovata per la classe %s ';
  SAttributeConversionError = 'Errore nel convertire il valore per l''attributo %s(''%s''): "%s"';
  SAttributeNotFound = 'Attributo ''%s'' non trovato per la classe %s ';
  SAttributeNotIndexed = 'L''attributo %s(''%s'') nella classe %s non è indicizzato';
  SAttributeNotQueryable = 'L''attributo %s(''%s'') nella classe %s non è ricercabile';
  SCannotBuildDB = 'Lo step "%s" di creazione del database non è supportato';
  SCapabilityNotSuppported = 'Funzionalità non supportata';
  SClassNotQueryable = 'Classe %s non ricercabile';
  SClassNotRegistered = 'Classe %s non registrata';
  SClassNotStreamable = 'La classe %s non è serializzabile: ';
  SClassReferenceMissing = 'Riferimento alla classe mancante';
  SConnectionDefError = 'Errore: definizioni della connessione %s mancanti nel file %s';
  SConnectionDefFilter = 'File delle connessioni|*.con;*.xml';
  SConnectionManagerCaption = 'Gestore connessioni';
  SConnectionManagerExecutorNotAssigned = 'Procedure di esecuzione del gestore delle connessioni non assegnata';
  SConnectionName = 'Nome connessione';
  SConnectorInUse = 'Connettore %s in uso dall''oggetto %d';
  SConnectorMismatch = 'Connector sbagliato';
  SConnectionSuccess = 'Connesso con successo!';
  SContainerNotFound = 'Contenitore: ''%s'' non trovato per la classe %s';
  SDatabaseBuildConfirmation = 'Creare il database con connessione "%s" e cancellare tutti i dati?';
  SDatabaseBuilt = 'Database creato correttamente';
  SDatabaseCreationNotSupported = '''%s'' non supporta la creazione automatica del database';
  SDatabaseEvolutionNonSupported = 'Questo broker non supporta database evolution';
  SDatabaseOpen = 'Non è possibile eseguire questa operazione su un database aperto';
  SDefaultContainerNotFound = 'Contenitore di default non trovato per la classe %s';
  SDefaultContainerNotSpecified = 'Contenitore di default non specificato per la classe %s';
  SDeleteConnectionConfirmation = 'Eliminare la connessione "%s"?';
  SDeniedDispose = 'Impossibile cancellare l''oggetto %s(''%s'')';
  SDeniedRefresh = 'Impossibile recuperare i dati per l''oggetto %s(''%s'')';
  SDeniedRetrieve = 'Impossibile recuperare l''oggetto %s(''%s'')';
  SDeniedStore = 'Impossibile memorizzare l''oggetto %s(''%s'')';
  SDisposeConflict = 'L''oggetto %s(''%s'') è stato cancellato da un''altra sessione';
  SErrorDisposingObject = 'Errore di disposizione dell''oggetto %s(''%s''): "%s"';
  SErrorLoadingConnectionDefs = 'Errore nel caricare le definizioni di connessione da %s: %s';
  SErrorLoadingFile = 'Errore nel caricamento del file "%s": %s';
  SErrorRefreshingObject = 'Errore nell''aggiornamento dell''oggetto %s(''%s''): "%s"';
  SErrorRetrievingObject = 'Errore nel recupero dell''oggetto %s(''%s''): "%s"';
  SErrorStoringObject = 'Errore durante la memorizzazione dell''oggetto %s(''%s''): "%s"';
  SExposerChanged = 'Non è possibile effettuare questa operazione su un Exposer cambiato';
  SFalseString = 'False';
  SFatalError = 'Errore irreversibile per l''oggetto %s(''%s''): "%s"';
  SFieldReadError = 'Errore di lettura del valore del campo ''%s'': %s';
  SFieldSizeRequired = 'Dimensione richiesta per il campo "%s" nella tabella "%s"';
  SFieldWriteError = 'Errore di scrittura del valore ''%s'' nel campo %s: %s';
  SFinalizationFailed = 'Finalizzazione fallita per l''oggetto %s(''%s''): "%s"';
  SGateClassNotFoundFor = 'Classe gate non trovata per la classe di connessione %s';
  SIndexOutOfBounds = 'Indice fuori dai limiti (%d)';
  SInitializationFailed = 'Inizializzazione fallita per l''oggetto della classe %s: "%s"';
  SInvalidArgument = 'Argomento non valido per la classe %s. Argomento della classe atteso: %s.';
  SInvalidAttributeValue = 'Valore non valido "%s" per l''attributo %s(''%s'')';
  SInvalidChar = 'Carattere non valido ''%s'' (#%d). Caratteri validi sono ''%s''';
  SInvalidClass = 'Classe %s non valida. Attesa %s';
  SInvalidConnector = 'Connettore non valido per l''oggetto %s(''%s'') nell''attributo %s(''%s'')';
  SInvalidDataType = 'Tipo di dato non valido';
  SInvalidDateTime = 'Date/time non valido: %s';
  SInvalidItemClass = 'Elenento di classe non valido %s';
  SInvalidMasterDataSetClass = 'Master dataset non valido, classe %s. Classe %s richiesta.';
  SInvalidObjectClass = 'Oggetto della classe %s non valido nell''attributo %s(''%s''). Richiesta classe %s';
  SInvalidObjectId = 'Id non valido (''%s'') per l''oggetto della classe %s';
  SInvalidOperator = 'Operatore non valido: %s';
  SInvalidPropertyValue = 'Valore della proprietà non valido';
  SInvalidToken = 'Token non valido ''%s''';
  SInvalidValueType = 'Tipo di valore non valido: ''%s''';
  SKeyViolation = 'Violazione di chiave per l''oggetto %s(''%s'')';
  SLoginPromptFailure = 'Non è stato possibile trovare la login dialog. Si prega di usare la unit DBLogDlg o impostare LoginPrompt a False';
  SMetadataMismatch = 'Disallineamento dei metadata per l''attributo %s(''%s'')';
  SMetadataNotFound = 'Metadata non trovato per la classe %s';
  SMethodNotFound = 'Metodo ''%s'' non trovato per la classe %s';
  SMissingImplementation = 'Il metodo %s non è implementato nella classe %s';
  SNotPublishedObjectProperty = '%s non è una proprietà pubblicata dell''oggetto';
  SObjectClassUndefined = 'ObjectClass non definito';
  SObjectError = 'Errore per l''oggetto della classe %s: "%s"';
  SObjectIsOwned = 'L''oggetto %s(''%s'') è posseduto.';
  SObjectNotAvailable = 'Oggetto non disponibile!';
  SOwnershipRecursion = 'Ricorsione del proprietario dell''oggetto %s(''%s'')';
  SPersistentObjectNotAllowed = 'Oggetto persistente %s(''%s'') non permesso.';
  SProtocolNotSupported = 'Protocol/DriverId ''%s'' non supportato';
  SSpecifierMissing = 'Specifica non definita';
  SSplashScreenTitle = 'InstantObjects - Object Persistence Framework';
  SSQLStatementIndexOutOfBounds = 'Indice dell''istruzione SQL fuori limite.';
  SSubqueryMissing = 'Subquery mancante';
  STablePathNotFound = 'Table path %s non trovato';
  STransactionInProgress = 'Transazione in corso';
  STrueString = 'Vero';
  SUnableToQueryAttribute = 'Impossibile effettuare una ricerca sull''attributo %s.%s';
  SUnassigned = '[Non assegnato]';
  SUnassignedAttributeMetadata = 'Metadata non assegnato per l''attributo %s(''%s'')';
  SUnassignedBroker = 'Broker non assegnato';
  SUnassignedClass = 'Classe non assegnata';
  SUnassignedClassMetadata = 'Metadata non assegnato per la classe %s';
  SUnassignedCommandObject = 'Comando dell''oggetto non assegnato';
  SUnassignedConnection = 'Connessione non assegnata';
  SUnassignedConnector = 'Connettore non assegnato';
  SUnassignedDataSet = 'Dataset non assegnato';
  SUnassignedGate = 'Porta non assegnata';
  SUnassignedMap = 'Mappa non assegnata';
  SUnassignedObjectInAttribute = 'Oggetto non assegnato nell''attributo %s(''%s'')';
  SUnassignedOnConnectError = 'Evento OnConnect di InstantConnectionManager non assegnato';
  SUnassignedQuery = 'Query non assegnata';
  SUnassignedResolver = 'Risolutore non assegnato';
  SUnassignedStream = 'Stream non assegnato';
  SUnassignedTranslator = 'Traduttore non assegnato';
  SUnassignedSubject = 'Subject non assegnato';
  SUndefinedCatalog = 'Catalogo non definito per %s';
  SUnexpectedAttributeCategory = 'Categoria dell''attributo %s inaspettata per %s(''%s'') nella classe %s. Avrebbe dovuto essere %s';
  SUnexpectedAttributeClass = 'Classe inaspettata per l''attributo %s(''%s'') nella classe %s. Avrebbe dovuto essere %s';
  SUnexpectedClass = 'Classe %s inattesa. Prevista classe %s';
  SUnexpectedToken = 'Token ''%s'' inatteso. Previsto token ''%s''';
  SUnexpectedTagName = 'Nome Tag ''%s'' inatteso';
  SUnknownAttributeClass = 'Attributo della classe sconosciuto per l''attributo %s(''%s'')';
  SUnspecifiedCommand = 'Comando non specificato';
  SUnsupportedColumnSkipped = 'Saltata colonna %s.%s. Tipo %s. non supportato';
  SUnsupportedAttributeOperation = 'Operazione (%s) non supportata per l''attributo %s(''%s''). Ragione: %s.';
  SUnsupportedDataType = 'Datatype non supportato: %s';
  SUnsupportedGraphicClass = 'Classe grafica non supportata';
  SUnsupportedGraphicStream = 'Formato file grafico non supportato';
  SUnsupportedOperation = 'Operazione non supportata: %s';
  SUnsupportedType = 'Tipo non supportato: %s';
  SUpdateConflict = 'L''oggetto %s(''%s'') è stato aggiornato da un''altra sessione';
  SUsingAttributeMissing = 'Attributo USING mancante';
  SUsingAttributeNotInitialized = 'Errore inizializzando l''attributo USING';
  SSubContextNotFoundForSubQuery = 'Oggetto SubContext non trovato per la subquery (''%s'')';
  SParentContextNotFoundForSubQuery = 'Oggetto del contesto padre non trovato per la subquery (''%s'')';
  SUsingAttributeMustBeAReference = 'L''attributo (''%s'') non è un reference: l''attributo USING deve essere un reference.';
  SUsingAttributeMetadataNotFound = 'Impossibile ottenere i metadati per l''attributo USING (''%s'')';
  SSQLExecuteErrorShort = 'Errore in esecuzione SQL: ''%s''';
  SSQLExecuteError = 'Errore in esecuzione SQL. Comando: ''%s''. Parametri: ''%s''. Errore: ''%s''';
  SUseUnicodeWrong = 'Errore: Unicode dell''exposer non corrisponde a Unicode del connector in uso.';
  SDefaultConnectorNotAvailable = 'Errore: InstantDefaultConnector non è disponibile in una app. console.';
  SAttributeNameAttributeValueDiff = 'Errore: Nomi degli attributi %d e valori degli attributi %d sono differenti.';
{$ELSE}
  SAccessError = 'Cannot access attribute %s(''%s'') as type: %s';
  SAccessorClassNotFoundFor = 'Accessor class not found for class %s ';
  SAttributeConversionError = 'Error converting value for attribute %s(''%s''): "%s"';
  SAttributeNotFound = 'Attribute ''%s'' not found for class %s ';
  SAttributeNotIndexed = 'Attribute %s(''%s'') in class %s not indexed';
  SAttributeNotQueryable = 'Attribute %s(''%s'') in class %s not queryable';
  SCannotBuildDB = 'Database build step "%s" not supported';
  SCapabilityNotSuppported = 'Capability not supported';
  SClassNotQueryable = 'Class %s not queryable';
  SClassNotRegistered = 'Class %s not registered';
  SClassNotStreamable = 'Class %s not streamable: ';
  SClassReferenceMissing = 'Class reference missing';
  SConnectionDefError = 'Error: connection definitions %s not found in file %s';
  SConnectionDefFilter = 'Connection files|*.con;*.xml';
  SConnectionManagerCaption = 'Connection Manager';
  SConnectionManagerExecutorNotAssigned = 'Connection manager execution procedure not assigned';
  SConnectionName = 'Connection Name';
  SConnectorInUse = 'Connector %s in use by %d object(s)';
  SConnectorMismatch = 'Connector mismatch';
  SConnectionSuccess = 'Successfully connected!';
  SContainerNotFound = 'Container: ''%s'' not found for class %s';
  SDatabaseBuildConfirmation = 'Build database via connection "%s" and clear all data?';
  SDatabaseBuilt = 'Database was built successfully';
  SDatabaseCreationNotSupported = '''%s'' does not support automatic database creation';
  SDatabaseEvolutionNonSupported = 'This broker does not support database evolution';
  SDatabaseOpen = 'Cannot perform this operation on an open database';
  SDefaultContainerNotFound = 'Default container not found for class %s';
  SDefaultContainerNotSpecified = 'Default container not specified for class %s';
  SDeleteConnectionConfirmation = 'Delete connection "%s"?';
  SDeniedDispose = 'Denied dispose of object %s(''%s'')';
  SDeniedRefresh = 'Denied refresh of object %s(''%s'')';
  SDeniedRetrieve = 'Denied retrieve of object %s(''%s'')';
  SDeniedStore = 'Denied store of object %s(''%s'')';
  SDisposeConflict = 'Object %s(''%s'') was disposed by another session';
  SErrorDisposingObject = 'Error disposing object %s(''%s''): "%s"';
  SErrorLoadingConnectionDefs = 'Error loading connection definitions from %s: %s';
  SErrorLoadingFile = 'Error loading file "%s": %s';
  SErrorRefreshingObject = 'Error refreshing object %s(''%s''): "%s"';
  SErrorRetrievingObject = 'Error retrieving object %s(''%s''): "%s"';
  SErrorStoringObject = 'Error storing object %s(''%s''): "%s"';
  SExposerChanged = 'Cannot perform this operation on a changed Exposer';
  SFalseString = 'False';
  SFatalError = 'Fatal Error for object %s(''%s''): "%s"';
  SFieldReadError = 'Error reading value from field ''%s'': %s';
  SFieldSizeRequired = 'Field size required for field "%s" in table "%s"';
  SFieldWriteError = 'Error writing value ''%s'' to field %s: %s';
  SFinalizationFailed = 'Finalization failed for object %s(''%s''): "%s"';
  SGateClassNotFoundFor = 'Gate class not found for connection class %s';
  SIndexOutOfBounds = 'Index out of bounds (%d)';
  SInitializationFailed = 'Initialization failed for object of class %s: "%s"';
  SInvalidArgument = 'Invalid argument for object of class %s. Expected argument of class %s';
  SInvalidAttributeValue = 'Invalid value "%s" for attribute %s(''%s'')';
  SInvalidChar = 'Invalid character ''%s'' (#%d). Valid characters are ''%s''';
  SInvalidClass = 'Invalid class %s. Expected %s';
  SInvalidConnector = 'Invalid connector for object %s(''%s'') in attribute %s(''%s'')';
  SInvalidDataType = 'Invalid data type';
  SInvalidDateTime = 'Invalid date/time: %s';
  SInvalidItemClass = 'Invalid item class %s';
  SInvalidMasterDataSetClass = 'Invalid master dataset class %s. Class %s required.';
  SInvalidObjectClass = 'Invalid object class %s in attribute %s(''%s''). Required class %s';
  SInvalidObjectId = 'Invalid Id (''%s'') for object of class %s';
  SInvalidOperator = 'Invalid operator: %s';
  SInvalidPropertyValue = 'Invalid property value';
  SInvalidToken = 'Invalid token ''%s''';
  SInvalidValueType = 'Invalid value type ''%s''';
  SKeyViolation = 'Key violation for object %s(''%s'')';
  SLoginPromptFailure = 'Couldn''t find login dialog. Please use unit DBLogDlg or set LoginPrompt to False';
  SMetadataMismatch = 'Metadata mismatch for attribute %s(''%s'')';
  SMetadataNotFound = 'Metadata not found for class %s';
  SMethodNotFound = 'Method ''%s'' not found for class %s';
  SMissingImplementation = 'Method %s is not implemented in class %s';
  SNotPublishedObjectProperty = '%s is not a published object property';
  SObjectClassUndefined = 'ObjectClass undefined';
  SObjectError = 'Error for object of class %s: "%s"';
  SObjectIsOwned = 'Object %s(''%s'') is owned.';
  SObjectNotAvailable = 'Object is not available!';
  SOwnershipRecursion = 'Ownership Recursion for object %s(''%s'')';
  SPersistentObjectNotAllowed = 'Persistent object %s(''%s'') not allowed.';
  SProtocolNotSupported = 'Protocol/DriverId ''%s'' not supported';
  SSpecifierMissing = 'Specifier missing';
  SSplashScreenTitle = 'InstantObjects - Object Persistence Framework';
  SSQLStatementIndexOutOfBounds = 'SQL statement index out of bounds.';
  SSubqueryMissing = 'Subquery missing';
  STablePathNotFound = 'Table path %s not found';
  STransactionInProgress = 'Transaction in progress';
  STrueString = 'True';
  SUnableToQueryAttribute = 'Unable to query on attribute %s.%s';
  SUnassigned = '[Unassigned]';
  SUnassignedAttributeMetadata = 'Unassigned metadata for attribute %s(''%s'')';
  SUnassignedBroker = 'Unassigned broker';
  SUnassignedClass = 'Unassigned class';
  SUnassignedClassMetadata = 'Unassigned metadata for class %s';
  SUnassignedCommandObject = 'Unassigned command object';
  SUnassignedConnection = 'Unassigned connection';
  SUnassignedConnector = 'Unassigned connector';
  SUnassignedDataSet = 'Unassigned DataSet';
  SUnassignedGate = 'Unassigned gate';
  SUnassignedMap = 'Unassigned map';
  SUnassignedObjectInAttribute = 'Unassigned object in attribute %s(''%s'')';
  SUnassignedOnConnectError = 'Unassigned OnConnect event of InstantConnectionManager';
  SUnassignedQuery = 'Unassigned query';
  SUnassignedResolver = 'Unassigned resolver';
  SUnassignedStream = 'Unassigned stream';
  SUnassignedTranslator = 'Unassigned translator';
  SUnassignedSubject = 'Subject unassigned';
  SUndefinedCatalog = 'Catalog not defined for %s';
  SUnexpectedAttributeCategory = 'Unexpected attribute category %s for %s(''%s'') in class %s. Expected attribute category %s';
  SUnexpectedAttributeClass = 'Unexpected attribute class for %s(''%s'') in class %s. Expected attribute class %s';
  SUnexpectedClass = 'Unexpected class %s. Expected class %s';
  SUnexpectedToken = 'Unexpected token ''%s''. Expected token ''%s''';
  SUnexpectedTagName = 'Unexpected tag name ''%s''';
  SUnknownAttributeClass = 'Unknown attribute class for attribute %s(''%s'')';
  SUnspecifiedCommand = 'Command is not specified';
  SUnsupportedColumnSkipped = 'Skipped column %s.%s. Unsupported type %s.';
  SUnsupportedAttributeOperation = 'Unsupported operation (%s) for attribute %s(''%s''). Reason: %s.';
  SUnsupportedDataType = 'Unsupported datatype: %s';
  SUnsupportedGraphicClass = 'Unsupported graphic class';
  SUnsupportedGraphicStream = 'Unsupported graphic stream format';
  SUnsupportedOperation = 'Unsupported operation: %s';
  SUnsupportedType = 'Unsupported type: %s';
  SUpdateConflict = 'Object %s(''%s'') was updated by another session';
  SUsingAttributeMissing = 'USING attribute missing';
  SUsingAttributeNotInitialized = 'Error initializing USING attribute';
  SSubContextNotFoundForSubQuery = 'SubContext object not found for subquery (''%s'')';
  SParentContextNotFoundForSubQuery = 'Parent context object not found for subquery (''%s'')';
  SUsingAttributeMustBeAReference = 'Attribute (''%s'') is not a reference: USING attribute must be a reference.';
  SUsingAttributeMetadataNotFound = 'Cannot get metadata for USING attribute (''%s'')';
  SSQLExecuteErrorShort = 'SQL execute error: ''%s''';
  SSQLExecuteError = 'SQL execute error. Statement: ''%s''. Parameters: ''%s''. Error: ''%s''';
  SUseUnicodeWrong = 'Error: Unicode of exposer don''t match with Unicode of connector in use.';
  SDefaultConnectorNotAvailable = 'Error: InstantDefaultConnector is not available in console app.';
  SAttributeNameAttributeValueDiff = 'Error: Attribute names %d and attribute values %d are different.';
{$ENDIF}

implementation

end.
