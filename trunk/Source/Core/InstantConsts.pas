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
 * Nando Dessena, Steven Mitchell, Joao Morais
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantConsts;

{$IFDEF LINUX}
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
  InstantDateString = 'DATE';
  InstantDefaultFieldSize = 32;
  InstantTimeFormat = 'hhnnsszzz';
  InstantDateTimeFormat = InstantDateFormat + InstantTimeFormat;
  InstantDot = '.';
  InstantFalseString = 'FALSE';
  InstantNowString = 'NOW';
  InstantTagEnd = '>';
  InstantTagStart = '<';
  InstantTimeString = 'TIME';
  InstantTrueString = 'TRUE';
  InstantEndTagFormat= InstantTagStart + '/%s' + InstantTagEnd;
  InstantIdFieldName = 'Id';
  InstantIdPropName = InstantIdFieldName;
  InstantIndexFieldNames = InstantClassFieldName + ';' + InstantIdFieldName;
  InstantModelResourceName = 'InstantObjects.Model';
  InstantPaletteName = 'InstantObjects';
  InstantStartTagFormat= InstantTagStart + '%s' + InstantTagEnd;
  InstantUpdateCountFieldName = 'UpdateCount';
  
  InstantParentIdFieldName = 'ParentId';
  InstantParentClassFieldName = 'ParentClass';
  InstantChildIdFieldName = 'ChildId';
  InstantSequenceNoFieldName = 'SequenceNo';
  InstantChildClassFieldName = 'ChildClass';

{$IFNDEF D6+}
const
  sLineBreak = #13#10;
{$ENDIF}

resourcestring
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
  SErrorRefreshingObject = 'Error refreshing object %s(''%s''): "%s"';
  SErrorRetrievingObject = 'Error retrieving object %s(''%s''): "%s"';
  SErrorStoringObject = 'Error storing object %s(''%s''): "%s"';
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
  SInvalidChar = 'Invalid character ''%s'' (#%d) for attribute %s(''%s'')';
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
  SInvalidValueType = 'Invalid value type';
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
  SOwnershipRecursion = 'Ownership Recursion for object %s(''%s'')';
  SPersistentObjectNotAllowed = 'Persistent object %s(''%s'') not allowed.';
  SProtocolNotSupported = 'Protocol ''%s'' not supported';
  SSpecifierMissing = 'Specifier missing';
  SSplashScreenTitle = 'InstantObjects - Object Persistence Framework';
  SSQLStatementIndexOutOfBounds = 'SQL statement index out of bounds.';
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
  SUndefinedCatalog = 'Catalog not defined for %s';
  SUnexpectedAttributeCategory = 'Unexpected attribute category %s for %s(''%s'') in class %s. Expected attribute category %s';
  SUnexpectedAttributeClass = 'Unexpected attribute class for %s(''%s'') in class %s. Expected attribute class %s';
  SUnexpectedClass = 'Unexpected class %s. Expected class %s';
  SUnexpectedToken = 'Unexpected token ''%s''. Expected token ''%s''';
  SUnknownAttributeClass = 'Unknown attribute class for attribute %s(''%s'')';
  SUnspecifiedCommand = 'Command is not specified';
  SUnsupportedColumnType = 'ColumnType %s not supported';
  SUnsupportedDataType = 'Unsupported datatype: %s';
  SUnsupportedGraphicClass = 'Unsupported graphic class';
  SUnsupportedGraphicStream = 'Unsupported graphic stream format';
  SUnsupportedOperation = 'Unsupported operation: %s';
  SUnsupportedType = 'Unsupported type: %s';
  SUpdateConflict = 'Object %s(''%s'') was updated by another session';

implementation

end.
