# **InstantObjects 3.0 for Delphi** [![License](https://img.shields.io/badge/License-Mozilla%202.0-yellowgreen.svg)](https://www.mozilla.org/en-US/MPL/2.0/)

![InstantObject Logo](https://github.com/EtheaDev/InstantObjects/wiki/instantobjects_logo.gif)

Welcome to InstantObjects, the integrated framework for developing object-oriented business solutions in Delphi (from 2010 to 11.0 Olympus version)

![Delphi10.4Sydney Support](https://github.com/EtheaDev/InstantObjects/wiki/SupportingDelphi.jpg)

Related links: https://www.embarcadero.com/ - https://learndelphi.org/

InstantObjects is a RAD technology that enables creation of applications based on persistent business objects. It simplifies the process of realizing ideas into products; shortens time-to-market, and helps keep business focus.

Even though it is still a bolted-on technology, InstantObjects integrates with the Delphi IDE and Object Pascal in a simple, flexible and intuitive manner. There are no sophisticated environments that have to be comprehended before application development can begin.

**InstantObjects offers:**

* Model realization in the Delphi IDE via integrated two-way tools (from editor to code)

* Object persistence in the most common relational databases or flat XML-based files (also with FireDAC Broker)

* Object presentation via standard data-aware controls.

* Serialization/Deserialization of object using [delphi-neon](https://github.com/paolo-rossi/delphi-neon) library

**Documentation**

Follow the complete manual present in this [wiki section](https://github.com/EtheaDev/InstantObjects/wiki) and look at the PrimerCross demo to find out how to build applications with an Object Persistence Layer very flexible.

![Primer demo](https://raw.githubusercontent.com/wiki/EtheaDev/InstantObjects/PrimerDemo.gif)

**Project Status**

The project is still managed by [Ethea S.r.l.](https://www.ethea.it), an italian company that uses InstantObjects for [InstantSolution Framework](https://ethea.it/prodotti/instantsolutions/).

You can find more information about this Open-Source project in the wiki section.

InstantObjects source files are released under the Mozilla Public License Version 2.0. You may obtain a copy of the License at 

[www.mozilla.org/en-US/MPL/2.0/](https://www.mozilla.org/en-US/MPL/2.0/)

**Full DUnitX Tests**

InstantObjects contains over 400 Tests written for DUnitX framework. From 3.0 version is also compatible with TestInsight test framework.
![TestInsight Running](/Tests/TestInsightRunning.png)


**Release Notes**

VERSION HISTORY
---------------
Version 3.0 (3.0.0.0) (2021-08-27)
- Added support for Delphi 11.0 Olympus
- Updated Test Framework to DUnitX / TestInsight (only for D10.2, D10.3, D10.4, D11.0)

Version 2.10 (2.10.0.0) (2021-03-29)
- Fixed InstantDBX for compilation with IO_CONSOLE directive
- Fixed FireDAC Broker for EncryptedPassword
- Added OnIdChanged for TInstantObject class
- Added Isolation property to FireDAC Broker

Version 2.9 (2.9.0.0) (2021-01-24)
- Added support delphi-NEON library
  for serialization/deserialization of objects
- Added support for MARS Rest server library
- Refactoring FireDAC connector/broker
- Changed font for editor to Segoe UI
- Added OnAssignParamValue event for Brokers

Version 2.8 (2.8.0.0) (2020-08-25)
- Added support for Delphi 10.4 Sydney
- Minor bug-fixing
- Editor font updated

Version 2.7 (2.7.0.0) (2018-12-06)
- Added support for Delphi 10.3 Rio
- Cleaned old LINUX/KYLIX support

Version 2.6 (2.6.0.0) (2017-04-04)
- Added support for Delphi 10.2 Tokyo

Version 2.5 (2.5.0.0) (2016-05-31)
- Added support for Delphi 10.1 Berlin

Version 2.4 (2.4.0.0) (2015-09-01)
- Added support for Delphi 10 Seattle

Version 2.3 (2.3.0.0) (2015-04-08)
- Added support for Delphi XE8
- Added support for Delphi XE7

Version 2.2 (2.2.0.0) (2014-05-12)
- Full UNICODE support: based on UseUnicode flag of InstantConnection:
  Exposers uses TWideStringField or TWideMemoField mapping string attributes
  
- Virtual Containers (for SQL brokers):
  Master-Detail relationship using a contaier without storage  
  (read VirtualContainers.txt for more informations)

- Added Broker for Embarcadero FireDac

- Added support for Delphi XE5 and XE6

- Updated UnitTesting for Exposers and Unicode Support

Version 2.1 (2.1.0.0) (2007-02-26)

- Support for SQL Server 2005; catalog support for SQL Server 7;
  better support for SQL Server in general.

- Support for ModelMaker 6.20 to 9; better ModelMaker support
  in general in ObjectFoundry.

- Support for InstantDate and InstantTime for date-only and
  time-only attribute types.
  
- Instant Model Explorer enhancements:
  - Class B has a base class A and subclasses C, D, etc.
    making a three tier class hierarchy. If Class B is deleted
    classes C, D, etc. will become subclasses of class A.
    Previously they would become subclasses of TInstantObject.
  - New class B in model unit X has a base class A in model unit Y.
    When adding new class B, unit Y will be automatically
    added to the Interface uses clause of unit X.  
  - Whenever a unit is added to the uses clause of an interface
    section, that unit is removed from the implementation
    section uses clause if present. Whenever a unit is added to the
    implementation section uses clause, check the interface section
    uses clause and if the unit is there then don't add anything.

- Instant model Explorer now uses the newer ToolsAPI on Delphi 7+.

- Improved object circular reference detection.

- Added ability to disable circular reference checking
  by undefining IO_CIRCULAR_REFERENCE_CHECK.

- Enhanced SQL statement logging: now logs all statements,
  including select statements.
  
- BDE broker: fixed incomplete implementation of IdDataType
  and IdSize.

- ADO broker: fixed incomplete implementation of IdDataType
  and IdSize.

- XML broker: now only creates one file for each object,
  in the concrete class' folder.

- XML broker: removed support for versioning and
  utf-8 "BOT" encoding.
  
- XML broker: fixed failure on query refresh.

- XML broker restructured for easier customization.

- Refactored InstantPersistence.pas into:
  InstantPersistence.pas - Remains the main 'IO interface' unit.
  InstantBrokers.pas - Contains the descendant brokers, connectors classes.
  InstantMetadata.pas - Contains the metadata, scheme, etc. classes.
  InstantTypes - Contains some type declarations.

- Ubmock and fpcunit integrated into the source tree
  for easier test development.

- Help file has been greatly improved.

- Added explanatory document for IO to InterBase
  and Firebird data type mappings.
  
- Various fixes and enhancements (see tracker).

Version 2.0 (2.0.0.0) (2006-08-07)

- All 2.0 RC changes.

Version 2.0 RC 1 (1.9.2.1) (2006-05-07)

- Help file has been improved.

- Bug fix for # 1496971 Bug In Model Explorer Setting Index and 
  Required properties.

- Bug fix for # 1475841. "TInstantContainer.Sort error if empty".

- Bug fix for # 1475982. Rebuilding a Firebird database creates
  disabled PKs.

- Changed the visibility of the 
  TInstantReferences.ObjectReferenceList property from private to
  protected.

- Bug fix for # 1479652. "Problem with PrimeCross demo and 
  InstantReference.Reset".

- Changed TInstantSQLBroker.AcquireDataSet method to virtual.

- Added a protected, virtual method UndoRecordBuffer to
  TInstantCustomExposer to enable customization of its Undo
  method.

- Bug fix for # 1466586. Minor Error in Attribute Editor.
  Force the main definition tabsheet to be active before trying
  to focus the attribute Name or Size edit controls.

- Fix for bug # 1467511. When adding a new attribute in
  ModelMaker, in the attribute editor there aren't field types
  other than "part" and "parts" available.

- Updated unit tests. Added TestInstantObjectReference.pas unit
  to the Tests folder. This new unit contains tests for the 
  TInstantObjectReference class.

- InstantPart and InstantReference tests updated.

- Bug Fix for # 1464661. TInstantReference.Assign causes AV. Check
  for nil needed before trying to clone a TInstantReference
  attribute object.

Version 2.0 Beta 3 (1.9.1.3) (2006-03-31)

- The IDE menu item "Build Database..." has been changed
  to "Build InstantObjects Database...".

- The BDS IDEs do not have a "Database" Main menu item, so
  the "&Build InstantObjects Database..." menu item is
  inserted in the Main View menu item under the "Data Explorer"
  item if it exists. Otherwise it is appended.

- Fixed bug # 1410657, where TInstantSelector.GetIsChanged
  fetches all non-retrieved objects looking for changes.

- Fixed bug #1416083 (Storage Name ignored for external
  Part attributes).

- IBX: Internal transaction now AutoCommits.
  Fix for bug 1402383 "Hanging Transactions".
  Warning: the bug is not fixed if you use Delphi 5.

- Fix for Bug 1423157. Parts Attribute Insert causes Error.

- Fixed Bug 1385748 "Nested calls to Store cause Stack Overflow".

- Added new public property RefItems to TInstantReferences.
  This is an indexed property that returns the
  TInstantObjectReference from the internal list, allowing you
  to get a list of referenced object IDs without retrieving them.

- FR # 1424512 - Added fields to enter user name, password,
  sql role and character set name to IBX's ConnectionDef form.
  InternalCreateDatabase uses current character set to create
  physical database.

- Added fields for user name, password, sql role, character
  set and db library name for UIB broker. FR # 1424520.

- Fixed leakage using TInstantObject instance via interface
  pointer. Bug # 1424540.

- PrimerCross: added ability to refresh grid contents
  through the F5 key.

- Test: minor changes to Model.pas to allow required circular
  reference test structures.

- Added test procedures to TestInstantReferences.pas to
  facilitate memory leak testing for object structures
  with circular references.

- Added another test procedure to TestInstantReferences.pas
  to facilitate memory leak testing for object structures
  with circular references:
  A -> B -> C -> A
       |
       + -> D -> E
  then delete E.

- Test case to demostrate the infinite loop in
  FreeCircularReferences.

- Many fixes and enhancements in demos and tests.

- Fixed leakage using TInstantObject instance via interface pointer.
  Bug # 1424540.

- Fixed bug # 1410138 where in some situations a Selector raises
  an exception because database component name wasn't uploaded
  from stream.

- Fixed leakages with part and parts attributes in circular
  reference check effort.

- Unsupported column types found in the database no longer stop
  the build/evolution process.

- Fix for bug #1426929. For an application compiled in D2006,
  adding entries into a new record in a DbGrid with an attached
  Selector results in an application crash (stack overflow)
  when trying to tab from the second column.

- InstantExplorer memory leak resolved (bug n. 1423344).

- Fix for bug #1423177 - Memory leak handling exceptions.

- Fix for Bug 1375131: Evolving database with part attributes
  causes "Class Not Registered" error when object is retrieved.

- Changed how packages include resource files: we're back to the
  standard approach of one res file per package.

- Implemented overloaded version of TInstantParts.DestroyObject
  and TInstantReferences.DestroyObject that receives a
  TInstantObject parameter.

- Fixed an unwished disconnection inside FreeCircularReference
  method.

- Fixes for ZeosDBO broker and catalog:
  - Changed SQLGenerator's drop index for MySQL database,
    that doesn't support DROP INDEX statement. Bug # 1434240;
  - Built own SQLDelimiter method, because ZeosDBO's method
    is available only when database (or at least the driver)
    is connected;
  - Changed GetDBMSName method to return property value for
    disconnected databases;
  - Changed physical IB/FB creation to support codepage
    parameter (Character Set);
  - Fixed evolve error. Bug # 1429529;
  - Performed work arounds for MySQL driver (Bug # 1434244) that:
    1. doesn't name Primary Key;
    2. return wrong value for Unique fields (true/false).

- Fixed AV (bug # 1434585) and leakage (bug # 1434710) inside
  TInstantNavigationalQuery.

- Fixed a leakage with some kinds of circular references.

- Fixed bug # 1430106.

- New Remember/Revert mechanism; fixed bugs:
  1232576 Deleting from Selectors without eoAutoApply;
  1410736 Revert method is overriding References attributes;
  1430109 InternalRemoveObject of Exposers doesn't refresh grid;
  1430117 Add/InsertObject (Exposers) with inconsistent behavior;
  1430119 Leakage inserting record;
  1430127 Leakage with InternalCancel and DeferInsert checked;
  1436858 Exposer.AutoDispose disposing referenced object.

- Fixed Info.Conflict assignment into
  TInstantSQLResolver.ExecuteStatement method.

- Fixed exception into D5 and IBX.
  InternalCommit and InternalRollback methods.

- Fixed bug #1430106 - TInstantBlobAttribute.Assign doesn't work
  properly because LoadDataFromStream doesn't clear stream.

- Fixed bug #1410143 - When an EditForm of a Person with image
  that was just edited is opened, Primer raises an exception
  with message "Unsupported graphic stream format", "Metafile
  is not valid" or "Stream read error".

- Fix for bug 1437815 - Exposer.RemoveObject messes up TDataSet
  data.

- Fixed bug # 1438840 Exposers with DeferInsert doesn't own object
  before store.

- Implemented Revert buffer for objects removed through
  RemoveObject method.

- Implemented bmp with TGraphicHeader recognition
  (bug # 1439017).

- Fixed bug # 1439025 Apply/RestoreState decrementing StateLevel.

- Fixed bug # 1436858 Exposer.AutoDispose disposing referenced
  object.

- FR# 1440209 - Pass the object instance to OnGenerateId.

- Fixed Bug # 1439851 - Required fields not checked in Exposers.

- Fixed Bug # 1439234 - Params not working with MS Access.

- Fixed bug # 1439091 - Exposer.AddObject duplicating object
  (sorted lists).

- Implemented Exposer's eoNotDisposeReferences option.

- Fixed stack overflow due to recursive calls to
  IsInsideCircularReference function;
  Improvements into circular reference check.

- Update of tests suite to be compatible with D5.
  Need ubMock project files modified for compatibility with
  D5 to compile and run the tests in D5.

- Fixed Index out of Bounds error into FreeCircularReference.

- Fixed bugs [SF #1447789] in InstantRtti.pas related to
  compilation and handling of Boolean type in private unit
  function AccessProperty.

- Bug fix [SF #1447794] for ObjectFoundry.

- Fixed bug [1446833] - Currency fields receives odd values.

- Update to NexusDB Broker. The modifications mainly apply
  to the connection dialog of the Remote server version.
  Modifications to the Embedded version were due to
  resource string renaming and some extra connection
  dialog
button hints. Main changes:
  - The default Servername 'NexusDB@localhost' has been removed.
    The default is now an empty string.
  - There is no longer an initial delay when launching the
    connection
dialog as there is no checking of available
    NexusDB servers until requested via the 'Load Servers' button.
  - The 'Load Servers' speed button has been enlarged and has a
    caption to make its function more obvious.
  - There is more user feedback in the entry combo boxes to
    indicate current status. Popup hints are also available on
    the speed buttons.
  - A bug fix for enablement of the OK button when using alias
    as path entries.

- Implemented TInstantCustomExposer.Remove method.

- Implemented eoAutoRemember option.

- Fixed bug # 1461222 - "ORA-01722: invalid number" with
  DBX broker.

- Many small bug fixes and improvements.


Version 2.0 Beta 2 (1.9.1.2) (2006-01)

- Fixed possible AV in TInstantBrokerCatalog.

- Many fixes to the circular references management code.

- ZeosDBO: added IB/Fb/MySQL database creation support.

- UIB: fixed AV that was raised trying to disconnect
  UIBDatabase while it's being destroyed.

- Delphi 2005/6: added InstantObjects entry for the
  start up splash screens using the handled sphere icon.

- ModelMaker: added a version resource to show in
  ModelMaker's Tools/Expert Manager dialog.

- Fixed bug #1403489. Fix for the problem that in some
  situations, where there aren't circular references,
  an object is incorrectly detached from a References
  attribute.

- Fix for EOleException in ModelMaker when cancelling
  the dialog during the addition of a new IOAttribute.

- Various internal refactorings and optimizations.

- Removal of several memory leaks and sources of AVs.


Version 2.0 Beta 1 (1.9.1.1) (2005-11)

- Added support for Delphi 2006/Win32 (experimental).

- Help file updated (Work In Progress).

- Added new BPGs for easier installation (Work In
  Progress).

- Added support for UIB in Delphi 2005.

- Added MSSqlCatalog in Delphi 2005.

- Fixed infinite loop on application shutdown with
  TInstantDBBuilder.

- Fixes for compatibility with Delphi 5 Professional.

- Several other bug fixes and small improvements.

- A growing suite of unit tests. Now over 300.
  (see the Tests folder).

- Fixed a bug, which would cause memory leakage if the
  public method TInstantReferences.LoadObjectsFromStream
  was used.

- Primer demo applications updated to reduce object memory
  leakage.

- Added ReferencedBy counting and processing to resolve
  memory leaks caused by the reference counting semantics
  of InstantObjects in circular references.

- Added enhanced Remember/Revert functionality for
  TInstantCustomExposer. Also includes Remember/Revert
  functionality for IO Memo attributes (assumes text only data).

- Added ZeosDBO (ZeosLib project, see http://zeosforum.net.ms/)
  broker.

- Changes for Kylix 3: Removed emf support under Linux and
  added catalogs. Removed unecessary files for PrimerK3.

- The storage name of a references attribute is now stored
  into metadata, so when you reopen the class editor you
  haven't lost this information.

- InternalRefreshObjects for TInstantNavigationalQuery and
  TInstantSQLQuery now uses a BusyObjects list with ownership
  reference added to InstantObjects. This ensures that the
  InstantObjects in the list are not destroyed prematurely
  causing AVs.

- Object ownership behaviour of TInstantNavigationalResolver
  was made consistent with TInstantSQLResolver. Adding or
  inserting objects to an InstantSelector now behaves
  consistently for Navigational and SQL based brokers.

- Fix bug in TInstantSQLResolver.ExecuteStatement that only
  indicated Info.Success as true if executed query returned
  one row.

- Fix bug in TInstantNavigationalQuery.SetRowNumber so that
  loop iteration does not try to go past Dataset.Eof.

- TInstantQuery.GetConnector now uses the global function
  InstantCheckConnector that will try to assign the
  DefaultConnector if one has not already been assigned.

- Embedded and External Parts now behave similarly with respect
  to their contained InstantObjects (ie RefCounts and
  ownership).

- Enhanced TInstantCodeAttribute.GetSingularName function.

- TInstantObject.RestoreState now checks that the object is 
  in the ObjectStore cache before trying to remove it.

- Removed TInstantNavigationalQuery.TranslateCommand method
  (duplicated code).

- Now using old DB builder for catalog-less brokers, and new one
  for the others.

- Connection events don't work when database is not connected
  through TInstantConnector decendant. They have been removed.

- Fix for bug # 1285523: Parts of Parts Are Lost when
  UpdateExternalPartsMap. When Model include a parts of parts 
  schema, sometimes when store first level part the second 
  level parts are lost in DataBase.

- Allow for catalogs that don't support reading all metadata 
  (like an XML catalog).


Version 2.0 Alpha 1 (1.9.0.1) (2005-07)

- Added UIB (Unified InterBase, see http://www.progdigy.com/modules.php?name=UIB)
  broker.

- Added support for Delphi 2005 (D9), Win32 personality only.
  Brokers supported: ADO, BDE, DBX, IBX, NexusDbSQL, XML.

- Removed UsePreparedQuery support; switch to the new StatementCache.

- New TInstantConnector.OnGenerateId event, used to supply a custom
  Id generation strategy. Use it together with IdDataType/IdSize.

- IdDataType and IdSize: see IdDataType_and_IdDataSize.txt.

- Many fixes and improvenet to the new external storage mapping strategy.

- StatementCache: see Statement_Cache.txt.

- New performance tests (Retrieve and Statement Cache) in Primer.

- A growing suite of unit tests (see the Tests folder).

- TInstantPump: see InstantPump.txt.

- Database structure evolution:
  see [RFC]_IO-001_Database_Structure_Evolution.txt
  until more complete docs are available.

- Many bug fixes and small improvements. Please see our trackers:

  http://sourceforge.net/tracker/?group_id=94747&atid=608935
  http://sourceforge.net/tracker/?group_id=94747&atid=608938


Version 1.6.7 (2004-11-17) UNOFFICIAL

- Added support for IOMETADATA Keyword in metadata class definition and
  upgrade of ObjectFoundry for ModelMaker 7 & 8 support.
  Please refer to IOMETADATA_Keyword.txt for details.

- ExternalPart, ExternalParts and ExternalReferences support.

- Added InstantNexusSQL Broker

- Changes to IBX Broker:
  - option to disable the use of delimited identifiers in dialect 3 databases
  - implemented InternalCreateDatabase and GetDatabaseExists

- LoginPrompt support and OnLogin event in Connections based on TCustomConnection


Version 1.6.6 (2004-07-29) UNOFFICIAL

- Currency Support:
  Look into Primer Demo for a little explanation.

- Graphic Support:
  Look into Primer Demo for a little explanation.

- PrimerCross chaged for Currency and Graphic support

- ConnectionManager changes:
  - ConnectionDefs moved from Form to ConnectionManager
  - ConnectByName method added in ConnectionManager to connect without showing form
  - atOpen option added in ConnectionManager to Open connections file (.con or .xml)

- UsePreparedQuery support:
  Added UsePreparedQuery support for TInstantSQLBroker
  (more details in Preprared_Query_Support.txt).

- Primer demo: PerformanceView form changed to make tests with UsePreparedQuery
  and other options

- Added support for XML streaming in blob fields (Part, Parts, References)
  (more details in XML_Blob_Streaming_release_notes.txt)

- Added OnAddClassFieldDef event and BreakThorough method to speed-up exposer's fieds creation.

- Exposer's Undo method now is Virtual and UndoBuffer is available.


Version 1.6.5 (2004-05-23) UNOFFICIAL

- Kylix Porting (design, core and DbExpress broker).
  More details in Kylix3_porting_release_notes.txt.

- Kylix porting of Primer Demo.

- XML format for connection file managed by connectionmanager.


Version 1.6 (2003-12-19) LAST COMMERCIAL VERSION

- SQL based broker architecture introduced.

- dbExpress broker added. (InterBase, MSSQL, DB2, Oracle, MySQL)

- IBX broker now SQL based.

- ADO/MSSQL broker now SQL based.

- Object caching improved.

- ObjectFoundry: Added attribute IsRequired awareness.

- ObjectFoundry: Added attribute DefaultValue awareness.

- ObjectFoundry: Fixed bug when applying attribute options and methods.

- TInstantRelationalQuery.RecNoOfObject now returns correct record number when sequenced.

- TInstantObject.AttributeAddress now protected and virtual.

- Added keyword 'embedded' to non-stored classes with no attributes to improve ModelMaker support.

- InstantRtti.AccessProperty; Fixed 'Invalid type' error on Boolean properties.

- TInstantCustomExposer.LoadFieldValue; Fixed bug causing strings to be copied beyond buffer size.

- TInstantCustomExposer: FieldDefs are now created from Fields to improve performance when using
  persistent fields.


Version 1.5 (2003-02-25)

- Added IProviderSupport to TInstantExposer and TInstantSelector.

- Added Params to TInstantSelector and all brokers.

- TInstantIQL; Added support for Params in; ":[ParamName]" format.

- Added eoSyncEdit to TInstantExposerOptions to enable refreshing
  of current edit buffer when in edit mode and object changes elsewhere.

- Added foRecurseNesting to TInstantFieldOptions to control recursive creation
  of nested DataSet fields.

- Added option 'required' to attributes.

- Added TInstantAttribute.IsRequired.

- Added TInstantAttribute.IsMandatory.

- TInstantObject; SaveState, ApplyState and RestoreState are now virtual.

- TInstantIndexMetadata.Options changed to TIndexOptions to support additional 
  options.

- Added TInstantObjectNotifiers.AcceptItem.

- Added TInstantRelationalQuery.CreateTranslator.

- Added TInstantCustomExposer.ReleaseObject.

- Added TInstantRelationalQuery.RecNoOfObject to support non-sequenced datasets 
  in GotoObject/InternalIndexOfObject.

- Added TInstantCustomExposer.RefreshCurrentObject.

- Added ConvertToText and ConvertToBinary methods to allow XML streaming.

- Changed streaming to allow XML streaming of foreign objects.

- XML-processor now accepts blanks in stream.

- Model can now be exported to XML via Model Explorer.

- Fixed problem when loading class metadatas from DLL resource.

- TInstantCustomExposer: Fixed bug causing metadata of inherited attributes
  to be ignored during field generation.

- InstantCustomExposer.GotoObject now moves cursor to the specified
  object if it exists regardless whether it has been loaded or not.

- TInstantCustomExposer.GotoObject: Fixed bug preventing object from being
  found when called immediately after opening dataset.

- TInstantCustomExposer.ApplyChanges now calls PostChanges first to commit any
  pending changes.

- TInstantSelector.RefreshData now refreshes objects from storage.

- Fixed bugs in TInstantBlob causing content to be repeated or truncated.

- TInstantRelationalTranslator.EmbraceString now uses LeftDelimiter and 
  RightDelimiter.

- Fixed refresh problem in exposers at design time when changing model via code.

- Fixed various issues with TInstantExposer when used with DataSnap.

- Added support for varOleStr, varNull and varEmpty to InstantCompareValues.

- TInstantRelationalBroker; Fixed bug causing objects not to be refreshed if 
  updates were performed by another session.

- TInstantSelector; Fixed AV error that occured when the selector is loading 
  and its associated connector is not yet loaded.

- TInstantBlobStream.Create; No longer fails if attribute cannot be found.

- InstantFindAttribute; Fixed attempt to traverse through unassigned object 
  property.

- Added ADS, DBISAM and FF brokers to Delphi 7 version.


Version 1.3 (2002-09-12)

- FlashFiler broker added.

- DBISAM 3.16 supported.

- TInstantConnector.Objects array added.

- TInstantConnector.GenerateId added.

- TInstantIBXBroker: Fixed duplicate index name error.

- TInstantAttribute: GetValue/SetValue is now virtual.


Version 1.22 (2002-07-30) - Delphi 7 Companion CD

- Delphi 7 supported.

- TInstantExposer: When exposing TCollection instances items are now created
  with TCollectionItem.Create to ensure proper initialization.

- TInstantCustomExposer: Fixed access violation when deleting objects that are
  not TInstantObject instances.

- Runtime package: Fixed bug hindering model from being loaded.

- ADO broker: Added DB2 awareness.

- TInstantDateTime: 'TIME' can now be used as default value for current time.

- TInstantDateTime: Fixed convert error when setting default value.

- TInstantObject: When refreshing object that was disposed by another session,
  the object is now marked as non-persistent and all attributes are reset.

- TInstantCustomExposer: Added RemoveObject and DeleteObject.

- TInstantObject: Added IsOperationAllowed and VerifyOperation.

- Primer demo: Import/export of multiple objects added.


Version 1.21 (2002-06-28)

- InstantGetClass: Now returns nil instead of raising exception if class is
  not TInstantObject descendant.

- Added InstantGetClasses to allow class enumeration.

- Metadata: 'DATE' is now recognized as default value for DateTime attributes.

- Fixed bug causing run-time model to be out of sync when removing or adding
  model units without changing code.

- Exposer/Selector: Fixed bug causing data to be left in record buffer
  when adding a new row to an empty dataset and cancelling.

- Exposer/Selector: Added Options property. Property AutoApply changed to
  eoAutoApply option.

- Exposer/Selector: Added eoDeferInsert option to control whether objects are
  inserted immediately when inserting/appending new rows or when posting
  the row.

- Exposer/Selector: Cursor no longer moves to first row when cancelling insert
  and Sorted is True.

- BDE broker: Fixed bug in SQL translation for MS SQL Server via ODBC

- BDE broker: Fixed bug in SQL translation for DB2 (ODBC and SQL Link)

- DBISAM broker: Fixed bug causing remote databases being treated as local.

- ObjectFoundry Expert: Fixed problem with classes not descending from
  TInstantObject being imported as embedded

- ObjectFoundry Expert: Fixed error "Too many arguments in metadata" for 
  long metadata sections.


Version 1.2 (2002-05-16)

- ObjectFoundry enabled.

- DBISAM broker compiled with DBISAM 3.10.

- Model Explorer now shows relations of non-stored classes in relation-view.

- Fixed error causing Memo attributes to become String attributes when defined via
  the Attribute Editor.

- Added virtual TInstantObject.GenerateId for generating custom Ids.

- Added TInstantObject.Unchanged to allow ignoring changes.

- Added TInstantReference.DestroyObject and TInstantReferences.DestroyObject to 
  release object without clearing reference.

- Added TInstantObject.OwnerChanged to notify when owner changes.

- Fixed error when reading empty values from XML file.

- Fixed stream position bug when streaming multiple objects from XML file.

- Fixed bug causing objects read from stream to be only partly stored.

- Added InstantReadObjects and InstantWriteObjects to read and write multiple objects 
  from and to a stream.

- BuildDatabase with IBX broker now fails on string fields without size.


Version 1.11 (2002-04-22)

- Fixed bug in exposer causing problems with lookup fields.

- Fixed bug in TInstantObject causing IsPersistent to be True after Dispose.

- Fixed bug in exposer causing CurrentObject to return wrong object when
  used with DevExpress QuantumGrid in standard grid mode.

- Added TInstantConnector.UseTransactions to allow disabling transactions.

- Added TInstantObjectNotifier.ObjectClass to allow filtering of notifications.

- EAbort is no longer wrapped in EInstantError.

- Fixed bug in TInstantObject.Refresh causing occasional blank values.

- Fixed bug in DBISAM broker causing exception "Database name already exists".

- TInstantConnectionBasedConnector introduced and used in BDE, ADO and IBX 
  brokers. TInstantRelationalConnector no longer requires a TCustomConnection.

- TInstantWriter.WriteString added to ensure stream compatibility between 
  Delphi 5 and Delphi 6.

- Added TInstantConnector.BuildDatabase that allows building tables for 
  specified classes only.


Version 1.1.0.1707 (2002-03-15)

- DBISAM broker added.

- Advantage Database Server broker added.

- TInstantContainer.OnContentChanged replaced by OnBeforeContentChange and 
  OnAfterContentChange.

- IsDefault property for TInstantConnector changed to allow cross-project
  default connectors to exist in the same project group.

- Fields for non-attribute object properties are now included in exposers
  and selectors at designtime.

- Fixed memory leak when cancelling insert operations in selectors.

- IBX resolver now converts booleans to and from smallints.

- Enum properties are now handled correctly at designtime.

- Exposers no longer clears new objects after they are constructed.

- Fixed bug causing Connect/Disconnect button to disappear from Connection
  Manager after connecting.

- Fixed bug in TInstantRelationalConnector causing "Unassigned connection" 
  error when loading selector components associated with connectors without 
  connections.

- Fixed bug blocking delete operations on exposers when exposing instances of
  classes not descending from TInstantObject.


Version 1.0.0.1706 (2002-02-14)

- ADO, BDE and IBX brokers are now placed in individual packages.
  The complete source code for the brokers is installed into Source\Brokers 
  along with a template broker package for creating custom brokers.
  The separate Broker Kit is no longer required.

- The design-time database creation is now based on the Connection Manager. 
  This allows you to define multiple connections for a project at design-time
  and (re)build each database from the IDE. Database creation can also be done 
  for custom brokers at design-time when they have been installed into the IDE.

- Database creation now creates fields from attribute storage names instead of
  attribute names.

- Fixed bug causing AV when attempting to store an object via a connector with
  no connection.

- Fixed bug causing AV when loading form with TInstantSelector that is linked
  to a TInstantIBXConnector with no connection.


Version 1.0.0.1705 (2002-02-07)

- InterBase dialect 3 is now supported.

- Memos and Blobs larger than 255 bytes are now exposed correctly.

- Picture attribute added to TPerson in Primer to demonstrate blob capabilities.

- Parser error: "expected parameter" when using custom storage names for 
  container attributes fixed.
  
- Object changes made during store when an exposer is auto-applying are now 
  reflected in the exposer.

- Length of inherited string attributes is now correct when exposing objects of 
  descendant classes.

- Fixed design time Access Violation when removing connection component that 
  is attached to a connector.

- Visibility scope of some exposer/accessor methods have changed.

- About box added to Model Explorer.


Version 1.0.0.1704 (2002-01-23)

- Exposer now refreshes record buffer when exposed objects are refreshed.

- Fixed bug in TInstantSelector causing use of ancestor table name instead of
  own table name when specifying attributes introduced in ancestor that is not
  stored.

- Fixed bug in TInstantObject.Retrieve causing stack problems when returning nil.


Version 1.0.0.1703 (2002-01-03)

- Fixed bug causing exposer/selector fields to be missing at design time.

- Removed brackets from SQL when using MS SQL Server via ADO.

- Fixed bug in InstantObjectBinaryToText when converting boolean values.

Version 1.0.0.1702 (2001-12-23)

- Fixed infinite loop when compiling immediately after changing model unit
  of large model on slow computers.

- Fixed error "'0.0' is not a valid timestamp" when posting blank dates to
  exposers in Delphi 6.

Version 1.0.0.1701 (2001-12-07)

- Fixed bug in code generator causing delete of model unit source code
  when editing class via Model Explorer.

Version 1.0.0.1659 (2001-12-03)

- First public release.

**Contributors are welcome**

All InstantObjects users are invited to join us in this project.
