------------------------------------------------------------------

  InstantObjects 2.0 Alpha 1 (1.9.01) for Delphi, Kylix, FPC

  Mozilla Public License 1.1 Edition
  July 2005 release

  Based on Seleqt InstantObjects.
  Portions created by Seleqt are Copyright (c) 2001-2003 Seleqt.
  Other portions and changes are Copyright (c) the authors.
  All rights reserved.

------------------------------------------------------------------


TABLE OF CONTENTS

o INTRODUCTION
o FEATURES
o INSTALLATION
o GETTING STARTED
o VERSION HISTORY


INTRODUCTION
------------

InstantObjects is an integrated framework for developing object-oriented business
solutions in Borland Delphi(tm), Borland Kylix(tm) and the open source FreePascal
compiler (non yet released in this alpha version).
The framework provides the foundation for the development process as
well as the engine that powers the final application.


FEATURES
--------

MODEL REALIZATION

InstantObjects simplifies the process of realizing an object-oriented business model
into Delphi/Kylix/FPC classes. The Model Explorer is a two-way tool that is
integrated with the Delphi or Kylix IDE (not yet available for Delphi 2005 and
Lazarus). With the Model Explorer you can manage the business model of your
application by defining classes and relations. Changes made via the Model Explorer
are automatically reflected in code and vice versa. When a class is created in the
Model Explorer, the definition of the class and its attributes are written to the
model unit along with the implementation of access methods for the defined attributes.

A database for storing the business objects can be automatically generated from the
defined model via the Database Builder. InstantObjects supports databases available
via BDE, ADO, InterBase Express, dbExpress, DBISAM, FlashFiler, NexusDb, UIB and 
Advantage Database Server, plus a native, file-based XML storage.

OBJECT PERSISTENCE

One of the major issues when developing object-oriented applications is object
persistence. InstantObjects adresses this issue by providing a powerful persistence
mechanism that allows using the most common databases as storage. Storing and
retrieving objects is a simple matter of invoking methods of the common ancestor
class, TInstantObject. Mapping of objects to and from tables is done automatically
by the persistence engine of InstantObjects. Relations between objects defined in
the business model are handled seamlessly.

OBJECT PRESENTATION

The business objects of your application can be exposed through datasets, meaning
they can be tied to the data-aware controls and reporting tools of your choice.
TInstantExposer is the dataset component that is used to expose specific objects.
TInstantSelector is a special exposer that enables you to expose a group of objects
that is selected from the database. Exposers can be tied together in master-detail
relationships allowing you to expose related objects. In addition, containers within
exposed objects are represented as nested datasets.


INSTALLATION
------------

1. Unpack zip file to <installdir>


2. Add the following folders to you development environment's library path:

   <installdir>/Source/Core
   <installdir>/Source/Brokers/<brokername>

   Add a line for each broker you plan to use.


3. Build and install the InstantObjects core packages:

   <installdir>/Source/Core/<version>/IOCore.dpk (build)
   <installdir>/Source/Design/<version>/DclIOCore.dpk (install)
   
   Where <version> identifies your version of Delphi, Kylix or
   FPC. Examples: D5, D6, D7, K3, D9 (Delphi 2005), etc.


4. Build and install the desired broker package(s):

   <installdir>/Source/Brokers/ADO/<version>/IOADO.dpk (build)
   <installdir>/Source/Brokers/ADO/<version>/DclIOADO.dpk (install)

   Please note that not all brokers are available in all versions
   of Delphi/Kylix/FPC. If you think you can make a supplied broker
   work in an unsupported version, or create a new broker from scratch
   then please contact us.
   
   Some broker packages depend on one or more catalogs. In that case,
   you'll have to build the catalog package(s) first. For example, the
   following catalog is required by the IBX, dbX and UIB brokers:
   
   <installdir>/Source/Catalogs/IBFb/<version>/IOIBFbCatalog.dpk (build)

   
GETTING STARTED
---------------

EXAMPLES

Before you start creating your own solutions with InstantObjects, we recommend
that you study the example projects that have been installed into
<installdir>/Demos.

The Demos folder contains several example projects; you should look at
Intro and Primer/PrimerExternal first.

Intro is a basic introduction to InstantObjects. It only works in Delphi.

Primer (in the PrimerCross folder - "cross" meaning "cross-platform") is a
thorough demonstration and explanation of nearly all the features
of InstantObjects. The application implements a small business model and a user
interface that shows how persistent business objects can be used in a real
application. It is compatible with Delphi and Kylix. The PrimerExternal variation
uses external storage for collections (see documentation).

The Demos folder also features a few other programs that help demonstrate or test
specific features:

IntroIW is an IntraWeb version of the Intro application. It shows how to use IO on
the server side of a web application (which, thanks to IntraWeb's paradigm, is
exactly the same as in a standard application).

EvolveTest is a testbed application for the database structure evolution feature.
Please see the documentation to know more about database evolution and the
TInstantDBEvolver component.

Pump is a testbed application for the object pump feature.
Please see the documentation to know more about the TInstantPump component.

The Test folder contains test applications of various sorts.

DOCUMENTATION

When you are ready to create your own InstantObjects solutions, please consult
the InstantObjects Guide io.hlp in the <installdir>\Help folder.

The section "User Guide" explains how to build applications with InstantObjects.
The section "Reference Guide" contains a detailed explanation of every class
and function in the InstantObjects framework.

Please note that this giude was not updated yet for IO2. Most of the new features,
though, are documented in text files you'll find in the Docs folder.


VERSION HISTORY
---------------



Version 2.0 Alpha 1 (1.9.01) (2005-07)

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
