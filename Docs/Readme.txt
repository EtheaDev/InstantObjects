------------------------------------------------------------------

  InstantObjects 2.0 Beta 1 (1.9.1.1) for Delphi, Kylix, FPC

  Mozilla Public License 1.1 Edition
  October 2005 release

  Based on Seleqt InstantObjects.
  Portions created by Seleqt are Copyright (c) 2001-2003 Seleqt.
  Other portions and changes are Copyright (c) the authors.
  All rights reserved.

------------------------------------------------------------------


TABLE OF CONTENTS

o INTRODUCTION
o FEATURES
o INSTALLATION
o DOCUMENTATION AND SUPPORT
o GETTING STARTED
o VERSION HISTORY


INTRODUCTION
------------

InstantObjects is an integrated framework for developing object-oriented business
solutions in Borland Delphi(tm), Borland Kylix(tm) and soon also the open source
FreePascal compiler. The framework provides the foundation for the development
process as well as the engine that powers the final application.


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
   If you are going to use a broker that needs a separate catalog
   (currently the IBX, UIB, DBX and ADO brokers), then
   you'll also need to add a line for each required catalog:

   <installdir>/Source/Catalogs/<catalogname>

   Here is a table of current broker-catalog dependencies:
   
   Broker              Depends on Catalog
   --------------------------------------
   IBX                 IBFb
   UIB                 IBFb
   DBX                 IBFb
   ADO                 MSSql
   
   Other brokers have their catalogs integrated into themselves,
   so there are not dependency issues for them.


3. Build and install the InstantObjects core packages:

   <installdir>/Source/Core/<version>/IOCore.dpk (build)
   <installdir>/Source/Design/<version>/DclIOCore.dpk (install)
   
   Where <version> identifies your version of Delphi, Kylix or
   FPC. Examples: D5, D6, D7, K3, D2005, etc.


4. Build and install the desired catalog and broker package(s):

   For example, if you need the ADO broker:
   
   <installdir>/Source/Catalogs/MSSql/<version>/IOMSSqlCatalog.dpk (build)
   <installdir>/Source/Brokers/ADO/<version>/IOADO.dpk (build)
   <installdir>/Source/Brokers/ADO/<version>/DclIOADO.dpk (install)

   If you need a broker that doesn't depend on a catalog, just build and
   install the broker's packages. Here's an example for the XML broker:
   
   <installdir>/Source/Brokers/XML/<version>/IOXML.dpk (build)
   <installdir>/Source/Brokers/XML/<version>/DclIOXML.dpk (install)
   
   Please note that not all brokers are available in all versions
   of Delphi/Kylix/FPC. If you think you can make a supplied broker
   work in an unsupported version, or create a new broker from scratch
   then please contact us.
   
   Note for Delphi 5 users: dpk files in Delphi 5 have an additional
   suffix "_D5". This is because Delphi 5 does not support the LIBSUFFIX
   compiler directive that InstantObjects uses in newer versions of
   Delphi.

********************************************************************************
Note: All InstantObjects packages use non-standard code in the package
source file. In particular, they miss the standard line:

{$R *.RES}

and have instead something similar to this line:

{$I '../../InstantVersion.inc'}

that includes the InstantVersion.inc file in the $(InstantObjects)\Source
folder (Substitute the correct relative path in this statement depending on
where in the installed folder tree the package file resides). However if you
open the Project Options for a package and click the OK button, the Delphi IDE
will restore the line with the $R directive, automatically create a
<packagename>.res file that IO doesn't need and delete the $I
directive. If you ever do that, please restore the original code by editing
the Project Source by hand or restoring the original file from the distribution
archive.
********************************************************************************


DOCUMENTATION AND SUPPORT
-------------------------

The InstantObjects Guide IOHelp.hlp in the <installdir>\Help folder contains more
informations about InstantObjects.

The section "User Guide" explains how to build applications with InstantObjects.
The section "Reference Guide" contains a detailed explanation of every class
and function in the InstantObjects framework.

Please note that this guide is not entirely up-to-date for IO v2. Most of the
new features, though, are documented in text files you can find in the Docs
folder.

You can also use our support newsgroups; more details at
http://www.instantobjects.org/.


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
uses external storage for collection attributes (see documentation).

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


VERSION HISTORY
---------------
Please see the "Changes.txt" file in the <installdir>/Docs folder.
