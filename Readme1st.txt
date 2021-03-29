------------------------------------------------------------------

  InstantObjects for Delphi - Full Unicode version
  ver. 2.10 for Delphi 10.4 Sydney

  Mozilla Public License 1.1 Edition
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
via BDE, ADO, InterBase Express, dbExpress, DBISAM, FlashFiler, NexusDB, UIB and 
Advantage Database Server, plus a native, file-based XML storage.

OBJECT PERSISTENCE

One of the major issues when developing object-oriented applications is object
persistence. InstantObjects addresses this issue by providing a powerful persistence
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

To cover the needs of most developers, we offer two different approaches to install
InstantObjects.

The first one is oriented to proficient developers, and is the same method we use
during the development of InstantObjects (that is, on the CVS materials).
This method is very flexible, as you can choose which parts of InstantObjects you
will install and when.
It will require a good knowledge about package compile, build and install.

You will find detailed instructions in the Install.txt file located in:

    <installdir>/Docs
    
The second one is more oriented to normal developers that would prefer installing
some more features even if they know they will never use it, but by means of an
easier procedure that does most of the job for them.

You will find detailed instructions in the Install.txt file located in:

    <installdir>/Source/PackageGroups

One reminder though.
The second option will be kept up to date only for official releases (that is
Alpha, Beta, RC and final), so it will be unsupported for the real CVS stuff.
If you foresee downloading the "bleeding edge" of IO from CVS quite often,
we advice you to go for the first way and stay there for good.


DOCUMENTATION AND SUPPORT
-------------------------

The InstantObjects Guide IOHelp.hlp in the <installdir>\Help folder contains more
information about InstantObjects.

The section "User Guide" explains how to build applications with InstantObjects.
The section "Reference Guide" contains a detailed explanation of every class
and function in the InstantObjects framework.

Please note that this guide is not entirely up-to-date for IO v2. Most of the
new features, though, are documented in text files you can find in the Docs
folder.

You can also use read the wiki page at:
https://github.com/EtheaDev/InstantObjects/wiki


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

EvolveTest is a testbed application for the database structure evolution feature.
Please see the documentation to know more about database evolution and the
TInstantDBEvolver component.

Pump is a testbed application for the object pump feature.
Please see the documentation to know more about the TInstantPump component.

The Test folder contains test applications of various sorts.


VERSION HISTORY
---------------
Please see the "Changes.txt" file in the <installdir>/Docs folder.