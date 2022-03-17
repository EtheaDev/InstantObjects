HelpScribble project file.
16
PvOv`bsg.pbz-16O773
0
2
InstantObjects Help



TRUE
0x0409   English (U.S.)
D:\ETHEA\GitHub_EtheaDev\InstantObjects\InstantObjects\Help
1
BrowseButtons()
0
FALSE
D:\ETHEA\GitHub_EtheaDev\InstantObjects\InstantObjects\Help\
FALSE
TRUE
16777215
0
16711680
8388736
255
FALSE
FALSE
FALSE
FALSE
150
50
600
500
TRUE
FALSE
1
FALSE
FALSE
Contents
%s Contents
Index
%s Index
Previous
Next
FALSE
D:\ETHEA\EtheaNewSite\trunk\home\docs\InstantObjects\index.html
43
10
Scribble10
InstantObjects Welcome



main:000010
Done



FALSE
25
{\rtf1\ansi\ansicpg1252\deff0{\fonttbl{\f0\fswiss\fcharset0 Arial;}{\f1\fswiss Arial;}{\f2\fnil Arial;}{\f3\fnil\fcharset0 Courier New;}{\f4\fnil Courier New;}}
{\colortbl ;\red0\green0\blue0;\red128\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;}
\viewkind4\uc1\pard\sb25\sa25\cf1\lang1040\b\f0\fs32 Welcome to \f1 InstantObjects \f0 for Delphi
\par \cf2\b0\f2\fs16\{keepn\}\cf1\b\f1 
\par \b0\fs18 Welcome to InstantObjects, the integrated framework for developing object-oriented business solutions in Delphi\f0  (from 2010 to 11 Alexandria version)\f1 
\par 
\par InstantObjects is a RAD technology that enables creation of applications based on persistent business objects. It simplifies the process of realizing ideas into products; shortens time-to-marke\lang1033\f0 t\lang1040\f1 , and helps keep business focus.
\par 
\par Even though it is still a bolted\lang1033\f0 -\lang1040\f1 on technology, InstantObjects integrates with the Delphi IDE and Object Pascal in a simple, flexible and intuitive manner. There are no sophisticated environments that have to be comprehended before application development can begin.
\par 
\par \f0 You can find more information about this Open-Source project at:  \cf3\strike\f3\fs20 github.com/EtheaDev/InstantObjects\cf2\strike0\f4\{link=*! ExecFile("https://github.com/EtheaDev/InstantObjects\f3 /\f4 ")\}\cf1\f1\fs18 
\par \f0 
\par \pard\sb25\sa25\tx200 InstantObjects source files are released under the Mozilla Public License Version 2.0. \f1 You may obtain a copy of the License at\f0  \cf3\strike\f4\fs20 www.mozilla.org/en-US/MPL/2.0/\cf2\strike0\{link=*! ExecFile("https://www.mozilla.org/en-US/MPL/2.0/")\}\cf1\f0\fs18 .\b\fs32 
\par \pard\sb25\sa25 
\par \fs24 See also:
\par \cf4\b0\strike\fs18 Installing InstantObjects\cf2\strike0\f1\{linkID=\f0 4\f1 0>main\}\cf1\f0 
\par \cf4\strike User Guide\cf2\strike0\f1\{linkID=\f0 5\f1 0>main\}
\par \cf4\strike\f0 Learning the Primer Demo\cf2\strike0\f1\{linkID=\f0 46\f1 0>main\}
\par \cf4\strike\f0 Glossary\cf2\strike0\f1\{linkID=\f0 2\f1 0>main\}\cf1\b\f0\fs32 
\par \cf2\b0\f1\fs18 
\par \{bmc instantobjects_logo.gif\}
\par 
\par \cf1\f0 Copyright (c) - original developer Seleqt
\par Project maintainer - Ethea S.r.l.\f1 
\par }
20
Scribble20
Glossary




Writing



FALSE
24
{\rtf1\ansi\ansicpg1252\deff0\deflang1040{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}{\f2\fswiss Arial;}{\f3\fswiss\fcharset0 Arial;}}
{\colortbl ;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\lang1033\b\fs24 Glossary\lang1040\b0\f1\fs20 
\par \cf1\fs16\{keepn\}\cf0\fs20 
\par \cf2\lang1033\f0\fs18 The following definitions are provided to enhance the clarity, consistency and understanding of some of the terms used in this document.
\par \f1\fs10 
\par \b\f0\fs18 Attribute\b0  - means InstantObjects Attribute. A business class property with persistence provided by the InstantObjects framework.\cf0\b\f1\fs22 
\par \cf2\f0\fs18 Business Class\b0  - is a class that models an object in the business domain. The class is derived from TInstantObject to provide persistence through the InstantObjects framework.\b 
\par Class\b0  - is a blueprint from which objects are created.\b 
\par Column\b0  - see Field.
\par \b Connection\b0  - is a component used to gain access to a specific database.
\par \b Container Attribute\b0  - has an attribute type of either Parts or References.\b 
\par Evolving a database\b0  - the process of restructuring a database to accommodate a modified model in such a way that existing data is preserved.
\par \b Field\b0  - the smallest structure in a database. It represents an attribute of a business class and actually stores the attribute value within the database.
\par \b Broker\b0  - is used by InstantObjects to manage objects persistently, and is database specific.\b 
\par \pard\sb25\sa25 InstantObjects\b0  - \lang1040\f2 the integrated framework for developing object-oriented \lang1033\f3 persistent \lang1040\f2 business solutions in Delphi.\lang1033\b\f0 
\par \pard Model\b0  - means an InstantObjects Model that is a collection of classes and their metadata representing a business domain and the information needed to provide data persistence.\b 
\par Object\b0  - is an instance of a class.
\par \b Persistence\b0  - the issue of how to save objects to permanent storage. Persistence provides the ability for objects to exist between executions of a system or application.
\par \lang1040\b\f2 RAD\lang1033\f3  -\b0  Rapid Application Development.\f0 
\par \b Record\b0  - A structure that is composed of a set of values for every field within a table and represents the attribute values for a unique instance of the table's business class.\lang3081\f1 
\par \lang1033\b\f0 Relational Attribute\b0  - has an attribute type of either Part, Reference, Parts or References.\b 
\par Table\b0  - the chief structure in a database. It is composed of fields and records and usually represents the collection of attribute values for instances of a single, specific business class.\cf0\lang1040\f1\fs20 
\par }
40
Scribble40
Installing InstantObjects
Installing InstantObjects;InstantObjects,Installing;


main:000030
Modified



FALSE
10
{\rtf1\ansi\ansicpg1252\deff0\deflang1040{\fonttbl{\f0\fswiss\fcharset0 Arial;}{\f1\fswiss Arial;}{\f2\fnil Arial;}}
{\colortbl ;\red0\green0\blue0;}
\viewkind4\uc1\pard\b\f0\fs24 Installing \f1 InstantObjects\cf1\b0\fs16 
\par \pard\sb25\tx1435\{keepn\} 
\par \f0\fs18 
\par You will find detailed instructions in the \b Install.txt\b0  file located in:
\par \b     <installdir>/Source/PackageGroups
\par \pard\sb25\b0\f2\fs10 
\par \f0\fs18 
\par }
50
Scribble50
User Guide for InstantObjects
Using InstantObjects;InstantObjects,Using,Guide;


main:000040
Done



FALSE
16
{\rtf1\ansi\ansicpg1252\deff0{\fonttbl{\f0\fswiss Arial;}{\f1\fswiss\fcharset0 Arial;}{\f2\fnil Arial;}{\f3\fnil\fcharset2 Symbol;}}
{\colortbl ;\red0\green0\blue0;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\lang1040\b\f0\fs24 User Guide for InstantObjects\cf1\b0\fs16 
\par \{keepn\}
\par \pard\sb25\sa25\fs18 This user guide contains \f1 practical\f0  guidelines for building InstantObjects\lang1033\f1  \lang1040\f0 based applications. 
\par The intention is to give an overview o\f1 f\f0  the classes, components and tools that make up the InstantObjects framework. 
\par Although the guide is very detailed, it has not been the intention to cover all aspects of the framework. 
\par Th\lang1033\f1 is\lang1040\f0  guide \lang1033\f1 contains\lang1040\f0  three parts \lang1033\f1 that\lang1040\f0  constitute almost any InstantObjects based development process.
\par \f2\fs10 
\par \pard{\pntext\f3\'B7\tab}{\*\pn\pnlvlblt\pnf3\pnindent0{\pntxtb\'B7}}\keep\fi-200\li295\sb25\sa25\tx280\lang1033\f1\fs18  \cf2\lang1040\strike\f0 Creating the Business Model\cf3\strike0\{linkID=70>main\}\cf1 
\par \lang1033\f1{\pntext\f3\'B7\tab} \cf2\lang1040\strike\f0 Creating the User Interface\cf3\strike0\{linkID=280>main\}\cf1 
\par \lang1033\f1{\pntext\f3\'B7\tab} \cf2\lang1040\strike\f0 Programming with Persistent Objects\cf3\strike0\{linkID=340>main\}\cf1 
\par \pard\sb25\sa25\f2\fs10 
\par \pard\keep\li95\sb25\sa25\lang1033\f1\fs18 Plus a section on \cf2\lang1040\strike learning the Primer Demo\cf3\strike0\{linkID=460>main\}\cf1\lang1033 .\lang1040\f0 
\par \pard\sb25\sa25 
\par }
70
Scribble70
Creating the Business Model
Creating the Business Model;Business Model,Creating;


creatingbusinessmodel:000010
Done



FALSE
40
{\rtf1\ansi\ansicpg1252\deff0{\fonttbl{\f0\fswiss Arial;}{\f1\fswiss\fcharset0 Arial;}{\f2\fnil Arial;}{\f3\fnil\fcharset2 Symbol;}}
{\colortbl ;\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;}
\viewkind4\uc1\pard\lang1040\b\f0\fs24 Creating the Business Model\cf1\b0\fs16 
\par \pard\sb25\tx1435\cf2\{keepn\}\cf1  
\par \pard\sb25\sa25\tx1435\fs18 Object Oriented applications are based on models. This section covers the definition of the InstantObjects business\lang1033\f1  \lang1040\f0 model, which is \lang1033\f1  \lang1040\f0 the initial step in the creation of an InstantObjects based application. The InstantObjects Model Explorer is the central tool in this process.
\par \lang1033\f1 T\lang1040\f0 he InstantObjects business\lang1033\f1  \lang1040\f0 model\lang1033\f1  consists of a collection of user designed classes, many of which are related through inheritance or association. Most if not all of the data controlled by these classes is required to be persisted or stored in such a way that the relationships among this data are retained.  In addition to the simple class attribute types, which provide persistence for the \lang1040\f0 Object Pascal equivalent simple type\lang1033\f1 s (eg string or integer)\lang1040\f0  \lang1033\f1 , the InstantObjects framework provides several class attribute types to facilitate the persistence of instances of related classes. These attribute types are known as relational or complex attributes.
\par \pard\sb25\sa25\f2\fs10 
\par \cf2\fs18\{target=RelationalAttributeTypes\}\cf0\b\fs22 Re\lang1040 lational \lang1033 Attribute T\lang1040 ypes\cf1\b0\f0\fs18 
\par The relational types are\lang1033\f1 :\lang1040\f0  Reference, Part, References and Parts.\lang1033\f1  It is important to understand the nature of the relationship that each of these attribute types provides so that the desired behaviour is reflected within the \lang1040\f0 business\lang1033\f1  \lang1040\f0 model\lang1033\f1 .
\par \f2\fs10 
\par \lang1040\b\f1\fs18 Container attributes \b0 is the collective name for the subset of relational types consisting of Parts and References type attributes.\lang1033\b 
\par \b0\f2\fs10 
\par \lang1040\b\f0\fs18 Reference\b0 
\par Reference is the simplest object relation supported by InstantObjects. A reference defines a unidirectional relation from one object to another. The object referred to lives outside the referring object and knows nothing about the relation itself.
\par See \cf0\b TInstantReference\b0\f1  class\cf1\f0  for more information.
\par \f2\fs10 
\par \b\f0\fs18 Part\f1 
\par \b0\f0 Objects in Part relations are tied together more closely. A Part relation is bi-directional, meaning the object at each end of the relation knows about the object at the other end. The object referred to in a Part relation is considered to be a part of the owning object and as such can only be reached via the owning object. In addition, an object referred to in a part relation is disposed along with its owner.
\par See \cf0\b TInstantPart\cf2\b0\f1  \cf1 class \f0 for more information.
\par \f2\fs10 
\par \b\f0\fs18 References\b0 
\par Like its singular counterpart, Reference, the References relation is unidirectional. But instead of just referring to a single object, a References relation can refer to any number of objects and thereby defining a one-to-many relation to objects outside of the referring object.
\par See \cf0\b TInstantReference\f1 s\b0  class \cf1\f0 for more information.
\par \f2\fs10 
\par \b\f0\fs18 Parts\f1 
\par \b0\f0 Parts is the one-to-many counterpart of the equivalent one-to-one relation type, Part. A Parts relation can refer to any number of objects that are all considered to be part of the referring object.
\par See \cf0\b TInstantParts\cf1\b0\f1  class f\f0 or more information.
\par 
\par Parts and References are known as container attributes. When defining an attribute of one of these types, a corresponding array property and optional methods to access the container attribute \lang1033\f1 are\lang1040\f0  added to the class. For any of the relational types, the class of the related obj\f1 ec\f0 (s) must be specified as the Object Class of the attribute.
\par \f2\fs10 
\par \b\f1\fs18 Notes:
\par \pard\sb25\sa25\tx200\b0 For \i "External Storage support"\i0  of the Part, Parts and References attribute, refer to \cf3\strike External Storage of Attributes\cf2\strike0\{linkID=220\f0 >main\f1\}\cf1 .
\par \pard\sb25\sa25\tx1435\f0 
\par \f1 See also:\f0 
\par \pard{\pntext\f3\'B7\tab}{\*\pn\pnlvlblt\pnf3\pnindent0{\pntxtb\'B7}}\fi-200\li200\sb25\sa25\tx200\cf3\strike The \lang1033\f1 InstantObjects \lang1040\f0 Model Explorer\cf2\strike0\{linkID=90>main\}\cf1 
\par \cf3\lang1033\strike\f1{\pntext\f3\'B7\tab}Declaring Model Units\cf2\strike0\{linkID=110>main\}\cf1\lang1040\f0 
\par \cf3\strike{\pntext\f3\'B7\tab}Defining Classes\cf2\strike0\{linkID=120>main\}
\par \cf3\strike{\pntext\f3\'B7\tab}Adding Business Rules\cf2\strike0\{linkID=240>main\}
\par \cf3\strike{\pntext\f3\'B7\tab}Building/Evolving the Business Model\cf2\strike0\{linkID=270>main\}\cf1 
\par }
90
Scribble90
The InstantObjects Model Explorer
InstantObjects Model Explorer;Model Explorer;


creatingbusinessmodel:000020
Modified



FALSE
29
{\rtf1\ansi\ansicpg1252\deff0{\fonttbl{\f0\fswiss Arial;}{\f1\fswiss\fcharset0 Arial;}{\f2\fnil Arial;}{\f3\fnil\fcharset0 Arial;}{\f4\fnil\fcharset2 Symbol;}}
{\colortbl ;\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;}
\viewkind4\uc1\pard\lang1040\b\f0\fs24 The InstantObjects Model Explorer \cf1\b0\fs16 
\par \cf2\{keepn\}\cf1  
\par \pard\sb25\sa25\fs18 The first step in building an application with InstantObjects is to define the classes and relations that make up the business model. The business model\lang1033\f1  for the application\lang1040\f0  is created\lang1033\f1  and managed\lang1040\f0  in the \lang1033\f1 InstantObjects \lang1040\f0 Model Explorer, which can be opened via the View\lang1033\f1  \lang1040\f0 menu in the Delphi IDE.\lang1033\f1  T\lang1040\f0 he \lang1033\f1 InstantObjects \lang1040\f0 Model Explorer\lang1033\f1  window is non-modal and can be docked, if desired, in the Delphi IDE. This allows the \lang1040\f0 Explorer\lang1033\f1  window to remain open while working in other areas of the IDE.
\par \lang1040\f2\fs10 
\par \cf0\b\fs22 Th\lang1033\f3 e \lang1040\f2 InstantObjects Model Explorer \lang1033\f3 (\lang1040\f2 with \f3 "\f2 Inheritance\f3 "\f2  view of classes\lang1033\f3 )\cf2\lang1040\b0\f0\fs18 
\par \{bmc ModelExplorerInheritance.gif\}
\par \cf1\f2\fs10 
\par \lang1033\f1\fs18 T\lang1040\f0 he \lang1033\f1 InstantObjects \lang1040\f0 Model Explorer\lang1033\f1  window has two main areas of interest.
\par \f2\fs10 
\par \pard{\pntext\f4\'B7\tab}{\*\pn\pnlvlblt\pnf4\pnindent0{\pntxtb\'B7}}\fi-200\li200\sb25\sa25\tx200\f1\fs18 The toolbar that holds the action speed buttons. \f3 The speed buttons display mouse-over, pop-up hints. These speed buttons provide access to the configuration aspects for model code units, persistence broker connections and type of class view presented in the tree view pane;\cf0\lang1040\b\f2\fs22 
\par \pard\sb25\sa25\cf1\b0\fs10 
\par \pard{\pntext\f4\'B7\tab}{\*\pn\pnlvlblt\pnf4\pnindent0{\pntxtb\'B7}}\fi-200\li200\sb25\sa25\tx200\lang1033\f1\fs18 The tree view pane that displays the model's classes.\cf0\b\f3\fs22  \cf1\b0\fs18 The tree view pane has a mouse right button click, \cf3\strike pop-up menu\cf2\strike0\{linkID=100>main\}\cf1  that is used to access the procedures for managing the model.\cf0\lang1040\b\f2\fs22 
\par \pard\sb25\sa25\cf1\b0\fs10 
\par \cf0\lang1033\b\fs22 Class Views
\par \cf1\b0\fs18 A\f3  useful feature of the \f1 InstantObjects \lang1040\f0 Model Explorer\lang1033\f1  when creating or managing the class model is the ability to toggle \f3 the tree view pane \f1 between inheritance and relational  type views. This is done using the 'View [Type]' speed button on the toolbar. The button has a context sensitive glyph, \cf2\{bmc ModelExplorerViewRelationsButton.gif\} \cf0 or\cf1  \cf2\{bmc ModelExplorerViewInheritanceButton.gif\}\cf0 ,\cf1  and pop-up hint.
\par \f2\fs10 
\par \pard{\pntext\f4\'B7\tab}{\*\pn\pnlvlblt\pnf4\pnindent0{\pntxtb\'B7}}\fi-200\li200\sb25\sa25\tx200\f3\fs18 Inheritance View - The tree view is based on the class inheritance structure;
\par \pard\sb25\sa25\f2\fs10 
\par \pard{\pntext\f4\'B7\tab}{\*\pn\pnlvlblt\pnf4\pnindent0{\pntxtb\'B7}}\fi-200\li200\sb25\sa25\tx200\f3\fs18 Relations View - The tree view is based on the relationships between the classes.
\par \pard\sb25\sa25\f2\fs10 
\par \f1\fs18 The Relations view for the above example Inheritance view is shown below.\cf0\lang1040\b\f2\fs22 
\par The InstantObjects Model Explorer \lang1033\f3 (\lang1040\f2 with \f3 "Relations"\f2  view of classes\lang1033\f3 )\cf2\lang1040\b0\f0\fs18 
\par \{bmc ModelExplorerRelations.gif\}\cf1 
\par \pard\cf0\lang1033\b\f3\fs24 
\par The InstantObjects Model Explorer Pop-up Menu
\par \pard\sb25\sa25\cf2\b0\f2\fs18\{bmc ModelExplorerPop-upMenu.gif\}\cf1\lang1040\f0 
\par }
100
Scribble100
The InstantObjects Model Explorer Pop-up Menu




Writing



FALSE
8
{\rtf1\ansi\ansicpg1252\deff0\deflang1040{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red128\green0\blue0;}
\viewkind4\uc1\pard\lang1033\b\fs24 The InstantObjects Model Explorer Pop-up Menu
\par 
\par \cf1\b0\f1\fs18\{bmc ModelExplorerPop-upMenu.gif\}\cf0\lang1040\fs20 
\par 
\par 
\par }
110
Scribble110
Declaring Model Units
Declaring Model Units;Model Units,Declaring;


creatingbusinessmodel:000030
Modified



FALSE
11
{\rtf1\ansi\ansicpg1252\deff0\deflang3081{\fonttbl{\f0\fnil Arial;}{\f1\fswiss Arial;}{\f2\fswiss\fcharset0 Arial;}{\f3\fnil\fcharset0 Arial;}}
{\colortbl ;\red128\green0\blue0;\red0\green0\blue0;\red0\green128\blue0;}
\viewkind4\uc1\pard\b\f0\fs24 Declaring Model Units
\par \cf1\lang1040\b0\f1\fs16\{keepn\}\cf2  
\par \pard\sb25\sa25\lang1033\f2\fs18 It is good practice to keep an application's business classes encapsulated and separated from other classes such as the user interface classes. The convention when using InstantObjects is to create separate code units within the application's IDE project solely for the purpose of containing the \lang1040\f1 business \lang1033\f2 classes that constitute an InstantObjects model. \lang1040\f1 Th\lang1033\f2 is\lang1040\f1  model can be placed in one or more \lang1033\f2 code \lang1040\f1 units in the project. These units are referred to as model units.
\par \f0\fs10 
\par \lang1033\f2\fs18 The InstantObjects Model Explorer needs to know which of the application project's units are intended to be model units. This is done using\lang1040\f1  the \cf1\{bmc ModelExplorerSelectUnitsButton.gif\}\cf2 Select Units button on the toolbar of the InstantObjects Model Explorer. \lang1033\f2 T\lang1040\f1 he Select Units button\lang1033\f2  \lang1040\f1 will open a dialog showing all the units of the current project. The left side of the dialog shows the model units and the right side shows the other units included in the project. Use the buttons between the two lists to move one or more units from one side to the other. \lang1033\f2  Select the OK or Cancel button to close the dialog and save or cancel, respectively, any changes. \lang1040\f1 When \lang1033\f2 the \lang1040\f1 model unit(s)\lang1033\f2  has been declared \lang1040\f1 the business model\lang1033\f2  can be created or maintained using the \f3 tree view pane's right button click, \cf3\strike pop-up menu\cf1\strike0\{linkID=100>main\}\cf2\lang1040\f1 .
\par \f0\fs10 
\par \cf1\f1\fs18\{bmc ModelUnitSelector.gif\}\cf2 
\par \pard\lang3081\f0 
\par }
120
Scribble120
Defining Classes
Defining Classes;Classes,Defining;


creatingbusinessmodel:000040
Modified



FALSE
13
{\rtf1\ansi\ansicpg1252\deff0{\fonttbl{\f0\fswiss Arial;}{\f1\fswiss\fcharset0 Arial;}{\f2\fnil Arial;}{\f3\fnil\fcharset2 Symbol;}}
{\colortbl ;\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;}
\viewkind4\uc1\pard\lang1040\b\f0\fs24 Defining Classes \cf1\b0\fs16 
\par \cf2\{keepn\}\cf1  
\par \pard\sb25\sa25\fs18 Classes can be added\lang1033\f1 , deleted\lang1040\f0  and edited\lang1033\f1  as follows (note that keyboard short cuts are also available):
\par \lang1040\f0\fs10 
\par \pard{\pntext\f3\'B7\tab}{\*\pn\pnlvlblt\pnf3\pnindent0{\pntxtb\'B7}}\fi-200\li200\sb25\sa25\tx200\fs18 Add a new class to the model by right-clicking in the empty area of the \lang1033\f1 tree view pane in InstantObjects \lang1040\f0 Model Explorer\lang1033\f1 . This will launch the \cf3\strike pop-up menu\cf2\strike0\{linkID=100>main\}\cf1\lang1040\f0 . Select New Class from the menu\lang1033\f1  to\lang1040\f0  bring up the Class Editor with an empty class definition\lang1033\f1 ;\lang1040\f0 
\par \pard\sb25\sa25\f2\fs10 
\par \pard{\pntext\f3\'B7\tab}{\*\pn\pnlvlblt\pnf3\pnindent0{\pntxtb\'B7}}\fi-200\li200\sb25\sa25\tx200\lang1033\f1\fs18 Delete a class selected in the tree view pane \lang1040\f0 by right-clicking\lang1033\f1  on it and selecting Delete Class from the \cf3\strike pop-up menu\cf2\strike0\{linkID=100>main\}\cf0 .  As expected, confirmation dialog is launched before the delete action is accepted;\cf1\lang1040\f0 
\par \pard\sb25\sa25\f2\fs10 
\par \pard{\pntext\f3\'B7\tab}{\*\pn\pnlvlblt\pnf3\pnindent0{\pntxtb\'B7}}\fi-200\li200\sb25\sa25\tx200\lang1033\f1\fs18 Edit a class selected in the tree view pane \lang1040\f0 by right-clicking\lang1033\f1  on it and selecting Edit Class from the \cf3\strike pop-up menu\cf2\strike0\{linkID=100>main\}\cf0 . This will \cf1\lang1040\f0 bring up the Class Editor \lang1033\f1 and will allow editing of many of the class's properties and attributes.\cf0\lang1040\f0 
\par \pard\sb25\sa25\tx200\cf2 
\par }
130
Scribble130
Class Editor
Class Editor;Editor, Class;


creatingbusinessmodel:000050
Modified



FALSE
12
{\rtf1\ansi\ansicpg1252\deff0\deflang3081{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}{\f2\fswiss Arial;}{\f3\fnil\fcharset2 Symbol;}}
{\colortbl ;\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;}
\viewkind4\uc1\pard\lang1033\b\fs24 Class Editor\cf1\lang3081\b0\f1\fs18 
\par \cf2\{keepn\}\cf1 
\par \lang1033\f0 The Class Editor is used to manage a class's properties and attributes. It contains the tabbed pages as follows:
\par \f1\fs10 
\par \pard{\pntext\f3\'B7\tab}{\*\pn\pnlvlblt\pnf3\pnindent0{\pntxtb\'B7}}\fi-200\li200\tx200\cf3\strike\f0\fs18 Class Page\cf2\strike0\{linkID=140>main\}\cf1  - This is where the class's properties are defined;\lang3081\f1 
\par \pard\fs10 
\par \pard{\pntext\f3\'B7\tab}{\*\pn\pnlvlblt\pnf3\pnindent0{\pntxtb\'B7}}\fi-200\li200\tx200\cf3\lang1033\strike\f0\fs18 Attributes Page\cf2\strike0\{linkID=150>main\}\cf1  - This is where the class attributes are managed.\lang3081\f1 
\par \pard\sb25\sa25\lang1040\f2 
\par \pard\lang3081\f1 
\par }
140
Scribble140
Class Editor - Class Page
Class Editor,Class Page;


creatingbusinessmodel:000060
Modified



FALSE
16
{\rtf1\ansi\ansicpg1252\deff0\deflang3081{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}{\f2\fswiss Arial;}{\f3\fswiss\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;}
\viewkind4\uc1\pard\lang1033\b\fs24 Class Editor - Class Page\cf1\lang3081\b0\f1\fs18 
\par \cf2\{keepn\}\cf1 
\par \pard\sb25\sa25\lang1040\f2 This is where you specify the name and base class, the model unit to hold the class and the persistence \lang1033\f3 properties\lang1040\f2  of the class.
\par \f1\fs10 
\par \cf2\f2\fs18\{bmc ClassEditor\lang1033\f3 Class\lang1040\f2 .gif\}
\par \cf1\f1\fs10 
\par \lang1033\f3\fs18 The \b class name \b0 can be any valid Object Pascal class identifier.\lang1040\f2 
\par The default \b base class \b0 of any business class is \cf0\b TInstantObject\cf1\b0 , the class from which persistence capabilities are inherited. When classes are present in your model, you can choose any of these as the immediate base class instead.
\par The \lang1033\f3 model \lang1040\b\f2 unit \b0 in which \lang1033\f3 a\lang1040\f2  new class \lang1033\f3 will\lang1040\f2  be placed can be selected from the list of available model units.\lang1033\f3  This entry is read only when editing an existing class.\lang1040\f2 
\par The class\lang1033\f3  \b persistence\lang1040\f2  \b0 can be defined as either \lang1033\i\f3 stored\lang1040\f2  \i0 or \i embedded\i0 . Instances of \lang1033\i\f3 stored\lang1040\f2  \i0 classes can be \lang1033\f3 independently \lang1040\f2 stored in and retrieved from \lang1033\f3 the database\lang1040\f2 . \lang1033\f3 An i\lang1040\f2 nstance of \lang1033\f3 an \lang1040\i\f2 embedded \i0 class can exist in the database only as \lang1033\f3 an attribute\lang1040\f2  of \lang1033\f3 its owner, which is \lang1040\f2 another \i embedded \i0 or \lang1033\i\f3 stored\lang1040\f2  \lang1033\i0\f3 class instance. In practice this means that the attribute values of an \lang1040\i\f2 embedded \i0 class \lang1033\f3 are stored in the database in a BLOB type field.\lang1040\f2  \lang1033\f3 Therefore, i\lang1040\f2 f you want instances of a class to be \lang1033\f3 independently \lang1040\f2 retrievable or available by query, define the class\lang1033\f3  persistence as\lang1040\f2  \lang1033\i\f3 stored\lang1040\i0\f2 .
\par \f3 S\i tored \i0 persistence is also required for those classes used by Reference attributes or other \cf3\strike attributes with \i external \i0 storage\cf2\strike0\{linkID=220>main\}\cf1 . A class, used only for objects that are owned by other objects, should be defined with \i embedded \i0 persistence if the owning objects use \i embedded \i0 storage for the attribute. If, however, the owning objects use \i external \i0 storage kind for the attribute, define the owned class with \i stored \i0 persistence.\f2 
\par For classes\lang1033\f3  with \i stored\lang1040\f2  \lang1033\i0\f3 persistence\lang1040\f2 , a \b storage name \b0 can be specified. The storage name defines the \lang1033\f3 name\lang1040\f2  \lang1033\f3 of the \lang1040\f2 table\lang1033\f3 , \lang1040\f2 in which attributes of instances of the class are stored. By default, the storage name equals the class name without the preceding \lang1033\f3 '\lang1040\f2 T\lang1033\f3 '\lang1040\f2 . Change the \lang1033\f3 default \lang1040\f2 storage name \lang1033\f3 as desired\lang1040\f2 .
\par \pard\lang3081\f1 
\par }
150
Scribble150
Class Editor - Attributes Page
Class Editor,Attributes Page;


creatingbusinessmodel:000070
Modified



FALSE
19
{\rtf1\ansi\ansicpg1252\deff0\deflang3081{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}{\f2\fswiss\fcharset0 Arial;}{\f3\fswiss Arial;}{\f4\fnil\fcharset2 Symbol;}}
{\colortbl ;\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;}
\viewkind4\uc1\pard\lang1033\b\fs24 Class Editor - Attributes Page\cf1\lang3081\b0\f1\fs18 
\par \cf2\{keepn\}\cf1 
\par \pard\sb25\sa25\lang1033\f2 This where the\lang1040\f3  attributes of the class\lang1033\f2  are managed\lang1040\f3 . The top view of the page lists the attributes that are \lang1033\b\f2 I\lang1040\f3 ntroduced \b0 by the class. The bottom view lists the attributes that are \lang1033\b\f2 I\lang1040\f3 nherited \b0 from the base class.
\par \f1\fs10 
\par \cf2\f3\fs18\{bmc ClassEditorAttributes.gif\}
\par \cf1\f1\fs10 
\par \f3\fs18 Attributes can be added\lang1033\f2 , deleted\lang1040\f3  and edited\lang1033\f2  as follows (note that keyboard short cuts are also available):
\par \lang1040\f1\fs10 
\par \pard{\pntext\f4\'B7\tab}{\*\pn\pnlvlblt\pnf4\pnindent0{\pntxtb\'B7}}\fi-200\li200\sb25\sa25\tx200\f3\fs18 Add a new \lang1033\f2 a\lang1040\f3 ttribute\lang1033\f2  \lang1040\f3 to the model by right-clicking in the \lang1033\b\f2 Introduced \b0 view pane. This will launch the \cf0 pop-up menu\cf1\lang1040\f3 . Select \lang1033\f2 New\lang1040\f3  from the menu\lang1033\f2  to\lang1040\f3  bring up the \cf3\lang1033\strike\f2 Attribute\lang1040\f3  Editor\cf2\strike0\{linkID=170>main\}\cf1  with an empty \lang1033\f2 attribute\lang1040\f3  definition\lang1033\f2  so that the new a\lang1040\f3 ttribute\lang1033\f2 's properties can be set;\lang1040\f3 
\par \pard\sb25\sa25\f1\fs10 
\par \pard{\pntext\f4\'B7\tab}{\*\pn\pnlvlblt\pnf4\pnindent0{\pntxtb\'B7}}\fi-200\li200\sb25\sa25\tx200\lang1033\f2\fs18 Delete an a\lang1040\f3 ttribute\lang1033\f2  selected in the \b Introduced \b0 view pane \lang1040\f3 by right-clicking\lang1033\f2  on it and selecting Delete from the \cf0 pop-up menu.  As expected, a confirmation dialog is launched before the delete action is accepted;\cf1\lang1040\f3 
\par \pard\sb25\sa25\f1\fs10 
\par \pard{\pntext\f4\'B7\tab}{\*\pn\pnlvlblt\pnf4\pnindent0{\pntxtb\'B7}}\fi-200\li200\sb25\sa25\tx200\lang1033\f2\fs18 Edit an a\lang1040\f3 ttribute\lang1033\f2  selected in the \b Introduced \b0 view pane \lang1040\f3 by right-clicking\lang1033\f2  on it and selecting Edit from the \cf0 pop-up menu. This will \cf1\lang1040\f3 bring up the \cf3\lang1033\strike\f2 Attribute\lang1040\f3  Editor\cf2\strike0\{linkID=170>main\}\cf1  \lang1033\f2 and will allow editing of many of the a\lang1040\f3 ttribute\lang1033\f2 's properties.\cf0\lang1040\f3 
\par \pard\cf1\lang3081\f1 
\par \cf0\lang1033\b\f0\fs20 Class Editor - Attributes Page Pop-up menu\fs24 
\par \cf2\b0\f1\fs18\{bmc ClassEditorAttributesPop-upMenu.gif\}\cf1\lang3081 
\par }
170
Scribble170
Attribute Editor
Attribute Editor;Defining Attributes;Attributes,Editor;


creatingbusinessmodel:000080
Modified



FALSE
15
{\rtf1\ansi\ansicpg1252\deff0{\fonttbl{\f0\fswiss Arial;}{\f1\fnil\fcharset0 Arial;}{\f2\fnil Arial;}{\f3\fnil\fcharset2 Symbol;}}
{\colortbl ;\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;}
\viewkind4\uc1\pard\lang1040\b\f0\fs24 Attribute Editor \cf1\b0\fs16 
\par \cf2\{keepn\}\cf1  
\par \lang1033\f1\fs18 The Attribute Editor is used to manage an attribute's properties. It contains the tabbed pages as follows:
\par \f2\fs10 
\par \pard{\pntext\f3\'B7\tab}{\*\pn\pnlvlblt\pnf3\pnindent0{\pntxtb\'B7}}\fi-200\li200\tx200\cf3\strike\f1\fs18 Definition Page\cf2\strike0\{linkID=190>main\}\cf1  - This is where the attribute's properties are defined;\lang3081\f2 
\par \pard\fs10 
\par \pard{\pntext\f3\'B7\tab}{\*\pn\pnlvlblt\pnf3\pnindent0{\pntxtb\'B7}}\fi-200\li200\tx200\cf3\lang1033\strike\f1\fs18 Access Page\cf2\strike0\{linkID=200>main\}\cf1  - This is where access for the attribute is defined;\lang3081\f2 
\par \pard\fs10 
\par \pard{\pntext\f3\'B7\tab}{\*\pn\pnlvlblt\pnf3\pnindent0{\pntxtb\'B7}}\fi-200\li200\sb25\sa25\tx200\cf3\lang1033\strike\f1\fs18 Presentation Page\cf2\strike0\{linkID=210>main\}\cf1  - This is where the attribute's presentation and default properties are defined.\lang1040\f0 
\par \pard\sb55\sa25\cf0\b\fs24 
\par See Also\cf1\b0\f2\fs10 
\par \pard\sb25\sa25\tx200\cf3\strike\fs18 External Storage of Attributes\cf2\strike0\{linkID=220>main\}\cf1\lang1033\f1 
\par }
190
Scribble190
Attribute Editor - Definition Page
Attribute Editor,Definition Page;


creatingbusinessmodel:000090
Modified



FALSE
13
{\rtf1\ansi\ansicpg1252\deff0\deflang3081{\fonttbl{\f0\fnil Arial;}{\f1\fswiss Arial;}{\f2\fswiss\fcharset0 Arial;}{\f3\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;}
\viewkind4\uc1\pard\b\f0\fs24 Attribute Editor - Definition Page\cf1\b0\fs18 
\par \cf2\{keepn\}\cf1 
\par \pard\sb25\sa25\lang1040\f1 This is where the \lang1033\f2 main properties \lang1040\f1 of \lang1033\f2 an\lang1040\f1  attribute \lang1033\f2 are managed\lang1040\f1 .\lang1033\f2  The edit controls on this page are context sensitive. The context varies based on attribute type and whether the attribute is new or existing.\lang1040\f1 
\par \f0\fs10 
\par \cf2\f1\fs18\{bmc AttributeEditorDefinition.gif\}
\par \cf1\f0\fs10 
\par \f1\fs18 The \b name \b0 of the attribute can be any valid Object Pascal identifier. The \b storage name \b0 specifies the \lang1033\f2 table\lang1040\f1  column \lang1033\f2 name \lang1040\f1 in which attribute values are stored. By default, the storage name is the same as the attribute name.
\par The \b type \b0 of the attribute can be selected from the list of all the types supported by InstantObjects. Some of the available types are known as simple types, others are known as relational\lang1033\f2  \lang1040\f1 types. The simple types generally have an Object Pascal equivalent simple type\lang1033\f2  (eg string or integer)\lang1040\f1 . The \cf3\strike relational types\cf2\strike0\{linkTarget=RelationalAttributeTypes>main\}\lang1033\f2  \cf1\lang1040\f1 are used to define relations\lang1033\f2 hips\lang1040\f1  \lang1033\f2 between\lang1040\f1  instances of the\lang1033\f2  attribute's owner\lang1040\f1  class \lang1033\f2 and\lang1040\f1  objects\lang1033\f2  of \lang1040\f1 other \lang1033\f2 classes\lang1040\f1 .\lang1033\f2  \lang1040\f1 The relational types are\lang1033\f2 :\lang1040\f1  Reference, Part, References and Parts.
\par \lang1033\f2 When defining relational \lang1040\f1 attribute \lang1033\f2 types the \b object class\b0  entry is used to nominate the business class type that the attribute will be referencing. The list of available existing classes is presented for selection in the drop-down list.\lang1040\f1 
\par \lang1033\f3 See \cf3\strike h\lang1040\f0 ow to use external storage\cf2\strike0\{linkTarget=HowToUseExternalStorage>main\}\cf0\lang1033\f3  for information on usage of the \b storage kind\b0  and \b external storage name\b0  entries.\cf1\lang3081\f0 
\par }
200
Scribble200
Attribute Editor - Access Page
Attribute Editor,Access Page;


creatingbusinessmodel:000110
Modified



FALSE
14
{\rtf1\ansi\ansicpg1252\deff0\deflang3081{\fonttbl{\f0\fnil Arial;}{\f1\fswiss\fcharset0 Arial;}{\f2\fswiss Arial;}}
{\colortbl ;\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;}
\viewkind4\uc1\pard\b\f0\fs24 Attribute Editor - Access Page\cf1\b0\fs18 
\par \cf2\{keepn\}\cf1 
\par \pard\sb25\sa25\lang1033\f1 This is where the\lang1040\f2  \lang1033\f1 properties relating to \lang1040\f2 access \lang1033\f1 to \lang1040\f2 the attribute \lang1033\f1 are\lang1040\f2  specif\lang1033\f1 ied\lang1040\f2 . \lang1033\f1 The edit controls on this page are context sensitive. The context varies based on attribute type and whether the attribute is new or existing.\lang1040\f2 
\par \f0\fs10 
\par \cf2\f2\fs18\{bmc AttributeEditorAccess.gif\}
\par \cf1\f0\fs10 
\par \f2\fs18 For every \lang1033\f1 persistent \lang1040\f2 attribute a corresponding property is added to the class\lang1033\f1 .\lang1040\f2  \lang1033\f1 T\lang1040\f2 he value of the attribute \lang1033\f1 is \lang1040\f2 accessed\lang1033\f1  through this property\lang1040\f2 .
\par The \lang1033\b\f1 V\lang1040\f2 isibility \lang1033\b0\f1 entry sets the\i  \i0 class \lang1040\f2 visibility scope of this property. 
\par \lang1033\f1 The \b Options \b0 check box group offers options for simple attribute properties to be \b indexed \b0 (in its table), \b required \b0 or \b readonly\b0 . The \b Default \b0 option relates to \cf3\strike container \lang1040\f0 attributes\cf2\strike0\{linkTarget=\lang1033 RelationalAttributeTypes\lang1040 >main\}\cf1\lang1033\f1 . It specifies whether this attribute is the default container attribute for its class.\lang1040\f2 
\par \lang1033\f1 The \b\f0 M\lang1040 ethods \lang1033\b0\f1 check box group relate to\lang1040\f2  \cf3\lang1033\strike\f1 container \lang1040\f0 attributes\cf2\strike0\{linkTarget=\lang1033 RelationalAttributeTypes\lang1040 >main\}\cf1\lang1033\f1 .\lang1040\f2  \lang1033\f1 M\lang1040\f2 ethods can be selected to manipulate the content of the attribute. Th\lang1033\f1 e selected\lang1040\f2  methods will be created with the same visibility scope as the property.\lang1033\f1  The \b Singular Name \b0 entry will be used when creating these methods. The Attribute Editor offers a default singular name derived from the attribute Name entered on the Definition page, however this might not always be appropriate and in these cases the entry can be edited. \lang1040\f2 In addition to the array property created to access objects contained in \lang1033\f1 these\lang1040\f2  attributes, a count property reflecting the number of objects in the container will\lang1033\f1  also\lang1040\f2  be added.
\par \pard\lang3081\f0 
\par }
210
Scribble210
Attribute Editor - Presentation Page
Attribute Editor,Presentation Page;


creatingbusinessmodel:000120
Modified



FALSE
12
{\rtf1\ansi\ansicpg1252\deff0\deflang3081{\fonttbl{\f0\fnil Arial;}{\f1\fswiss\fcharset0 Arial;}{\f2\fswiss Arial;}{\f3\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;}
\viewkind4\uc1\pard\b\f0\fs24 Attribute Editor - Presentation Page\cf1\b0\fs18 
\par \cf2\{keepn\}\cf1 
\par \pard\sb25\sa25\lang1033\f1 This where the\lang1040\f2  \lang1033\f1 properties relating to the presentation and other miscellaneous aspects\lang1040\f2  \lang1033\f1 of \lang1040\f2 the attribute \lang1033\f1 is\lang1040\f2  specif\lang1033\f1 ied\lang1040\f2 . \lang1033\f1 The visibility of this page is context sensitive based on attribute type.\lang1040\f2 
\par \f0\fs10 
\par \cf2\f2\fs18\{bmc AttributeEditor\lang1033\f1 Presentation\lang1040\f2 .gif\}
\par \cf1\f0\fs10 
\par \f2\fs18 For some of the simple attribute types, settings regarding the presentation of the attribute can be specified on the last page of the Attribute Editor. An \b edit mask \b0 to be used when editing the value of the attribute can be specified in Mask. The characters that are accepted during input when editing the attribute value can be specified in \b Valid Characters\b0 .\lang1033\f1  \b Display Width \b0 can be used to set the default display width to be used when \lang1040\f2 editing the value of the attribute\lang1033\f1 . These properties are used by the \lang1040\f2 InstantObjects \cf3\strike user interface\lang1033\f1  \lang1040\f2 components\cf2\strike0\{linkID=210>main\}\cf1\lang1033\f1 .\lang1040\f2  
\par \pard\lang3081\f0 
\par \lang1033\f3 A \b Default Value \b0 can be entered that will be used as the initial value for new attribute instances and whenever the value is re-initialised (eg by calling the attribute's Reset method).\lang3081\f0 
\par }
220
Scribble220
External Storage of Attributes
External Storage of Attributes;Attributes,External Storage;


creatingbusinessmodel:000100
Modified



FALSE
32
{\rtf1\ansi\ansicpg1252\deff0\deflang1040{\fonttbl{\f0\fswiss\fcharset0 Arial;}{\f1\fswiss Arial;}{\f2\fnil Arial;}{\f3\fnil\fcharset2 Symbol;}}
{\colortbl ;\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;}
\viewkind4\uc1\pard\b\f0\fs24 External Storage of Attributes\cf1\b0\f1\fs16 
\par \pard\sb25\sa25\cf2\{keepn\} \cf1\f0\fs18 
\par \lang1033 The E\lang1040\f1 xternal \lang1033\f0 S\lang1040\f1 torage\lang1033\f0  option\lang1040\f1  \lang1033\f0 for \cf3\strike relational attributes\cf2\strike0\{linkTarget=RelationalAttributeTypes>main\}\cf1\lang1040\f1  \lang1033\f0 was introduced to resolve several shortcomings and issues in earlier versions of InstantObjects. These include the following:
\par \f2\fs10 
\par \pard{\pntext\f3\'B7\tab}{\*\pn\pnlvlblt\pnf3\pnindent0{\pntxtb\'B7}}\fi-200\li200\sb25\sa25\tx200\f0\fs18 the \lang1040\f1 inability to fully query the database from a SQL-enabled (non-IO) interface;
\par \pard\sb25\sa25\f2\fs10 
\par \pard{\pntext\f3\'B7\tab}{\*\pn\pnlvlblt\pnf3\pnindent0{\pntxtb\'B7}}\fi-200\li200\sb25\sa25\tx200\lang1033\f0\fs18 the \lang1040\f1 inability to define foreign keys to enforce referential integrity at the database level;
\par \pard\sb25\sa25\f2\fs10 
\par \pard{\pntext\f3\'B7\tab}{\*\pn\pnlvlblt\pnf3\pnindent0{\pntxtb\'B7}}\fi-200\li200\sb25\sa25\tx200\f1\fs18 difficulty in repairing a "corrupted"\lang1033\f0  \lang1040\f1 blob\lang1033\f0  due to the nature of its binary storage. This was somewhat alleviated with the introduction of the XML option for blobs but had the added problem of increased blob size and resulted in poorer performance\lang1040\f1 ;
\par \pard\sb25\sa25\f2\fs10 
\par \pard{\pntext\f3\'B7\tab}{\*\pn\pnlvlblt\pnf3\pnindent0{\pntxtb\'B7}}\fi-200\li200\sb25\sa25\tx200\lang1033\f0\fs18 the \lang1040\f1 poor retrieve performance due to \lang1033\f0 the \lang1040\f1 missing implementation of load-on-demand feature for part, parts and references attributes\lang1033\f0 .\lang1040\f1 
\par \pard\sb25\sa25\f2\fs10 
\par \lang1033\f0\fs18 The e\lang1040\f1 xternal storage\lang1033\f0  option\lang1040\f1  \lang1033\f0 provides\lang1040\f1  the ability to \lang1033\f0 specify that\lang1040\f1  a particular part, parts or reference\f0 s\f1  attribute should be mapped outside of \lang1033\f0 its\lang1040\f1  class\lang1033\f0  \lang1040\f1 table\lang1033\f0 . For \cf3\strike container attributes\cf2\strike0\{linkTarget=RelationalAttributeTypes>main\}\cf1\lang1040\f1  \lang1033\f0 this mapping is implemented through an\lang1040\f1  additional \lang1033\f0 linking\lang1040\f1  \lang1033\f0 table\lang1040\f1 . In the case of part attributes, the intermediate \lang1033\f0 object\lang1040\f1  is not needed because the relationship \lang1033\f0 between\lang1040\f1  the owner and the part is 1:1.
\par \f2\fs10 
\par \f1\fs18 Th\lang1033\f0 e\lang1040\f1  \lang1033\f0 linking\lang1040\f1  table has fields to hold information about\lang1033\f0  both\lang1040\f1  the relationships and the sequence of elements in the \lang1033\f0 container\lang1040\f1  attribute.\f2\fs10 
\par \f1\fs18 The mapping of the \lang1033\f0 external reference(s)\lang1040\f1  attribute is very similar\lang1033\f0  to that for the external p\lang1040\f1 art\lang1033\f0 (\lang1040\f1 s\lang1033\f0 ) \lang1040\f1 attribute\lang1033\f0 .\lang1040\f1  \lang1033\f0 T\lang1040\f1 he only difference is \lang1033\f0 one of ownership which results in \lang1040\f1 th\lang1033\f0 e\lang1040\f1  part\lang1033\f0 (\lang1040\f1 s\lang1033\f0 )\lang1040\f1  attribute's \lang1033\f0 table record(s)\lang1040\f1  \lang1033\f0 being\lang1040\f1  deleted when the \lang1033\f0 its related class\lang1040\f1  \lang1033\f0 table record\lang1040\f1  is deleted\lang1033\f0 .\lang1040\f1  \lang1033\f0 T\lang1040\f1 his of course does not happen with reference\lang1033\f0 (\lang1040\f1 s\lang1033\f0 )\lang1040\f1  attributes.
\par \f2\fs10 
\par \cf2\b\fs22\{target=HowToUseExternalStorage\}\cf0 How to use external storage\cf1\ul\f1\fs18 
\par \ulnone\b0\f0 The external storage option can be applied in the \f2 Attribute Editor\f0  by setting \i Storage Kind \i0 to the value "External" when defining a new attribute.
\par \f2\fs10 
\par \cf2\f1\fs18\{bmc AttributeEditorDefinitionExternal.gif\}\cf1 
\par \f2\fs10 
\par \f1\fs18 The\lang1033\f0  \lang1040\b\f1 Storage Kind\b0\i  \i0 combo-box \lang1033\f0 allows a selection\lang1040\f1  between \i Embedded \i0 (the classic model) and \i External \i0 (the new model). For Part attributes that (in addition to defining the target class \i stored \i0 and not \i embedded\i0 ) is enough. For Parts and References you also need \lang1033\f0 an\lang1040\f1  \b External Storage Name\lang1033\b0\f0  entry\lang1040\f1 , that is the name the \lang1033\f0 linking\lang1040\f1  table will have. \lang1033\f0 The default naming \lang1040\f1 convention\lang1033\f0  is to \lang1040\f1 use "<\lang1033\f0 class\lang1040\f1  \lang1033\f0 storage\lang1040\f1  name>_<attribute name>"\lang1033\f0  for new attributes\lang1040\f1 , but\lang1033\f0  any other suitable\lang1040\f1  naming scheme\lang1033\f0  may be used\lang1040\f1 .\lang1033\f0  If \b Auto\b0  is checked the default naming scheme will be used to update the \lang1040\b\f1 External Storage Name\lang1033\b0\f0  entry to the current values of its component names (ie \lang1040\f1 <\lang1033\f0 class\lang1040\f1  \lang1033\f0 storage\lang1040\f1  name>\lang1033\f0  and \lang1040\f1 <attribute name>\lang1033\f0 ).\lang1040\f1 
\par \pard\f2\fs10 
\par \pard\sb25\sa25\lang1033\f0\fs18 IMPORTANT\lang1040\f1  NOTE\lang1033\f0 S:
\par \pard{\pntext\f3\'B7\tab}{\*\pn\pnlvlblt\pnf3\pnindent0{\pntxtb\'B7}}\fi-200\li200\sb25\sa25\tx200\b\i External Storage and Class Persistence\b0\i0  - A cl\lang1040\f1 ass must \lang1033\f0 have its Persistence property set to\lang1040\f1  \i stored\i0  \lang1033\f0 if it is referred\lang1040\f1  \lang1033\f0 to by attributes that use\lang1040\f1  \i external \i0 storage. \lang1033\f0 A c\lang1040\f1 lass\lang1033\f0  defined with\lang1040\f1  \i embedded\i0  \lang1033\f0 Persistence may only be used by\lang1040\f1  \i embedded\i0  (the \lang1033\f0 InstantObjects \lang1040\f1 historical arrangement) part and parts attributes\lang1033\f0 ;\lang1040\f1 
\par \lang1033\b\i\f0{\pntext\f3\'B7\tab}Performance Hint\b0\i0  - Currently (V2.0), the InstantObjects (IO) library does not create parent class indices for the linking tables in the database. A significant improvement in performance, therefore, can often be achieved by manually adding these indices (fields 'ParentClass' and 'ParentId', non-unique) to the linking tables for your external \cf3\strike container attributes\cf2\strike0\{linkTarget=RelationalAttributeTypes>main\}\cf1 . Remember that these manually added indices will be removed by each IO database build and probably after an evolve, so they will need to monitored and re-added as necessary. The need to add these indices manually will be eliminated after the introduction of Referential Integrity (RI) options to IO. This is expected to happen in the near future and will enhance both the integrity of data in and performance of the database for brokers that can implement RI. \lang1040\f1 
\par \pard\sb25\sa25\f2\fs10 
\par \pard\cf0\fs20 
\par }
230
Scribble230
Example Class Definition with External Attributes




Modified



FALSE
22
{\rtf1\ansi\ansicpg1252\deff0\deflang3081{\fonttbl{\f0\fnil Arial;}{\f1\fnil\fcharset0 Arial;}{\f2\fswiss Arial;}{\f3\fswiss\fcharset0 Arial;}{\f4\fnil\fcharset0 Courier New;}}
{\colortbl ;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\b\f0\fs24 Example\lang1033\f1  -\lang3081\f0  Class Definition with External Attributes\cf1\{keepn\}\cf2\b0\fs18 
\par \pard\sb25\sa25\lang1040\f2 Here is a snippet from an example class definition with external attributes\lang1033\f3 . The storage name for the class TTest is assumed to be Test\lang1040\f2 :
\par \f0\fs10 
\par \f4\fs18   TTest2 = class(TInstantObject)
\par   \{IOMETADATA stored;
\par     TestPart: Part(TTest) external;
\par     TestParts: Parts(TTest) external 'Test2_TestParts';
\par     TestRef: Reference(TTest);
\par     TestRefs: References(TTest) external 'Test2_TestRefs'; \}
\par \fs10 
\par \f2\fs18 The TestPart attribute is an external part attribute, which \lang1033\f3 results in\lang1040\f2  a mapping very similar to TestRef\lang1033\f3 .\lang1040\f2  \lang1033\f3 I\lang1040\f2 nstead of having a blob field called TestPart, the Test2 table has \lang1033\f3 two reference fields,\lang1040\f2  TestPartId\lang1033\f3  and \lang1040\f2 TestPartClass, and the \lang1033\f3 referenced\lang1040\f2  \lang1033\f3 T\lang1040\f2 Test\lang1033\f3  \lang1040\f2 object\lang1033\f3 's attributes\lang1040\f2  \lang1033\f3 are\lang1040\f2  stored\lang1033\f3  as a record\lang1040\f2  in \lang1033\f3 the \lang1040\f2 Test\lang1033\f3  table \lang1040\f2 .
\par \f0\fs10 
\par \f2\fs18 The TestParts\lang1033\f3  external\lang1040\f2  attribute \lang1033\f3 results in\lang1040\f2  the creation of a \lang1033\f3 linking\lang1040\f2  table \lang1033\f3 named\lang1040\f2  Test2_TestParts which has relationships with both Test2 and Test:
\par Test2 --1:N--> Test2_TestParts --1:1--> Test.
\par \lang1033\f3 The\lang1040\f2  Test2_TestParts \lang1033\f3 linking\lang1040\f2  table has fields to hold information about \lang1033\f3 both \lang1040\f2 the relationships and the sequence of elements in the TestParts\lang1033\f3  \lang1040\f2 attribute.
\par \f0\fs10 
\par \f2\fs18 The mapping of the TestRefs attribute is very similar\lang1033\f3  to that for the \lang1040\f2 TestParts\lang1033\f3  \lang1040\f2 attribute\lang1033\f3 .\lang1040\f2  \lang1033\f3 T\lang1040\f2 he only difference is that the TestParts\lang1033\f3  \lang1040\f2 attribute's\lang1033\f3  Test\lang1040\f2  \lang1033\f3 table records\lang1040\f2  \lang1033\f3 are\lang1040\f2  deleted when the\lang1033\f3  related\lang1040\f2  Test2 \lang1033\f3 table record\lang1040\f2  is deleted\lang1033\f3 .\lang1040\f2  \lang1033\f3 T\lang1040\f2 his\lang1033\f3  cascade delete process,\lang1040\f2  of course\lang1033\f3 ,\lang1040\f2  does not happen with \lang1033\f3 the \lang1040\f2 TestRefs attribute\lang1033\f3 's Test\lang1040\f2  \lang1033\f3 table records\lang1040\f2 .
\par \pard\lang3081\f0 
\par 
\par }
240
Scribble240
Adding Business Rules
Adding Business Rules;Business Rules,Adding;


creatingbusinessmodel:000130
Done



FALSE
64
{\rtf1\ansi\ansicpg1252\deff0{\fonttbl{\f0\fswiss Arial;}{\f1\fnil\fcharset0 Arial;}{\f2\fnil Arial;}{\f3\fnil\fcharset0 Courier New;}}
{\colortbl ;\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;}
\viewkind4\uc1\pard\lang1040\b\f0\fs24 Adding Business Rules \cf1\b0\fs16 
\par \cf2\{keepn\}\cf1  
\par \pard\sb25\sa25\fs18 Validation rules and other business related behavior is added to your business classes by adding the required code plus additional methods and properties to the class. Validation rules and side effects of changing the value of an attribute are often added to the setter method of the corresponding property. Complete validation of business objects before they are stored to the database can be added by overriding the BeforeStore method of the class. New objects can be initialized by overriding the Initialize method. For a complete list of the virtual methods of \cf0\b TInstantObject\cf1\b0 , please refer to the InstantObjects Reference Guide.
\par 
\par \pard\cf0\lang3081\f1 Note: BeforeStore is currently NOT called for \i embedded\i0  objects. Validation of \i embedded \i0 objects, however, can be done by using its owner's BeforeStore event.
\par 
\par \lang1040\b\f2\fs24 Example 1\b0\fs20 
\par \f1 In InstantObjects you usually apply single-attribute business rules in the attribute's property setter method:
\par 
\par \b\f3 procedure\b0  TAddress.SetPostalCode(\b const \b0 Value: \b string\b0 );
\par \b begin\b0 
\par   \i // Validation: raise exceptions to prevent value assignment.
\par \i0   \b if \b0 (Length(Value) <> 6) \b and \b0 (Value <> '') \b then\b0 
\par     \b raise \b0 Exception.Create(\i 'Postal Code must be filled with 6 chars'\i0 );
\par   \i // Assignment: this code will usually be written by InstantObjects automatically.  
\par \i0   _PostalCode.Value := Value;
\par \i   // Post-assignment: do things as a consequence of an attribute's value change
\par   // (see also the virtual method AttributeChanged).
\par \i0   \b if \b0 (Value <> '') \b and \b0 (City = '') \b then\b0 
\par     GetDefaultCityByPostalCode(Value);
\par \b end\b0 ;
\par \f2 
\par \f1 If you need to apply business rules that involve more than one attribute, instead, you often use the BeforeStore virtual method (see Example 2).
\par 
\par When you code side effects, be aware that the property setters might be called more often than expected, for example when reading an object from an XML file or when you use the data-aware presentation layer. Here is an example:
\par 
\par \b\f3 procedure \b0 TShipment.SetMinShipDate(\b const \b0 Value: TDateTime);
\par \b begin\b0 
\par   \i // Assignment: this code will usually be written by InstantObjects automatically.  
\par \i0   _MinShipDate.Value := Value;
\par   MaxShipDate := 0;    
\par \b end\b0 ;
\par \f1 
\par The intent of this code is to reset MaxShipDate whenever MinShipDate changes, so that a user, in a hypothetical data-entry scenario, will have to re-enter a value for MaxShipDate. But things might not work always as expected. For example, SetMinShipDate might be called after SetMaxShipDate when streaming in an object from a XML file. The lesson here is: use the property setters and the BeforeStore method only to apply real business rules (like "MaxShipDate must be equal to or greater than MinShipDate"), and code data-entry rules (like "whenever a value for MinShipDate is entered, MaxShipDate should be reset") at the data-entry level (that is, not in the model classes).
\par \cf2\f0\fs16 
\par \cf0\b\f2\fs24 Example 2\b0\fs20 
\par \f1 Business rules that involve more than one attribute are usually applied in BeforeStore, which gets called whenever the Store method is called to write an object (back) to the storage. Example:
\par 
\par \b\f3 procedure \b0 TShipment.BeforeStore;
\par \b begin\b0 
\par   \b if \b0 MinShipDate > MaxShipDate \b then\b0 
\par     \b raise \b0 Exception.Create(\i 'Date range error'\i0 );
\par   \b inherited\b0 ;
\par \b end\b0 ;
\par \f2 
\par \f1 You initialize an object by overriding Initialize (if you need to apply initialization code both when a new object is created and when it is retrieved from the storage), or AfterCreate (if you need to apply initialization code for newly created objects only). There is also an AfterRetrieve method that you can override to apply initialization code only when an existing object is read from the storage and materialized in memory. Here is an example of AfterCreate:
\par \f2 
\par \b\f3 procedure \b0 TContact.AfterCreate;
\par \b var
\par \b0   vCategory: TCategory;
\par \b begin\b0 
\par   \b inherited\b0 ;
\par   Id := InstantGenerateId;
\par   vCategory := TCategory.Retrieve(\i 'CAT000'\i0 );
\par   \b try\b0 
\par     Category := vCategory;
\par   \b finally\b0 
\par     vCategory.Free;
\par   \b end\b0 ;
\par \b end\b0 ;
\par \cf3\strike\f0\fs16 
\par }
270
Scribble270
Building/Evolving the Business Model
Building/Evolving the Business Model;Business Model,Building,Evolving;


creatingbusinessmodel:000140
Modified



FALSE
31
{\rtf1\ansi\ansicpg1252\deff0{\fonttbl{\f0\fswiss Arial;}{\f1\fswiss\fcharset0 Arial;}{\f2\fnil Arial;}{\f3\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;}
\viewkind4\uc1\pard\cf1\lang1040\b\f0\fs24 Building/Evolving the Business Model \b0\fs16 
\par \cf2\{keepn\}\cf1  
\par \pard\sb25\sa25\fs18 The final step in the modeling phase is to make the model physical in terms of storage for the objects. The required tables and indices have to be created from the business model. InstantObjects allows \lang1033\f1 the\lang1040\f0  use\lang1033\f1  of\lang1040\f0  various types of databases as storage for \lang1033\f1 the business class \lang1040\f0 objects\lang1033\f1  by\lang1040  using different brokers.
\par \f2\fs10 
\par \f0\fs18 The Connection Manager \lang1033\f1 manages the\lang1040\f0  \f1 definition, \f0 creat\lang1033\f1 ion\lang1040\f0  \f1 and evolution of \lang1033 the\lang1040\f0  database. \lang1033\f1 It\lang1040\f0  \lang1033\f1 can be launched using the \cf2\{bmc ModelExplorerBuildDatabaseButton.gif\} \cf1 Build Database speed button\lang1040\f0  on the toolbar of the\lang1033\f1  \cf3\strike InstantObjects\lang1040\f0  Model Explorer\cf2\strike0\{linkID=90>main\}\f1 
\par \cf1\f2\fs10 
\par \cf2\f1\fs18\{bmc ConnectionManager.gif\}
\par \cf1\f2\fs10 
\par \b\f3\fs18 Defining a Broker Connection\b0\f0 
\par The first step in preparing the database is to \f1 define\f0  a connect\f1 or\f0  \f1 using a specific broker to establish a connection \f0 to the database.\f1 
\par Click the right mouse button over the \f0 Connection Manager\lang1033\f1  dialog and s\lang1040 elect New in the pop-up context menu. Choose the preferred Broker and its ConnectionDef Editor appears. The contents of the ConnectionDef Editor varies depending on the requirements of its related broker. The example image below is for the Interbase/Firebird IBX broker.
\par \f2\fs10 
\par \cf2\f0\fs18\{bmc ConnectionDefEditor.gif\}
\par \cf1\f2\fs10 
\par \f1\fs18 Define the connection properties and save the connection definition. Repeat this procedure for any other broker connections required.
\par \f2\fs10 
\par \b\f3\fs18 Building the Database for a Model\b0\f0 
\par \f1 WARNING: \i Any existing data in the database will be destroyed during the build process!
\par \i0 Select the desired Broker Connection in the \f0 Connection Manager\lang1033\f1  then\lang1040  to Build its database click the Build button. The Database Builder dialog appears. Click on the Show Build Sequence button and the Evolution Sequence pane will display the sequence of commands that will be executed during the actual build process. The individual steps can be disabled, enabled and re-ordered as desired using the check boxes on the left of each step in the sequence and the buttons on the right of the dialog. When the desired sequence is ready, click the Build Database button to execute it. The build progress and results are reported in the Evolution Log pane. 
\par \f2\fs10 
\par \cf2\f1\fs18\{bmc DatabaseBuilder.gif\}
\par 
\par \cf1\b\f3 Evolving an Existing Database for a Modified Model\b0\f0 
\par \f1 If the database for a model has been built and populated with data, subsequent changes to the model requiring a database restructure can be accommodated within the InstantObjects framework by Evolving the database. Unlike the Build procedure, which will destroy and recreate all database tables, the evolution process will try to save the existing data. The effectiveness of the evolution process depends on the features of the particular kind of database you are using.
\par WARNING: \i While the InstantObjects evolution process will attempt to preserve existing data, success is NOT guaranteed! It is strongly recommended that a verified back up of the database is available before this evolution process is applied.\i0 
\par To evolve an existing database, select the desired Broker Connection and click the Evolve button. The Database Evolver dialog similar to the Database Builder dialog will appear. Click on the Show Evolution Sequence button and the Evolution Sequence pane will display the sequence of commands that will be executed during the actual evolve process. The individual steps can be disabled, enabled and re-ordered as desired using the check boxes on the left of each step in the sequence and the buttons on the right of the dialog. When the desired sequence is ready, click the Evolve Database button to execute it. The evolve progress and results are reported in the Evolution Log pane.\b 
\par 
\par \b0\f0 
\par }
280
Scribble280
Creating the User Interface
Creating the User Interface;User Interface,Creating;


creatinguserinterface:000010
Done



FALSE
12
{\rtf1\ansi\ansicpg1252\deff0{\fonttbl{\f0\fswiss Arial;}{\f1\fnil\fcharset0 Arial;}{\f2\fswiss\fcharset0 Arial;}{\f3\fnil\fcharset2 Symbol;}}
{\colortbl ;\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;}
\viewkind4\uc1\pard\lang1040\b\f0\fs24 Creating the User Interface\cf1\b0\fs16 
\par \pard\sb25\tx1435\cf2\{keepn\}\cf1  
\par \pard\sb25\sa25\tx1435\fs18 The next phase in the development process is to create a user interface through which the business objects may interact with the user. This section explains the components that are involved in the creation of the user interface.
\par 
\par \pard{\pntext\f3\'B7\tab}{\*\pn\pnlvlblt\pnf3\pnindent0{\pntxtb\'B7}}\fi-200\li200\sb25\sa25\tx200\cf3\strike Persistence by RAD\cf2\strike0\{linkID=300>main\}\cf1 
\par \cf3\strike{\pntext\f3\'B7\tab}The Connector\cf2\strike0\{linkID=\f1 310\f0 >main\}\cf1 
\par \cf3\strike{\pntext\f3\'B7\tab}The Exposer\cf2\strike0\{linkID=\f1 320\f0 >main\}\cf1 
\par \cf3\strike{\pntext\f3\'B7\tab}The Selector\cf2\strike0\{linkID=330>main\}\cf1 
\par \pard\sb25\sa25\tx200\f2 
\par }
300
Scribble300
Persistence by RAD
Persistence by RAD;RAD, Persistence;


creatinguserinterface:000020
Done



FALSE
9
{\rtf1\ansi\ansicpg1252\deff0{\fonttbl{\f0\fswiss Arial;}{\f1\fswiss\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\lang1040\b\f0\fs24 Persistence by RAD \cf1\b0\fs16 
\par \cf2\{keepn\}\cf1  
\par \pard\sb25\sa25\fs18 When InstantObjects has been installed in your Delphi environment, a set of components is available on the InstantObjects tab of the Delphi component palette.
\par \cf2\{bmc InstantObjectsPalette.gif\}\cf1 
\par Applications can be built with InstantObjects using the same RAD approach used when building traditional database applications in Delphi. InstantObjects allows you to use standard VCL data-aware controls with your persistent business objects. The InstantObjects components are used to connect to the database and to make business objects available in the user interface of the application\f1 .\f0 
\par 
\par }
310
Scribble310
The Connector
Connector;


creatinguserinterface:000030
Modified



FALSE
22
{\rtf1\ansi\ansicpg1252\deff0{\fonttbl{\f0\fswiss Arial;}{\f1\fswiss\fcharset0 Arial;}{\f2\fnil Arial;}}
{\colortbl ;\red0\green0\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\lang1040\b\f0\fs24 The Connector \cf1\b0\fs16 
\par \cf2\{keepn\}\cf1  
\par \pard\sb25\sa25\fs18 In order for your application to use the database \lang1033\f1 for\lang1040\f0  object storage, you must use a connector component. The connector acts as a gateway between your application and the database\f1  (by means of an internal component called the broker) and \f0 manag\lang1033\f1 es\lang1040\f0  all the objects that are stored to and retrieved from the database. A connector component for each type of data access layer that \lang1033\f1 has been installed from\lang1040\f0  InstantObjects is available on the component palette. A connector is attached to a database by assigning a connection component to its Connection \f1 (or equivalent) \f0 property. Each connector component supports its own connection type\f1 . For example:\f0 
\par \f2\fs10 
\par \pard\sb25\sa25\tx1980\tx4800\b\f0\fs18 Access type\tab Connector type\tab\f1 Type of \f0 Connection\b0 
\par ADO\tab\cf0 TInstantADOConnector\cf1\tab TADOConnection
\par BDE\tab\cf0 TInstantBDEConnector\cf1\tab TDatabase
\par IBX\tab\cf0 TInstantIBXConnector\cf1\tab TIBDatabase
\par \f1 D\f0 BX\tab\cf0 TInstant\f1 D\f0 BXConnector\cf1\tab TSQLConnection
\par \f1 XML\tab\cf0\f0 TInstantX\f1 ML\f0 Connector\cf1\tab TXMLFilesAccessor
\par \f1 FireDAC\tab\cf0\f0 TInstant\f1 FireDAC\f0 Connector\cf1\tab TFDConnection
\par 
\par \pard\f2\fs10 
\par \pard\sb25\sa25\f1\fs18 InstantObjects supports a variety of databases and data-access technologies by means of packages called Brokers. You can build and install the broker(s) you need from the Brokers sub-folder of InstantObjects's Source folder.
\par \pard\f2\fs10 
\par \f0\fs18 Drop an appropriate connector component and a matching connection component on a form or a data module in your project. Configure the connection component to access the database and assign it to the Connection \f1 (or equivalent) \f0 property of the connector component. The connector will use the connection to gain access to the database. By setting the IsDefault property to True, the connector will be used as the default connector in the application.
\par \f2\fs10 
\par \f1\fs18 You can also use a TInstantConnectionManager component that stores connection data in external files and can create connectors and connections automatically based on this data. This is particularly useful when you don't want to hard code a particular broker or set of brokers into your program. See the Primer Demo application for an example of this technique.
\par \f0 
\par }
320
Scribble320
The Exposer
Exposer;


creatinguserinterface:000040
Modified



FALSE
13
{\rtf1\ansi\ansicpg1252\deff0{\fonttbl{\f0\fswiss Arial;}{\f1\fswiss\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\lang1040\b\f0\fs24 The Exposer \b0\fs16 
\par \cf2\{keepn\}\cf1  
\par \pard\sb25\sa25\cf0\b\fs18 TInstantExposer\cf2\b0\f1  \cf1\f0 is a dataset component that maps objects to the user interface of your application.
\par Attributes defined in the business model are accessed through properties. Properties that are published can be accessed by data-aware controls via this component. In addition, the content of container attributes can be accessed too. The exposer component maps the published properties of objects to fields in a dataset. The objects being exposed are represented as rows in the dataset.
\par \cf0 To expose an object, it must be assigned to the Subject\f1  \f0 property of a TInstantExposer. To expose multiple objects contained within another object, assign the main object to the Subject property and enter content mode by changing the Mode property from amObject to amContent. If the exposed class has no default container, specify the desired container in the property ContainerName. Specify the class of the exposed\lang1033\f1 , contained\lang1040\f0  object(s) in the property ObjectClassName.
\par By default, an exposer will make all simple properties of each exposed object as well as any related object available through fields in the dataset. Every field will have a \f1 F\f0 ield\f1 N\f0 ame matching the property it represents. For related objects, the fieldname will be the complete path to the property using regular dot notation. The property FieldOptions and the event OnIncludeField allow you to limit or extend the number fields to include.
\par Container attributes of exposed objects are automatically recognized by the exposer and \lang1033\f1 are \lang1040\f0 represented as nested datasets within the exposer.
\par Exposers can be linked together in master/detail relation\f1 ships\f0 . To link one exposer to another, assign the master exposer to the DataSet property of a TDataSource and assign the TDataSource to the MasterSource property of the detail exposer. The Subject of the detail exposer will be set to the current object of the master exposer\lang1033\f1  and updated\lang1040\f0  whenever this changes. If you want the detail exposer to expose an object that is related to the current object of the master exposer instead, simply specify the desired property path in MasterProperty.
\par TInstantExposer is not limited to exposing TInstantObject descendants. Any object with published propertied can be exposed. The content of standard VCL containers like TList, TObjectList and TCollection can be exposed in content mode.
\par 
\par }
330
Scribble330
The Selector
Selector;


creatinguserinterface:000050
Done



FALSE
40
{\rtf1\ansi\ansicpg1252\deff0{\fonttbl{\f0\fswiss Arial;}{\f1\fswiss\fcharset0 Arial;}{\f2\fmodern Courier New;}}
{\colortbl ;\red0\green0\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\lang1040\b\f0\fs24 The Selector \cf1\b0\fs16 
\par \cf2\{keepn\}\cf1  
\par \pard\sb25\sa25\fs18 The \cf0\b TInstantSelector\cf1  \b0 is a dataset component \lang1033\f1 that \lang1040\f0 allows you to select objects from the database and optionally expose them in the user interface.
\par To select objects from the database, a command must be specified \lang1033\f1 in\lang1040\f0  the Command property. The syntax of this command\f1  (called an IQL command, from Instant Query Language) \f0  is somewhat similar to an SQL SELECT-statement\f1 , \f0 but instead of tables and columns, you specify classes and attributes:
\par \pard\keep\f2 
\par SELECT [DISTINCT] *|<Attribute>
\par FROM [ANY] <Class>
\par [WHERE <Expression>]
\par [ORDER BY <Attribute list>]
\par \pard\sb25\sa25\f0 
\par The simplest command that can be specified looks like this:
\par \pard\keep\f2 
\par SELECT * FROM TContact
\par \pard\sb25\sa25\f0 
\par This command select\f1 s\f0  all concrete instances of the class TContact. To select instances of TContact and any descendant class, add the ANY keyword:
\par \pard\keep\f2 
\par SELECT * FROM ANY Tcontact
\par \pard\sb25\sa25\f0 
\par If you want to select objects that are related to instances of a class, specify the relation attribute instead of the \f1 *\f0 , like this:
\par \pard\keep\f2 
\par SELECT Address FROM ANY Tcontact
\par \pard\sb25\sa25\f0 
\par If the same object is related to from several objects via the specified attribute and you do not want it to appear more than once in the result, use the DISTINCT keyword with the attribute:
\par \pard\keep\f2 
\par SELECT DISTINCT Address FROM ANY Tcontact
\par \pard\sb25\sa25\f0 
\par To select from objects that meet a certain criteria, you must add a WHERE clause to the command. This clause must contain an expression that evaluates to True or False. The expression can use attributes, constants and functions in combination with the most common operators. The following example will select all customers with a negative balance.
\par \pard\keep\f2 
\par SELECT * FROM TCustomer WHERE Balance < 0
\par \pard\sb25\sa25\f0 
\par To order the selected objects by one or more attributes, specify the attributes with ORDER BY:
\par \pard\keep\f2 
\par SELECT * FROM TCustomer ORDER BY Balance, Name
\par \pard\sb25\sa25\f0 
\par When the selector is opened, it performs a query against the database. The resulting objects are available via the Objects property. The number of objects selected can be read via the property ObjectCount.
\par If you want to expose the selected objects in the user interface, simply assign the selector to a TDataSource that is attached to data-aware controls.
\par 
\par }
340
Scribble340
Programming with Persistent Objects
Programming with Persistent Objects;Persistent Objects,Programming;


programming:000010
Modified



FALSE
11
{\rtf1\ansi\ansicpg1252\deff0{\fonttbl{\f0\fswiss Arial;}{\f1\fswiss\fcharset0 Arial;}{\f2\fnil\fcharset2 Symbol;}}
{\colortbl ;\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;}
\viewkind4\uc1\pard\lang1040\b\f0\fs24 Programming with Persistent Objects\cf1\b0\fs16 
\par \pard\sb25\tx1435\cf2\{keepn\}\cf1  
\par \pard\sb25\sa25\tx1435\fs18 The driving force in InstantObjects based applications \lang1033\f1 is\lang1040\f0  the persistent classes \lang1033\f1 that\lang1040\f0  contain the business logic. These business classes all descend from the fundamental \cf0\b TInstantObject\cf1  \b0 class. This section covers aspects of working directly with the business classes of \lang1033\f1 an\lang1040\f0  application.
\par 
\par \pard{\pntext\f2\'B7\tab}{\*\pn\pnlvlblt\pnf2\pnindent0{\pntxtb\'B7}}\fi-200\li200\sb25\sa25\tx200\cf3\strike Creating New Objects\cf2\strike0\{linkID=360>main\}\cf1 
\par \cf3\strike{\pntext\f2\'B7\tab}Retrieving Existing Objects\cf2\strike0\{linkID=380>main\}\cf1 
\par \cf3\strike{\pntext\f2\'B7\tab}Associating Objects\cf2\strike0\{linkID=420>main\}\cf1 
\par \cf3\strike\f1{\pntext\f2\'B7\tab}Using t\f0 he \f1 InstantQuery\cf2\strike0\f0\{linkID=440>main\}\cf1 
\par }
360
Scribble360
Creating New Objects
Creating New Objects;New Objects,Creating;


programming:000020
Imported



FALSE
25
{\rtf1\ansi\ansicpg1252\deff0{\fonttbl{\f0\fswiss Arial;}{\f1\fswiss\fcharset0 Arial;}{\f2\fmodern Courier New;}}
{\colortbl ;\red0\green0\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\lang1040\b\f0\fs24 Creating New Objects\b0\fs16 
\par \pard\sb25\tx1435\cf2\{keepn\}\cf1  
\par \pard\sb25\sa25\tx1435\fs18 Business objects are created just like any other object. The Create constructor of the class creates a new instance of the class.
\par The constructor can be called with an optional Connector parameter. This connector specifies the database in which to make the object persistent. If no connector is specified, the default connector will be used.
\par To make an object persistent, call its Store method. Only instances of classes declared \lang1033\f1 with\lang1040\f0  \lang1033\f1 a \lang1040\f0 persisten\lang1033\f1 ce property of \i stored\lang1040\f0  \i0 in the business model can be \lang1033\f1 S\lang1040\f0 tored.
\par If an object does not\lang1033\f1  already\lang1040\f0  have \lang1033\f1 its\lang1040\f0  Id\lang1033\f1  assigned\lang1040\f0 , \lang1033\f1 an\lang1040\f0  id\lang1033\f1  value\lang1040\f0  will be \lang1033\f1 generated\lang1040\f0  \lang1033\f1 and assigned to \lang1040\f0 it\lang1033\f1  prior to storage\lang1040\f0 .
\par 
\par \cf0\b\fs24 Example\cf1\b0\fs18 
\par \pard\keep\b\f2 var\b0 
\par   Person: TPerson
\par \b begin\b0 
\par   Person := TPerson.Create(MyConnector);
\par   \b try\b0 
\par     Person.Id := '12345678'
\par     Person.Name := 'John Doe';
\par     Person.Store;
\par   \b finally\b0 
\par     Person.Free;
\par   \b end\b0 ;
\par \b end\b0 ;
\par 
\par \pard\sb25\sa25\tx1435\f0 
\par }
380
Scribble380
Retrieving Existing Objects
Retrieving Existing Objects;Existing Objects,Retrieving;


programming:000030
Imported



FALSE
61
{\rtf1\ansi\ansicpg1252\deff0{\fonttbl{\f0\fswiss Arial;}{\f1\fswiss\fcharset0 Arial;}{\f2\fnil Arial;}{\f3\fmodern Courier New;}{\f4\fnil\fcharset0 Courier New;}{\f5\fmodern\fcharset0 Courier New;}{\f6\fnil\fcharset2 Symbol;}}
{\colortbl ;\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;}
\viewkind4\uc1\pard\lang1040\b\f0\fs24 Retrieving Existing Objects\cf1\b0\fs16 
\par \pard\sb25\tx1435\tx2875\cf2\{keepn\}\cf1  
\par \pard\sb25\sa25\tx1435\tx2875\fs18 Objects that have been stored to the database can be retrieved into memory \lang1033\f1 by using any of the following techniques:
\par \pard\sb25\sa25 If the Id of the object is known \lang1040\f0 the Retrieve constructor\lang1033\f1  can be used\lang1040\f0 . An optional connector through which the object shall be retrieved can be specified. If no connector is specified, the default connector is used. If the object already exists in memory, the existing object is returned. If the object is not found, the constructor returns nil.\lang1033\f1  See example1.\lang1040\f2\fs10 
\par \pard{\pntext\f6\'B7\tab}{\*\pn\pnlvlblt\pnf6\pnindent0{\pntxtb\'B7}}\fi-200\li200\sb25\sa25\tx200\tx2875\f0\fs18 If the Id of the object is not known or multiple objects meeting a certain criteria \lang1033\f1 need to\lang1040\f0  be retrieved, a \cf3\lang1033\strike\f1 S\lang1040\f0 elector\cf2\strike0\fs16\{linkID=330>main\}\cf1\fs18  \lang1033\f1 or an \cf3\lang1040\strike InstantQuery\cf2\strike0\f0\fs16\{linkID=440>main\}\cf1\lang1033\f1  \lang1040\f0\fs18 can be used\lang1033\f1 . See example 2 and example 3.\lang1040\f0 
\par \pard\sb25\sa25\tx200\tx2875\f2\fs10 
\par \pard\cf0\b\f0\fs24 Example 1 \cf1\b0\fs16 
\par \pard\keep\b\f3\fs18 var\b0 
\par   Person: TPerson;
\par \b begin\b0 
\par   Person := TPerson.Retrieve('12345678', MyConnector);
\par   \b try\b0 
\par     ShowMessage(Person.Name);
\par   \b finally\b0 
\par     Person.Free;
\par   \b end\b0 ;
\par \pard\sb25\sa25\tx200\tx2875\b end\b0 ;
\par 
\par \pard\cf0\b\f0\fs24 Example 2\cf1\b0\f3\fs18 
\par \pard\keep\b var\b0 
\par   I: Integer;
\par   Person: TPerson;
\par \b begin\b0 
\par   \b with \b0 TInstantSelector.Create(nil) \b do\b0 
\par   \b try\b0 
\par     Connector := MyConnector;
\par     Command.Text := 'SELECT * FROM TPerson WHERE Company.Name = "Happy Donuts"';
\par     Open;
\par     \b for \b0 I := 0 \b to \b0 Pred(ObjectCount) \b do\b0 
\par     \b begin\b0 
\par       Person := Objects[I] as TPerson;
\par       Person.Salary := Person.Salary * 1.05;
\par       Person.Store;
\par     \b end\b0 ;
\par   \b finally\b0 
\par     Free;
\par   \b end\b0 ;
\par \pard\sb25\sa25\tx200\tx2875\b end\b0 ;
\par 
\par \pard\cf0\b\f0\fs24 Example \lang1033\f1 3\cf1\lang1040\b0\f3\fs18 
\par \pard\keep\b var\b0 
\par   I: Integer;
\par   Person: TPerson;
\par \pard\b\f4 begin\b0 
\par \pard\keep\f3   \lang1033\b\f5 with \lang1040\b0\f3 MyConnector\f4 .CreateQuery \b do\b0\f3 
\par \lang1033\f5   \lang1040\b\f3 try\b0 
\par     Command := 'SELECT * FROM TPerson WHERE Company.Name = "Happy Donuts"';
\par     Open;
\par     \b for \b0 I := 0 \b to \b0 Pred(ObjectCount) \b do\b0 
\par     \b begin\b0 
\par       Person := Objects[I] as TPerson;
\par       Person.Salary := Person.Salary * 1.05;
\par       Person.Store;
\par     \b end\b0 ;
\par   \b finally\b0 
\par     Free;
\par   \b end\b0 ;
\par \pard\sb25\sa25\tx200\tx2875\b end\b0 ;
\par }
420
Scribble420
Associating Objects
Associating Objects;Objects,Associating;


programming:000040
Imported



FALSE
152
{\rtf1\ansi\ansicpg1252\deff0{\fonttbl{\f0\fswiss Arial;}{\f1\fswiss\fcharset0 Arial;}{\f2\fnil Arial;}{\f3\fmodern Courier New;}{\f4\fmodern\fcharset0 Courier New;}{\f5\fnil\fcharset2 Symbol;}}
{\colortbl ;\red0\green0\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\lang1040\b\f0\fs24 Associating Objects\cf1\b0\fs16 
\par \pard\sb25\tx1435\cf2\{keepn\}\cf1  
\par \pard\sb25\sa25\tx1435\fs18 Persistent object relations \lang1033\f1 are \lang1040\f0 defined by relational attributes in the business model \lang1033\f1 and \lang1040\f0 can be accessed just as easy as any other attribute.
\par Single object relations defined by Part and Reference attributes are accessed through the corresponding object property of the class.
\par Multiple object references defined by Parts and References attributes are accessed through the corresponding array property and the container methods defined for the attribute.
\par 
\par \lang1033\b\f1 Assignment to \lang1040\f0 relational attributes
\par \lang1033\b0\f1 (A close study of the code in the Example below  will help to clarify the points below.)\lang1040\f0 
\par \pard{\pntext\f5\'B7\tab}{\*\pn\pnlvlblt\pnf5\pnindent0{\pntxtb\'B7}}\fi-200\li200\sb25\sa25\tx200\lang1033\f1 O\lang1040\f0 bjects created \lang1033\f1 and\lang1040\f0  then assigned to \i part/parts attributes\i0  \lang1033\f1 SHOULD \lang1040\f0 NOT be freed after assignment. These attributes expect \lang1033\f1 the \lang1040\f0 transfer to them \lang1033\f1 of\lang1040\f0  ownership of the objects that they are assigned.\lang1033\f1  \lang1040\f0 The reference count\lang1033\f1 s\lang1040\f0  of these objects do not change during assignment.\lang1033\f1  \lang1040\f0 This means that\lang1033\f1 ,\lang1040\f0  when an object\lang1033\f1  is\lang1040\f0  create\lang1033\f1 d\lang1040\f0  and assign\lang1033\f1 ed\lang1040\f0  to a part/parts\lang1033\f1  \lang1040\f0 attribute\lang1033\f1 ,\lang1040\f0  the object that \lang1033\f1 was\lang1040\f0  created must \lang1033\f1 NOT be\lang1040\f0  free\lang1033\f1 d\lang1040\f0  unless an exception is raised during the\lang1033\f1  a\lang1040\f0 ssignment.\lang1033\f1  \lang1040\f0 
\par \lang1033\f1{\pntext\f5\'B7\tab}O\lang1040\f0 bjects created \lang1033\f1 and\lang1040\f0  then assigned to \i reference/references\lang1033\f1  \lang1040\f0 attributes\i0  DO need to be freed after assignment. These\lang1033\f1  \lang1040\f0 attributes DO NOT expect \lang1033\f1 the\lang1040\f0  transfer \lang1033\f1 of\lang1040\f0  ownership to them of the\lang1033\f1  \lang1040\f0 objects that they are assigned. The reference count of each of these\lang1033\f1  \lang1040\f0 objects is incremented during assignment. This means that\lang1033\f1 ,\lang1040\f0  when an object \lang1033\f1 is \lang1040\f0 create\lang1033\f1 d\lang1040\f0  and assign\lang1033\f1 ed\lang1040\f0  to a reference/references attribute\lang1033\f1 ,\lang1040\f0  the object that \lang1033\f1 was\lang1040\f0  created must \lang1033\f1 be \lang1040\f0 free\lang1033\f1 d\lang1040\f0  at some point after the\lang1033\f1  \lang1040\f0 assignment\lang1033\f1  to avoid a possible memory leak\lang1040\f0 . 
\par \pard\sb25\sa25\tx200 
\par \pard\cf0\b\fs24 Example\cf1\b0\fs16 
\par \pard\sb25\sa25\tx1435\fs18 The \lang1033\f1 following\lang1040\f0  code defines a class containing the various relation types. The procedure CreateSamplePerson creates a TPerson object with a sample name and address, associates it with a company and adds some phones and colleagues to it. The procedures ShowPhones and ShowFriends shows a message with the caption of each of the corresponding objects associated with the person. The code assumes that a default connector has been created.
\par \pard\sb25\sa25\f2\fs10 
\par \pard\keep\cf0\b\f3\fs18 type
\par   TPerson = class(TInstantObject)
\par   \{\-IOMETADATA stored;
\par     Address: Part(TAddress);
\par     Company: Reference(TCompany);
\par     Friends: References(TPerson);
\par     Name: String(30);
\par     Phones: Parts(TPhone); \}
\par     _Address: TInstantPart;
\par     _Company: TInstantReference;
\par     _Friends: TInstantReferences;
\par     _Name: TInstantString;
\par     _Phones: TInstantParts;
\par   private
\par     function GetAddress: TAddress;
\par     function GetCompany: TCompany;
\par     function GetFriendCount: Integer;
\par     function GetFriends(Index: Integer): TPerson;
\par     function GetName: string;
\par     function GetPhoneCount: Integer;
\par     function GetPhones(Index: Integer): TPhone;
\par     procedure SetAddress(Value: TAddress);
\par     procedure SetCompany(Value: TCompany);
\par     procedure SetFriends(Index: Integer; Value: TPerson);
\par     procedure SetName(const Value: string);
\par     procedure SetPhones(Index: Integer; Value: TPhone);
\par   public
\par     function AddFriend(Friend: TPerson): Integer;
\par     function AddPhone(Phone: TPhone): Integer;
\par     function RemoveFriend(Friend: TPerson): Integer;
\par     function RemovePhone(Phone: TPhone): Integer;
\par     property FriendCount: Integer read GetFriendCount;
\par     property Friends[Index: Integer]: TPerson read GetFriends write SetFriends;
\par     property PhoneCount: Integer read GetPhoneCount;
\par     property Phones[Index: Integer]: TPhone read GetPhones write SetPhones;
\par   published
\par     property Address: TAddress read GetAddress write SetAddress;
\par     property Company: TCompany read GetCompany write SetCompany;
\par     property Name: string read GetName write SetName;
\par   end;
\par 
\par function CreateSamplePerson(Company: TCompany): TPerson;
\par var
\par   I: Integer;
\par   Phone: TPhone;
\par begin
\par   Result := TPerson.Create;
\par   try
\par     Result.Name := 'John Doe';
\par     Result.Address.Street := 'Summer Street 1';
\par     Result.Company := Company;
\par 
\par     \{\f4\- Add phones \}
\par     Phone := TPhone.Create;
\par     try
\par       Phone.Name := 'Home';
\par       Phone.Number := '12345678';
\par       Result.AddPhone(Phone);
\par     except
\par       Phone.Free;
\par       raise;
\par     end;
\par     Phone := TPhone.Create;
\par     try
\par       Phone.Name := 'Office';
\par       Phone.Number := '32187654';
\par       Result.AddPhone(Phone);
\par     except
\par       Phone.Free;
\par       raise;
\par     end;
\par \f3 
\par \f4     \f3\{\- Add \lang1033\f4 new Person as a Friend\lang1040\f3  \}\f4 
\par     Person := TPerson.Create;
\par     try
\par       Person.Name := 'Fred Bloggs';
\par       Result.\f3 AddFriend\f4 (Person);
\par     finally
\par       Person.Free;
\par     end;
\par \f3 
\par     \{\- Add colleagues\lang1033\f4  as Friends\lang1040\f3  \}
\par     with TInstantSelector.Create(nil) do
\par     try
\par       Command.Text := 'SELECT * FROM TPerson WHERE Company = ' + Company.Id;
\par       Open;
\par       for I := 0 to Pred(ObjectCount) do
\par         Result.AddFriend(Objects[I] as TPerson);
\par     finally
\par       Free;
\par     end;
\par 
\par     Result.Store;
\par   except
\par     Result.Free;
\par     raise;
\par   end;
\par end;
\par 
\par procedure ShowPhones(Person: TPerson);
\par var
\par   I: Integer;
\par begin
\par   for I := 0 to Pred(Person.PhoneCount) do
\par     ShowMessage(Person.Phones[I].Caption);
\par end;
\par 
\par procedure ShowFriends(Person: TPerson);
\par var
\par   I: Integer;
\par begin
\par   for I := 0 to Pred(Person.FriendCount) do
\par     ShowMessage(Person.Friends[I].Caption);
\par end;
\par 
\par procedure DemonstrateRelations;
\par var
\par   Company: TCompany;
\par   Person: TPerson;
\par begin
\par   Company := TCompany.Retrieve('Happy Donuts');
\par   try
\par     Person := CreateSamplePerson(Company);
\par     try
\par       ShowPhones(Person);
\par       ShowFriends(Person);
\par     finally
\par       Person.Free;
\par     end;
\par   finally
\par     Company.Free;
\par   end;
\par end;
\par \cf1\b0 
\par \pard\sb25\sa25\tx200\f0 
\par }
440
Scribble440
Using an InstantQuery
Using an InstantQuery;InstantQuery,Using;


programming:000050
Done



FALSE
26
{\rtf1\ansi\ansicpg1252\deff0\deflang1040{\fonttbl{\f0\fswiss Arial;}{\f1\fswiss\fcharset0 Arial;}{\f2\fnil\fcharset0 Courier New;}}
{\colortbl ;\red0\green0\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs24 Using an InstantQuery\b0\fs16 
\par \cf2\{keepn\}\cf1 
\par \cf0\f1\fs18 TInstantQuery is not available in Delphi's Component or Tool Palette, but it is useful to fetch a list of objects from the storage through an IQL statement. TInstantQuery basically represents the core of  the Selector\cf2\f0\{linkID=\f1 330\f0 >main\}\cf1\f1 .
\par 
\par TInstantQuery is an abstract class. What you actually use are concrete descendant classes, which you instantiate through the connector's CreateQuery method. The advantage in using a TInstantQuery descendant lies directly in the lower overhead, as there is no TDataSet buffer management involved. So, if you want to fetch objects and don't have a data-aware presentation layer, TInstantQuery is the preferred way to do it. 
\par 
\par \cf0\b\f0\fs24 Example
\par \cf1\f2\fs18 function \b0 CompanyOfCityCount(\b const\b0  ACityId: \b string\b0 ): Integer;
\par \b begin\b0 
\par   \b with\b0  InstantDefaultConnector.CreateQuery \b do\b0 
\par   \b try\b0 
\par     Command := \i 'SELECT * FROM ANY TCompany WHERE City = :City'\i0 ;
\par \i     // Since this is a parameterized query, fetch the param definitions.\i0 
\par     FetchParams(InstantQuery.Command, InstantQuery.Params);
\par \i     // Set the param values
\par \i0     Params.ParamByName('City').AsString := ACityId;
\par     Open;
\par     Result := ObjectCount;
\par   \b finally\b0 
\par     Free;
\par   \b end\b0 ;
\par \b end\b0 ;
\par 
\par }
460
Scribble460
Learning the Primer Demo
Learning The Primer Demo;Primer Demo,Learning;


primerdemo:000010
Done



FALSE
29
{\rtf1\ansi\ansicpg1252\deff0\deflang1040{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fswiss Arial;}{\f2\fnil\fcharset2 Symbol;}}
{\colortbl ;\red128\green0\blue0;\red0\green0\blue0;\red0\green128\blue0;}
\viewkind4\uc1\pard\b\fs24 Learning the Primer Demo
\par \cf1\b0\f1\fs16\{keepn\}\cf2  \cf0\f0\fs20 
\par 
\par \fs18 InstantObjects Primer is an example application that was built using the InstantObjects Object Persistence Framework. It demonstrates how InstantObjects enables you to build truly object oriented database applications in Delphi using the same RAD approach you use, when building traditional database applications. 
\par You can find the complete demo into \b Demos\\PrimerCross\b0  folder.
\par \cf1\b\{bmc PrimerDemo.gif\}\cf0\b0 
\par \ul\b\fs20 
\par \ulnone Summary\b0 
\par \pard{\pntext\f2\'B7\tab}{\*\pn\pnlvlblt\pnf2\pnindent0{\pntxtb\'B7}}\fi-200\li200\tx200\cf3\strike\fs18 Introduction\cf1\strike0\{linkID=480>main\}\cf0 
\par \cf3\strike{\pntext\f2\'B7\tab}The Business Model\cf1\strike0\{linkID=490>main\}\cf0 
\par \cf3\strike{\pntext\f2\'B7\tab}The User Interface\cf1\strike0\{linkID=500>main\}\cf0 
\par \pard 
\par \b Persistence
\par \pard{\pntext\f2\'B7\tab}{\*\pn\pnlvlblt\pnf2\pnindent0{\pntxtb\'B7}}\fi-200\li200\tx200\cf3\b0\strike Persisting Objects\cf1\strike0\{linkID=510>main\}\cf0 
\par \cf3\strike{\pntext\f2\'B7\tab}Mapping Scheme\cf1\strike0\{linkID=520>main\}\cf0 
\par \cf3\strike{\pntext\f2\'B7\tab}Streaming\cf1\strike0\{linkID=530>main\}\cf0 
\par \pard 
\par \b Presentation\b0 
\par \pard{\pntext\f2\'B7\tab}{\*\pn\pnlvlblt\pnf2\pnindent0{\pntxtb\'B7}}\fi-200\li200\tx200\cf3\strike Exposing Objects\cf1\{linkID=540>main\}\cf0 
\par \cf3{\pntext\f2\'B7\tab}Selecting Objects\cf1\{linkID=550>main\}\cf0 
\par \cf3{\pntext\f2\'B7\tab}Filtering\cf1\{linkID=560>main\}\cf0 
\par \cf3{\pntext\f2\'B7\tab}Sorting\cf1\{linkID=570>main\}\cf0\strike0 
\par \pard 
\par \b Tools \b0 
\par \pard{\pntext\f2\'B7\tab}{\*\pn\pnlvlblt\pnf2\pnindent0{\pntxtb\'B7}}\fi-200\li200\tx200\cf3\ul Object Explorer\cf1\{linkID=580>main\}\cf0\fs20 
\par \cf3\fs18{\pntext\f2\'B7\tab}Connection Manager\cf1\{linkID=590>main\}\cf0\fs20 
\par }
480
Scribble480
Primer Demo: Introduction
Primer Demo,Introduction;


primerdemo:000020
Done



FALSE
15
{\rtf1\ansi\ansicpg1252\deff0\deflang1040{\fonttbl{\f0\fnil Arial;}{\f1\fnil\fcharset0 Arial;}}
{\colortbl ;\red128\green0\blue0;}
\viewkind4\uc1\pard\b\f0\fs24 Primer Demo: Introduction\fs26 
\par \cf1\b0\f1\fs16\{keepn\}\cf0\fs20 
\par \b\fs18 
\par Contact book\b0 
\par InstantObjects Primer features a contact book in which contacts can be registered and maintained. The main screen of the contact book is available via the Contacts icon on the navigation bar to the left. The following topics in this section describe the business model and user interface of the contact book.
\par  
\par \b Query Tester\b0 
\par With the query tester available via the Query icon on the navigation bar, you can try out the query engine of InstantObjects. The query tester allows you to write and execute your own queries or choose from a list of example queries.
\par 
\par \b Performance Tester\b0 
\par InstantObjects can use various types of databases as the storage for your persistent objects. In fact, multiple database connections of different types can be used simultaneously by the same application. With the performance tester available via the Performance icon on the navigation bar, you can compare the performance of the database types of interest.
\par \f0 
\par }
490
Scribble490
Primer Demo: The Business Model
Primer Demo,Business Model;


primerdemo:000030
Done



FALSE
10
{\rtf1\ansi\ansicpg1252\deff0\deflang1040{\fonttbl{\f0\fnil Arial;}{\f1\fnil\fcharset0 Arial;}}
{\colortbl ;\red128\green0\blue0;\red0\green0\blue0;\red0\green128\blue0;}
\viewkind4\uc1\pard\b\f0\fs24 Primer Demo: \f1 The Business Model\f0\fs26 
\par \cf1\b0\f1\fs16\{keepn\}\cf0\b\fs20 
\par \cf2\b0\f0\fs18 The contact book featured in this application is based on a simple business model of contact related information. The following UML class diagram shows how this information is organized.
\par \fs10 
\par \cf1\f1\fs20\{bmc model.gif\}
\par \cf2\f0\fs10 
\par \fs18 The business model was implemented entirely in the Delphi IDE using the integrated \cf3\strike InstantObjects Model Explorer\cf1\strike0\f1\fs20\{linkID=90\}\cf0 . 
\par }
500
Scribble500
Primer Demo: The User Interface
Primer Demo,User Interface;


primerdemo:000040
Done



FALSE
13
{\rtf1\ansi\ansicpg1252\deff0\deflang1040{\fonttbl{\f0\fnil Arial;}{\f1\fnil\fcharset0 Arial;}}
{\colortbl ;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\b\f0\fs24 Primer Demo: The User Interface\fs26 
\par \cf1\b0\f1\fs16\{keepn\}\cf0\fs20 
\par \cf2\f0\fs18 The user interface of the contact book relies on visual form inheritance. The following diagram shows the form class hierarchy. 
\par 
\par TBasicEditForm introduces basic editing capabilities for editing single objects. Descendant forms introduce capabilities required for editing objects of their corresponding business class. 
\par 
\par TBasicBrowseForm is used for browsing and editing multiple objects at once. Descendants hereof are customized for browsing objects of specific classes.
\par \fs10 
\par \cf1\fs20\{bmc ui.gif\}
\par \cf0 
\par }
510
Scribble510
Primer Demo: Persisting Objects
Primer Demo,Persisting Objects;


primerdemo:000050
Done



FALSE
14
{\rtf1\ansi\ansicpg1252\deff0\deflang1040{\fonttbl{\f0\fnil Arial;}{\f1\fnil\fcharset0 Arial;}}
{\colortbl ;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\b\f0\fs24 Primer Demo: Persisting Objects\fs26 
\par \cf1\b0\f1\fs16\{keepn\}\cf0\fs20 
\par \cf2\f0\fs18 One of the main issues when building truly object oriented business applications is the ability to persist business objects. InstantObjects addresses this issue by supporting the most common relational databases as object storage. Relations between objects defined in the business model are handled seamlessly by InstantObjects when storing objects to or retrieving objects from the database.
\par 
\par In most cases, object persistence is handled automatically by the presentation components through which the objects are exposed in the user interface. See the Presentation section for more details about object presentation.
\par 
\par When objects are not accessed via the user interface, persistence is achieved by application code instead. 
\par 
\par Business classes inherit their persistent capabilities from\f1  TInstantObject\f0 . Like other classes in Delphi, instances of classes that descent from TInstantObject are created with the Create constructor of the class. Objects can be stored in the database by invoking the Store method. Existing objects can be retrieved from the database with the Retrieve constructor. When objects are no longer needed, they can be removed from the database by invoking the Dispose method.
\par 
\par Examples of persisting objects by code can be found in the unit DemoData.pas. This unit contains the code for random generation of contacts. Study the code to learn how easy it is to create persistent objects with InstantObjects.
\par }
520
Scribble520
Primer Demo: Mapping Scheme
Primer Demo,Mapping Scheme;


primerdemo:000060
Done



FALSE
10
{\rtf1\ansi\ansicpg1252\deff0\deflang1040{\fonttbl{\f0\fnil Arial;}{\f1\fnil\fcharset0 Arial;}}
{\colortbl ;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\b\f0\fs24 Primer Demo: Mapping Scheme\fs26 
\par \cf1\b0\f1\fs16\{keepn\}\cf0\fs20 
\par \cf2\f0\fs18 One aspect to consider when creating the business model of your application is the  mapping scheme. The mapping scheme defines how the business objects are mapped to tables in the relational database.
\par 
\par By default, InstantObjects defines a mapping scheme that uses one table per class in the business model. This mapping scheme can be changed by changing the storage name of classes and attributes. InstantObjects allows you to combine the storage of several classes into one table by specifying the same storage name for the classes.
\par 
\par Custom mapping schemes can be used to optimize performance of particular classes or to support legacy databases.
\par }
530
Scribble530
Primer Demo: Streaming
Primer Demo,Streaming;


primerdemo:000070
Done



FALSE
8
{\rtf1\ansi\ansicpg1252\deff0\deflang1040{\fonttbl{\f0\fnil Arial;}{\f1\fnil\fcharset0 Arial;}}
{\colortbl ;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\b\f0\fs24 Primer Demo: Streaming\fs26 
\par \cf1\b0\f1\fs16\{keepn\}\cf0\fs20 
\par \cf2\f0\fs18 All business objects created with InstantObjects can be written to and read from any standard VCL stream. The structure of the stream can be either binary or XML.
\par 
\par An example of streaming objects to an XML file is supplied by the Export to XML option on the toolbar of the Contacts view. This option can also be selected by right-clicking on a contact in the grid. When executed, the export function writes the currently selected contact to an XML file and launches your default XML viewer with the file. This would normally be your Internet browser.
\par }
540
Scribble540
Primer Demo: Exposing Objects
Primer Demo,Exposing Objects;


primerdemo:000080
Modified



FALSE
16
{\rtf1\ansi\ansicpg1252\deff0\deflang1040{\fonttbl{\f0\fnil Arial;}{\f1\fnil\fcharset0 Arial;}}
{\colortbl ;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\b\f0\fs24 Primer Demo: Exposing Objects\fs26 
\par \cf1\b0\f1\fs16\{keepn\}\cf0\fs20 
\par \cf2\f0\fs18 Making objects available in the user interface of an application through data-aware controls is known as  exposing objects. InstantObjects allows you to expose any object with published properties.
\par 
\par \cf0 TInstantExposer\cf2  is the component to use when an object or part of an object that is already present in memory needs to be exposed. TInstantExposer is a TDataSet descendant, which means it can be attached to data-aware controls via a TDataSource. When an object is assigned to the Subject property of a TInstantExposer component, the object or the objects contained in the object are represented as rows in the dataset. The published properties of the exposed object(s) are represented as columns in the dataset.
\par 
\par The contact book demonstrates how exposers can be used to show and edit business objects in the user interface. TBasicEditForm is the base for all forms used by the application to edit objects. This form contains an exposer which is connected through a TDataSource to data-aware controls introduced in descending forms designed for editing objects of specific classes. When an object needs to be edited, it is passed to the exposer in a suitable edit form.
\par 
\par When exposing business objects, relations to other objects defined in the business model are recognized and handled automatically. One-to-many relations are represented as nested datasets within the exposer. In addition, exposers can be linked in a master-detail fashion allowing any related object to be exposed via the referring object. The exposers PhonesExposer on the form TContactEditForm and EmailsExposer on the form TPersonEditForm are examples of detail exposers that are linked to a master exposer.
\par \fs10 
\par \cf1\fs18\{bmc PersonEditForm.gif\}\cf2 
\par \fs10 
\par \fs18 If you study TBasicEditForm and the various descendants hereof you will notice that they contain no code for handling the editing of the object - this is done automatically by the exposer.
\par }
550
Scribble550
Primer Demo: Selecting Objects
Primer Demo,Selecting Objects;


primerdemo:000090
Modified



FALSE
24
{\rtf1\ansi\ansicpg1252\deff0\deflang1040{\fonttbl{\f0\fnil Arial;}{\f1\fnil\fcharset0 Arial;}}
{\colortbl ;\red128\green0\blue0;\red0\green0\blue0;\red0\green128\blue0;}
\viewkind4\uc1\pard\b\f0\fs24 Primer Demo: Selecting Objects\fs26 
\par \cf1\b0\f1\fs16\{keepn\}\cf0\fs20 
\par \cf2\f0\fs18 In order to work with business objects they have to be retrieved into memory. Specific objects can be retrieved via the  Retrieve constructor of their class if their object Id is known. If this is not the case or if multiple objects need to be accessed at once, either for presentation or manipulation, they can be  selected from the database.
\par 
\par Objects are selected from the database via the \cf3\strike TInstantSelector\cf1\strike0\{linkID=330\}\cf2  component. This component is a special exposer that allows you to specify a command that defines the objects to be retrieved and optionally exposed. The syntax of the command is somewhat similar to SQL, but instead of tables and columns, you specify classes and attributes. When opened, TInstantSelector queries the database for the corresponding objects and exposes those as rows in the dataset. In addition, the selected objects are available via the Objects array property of the selector. See the section Exposing Objects for details about exposing.
\par 
\par In its simplest form, the selector command looks like this: 
\par 
\par SELECT * FROM TPerson
\par 
\par The above command will select all objects of the TPerson class. By adding a WHERE clause and an ORDER BY clause, the selection is limited to objects meeting a certain criteria and sorted as well:
\par \fs10 
\par \cf1\fs18\{bmc PrimerQuery.gif\}\cf2 
\par \fs10 
\par \fs18 Attributes are specified using regular dot-notation. This allows you to specify attributes of the selected object and objects related to this object. Please consult the InstantObjects Reference Guide for an in-depth explanation of the TInstantSelector Command property.
\par 
\par TInstantSelector is being used in the contact book in several areas. First of all, the main contact grid used for browsing contacts is connected via a TDataSource to a selector. The command of this selector is changed when clicking one of the index-tabs above the grid and when performing a search via the Find feature.
\par 
\par In TContactEditForm, The country and category fields of a contact are edited via a standard TDBLookupComboBox in which the drop down list is connected to a selector that is exposing the objects that can be associated with the field.
\par 
\par All forms used for browsing and looking up objects descent from TBasicBrowseForm. These forms all use a selector to select and expose the desired objects.
\par }
560
Scribble560
Primer Demo: Filtering
Primer Demo,Filtering;


primerdemo:000100
Done



FALSE
12
{\rtf1\ansi\ansicpg1252\deff0\deflang1040{\fonttbl{\f0\fnil Arial;}{\f1\fnil\fcharset0 Arial;}}
{\colortbl ;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\b\f0\fs24 Primer Demo: Filtering\fs26 
\par \cf1\b0\f1\fs16\{keepn\}\cf0\fs20 
\par \cf2\f0\fs18 When exposing objects through an exposer or a selector, a filter can be used to hide unwanted objects. With selectors you can select objects from the database by specifying criteria to be met for the persistent attributes of the objects. Filters, on the other hand, allow you to determine which objects to include at business logic level. This means that objects can be shown or hidden depending on business rules defined by their class. Filters come in two f\lang1033\f1 l\lang1040\f0 avors:  dynamic and static.
\par 
\par Dynamic filters are applied to each object when navigating through the dataset. Since only the objects that are touched during navigation are examined by the filter, this is the fastest filtering method if exposing a large number of objects. Dynamic filters are implemented via the FilterRecord event.
\par 
\par Static filters can be used if you want to pre-filter the exposed objects before navigation. When applying a static filter, all objects will be examined by the filter at once. For selectors, this means that all selected objects will be retrieved from the database. Since all objects are pre-filtered, no filtering takes place during navigation. Use static filters when objects are already present in memory, when exposing a limited number of objects or when the number of objects passing the filter needs to be known during navigation. Static filters are implemented via the Limit event.
\par 
\par InstantObjects Primer \lang1033\f1 d\lang1040\f0 emonstrates both static and dynamic filtering via the Filter option available on the toolbar of the Contacts view.
\par }
570
Scribble570
Primer Demo: Sorting
Primer Demo,Sorting;


primerdemo:000110
Done



FALSE
8
{\rtf1\ansi\ansicpg1252\deff0\deflang1040{\fonttbl{\f0\fnil Arial;}{\f1\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\b\f0\fs24 Primer Demo: Sorting\fs26 
\par \cf1\b0\strike\f1\fs16 Learning the Primer Demo\cf2\strike0\{linkID=460>main\}\{keepn\}\cf0\fs20 
\par \cf3\f0\fs18 Objects retrieved via a selector can be sorted by the underlying database by specifying one or more persistent attributes of the object or related objects with the ORDER BY clause. In addition, objects exposed through an exposer or a selector can be sorted in memory by adding a custom sort method to the OnCompare event.
\par 
\par InstantObjects Primer Demonstrates both sort methods via the Sort option available on the toolbar of the Contacts view. A custom sort event is also used by the exposer EmployeeExposer in TCompanyEditForm to order the employees by their name.
\par }
580
Scribble580
Primer Demo: Object Explorer
Primer Demo,Object Explorer;


primerdemo:000120
Modified



FALSE
11
{\rtf1\ansi\ansicpg1252\deff0\deflang1040{\fonttbl{\f0\fnil Arial;}{\f1\fnil\fcharset0 Arial;}}
{\colortbl ;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\b\f0\fs24 Primer Demo: Object Explorer\fs26 
\par \cf1\b0\f1\fs16\{keepn\}\cf0\fs20 
\par \f0\fs18 TInstantExplorer\cf2  is a visual component that display\lang1033 s\lang1040  an object and its related objects in a tree\lang1033  view\lang1040 . In its basic form, the component can be used to inspect and edit any object with published properties. The explorer can be customized for more application specific use by adding various event handlers.
\par \fs10 
\par \cf1\fs18\{bmc PrimerExplorer.gif\}
\par \cf2\fs10 
\par \fs18 InstantObjects Primer uses an instance of TInstantExplorer to explore the details of the currently selected contact in the contact grid. This explorer can be turned on and off via a button in the toolbar.
\par 
\par }
590
Scribble590
Primer Demo: Connection Manager
Primer Demo,Connection Manager;


primerdemo:000130
Done



FALSE
8
{\rtf1\ansi\ansicpg1252\deff0\deflang1040{\fonttbl{\f0\fnil Arial;}{\f1\fnil\fcharset0 Arial;}}
{\colortbl ;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\b\f0\fs24 Primer Demo: Connection Manager\fs26 
\par \cf1\b0\f1\fs16\{keepn\}\cf0\fs20 
\par \cf2\f0\fs18 If you want the end-user of your application to be able to manage connections to databases, the  \cf0 TInstantConnectionManager\cf2  component and the TInstantConnectionManagerForm will provide your application with the necessary tools. The component wraps a dialog in which the user can define and manage connections to various databases.
\par 
\par InstantObjects Primer uses an instance of TInstantConnectionManager to allow you to test the application with the database of your choice. Click the Connection Manager button on the main toolbar or press Shift+Ctrl+C to open the Connection Manager.
\par }
22
*InternetLink
16711680
Courier New
0
10
1
....
0
0
0
0
0
0
*ParagraphTitle
-16777208
Arial
0
11
1
B...
0
0
0
0
0
0
