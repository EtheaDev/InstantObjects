ObjectFoundry (for IO V2.x) Readme
by Carlo Wolter/Steven Mitchell - 21 Mar 2005
Revised by Steven Mitchell: 29 Nov 2006

Introduction
------------
This file contains instructions and information for the
Object Foundry (OF) integration between IO version 2 and 
ModelMaker(c)[http://www.modelmakertools.com].

ModelMaker (MM) is an UML designer integrated with Delphi.
It can be used also for InstantObject design, provided
you place the
  OFExpt.dll
expert file in the
  $(ProgramFiles)\ModelMakerTools\ModelMaker\x.x\Experts
directory. MM detects and loads it during startup and 
"ObjectFoundry enabled" is included on the MM startup splash 
screen. It is also listed in the "Plug in expert manager" 
dialog launched from the Tools/Expert Manager menu option
in MM.

Currently MM versions 6 to 9 are supported with OF.

Compiling
---------
This DLL can be compiled using the project in this directory.

Please take note that the project needs to know where the
MM Expert files are. Therefore make sure the subdir
  $(ProgramFiles)\ModelMakerTools\ModelMaker\x.x\Experts
is in the project options search path 
  (ie Project/Options/Directories-Conditionals/SearchPath).
This is required because in the MM experts directory there is
a single file that is needed:
  MMToolsApi.PAS
Also ensure that the appropriate compiler defines are entered
in the project options Conditional defines (see table below).
(ie Project/Options/Directories-Conditionals/Conditional defines)

 MM Version                     Define
 ----------                     ------
   6.x                          [none]
 7.x or 8.x                     MM7+
   9.x                          MM9

Note: The MMToolsApi.PAS file is protected by copyright of 
ModelMakerTools and cannot be put into CVS. Every legitimate 
owner of a MM licence, though, should have no problems in 
finding it.

Notes on Usage
--------------
To operate correctly, this version of OF expects and 
generates the IO Metadata identifier tag in the class 
metadata info as follows:
"{IOMETADATA " (without quotes but including trailing space).

Conversion of IO MM projects that did not have the IO 
Metadata identifier tag:
Make sure that the model is up to date then save and close 
Modelmaker. Backup the MM project file. Backup any previous
'OFExpt.dll' file and copy the new 'OFExpt.dll' file to the
{$Modelmaker}\Experts folder as indicated above. Re-open 
Modelmaker. Re-generating the Delphi code from ModelMaker 
should update the model code units to include the new class 
metadata identifier tag.

Feedback
--------
Please report any problems to the IO news support group at 
"news.instantobjects.org/instantobjects.org.support".
