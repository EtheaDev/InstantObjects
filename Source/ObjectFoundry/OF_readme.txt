ObjectFoundry (for IO V2) Readme
by Carlo Wolter/Steven Mitchell - 18 Feb 2005

This file contains instructions and information for the
Object Foundry (OF) integration between IO version 2 and 
ModelMaker(c) versions 7 and 8.

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

This DLL can be compiled using the project in this directory.

Please take note that the project needs to know where the
MM Expert files are. Therefore make sure the subdir
  $(ProgramFiles)\ModelMakerTools\ModelMaker\x.x\Experts
is in the project options search path 
  (ie Project/Directories-Conditionals/SearchPath).
This is required because in the MM experts directory there is
a single file that is needed:
  MMToolsApi.PAS
Also ensure that 'MM7' is defined in 'Conditional defines'.

Note: The MMToolsApi.PAS file is protected by copyright of 
ModelMakerTools and cannot be put into CVS. Every legitimate 
owner of a MM licence, though, should have no problems in 
finding it.

To operate correctly this version of OF expects and 
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

Please report any problems to the IO news groups at 
"news.instantobjects.org".
