ObjectFoundry Readme
by Carlo Wolter - 06/09/2004

This file contains compilation instructions for the
Object Foundry integration between IO and ModelMaker(c)

ModelMaker is an UML designer integrated with Delphi.
It can be used also for InstantObject design, provided
you place in the
  $(ProgramFiles)\ModelMakerTools\ModelMaker\6.2\Experts
directory the
  OFExpt.dll
expert.

This DLL can be compiled using the project in this directory.

Please take note that the project needs to know where the
MM Expert files are. This requires to put in
  Project/Directories-Conditionals/SearchPath
the subdir
	$(ProgramFiles)\ModelMakerTools\ModelMaker\x.x\Experts
or the like.
In this directory there is a single file needed:
	MMToolsApi.PAS
This file being protected by copyright of ModelMakerTools
cannot be put on CVS.
Every rightful owner of a MM license, though, should have no
problems to find it.

Currently there are a few glitches, namely some missing glyph.
I thought it would be better to allow starting the test in advance.
Please make us know (email, forum) abouty any finding and problem.

Thanks

Carlo Wolter
