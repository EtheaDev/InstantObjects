BuildPrimerDatabase (command line utility)
------------------------------------------

This console application demonstrate building and population of an InstantObject Database.
It uses:
- same model unit of Primer demo located in '..\..\PrimerCross\Model\Model.pas'
  If you want to test external Model use '..\..\PrimerCross\ModelExternal\Model.pas'
- same DemoData unit of Primer demo located in '..\..\PrimerCross\DemoData.pas'
- same RandomData unit of Primer demo located in '..\..\PrimerCross\RandomData.pas'

steps:

1) Prepare Connections.xml file:
 - Compile the Demos\PrimerCross\Primer.dpr with those lines:
   // To use XML format for ConnectionManager file:
   ConnectionManager.FileFormat := sfXML;
   ConnectionManager.FileName := ChangeFileExt(Application.ExeName, '.xml');

 - Run Primer.exe and build your custom connection definitions with LoginPrompt set to False!

 - Test Connection, Build and database population into Primer, then exit.

 - Copy Primer.xml file generated into this folder and rename it to Connections.xml.

2) Include brokers:
Change lines of BuildPrimerDatabase to include/exclude your preferred Brokers.

3) Compile and run with those parameters:
   - Connection Name (stored in Connections.xml)
   - Number of random contacts to create
   - /picture if you want to add pictures to database.

Enjoy,
Carlo Barazzetta

Last revision: 3 Feb 2005
