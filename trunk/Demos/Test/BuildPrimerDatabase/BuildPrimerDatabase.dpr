program BuildPrimerDatabase;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  InstantPersistence,
  DemoData in '..\..\PrimerCross\DemoData.pas',
  RandomData in '..\..\PrimerCross\RandomData.pas',
  Model in '..\..\PrimerCross\Model\Model.pas',
  UBuilder in 'UBuilder.pas';

{$R *.mdr} {Model}

var
  ApplicationPath : string;
  Count : integer;
  ConnectionFile : string;

begin
  //Connect to database
  Try
    ApplicationPath := ExtractFilePath(ParamStr(0));
    ConnectionFile := ApplicationPath+'Connections.xml';

    if ParamStr(1) = '' then
      raise Exception.Create('Connection Name Missing');

    WriteLn('Building Database structure and connect... please wait.');
    UBuilder.BuildAndConnect(ConnectionFile, ParamStr(1));

    if ParamStr(2) <> '' then
      Count := StrToInt(ParamStr(2))
    else
      Count := 100;

    if SameText(ParamStr(3),'/pictures') then
    begin
      //Populate Database with Random-Data and Pictures
      WriteLn(Format('Building %d Contacs with Pictures... please wait.',[Count]));
      UBuilder.CreateRandomContacts(Count, True, ApplicationPath+'..\..\PrimerCross\Pictures\');
    end
    else
    begin
      //Populate Database with Random-Data, without Pictures
      WriteLn(Format('Building %d Contacs without Pictures... please wait.',[Count]));
      UBuilder.CreateRandomContacts(Count);
    end;

    //Close connection
    InstantDefaultConnector.Disconnect;
    WriteLn('Done!');
  Except
    on E: Exception do WriteLn(E.Message);
  End;
end.
