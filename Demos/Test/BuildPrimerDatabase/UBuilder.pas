unit UBuilder;

interface

uses
  InstantPersistence;

function Connect(const ConnectionFileName, ConnectionName : string) : TInstantConnector;
procedure BuildDatabase(Connector : TInstantConnector);
procedure CreateRandomContacts(Count: Integer; LoadPictures : boolean = False;
  const PicturePath : string = '');

implementation

uses
  SysUtils,
{ Note: This demo attempts to include brokers for the data access
  layers supported natively by Delphi. To include additional brokers,
  please add the broker unit(s) to the following list. If you have not
  installed all brokers, please remove the missing broker unit(s) from
  the list. }

//Begin Broker inclusion section
{$IFDEF MSWINDOWS}
  {$IFNDEF VER130}
  InstantDBX,
  {$ENDIF}
  InstantADO, InstantBDE, InstantIBX,
{$ENDIF}
{$IFDEF LINUX}
  InstantDBX,
{$ENDIF}
  InstantXML,
//End Broker inclusion section

{$IFDEF MSWINDOWS}
    Jpeg, Graphics,
{$ENDIF}
{$IFDEF LINUX}
  QGraphics, 
{$ENDIF}
  InstantConnectionManager, InstantClasses, 
  Classes, Contnrs,
  DemoData, RandomData, Model;

var
  ConnectionManager : TInstantConnectionManager;

function Connect(const ConnectionFileName, ConnectionName : string) : TInstantConnector;
var
  ConnectionDef : TInstantConnectionDef;
begin
  Result := nil;
  // To use XML format for ConnectionManager file:
  ConnectionManager.FileFormat := sfXML;
  ConnectionManager.FileName := ConnectionFileName;
  ConnectionManager.LoadConnectionDefs;
  ConnectionDef := ConnectionManager.ConnectionDefs.Find(ConnectionName) as TInstantConnectionDef;
  if Assigned(ConnectionDef) then
  begin
    ConnectionManager.CurrentConnectionDef := ConnectionDef;
    Result := ConnectionDef.CreateConnector(nil);
  end;
end;

procedure BuildDatabase(Connector : TInstantConnector);
begin
  //This line works only on few Brokers (eg. IBX or UIB)
  //For other brokers the database must exists
  if not Connector.DatabaseExists then
    Connector.CreateDatabase;
  //Recreate data structures (and lost data)
  Connector.BuildDatabase(InstantModel);
end;

//procedure similar to then same contained in main.pas of Primer Demo
procedure AssignRandomPicture(Male : boolean; InstantBlob : TInstantBlob;

  const PicturePath : string);

const
{$IFDEF MSWINDOWS}
  ARandomExt : Array[0..2] of string = ('.bmp','.jpg','.emf');
{$ENDIF}
{$IFDEF LINUX}
  ARandomExt : Array[0..3] of string = ('.bmp','.jpg','.emf','.png');
{$ENDIF}
var
  Picture: TPicture;
  PictureName : string;
begin
  PictureName := '0'+IntToStr(Random(5)+1)+ARandomExt[Random(High(ARandomExt)+1)];
  if Male then
    PictureName := 'man'+PictureName
  else
    PictureName := 'woman'+PictureName;
  PictureName := PicturePath+PictureName;
  if FileExists(PictureName) then
  begin
    Picture := TPicture.Create;
    try
      Picture.LoadFromFile(PictureName);
      InstantBlob.AssignPicture(Picture);
    finally
      Picture.Free;
    end;
  end;
end;

//procedure similar to the same contained in main.pas of Primer Demo
procedure CreateRandomContacts(Count: Integer; LoadPictures : boolean = False;
  const PicturePath : string = '');
var
  Companies: TObjectList;
  Gender: TGender;

  function CreateRandomContact: TContact;
  var
    Company: TCompany;
  begin
    if Random(2) = 0 then
    begin
      if (Random(2) = 0) and (Companies.Count > 10) then
        Company := Companies[Random(Companies.Count)] as TCompany
      else
        Company := nil;
      Result := CreateRandomPerson(Company, Gender);
      if LoadPictures then
        AssignRandomPicture(Gender=gnMale, TPerson(Result)._Picture, PicturePath);
    end else
    begin
      Result := CreateRandomCompany;
      if Random(2) = 0 then
      begin
        if Companies.Count > 50 then
          Companies.Delete(0);
        Result.AddRef;
        Companies.Add(Result);
      end;
    end;
  end;

var
  I, CommitCount: Integer;
begin
  CommitCount := 200;
  Randomize;
  Companies := TObjectList.Create;
  try
    InstantDefaultConnector.StartTransaction;
    try
      for I := 0 to Pred(Count) do
      begin
        with CreateRandomContact do
        try
          Store;
        finally
          Free;
        end;
        if (Succ(I) mod CommitCount) = 0 then
          with InstantDefaultConnector do
          begin
            CommitTransaction;
            StartTransaction;
          end;
      end;
      InstantDefaultConnector.CommitTransaction;
    except
      InstantDefaultConnector.RollbackTransaction;
      raise;
    end;
  finally
    Companies.Free;
  end;
end;


initialization
  ConnectionManager := TInstantConnectionManager.Create(nil);
{$IFDEF MSWINDOWS}
  InstantRegisterGraphicClass(gffJpeg, TJPEGImage);
{$ENDIF}

finalization
  ConnectionManager.Free;

end.
