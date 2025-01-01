unit DemoData;

interface

{$I '..\..\Source\InstantDefines.inc'}

uses
  System.Classes,
  Model,
  InstantPersistence,
  {$IFDEF INSTANTOBJECTS_FMX}
  FMX.Objects,
  {$ELSE}
  Graphics,
  {$ENDIF}
  RandomData;


procedure CreateCategories;
procedure CreateCountries;
function CreateRandomCompany: TCompany;
function CreateRandomPerson(Company: TCompany; out Gender : TGender): TPerson;
procedure CreateRandomContacts(Count: Integer; LoadPictures : boolean);

implementation

uses
  System.SysUtils,
  System.Contnrs,
  InstantUtils;

procedure AssignRandomPicture(Male : boolean; InstantBlob : TInstantBlob);
const
  ARandomExt : Array[0..2] of string = ('.bmp','.jpg','.png');
var
  Picture: TImage;
  PictureName : string;
begin
  PictureName := '0'+IntToStr(Random(5)+1)+ARandomExt[Random(High(ARandomExt)+1)];
  if Male then
    PictureName := 'man'+PictureName
  else
    PictureName := 'woman'+PictureName;
  PictureName := ExtractFilePath(ParamStr(0))+'..\Pictures'+PathDelim+PictureName;
  if FileExists(PictureName) then
  begin
    Picture := TImage.Create(nil);
    try
      Picture.Bitmap.LoadFromFile(PictureName);
      InstantBlob.AssignPicture(Picture);
    finally
      Picture.Free;
    end;
  end;
end;

procedure CreateRandomContacts(Count: Integer; LoadPictures : boolean);
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
        AssignRandomPicture(Gender=gnMale, TPerson(Result)._Picture);
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

procedure CreateCategories;
const
  CategoryNames: array[0..5] of string = (
    'Undefined',
    'Customer',
    'Supplier',
    'Family',
    'Friend',
    'Colleague'
  );
var
  I: Integer; 
begin
  for I := Low(CategoryNames) to High(CategoryNames) do
    with TCategory.Create do
    try
      Id := Format('CAT%.3d', [I]);
      Name := CategoryNames[I];
      Store;
    finally
      Free;
    end;
end;

procedure CreateCountries;
const
  CountryNames: array[0..29] of string = (
    'AR:Argentina',
    'AT;Austria',
    'AU;Australia',
    'BE;Belgium',
    'BR;Brazil',
    'CA;Canada',
    'CH;Switzerland',
    'DE;Germany',
    'DK;Denmark',
    'EE;Estonia',
    'ES;Spain',
    'FR;France',
    'GR;Greece',
    'HK;Hong Kong',
    'IT;Italy',
    'MX;Mexico',
    'MY;Malaysia',
    'NL;Netherlands',
    'NO;Norway',
    'NZ;New Zealand',
    'PL;Poland',
    'PT;Portugal',
    'RU;Russia',
    'SE;Sweden',
    'SG;Singapore',
    'TW;Taiwan',
    'UA;Ukraine',
    'UK;United Kingdom',
    'US;United States',
    'ZA;South Africa'
  );
var
  I: Integer;
  S: string;
begin
  for I := Low(CountryNames) to High(CountryNames) do
    with TCountry.Create do
    try
      S := CountryNames[I];
      Id := Copy(S, 1, 2);
      Name := Copy(S, 4, Length(S));
      Store;
    finally
      Free;
    end;
end;

function CreateRandomAddress: TAddress;
var
  Country: TCountry;
begin
  Result := TAddress.Create;
  try
    Result.Street := RandomStreet;
    Result.City := RandomCity;
    Result.Zip := RandomNumber(6);
    Country := TCountry.Retrieve('US');
    try
      Result.Country := Country;
    finally
      Country.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure CreateRandomPhones(Contact: TContact; Names: array of string);
var
  I: Integer;
  Phone: TPhone;
begin
  for I := Low(Names) to High(Names) do
    if Random(3) > 0 then
    begin
      Phone := TPhone.Create;
      try
        Phone.Name := Names[I];
        Phone.Number := RandomNumber(10);
        Contact.AddPhone(Phone);
      except
        Phone.Free;
        raise;
      end;
    end;
end;

procedure CreateRandomEmails(Person: TPerson; Domains: array of string);
var
  I: Integer;
  Email: TEmail;
begin
  for I := Low(Domains) to High(Domains) do
    if Random(3) > 0 then
    begin
      Email := TEmail.Create;
      try
        Email.Address := RandomEmail(Person.Name, Domains[I]);
        Person.AddEmail(Email);
      except
        Email.Free;
        raise;
      end;
    end;
end;

function CreateRandomCompany: TCompany;
begin
  Result := TCompany.Create;
  try
    Result.Name := RandomCompanyName;
    Result.Address := CreateRandomAddress;
    CreateRandomPhones(Result, ['Business', 'Fax', 'Mobile']);
  except
    Result.Free;
    raise;
  end;
end;

function CreateRandomPerson(Company: TCompany; out Gender : TGender): TPerson;
var
  CompanyName: string;
begin
  Result := TPerson.Create;
  try
    Gender := TGender(Random(2));
    Result.Name := RandomFullName(Gender);
    Result.BirthDate := Date - (20 * 365 + Random(365 * 50)); // 20 - 70 years old
    Result.BirthTime := Random;
    Result.Address := CreateRandomAddress;
//    Result.Salary := 922337203685470;
    Result.Salary := 500 + Random(5000);
    CreateRandomPhones(Result, ['Home', 'Mobile']);
    if Assigned(Company) then
    begin
      Result.EmployBy(Company);
      CompanyName := InstantPartStr(Company.Name, 1, ' ');
    end else
      CompanyName := LowerCase(RandomName);
    CreateRandomEmails(Result, [CompanyName, 'hotmail']);
  except
    Result.Free;
    raise;
  end;
end;

end.
