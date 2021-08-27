unit TestDemoData;

interface

{$I '..\..\Source\InstantDefines.inc'}

uses
  Classes, TestModel, InstantPersistence, TestRandomData;

procedure CreateCategories(AConnector: TInstantConnector);
procedure CreateCountries(AConnector: TInstantConnector);
function CreateRandomCompany(AConnector: TInstantConnector): TCompany;
function CreateRandomPerson(AConnector: TInstantConnector;
  Company: TCompany; out Gender : TGender): TPerson;
function CreateRandomSampleClass(AConnector: TInstantConnector): TSampleClass;

implementation

uses
  SysUtils,
  Windows,
  InstantUtils;

procedure CreateCategories(AConnector: TInstantConnector);
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
  LCategory: TCategory;
begin
  for I := Low(CategoryNames) to High(CategoryNames) do
  begin
    LCategory := TCategory.Create(AConnector);
    try
      LCategory.Id := Format('CAT%.3d', [I]);
      LCategory.Name := CategoryNames[I];
      LCategory.Store;
    finally
      LCategory.Free;
    end;
  end;
end;

procedure CreateCountries(AConnector: TInstantConnector);
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
    with TCountry.Create(AConnector) do
    try
      S := CountryNames[I];
      Id := Copy(S, 1, 2);
      Name := Copy(S, 4, Length(S));
      Store;
    finally
      Free;
    end;
end;

function CreateRandomAddress(AConnector: TInstantConnector): TAddress;
var
  Country: TCountry;
begin
  Result := TAddress.Create(AConnector);
  try
    Result.Street := RandomStreet;
    Result.City := RandomCity;
    Result.Zip := RandomNumber(6);
    Country := TCountry.Retrieve('US', False, False, AConnector);
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

procedure CreateRandomPhones(AConnector: TInstantConnector;
  Contact: TContact; Names: array of string);
var
  I: Integer;
  Phone: TPhone;
begin
  for I := Low(Names) to High(Names) do
    if Random(3) > 0 then
    begin
      Phone := TPhone.Create(AConnector);
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

procedure CreateRandomEmails(AConnector: TInstantConnector;
  Person: TPerson; Domains: array of string);
var
  I: Integer;
  Email: TEmail;
begin
  for I := Low(Domains) to High(Domains) do
    if Random(3) > 0 then
    begin
      Email := TEmail.Create(AConnector);
      try
        Email.Address := RandomEmail(Person.Name, Domains[I]);
        Person.AddEmail(Email);
      except
        Email.Free;
        raise;
      end;
    end;
end;

function CreateRandomCompany(AConnector: TInstantConnector): TCompany;
begin
  Result := TCompany.Create(AConnector);
  try
    Result.Name := RandomCompanyName;
    Result.Address := CreateRandomAddress(AConnector);
    CreateRandomPhones(AConnector, Result, ['Business', 'Fax', 'Mobile']);
  except
    Result.Free;
    raise;
  end;
end;

function CreateRandomPerson(AConnector: TInstantConnector;
  Company: TCompany; out Gender : TGender): TPerson;
var
  CompanyName: string;
begin
  Result := TPerson.Create(AConnector);
  try
    Gender := TGender(Random(2));
    Result.Name := RandomFullName(Gender);
    Result.BirthDate := Date - (20 * 365 + Random(365 * 50)); // 20 - 70 years old
    Result.BirthTime := Random;
    Result.Address := CreateRandomAddress(AConnector);
//    Result.Salary := 922337203685470;
    Result.Salary := 500 + Random(5000);
    CreateRandomPhones(AConnector, Result, ['Home', 'Mobile']);
    if Assigned(Company) then
    begin
      Result.EmployBy(Company);
      CompanyName := InstantPartStr(Company.Name, 1, ' ');
    end else
      CompanyName := LowerCase(RandomName);
    CreateRandomEmails(AConnector, Result, [CompanyName, 'hotmail']);
  except
    Result.Free;
    raise;
  end;
end;

function CreateRandomSampleClass(AConnector: TInstantConnector): TSampleClass;
begin
  Result := TSampleClass.Create(AConnector);
  Try
    Result.CharacterListAttribute:=   RandomName;
    Result.CharacterFileName:=   RandomName;
    Result.CharacterDirAttribute:=   RandomName;
    Result.TimeAttribute:= now ;
    Result.DateAttribute:= Date - (20 * 365 + Random(365 * 50));
    Result.DateTimeAttribute:= now - (20 * 365 + Random(365 * 50));
    Result.MemoAttribute:=   RandomName;
    Result.ImageLinkAttribute:=   RandomName;
    Result.BLobAttribute:=   RandomName;
    Result.MemoHTMLAttribute:=   RandomName;
    Result.SmallIntegerAttribute:= Random(90);
    Result.IntegerAttribute:= Random(20000000);
    Result.BooleanAttribute:= True;
    Result.FloatingPointAttribute:= Random(20000000)/Random(20000000);
    Result.CurrencyAttribute:= Random(20000000)/Random(20000000);
    Result.ColorAttribute:= Random(20000000);
    Result.MultiReference:=   RandomName;
    Result.MultiLangDesc:=   RandomName;
    Result.MultiLangMemo:=   RandomName;
  except
    Result.Free;
    raise;
  end;
end;

end.
