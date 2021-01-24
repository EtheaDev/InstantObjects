unit TestDemoData;

interface

{$I '..\..\Source\InstantDefines.inc'}

uses
  Classes, TestModel, InstantPersistence, TestRandomData;

procedure CreateCategories;
procedure CreateCountries;
function CreateRandomCompany: TCompany;
function CreateRandomPerson(Company: TCompany; out Gender : TGender): TPerson;
function CreateRandomSampleClass: TSampleClass;

implementation

uses
  SysUtils,
  Windows,
  InstantUtils;

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

function CreateRandomSampleClass: TSampleClass;
begin
  Result := TSampleClass.Create;
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
