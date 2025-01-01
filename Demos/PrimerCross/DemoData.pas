unit DemoData;

interface

{$I '..\..\Source\InstantDefines.inc'}

uses
  System.Classes,
  Model,
  InstantPersistence,
  RandomData;

const
  ProfileIds: array[0..3] of string = ('READONLY', 'USER', 'ADMIN', 'SYSTEM');
  RoleNames: array[0..3] of string = ('reader', 'standard', 'admin', 'system');
  UserNames: array[0..3] of string = ('ReadOnlyUser', 'StandardUser', 'AdminUser', 'SystemUser');
  UserPasswords: array[0..3] of string = ('ReadOnlyPassword', 'StandardPassword', 'AdminPassword', 'SystemPassword');

  CategoryNames: array[0..5] of string = (
    'Undefined',
    'Customer',
    'Supplier',
    'Family',
    'Friend',
    'Colleague'
  );

procedure CreateProfiles;
procedure CreateUsers;
procedure CreateCategories;
procedure CreateCountries;
function CreateRandomCompany: TCompany;
function CreateRandomPerson(Company: TCompany; out Gender : TGender): TPerson;
function CreateRandomAddress: TAddress;

implementation

uses
  Winapi.Windows,
  System.SysUtils,
  InstantUtils;

procedure CreateProfiles;
var
  I: Integer;
  LProfile: TProfile;
begin
  for I := Low(ProfileIds) to High(ProfileIds) do
  begin
    LProfile := TProfile.Create;
    try
      LProfile.Id := ProfileIds[I];
      LProfile.AccessRoles := RoleNames[I];
      LProfile.Store;
    finally
      LProfile.Free;
    end;
  end;
end;

procedure CreateUsers;
var
  I: Integer;
  LUser: TUser;
begin
  for I := Low(UserNames) to High(UserNames) do
  begin
    LUser := TUser.Create;
    try
      LUser.Id := UserNames[I];
      LUser.Password := UserPasswords[I];
      if Odd(I) then
        LUser.Language := 'it'
      else
        LUser.Language := 'en';
      LUser.Profile := TProfile.Retrieve(ProfileIds[I]);
      LUser.Profile.Free;
      LUser.Store;
    finally
      LUser.Free;
    end;
  end;
end;

procedure CreateCategories;
var
  I: Integer;
  LCategory: TCategory;
begin
  for I := Low(CategoryNames) to High(CategoryNames) do
  begin
    LCategory := TCategory.Create;
    try
      LCategory.Id := Format('CAT%.3d', [I]);
      LCategory.Name := CategoryNames[I];
      LCategory.Store;
    finally
      LCategory.Free;
    end;
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
  LCountry: TCountry;
begin
  for I := Low(CountryNames) to High(CountryNames) do
  begin
    LCountry := TCountry.Create;
    try
      S := CountryNames[I];
      LCountry.Id := Copy(S, 1, 2);
      LCountry.Name := Copy(S, 4, Length(S));
      LCountry.Store;
    finally
      LCountry.Free;
    end;
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

procedure CreateRandomPhones(AContact: TContact; ANames: array of string);
var
  I: Integer;
  LPhone: TPhone;
begin
  for I := Low(ANames) to High(ANames) do
  begin
    LPhone := TPhone.Create;
    try
      LPhone.Name := ANames[I];
      LPhone.Number := RandomNumber(10);
      AContact.AddPhone(LPhone);
    except
      LPhone.Free;
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
  LCategoryId: string;
begin
  Result := TPerson.Create;
  try
    Gender := TGender(Random(2));
    Result.Name := RandomFullName(Gender);
    Result.BirthDate := Date - (20 * 365 + Random(365 * 50)); // 20 - 70 years old
    Result.BirthTime := Random;
    Result.Address := CreateRandomAddress;
    Result.Salary := 500 + Random(5000);

    LCategoryId := Format('CAT%.3d', [Random(5)+1]);
    Result.Category := TCategory.Retrieve(LCategoryId);
    Result.Category.Free; //dereference Retrieve
    CreateRandomPhones(Result, ['Home', 'Mobile']);
    if Assigned(Company) then
    begin
      Result.EmployBy(Company);
      CompanyName := InstantPartStr(Company.Name, 1, ' ');
    end else
      CompanyName := LowerCase(RandomName);
    CreateRandomEmails(Result, [CompanyName, 'gmail']);
  except
    Result.Free;
    raise;
  end;
end;

end.
