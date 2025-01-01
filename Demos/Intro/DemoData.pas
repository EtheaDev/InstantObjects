unit DemoData;

interface

{$I '..\..\Source\InstantDefines.inc'}

uses
  System.Classes,
  Model,
  InstantPersistence,
  RandomData;

function CreateRandomCompany(ContactPerson: TPerson): TCompany;
function CreateRandomPerson: TPerson;

implementation

uses
  Winapi.Windows,
  System.SysUtils,
  InstantUtils;

function CreateRandomAddress: TAddress;
begin
  Result := TAddress.Create;
  try
    Result.Street := RandomStreet;
    Result.City := RandomCity;
    Result.PostalCode := RandomNumber(6);
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

function CreateRandomCompany(ContactPerson: TPerson): TCompany;
begin
  Result := TCompany.Create;
  try
    Result.Name := RandomCompanyName;
    Result.Address := CreateRandomAddress;
    CreateRandomPhones(Result, ['Business', 'Fax', 'Mobile']);
    Result.ContactPerson := ContactPerson;
  except
    Result.Free;
    raise;
  end;
end;

function CreateRandomPerson: TPerson;
var
  Gender: TGender;
begin
  Result := TPerson.Create;
  try
    Gender := TGender(Random(2));
    Result.Name := RandomFullName(Gender);
    Result.DateOfBirth := Date - (20 * 365 + Random(365 * 50)); // 20 - 70 years old
    Result.Address := CreateRandomAddress;
    CreateRandomPhones(Result, ['Home', 'Mobile']);
  except
    Result.Free;
    raise;
  end;
end;

end.
