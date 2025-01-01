(*
 *   PrimerCross demo:
 *   Model.pas with "internal storage" of Part and Parts
 *
 *)
unit Model;

interface

uses
  InstantPersistence,
  InstantTypes;

type
  TAddress = class;
  TCategory = class;
  TCompany = class;
  TContact = class;
  TContactFilter = class;
  TCountry = class;
  TEmail = class;
  TPerson = class;
  TPhone = class;


  TAddress = class(TInstantObject)
    {IOMETADATA City: String(30) index;
    Country: Reference(TCountry);
    State: String(4);
    Street: Memo;
    Zip: String(10); }
    _City: TInstantString;
    _Country: TInstantReference;
    _State: TInstantString;
    _Street: TInstantMemo;
    _Zip: TInstantString;
  private
    function GetCity: string;
    function GetCountry: TCountry;
    function GetState: string;
    function GetStreet: string;
    function GetZip: string;
    procedure SetCity(const Value: string);
    procedure SetCountry(Value: TCountry);
    procedure SetState(const Value: string);
    procedure SetStreet(const Value: string);
    procedure SetZip(const Value: string);
  published
    property City: string read GetCity write SetCity;
    property Country: TCountry read GetCountry write SetCountry;
    property State: string read GetState write SetState;
    property Street: string read GetStreet write SetStreet;
    property Zip: string read GetZip write SetZip;
  end;

  TCountry = class(TInstantObject)
    {IOMETADATA stored;
    Name: String(30); }
    _Name: TInstantString;
  private
    function GetName: string;
    procedure SetName(const Value: string);
  protected
    procedure BeforeStore; override;
    function GetCaption: string; override;
  published
    property Id;
    property Name: string read GetName write SetName;
  end;

  TPhone = class(TInstantObject)
    {IOMETADATA Name: String(20);
    Number: String(20) mask '(000) 000-0000;0;_'; }
    _Name: TInstantString;
    _Number: TInstantString;
  private
    function GetName: string;
    function GetNumber: string;
    procedure SetName(const Value: string);
    procedure SetNumber(const Value: string);
  published
    property Name: string read GetName write SetName;
    property Number: string read GetNumber write SetNumber;
  end;

  TEmail = class(TInstantObject)
    {IOMETADATA Address: String(100); }
    _Address: TInstantString;
  private
    function GetAddress: string;
    procedure SetAddress(const Value: string);
  published
    property Address: string read GetAddress write SetAddress;
  end;

  TCategory = class(TInstantObject)
    {IOMETADATA stored;
    Name: String(30); }
    _Name: TInstantString;
  private
    function GetName: string;
    procedure SetName(const Value: string);
  protected
    function GetCaption: string; override;
  published
    property Name: string read GetName write SetName;
  end;

  TContact = class(TInstantObject)
    {IOMETADATA stored;
    Address: Part(TAddress);
    Category: Reference(TCategory);
    City: String(30) index;
    Name: String(50) index;
    Phones: Parts(TPhone); }
    _Address: TInstantPart;
    _Category: TInstantReference;
    _City: TInstantString;
    _Name: TInstantString;
    _Phones: TInstantParts;
  private
    function GetAddress: TAddress;
    function GetCategory: TCategory;
    function GetCity: string;
    function GetMainPhoneNumber: string;
    function GetName: string;
    function GetPhoneCount: Integer;
    function GetPhones(Index: Integer): TPhone;
    procedure SetAddress(Value: TAddress);
    procedure SetCategory(Value: TCategory);
    procedure SetCity(const Value: string);
    procedure SetMainPhoneNumber(const Value: string);
    procedure SetName(const Value: string);
    procedure SetPhones(Index: Integer; Value: TPhone);
  protected
    procedure AfterCreate; override;
    procedure BeforeStore; override;
    function GetCaption: string; override;
  public
    function AddPhone(Phone: TPhone): Integer;
    procedure ClearPhones;
    procedure DeletePhone(Index: Integer);
    function IndexOfPhone(Phone: TPhone): Integer;
    procedure InsertPhone(Index: Integer; Phone: TPhone);
    function RemovePhone(Phone: TPhone): Integer;
    property PhoneCount: Integer read GetPhoneCount;
    property Phones[Index: Integer]: TPhone read GetPhones write SetPhones;
  published
    property Address: TAddress read GetAddress write SetAddress;
    property Category: TCategory read GetCategory write SetCategory;
    property City: string read GetCity write SetCity;
    property MainPhoneNumber: string read GetMainPhoneNumber write SetMainPhoneNumber;
    property Name: string read GetName write SetName;
  end;

  TContactFilter = class(TContact)
    {IOMETADATA embedded; }
  private
    FIsDynamic: Boolean;
    function GetIsEmpty: Boolean;
  public
    function Matches(Contact: TContact): Boolean;
  published
    property IsDynamic: Boolean read FIsDynamic write FIsDynamic;
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  TPerson = class(TContact)
    {IOMETADATA stored;
    Emails: Parts(TEmail);
    Employer: Reference(TCompany);
    Picture: Graphic;
    Salary: Currency;
    BirthDate: Date;
    BirthTime: Time; }
    _BirthDate: TInstantDate;
    _BirthTime: TInstantTime;
    _Emails: TInstantParts;
    _Employer: TInstantReference;
    _Picture: TInstantGraphic;
    _Salary: TInstantCurrency;
  private
    function GetBirthDate: TDate;
    function GetBirthTime: TTime;
    function GetEmailCount: Integer;
    function GetEmails(Index: Integer): TEmail;
    function GetEmployer: TCompany;
    function GetMainEmailAddress: string;
    function GetPicture: string;
    function GetSalary: Currency;
    procedure SetBirthDate(Value: TDate);
    procedure SetBirthTime(Value: TTime);
    procedure SetEmails(Index: Integer; Value: TEmail);
    procedure SetMainEmailAddress(const Value: string);
    procedure SetPicture(const Value: string);
    procedure SetSalary(Value: Currency);
  protected
    procedure BeforeDispose; override;
  public
    function AddEmail(Email: TEmail): Integer;
    procedure ClearEmails;
    procedure DeleteEmail(Index: Integer);
    procedure EmployBy(NewEmployer: TCompany);
    function IndexOfEmail(Email: TEmail): Integer;
    procedure InsertEmail(Index: Integer; Email: TEmail);
    function RemoveEmail(Email: TEmail): Integer;
    property EmailCount: Integer read GetEmailCount;
    property Emails[Index: Integer]: TEmail read GetEmails write SetEmails;
  published
    property BirthDate: TDate read GetBirthDate write SetBirthDate;
    property BirthTime: TTime read GetBirthTime write SetBirthTime;
    property Employer: TCompany read GetEmployer;
    property MainEmailAddress: string read GetMainEmailAddress write SetMainEmailAddress;
    property Picture: string read GetPicture write SetPicture;
    property Salary: Currency read GetSalary write SetSalary;
  end;

  TCompany = class(TContact)
    {IOMETADATA stored;
    Employees: References(TPerson); }
    _Employees: TInstantReferences;
  private
    function GetEmployeeCount: Integer;
    function GetEmployees(Index: Integer): TPerson;
  public
    function AddEmployee(Employee: TPerson): Integer;
    procedure ClearEmployees;
    procedure DeleteEmployee(Index: Integer);
    function IndexOfEmployee(Employee: TPerson): Integer;
    procedure InsertEmployee(Index: Integer; Employee: TPerson);
    function RemoveEmployee(Employee: TPerson): Integer;
    property EmployeeCount: Integer read GetEmployeeCount;
    property Employees[Index: Integer]: TPerson read GetEmployees;
  end;

implementation

uses
  System.SysUtils,
  InstantUtils,
  InstantMetadata;

{ TAddress }

function TAddress.GetCity: string;
begin
  Result := _City.Value;
end;

function TAddress.GetCountry: TCountry;
begin
  Result := _Country.Value as TCountry;
end;

function TAddress.GetState: string;
begin
  Result := _State.Value;
end;

function TAddress.GetStreet: string;
begin
  Result := _Street.Value;
end;

function TAddress.GetZip: string;
begin
  Result := _Zip.Value;
end;

procedure TAddress.SetCity(const Value: string);
begin
  _City.Value := Value;
end;

procedure TAddress.SetCountry(Value: TCountry);
begin
  _Country.Value := Value;
end;

procedure TAddress.SetState(const Value: string);
begin
  _State.Value := Value;
end;

procedure TAddress.SetStreet(const Value: string);
begin
  _Street.Value := Value;
end;

procedure TAddress.SetZip(const Value: string);
begin
  _Zip.Value := Value;
end;

{ TCountry }

procedure TCountry.BeforeStore;
begin
  if Id = '' then
    raise Exception.Create('Country ID missing');
  inherited;
end;

function TCountry.GetCaption: string;
begin
  Result := Name;
end;

function TCountry.GetName: string;
begin
  Result := _Name.Value;
end;

procedure TCountry.SetName(const Value: string);
begin
  _Name.Value := Value;
end;

{ TPerson }

function TPerson.AddEmail(Email: TEmail): Integer;
begin
  Result := _Emails.Add(Email);
end;

procedure TPerson.BeforeDispose;
begin
  inherited;
  EmployBy(nil);
end;

procedure TPerson.ClearEmails;
begin
  _Emails.Clear;
end;

procedure TPerson.DeleteEmail(Index: Integer);
begin
  _Emails.Delete(Index);
end;

procedure TPerson.EmployBy(NewEmployer: TCompany);

  procedure AddToEmployer(AEmployer: TCompany);
  begin
    if Assigned(AEmployer) then
      AEmployer.AddEmployee(Self);
  end;

  procedure RemoveFromEmployer(AEmployer: TCompany);
  begin
    if Assigned(AEmployer) then
      AEmployer.RemoveEmployee(Self);
  end;

  procedure StoreEmployer(AEmployer: TCompany);
  begin
    if Assigned(AEmployer) then
      AEmployer.Store;
  end;

  procedure ReferenceEmployer(AEmployer: TCompany);
  begin
    _Employer.Reset;
    if Assigned(AEmployer) then
      _Employer.ReferenceObject(AEmployer.ClassType, AEmployer.Id);
  end;

var
  OldEmployer: TCompany;
begin
  OldEmployer := Employer;
  Connector.StartTransaction;
  try
    AddToEmployer(NewEmployer);
    try
      StoreEmployer(NewEmployer);
      RemoveFromEmployer(OldEmployer);
      try
        StoreEmployer(OldEmployer);
        ReferenceEmployer(NewEmployer);
        try
          Store;
          Connector.CommitTransaction;
        except
          ReferenceEmployer(OldEmployer);
          raise;
        end;
      except
        AddToEmployer(OldEmployer);
        raise;
      end;
    except
      RemoveFromEmployer(NewEmployer);
      raise;
    end;
  except
    Connector.RollbackTransaction;
    raise;
  end;
end;

function TPerson.GetBirthDate: TDate;
begin
  Result := _BirthDate.Value;
end;

function TPerson.GetBirthTime: TTime;
begin
  Result := _BirthTime.Value;
end;

function TPerson.GetEmailCount: Integer;
begin
  Result := _Emails.Count
end;

function TPerson.GetEmails(Index: Integer): TEmail;
begin
  Result := _Emails[Index] as TEmail;
end;

function TPerson.GetEmployer: TCompany;
begin
  Result := _Employer.Value as TCompany;
end;

function TPerson.GetMainEmailAddress: string;
begin
  if EmailCount > 0 then
    Result := Emails[0].Address
  else
    Result := '';
end;

function TPerson.GetPicture: string;
begin
  Result := _Picture.Value;
end;

function TPerson.GetSalary: Currency;
begin
  Result := _Salary.Value;
end;

function TPerson.IndexOfEmail(Email: TEmail): Integer;
begin
  Result := _Emails.IndexOf(Email);
end;

procedure TPerson.InsertEmail(Index: Integer; Email: TEmail);
begin
  _Emails.Insert(Index, Email);
end;

function TPerson.RemoveEmail(Email: TEmail): Integer;
begin
  Result := _Emails.Remove(Email);
end;

procedure TPerson.SetBirthDate(Value: TDate);
begin
  _BirthDate.Value := Value;
end;

procedure TPerson.SetBirthTime(Value: TTime);
begin
  _BirthTime.Value := Value;
end;

procedure TPerson.SetEmails(Index: Integer; Value: TEmail);
begin
  _Emails[Index] := Value;
end;

procedure TPerson.SetMainEmailAddress(const Value: string);
var
  Email: TEmail;
begin
  if Value <> MainEmailAddress then
  begin
    if EmailCount = 0 then
    begin
      Email := TEmail.Create;
      AddEmail(Email);
    end else
      Email := Emails[0];
    Email.Address := Value;
  end;
end;

procedure TPerson.SetPicture(const Value: string);
begin
  _Picture.Value := Value;
end;

procedure TPerson.SetSalary(Value: Currency);
begin
  _Salary.Value := Value;
end;

{ TPhone }

function TPhone.GetName: string;
begin
  Result := _Name.Value;
end;

function TPhone.GetNumber: string;
begin
  Result := _Number.Value;
end;

procedure TPhone.SetName(const Value: string);
begin
  _Name.Value := Value;
end;

procedure TPhone.SetNumber(const Value: string);
begin
  _Number.Value := Value;
end;

{ TEmail }

function TEmail.GetAddress: string;
begin
  Result := _Address.Value;
end;

procedure TEmail.SetAddress(const Value: string);
begin
  _Address.Value := Value;
end;

{ TCategory }

function TCategory.GetCaption: string;
begin
  Result := Name;
end;

function TCategory.GetName: string;
begin
  Result := _Name.Value;
end;

procedure TCategory.SetName(const Value: string);
begin
  _Name.Value := Value;
end;

{ TContact }

function TContact.AddPhone(Phone: TPhone): Integer;
begin
  Result := _Phones.Add(Phone);
end;

procedure TContact.AfterCreate;
begin
  inherited;
  Id := InstantGenerateId;
  _Category.ReferenceObject(TCategory, 'CAT000');
end;

procedure TContact.BeforeStore;
begin
  if Name = '' then
    raise Exception.Create('Contact name required');
  inherited;
  City := Address.City;
end;

procedure TContact.ClearPhones;
begin
  _Phones.Clear;
end;

procedure TContact.DeletePhone(Index: Integer);
begin
  _Phones.Delete(Index);
end;

function TContact.GetAddress: TAddress;
begin
  Result := _Address.Value as TAddress;
end;

function TContact.GetCaption: string;
begin
  Result := Name;
end;

function TContact.GetCategory: TCategory;
begin
  Result := _Category.Value as TCategory;
end;

function TContact.GetCity: string;
begin
  Result := _City.Value;
end;

function TContact.GetMainPhoneNumber: string;
begin
  if PhoneCount > 0 then
    Result := Phones[0].Number
  else
    Result := '';
end;

function TContact.GetName: string;
begin
  Result := _Name.Value;
end;

function TContact.GetPhoneCount: Integer;
begin
  Result := _Phones.Count
end;

function TContact.GetPhones(Index: Integer): TPhone;
begin
  Result := _Phones[Index] as TPhone;
end;

function TContact.IndexOfPhone(Phone: TPhone): Integer;
begin
  Result := _Phones.IndexOf(Phone);
end;

procedure TContact.InsertPhone(Index: Integer; Phone: TPhone);
begin
  _Phones.Insert(Index, Phone);
end;

function TContact.RemovePhone(Phone: TPhone): Integer;
begin
  Result := _Phones.Remove(Phone);
end;

procedure TContact.SetAddress(Value: TAddress);
begin
  _Address.Value := Value;
end;

procedure TContact.SetCategory(Value: TCategory);
begin
  _Category.Value := Value;
end;

procedure TContact.SetCity(const Value: string);
begin
  _City.Value := Value;
end;

procedure TContact.SetMainPhoneNumber(const Value: string);
var
  Phone: TPhone;
begin
  if Value <> MainPhoneNumber then
  begin
    if PhoneCount = 0 then
    begin
      Phone := TPhone.Create;
      AddPhone(Phone);
      Phone.Name := 'Main';
    end else
      Phone := Phones[0];
    Phone.Number := Value;
  end;
end;

procedure TContact.SetName(const Value: string);
begin
  _Name.Value := Value;
end;

procedure TContact.SetPhones(Index: Integer; Value: TPhone);
begin
  _Phones[Index] := Value;
end;

{ TContactFilter }

function TContactFilter.GetIsEmpty: Boolean;
begin
  Result :=
    (Name = '') and
    (Address.Street = '') and
    (Address.City = '') and
    (Address.Zip = '') and
    (Address.State = '') and
    (Address.Country = nil) and
    (Category = nil);
end;

function TContactFilter.Matches(Contact: TContact): Boolean;

  function MatchStr(const Str1, Str2: string): Boolean;
  begin
    Result := (Str1 = '') or (Pos(UpperCase(Str1), UpperCase(Str2)) > 0);
  end;

  function MatchObj(Obj1, Obj2: TObject): Boolean;
  begin
    Result := not Assigned(Obj1) or (Obj1 = Obj2);
  end;

begin
  Result := Assigned(Contact) and
    MatchStr(Name, Contact.Name) and
    MatchStr(Address.Street, Contact.Address.Street) and
    MatchStr(Address.City, Contact.Address.City) and
    MatchStr(Address.Zip, Contact.Address.Zip) and
    MatchStr(Address.State, Contact.Address.State) and
    MatchObj(Address.Country, Contact.Address.Country) and
    MatchObj(Category, Contact.Category);
end;

{ TCompany }

function TCompany.AddEmployee(Employee: TPerson): Integer;
begin
  Result := _Employees.Add(Employee);
end;

procedure TCompany.ClearEmployees;
begin
  _Employees.Clear;
end;

procedure TCompany.DeleteEmployee(Index: Integer);
begin
  _Employees.Delete(Index);
end;

function TCompany.GetEmployeeCount: Integer;
begin
  Result := _Employees.Count;
end;

function TCompany.GetEmployees(Index: Integer): TPerson;
begin
  Result := _Employees[Index] as TPerson;
end;

function TCompany.IndexOfEmployee(Employee: TPerson): Integer;
begin
  Result := _Employees.IndexOf(Employee);
end;

procedure TCompany.InsertEmployee(Index: Integer; Employee: TPerson);
begin
  _Employees.Insert(Index, Employee);
end;

function TCompany.RemoveEmployee(Employee: TPerson): Integer;
begin
  Result := _Employees.Remove(Employee);
end;

initialization
  InstantRegisterClasses([
    TAddress,
    TCategory,
    TCompany,
    TContact,
    TContactFilter,
    TCountry,
    TEmail,
    TPerson,
    TPhone
  ]);

end.
