(*
 *   InstantObjects Test Suite
 *   TestModel
 *)

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is: InstantObjects Test Suite/TestModel
 *
 * The Initial Developer of the Original Code is: Uberto Barbini
 *
 * Portions created by the Initial Developer are Copyright (C) 2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Steven Mitchell
 *
 * ***** END LICENSE BLOCK ***** *)

unit TestModel;

interface

uses
  InstantPersistence;

type
  TAddress = class;
  TCategory = class;
  TCompany = class;
  TContact = class;
  TContactFilter = class;
  TCountry = class;
  TEmail = class;
  TPartExternal = class;
  TPartsExternal = class;
  TPerson = class;
  TPhone = class;
  TProject = class;

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
    property Name: string read GetName write SetName;
  end;

  TPhone = class(TInstantObject)
  {IOMETADATA stored;
    Name: String(20);
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
  {IOMETADATA stored;
    Address: String(100); }
    _Address: TInstantString;
  private
    function GetAddress: string;
    procedure SetAddress(const Value: string);
  published
    property Address: string read GetAddress write SetAddress;
  end;

  TCategory = class(TInstantObject)
  {IOMETADATA stored 'Categories';
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
    Phones: Parts(TPhone);
    Projects: References(TProject) external 'Contact_Projects';
    PartExternal: Part(TPartExternal) external;
    ExternalParts: Parts(TPartsExternal) external 'Contact_ExternalParts'; }
    _Address: TInstantPart;
    _Category: TInstantReference;
    _City: TInstantString;
    _ExternalParts: TInstantParts;
    _Name: TInstantString;
    _PartExternal: TInstantPart;
    _Phones: TInstantParts;
    _Projects: TInstantReferences;
  private
    function GetAddress: TAddress;
    function GetCategory: TCategory;
    function GetCity: string;
    function GetExternalPartCount: Integer;
    function GetExternalParts(Index: Integer): TPartsExternal;
    function GetMainPhoneNumber: string;
    function GetName: string;
    function GetPartExternal: TPartExternal;
    function GetPhoneCount: Integer;
    function GetPhones(Index: Integer): TPhone;
    function GetProjectCount: Integer;
    function GetProjects(Index: Integer): TProject;
    procedure SetAddress(Value: TAddress);
    procedure SetCategory(Value: TCategory);
    procedure SetCity(const Value: string);
    procedure SetExternalParts(Index: Integer; Value: TPartsExternal);
    procedure SetMainPhoneNumber(const Value: string);
    procedure SetName(const Value: string);
    procedure SetPartExternal(Value: TPartExternal);
    procedure SetPhones(Index: Integer; Value: TPhone);
    procedure SetProjects(Index: Integer; Value: TProject);
  protected
    procedure AfterCreate; override;
    procedure BeforeStore; override;
    function GetCaption: string; override;
  public
    function AddExternalPart(ExternalPart: TPartsExternal): Integer;
    function AddPhone(Phone: TPhone): Integer;
    function AddProject(Project: TProject): Integer;
    procedure ClearExternalParts;
    procedure ClearPhones;
    procedure ClearProjects;
    procedure DeleteExternalPart(Index: Integer);
    procedure DeletePhone(Index: Integer);
    procedure DeleteProject(Index: Integer);
    function IndexOfExternalPart(ExternalPart: TPartsExternal): Integer;
    function IndexOfPhone(Phone: TPhone): Integer;
    function IndexOfProject(Project: TProject): Integer;
    procedure InsertExternalPart(Index: Integer; ExternalPart: TPartsExternal);
    procedure InsertPhone(Index: Integer; Phone: TPhone);
    procedure InsertProject(Index: Integer; Project: TProject);
    function RemoveExternalPart(ExternalPart: TPartsExternal): Integer;
    function RemovePhone(Phone: TPhone): Integer;
    function RemoveProject(Project: TProject): Integer;
    property ExternalPartCount: Integer read GetExternalPartCount;
    property ExternalParts[Index: Integer]: TPartsExternal read GetExternalParts write SetExternalParts;
    property PhoneCount: Integer read GetPhoneCount;
    property Phones[Index: Integer]: TPhone read GetPhones write SetPhones;
    property ProjectCount: Integer read GetProjectCount;
    property Projects[Index: Integer]: TProject read GetProjects
        write SetProjects;
  published
    property Address: TAddress read GetAddress write SetAddress;
    property Category: TCategory read GetCategory write SetCategory;
    property City: string read GetCity write SetCity;
    property MainPhoneNumber: string read GetMainPhoneNumber
        write SetMainPhoneNumber;
    property Name: string read GetName write SetName;
    property PartExternal: TPartExternal read GetPartExternal write SetPartExternal;
  end;

  TContactFilter = class(TContact)
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
    BirthDate: DateTime;
    Emails: Parts(TEmail);
    Employer: Reference(TCompany);
    Picture: Blob;
    Salary: Currency;
    Employed: Boolean;
    AL_hours: Float; }
    _AL_hours: TInstantFloat;
    _BirthDate: TInstantDateTime;
    _Emails: TInstantParts;
    _Employed: TInstantBoolean;
    _Employer: TInstantReference;
    _Picture: TInstantGraphic;
    _Salary: TInstantCurrency;
  private
    function GetAL_hours: Double;
    function GetBirthDate: TDateTime;
    function GetEmailCount: Integer;
    function GetEmails(Index: Integer): TEmail;
    function GetEmployed: Boolean;
    function GetEmployer: TCompany;
    function GetMainEmailAddress: string;
    function GetPicture: string;
    function GetSalary: Currency;
    procedure SetAL_hours(Value: Double);
    procedure SetBirthDate(Value: TDateTime);
    procedure SetEmails(Index: Integer; Value: TEmail);
    procedure SetEmployed(Value: Boolean);
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
    property AL_hours: Double read GetAL_hours write SetAL_hours;
    property BirthDate: TDateTime read GetBirthDate write SetBirthDate;
    property Employed: Boolean read GetEmployed write SetEmployed;
    property Employer: TCompany read GetEmployer;
    property MainEmailAddress: string read GetMainEmailAddress write SetMainEmailAddress;
    property Picture: string read GetPicture write SetPicture;
    property Salary: Currency read GetSalary write SetSalary;
  end;

  TCompany = class(TContact)
  {IOMETADATA stored;
    Employees: References(TPerson);
    NoOfBranches: Integer; }
    _Employees: TInstantReferences;
    _NoOfBranches: TInstantInteger;
  private
    function GetEmployeeCount: Integer;
    function GetEmployees(Index: Integer): TPerson;
    function GetNoOfBranches: Integer;
    procedure SetNoOfBranches(Value: Integer);
  public
    function AddEmployee(Employee: TPerson): Integer;
    procedure ClearEmployees;
    procedure DeleteEmployee(Index: Integer);
    function IndexOfEmployee(Employee: TPerson): Integer;
    procedure InsertEmployee(Index: Integer; Employee: TPerson);
    function RemoveEmployee(Employee: TPerson): Integer;
    property EmployeeCount: Integer read GetEmployeeCount;
    property Employees[Index: Integer]: TPerson read GetEmployees;
  published
    property NoOfBranches: Integer read GetNoOfBranches write SetNoOfBranches;
  end;

  TProject = class(TInstantObject)
  {IOMETADATA stored;
    Name: String(30); }
    _Name: TInstantString;
  private
    function GetName: string;
    procedure SetName(const Value: string);
  published
    property Name: string read GetName write SetName;
  end;

  TPartExternal = class(TInstantObject)
  {IOMETADATA stored;
    Name: String(30) default; }
    _Name: TInstantString;
  private
    function GetName: string;
    procedure SetName(const Value: string);
  published
    property Name: string read GetName write SetName;
  end;

  TPartsExternal = class(TInstantObject)
  {IOMETADATA stored;
    Name: String; }
    _Name: TInstantString;
  private
    function GetName: string;
    procedure SetName(const Value: string);
  published
    property Name: string read GetName write SetName;
  end;

implementation

uses
  SysUtils, InstantUtils;

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

function TPerson.GetAL_hours: Double;
begin
  Result := _AL_hours.Value;
end;

function TPerson.GetBirthDate: TDateTime;
begin
  Result := _BirthDate.Value;
end;

function TPerson.GetEmailCount: Integer;
begin
  Result := _Emails.Count
end;

function TPerson.GetEmails(Index: Integer): TEmail;
begin
  Result := _Emails[Index] as TEmail;
end;

function TPerson.GetEmployed: Boolean;
begin
  Result := _Employed.Value;
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

procedure TPerson.SetAL_hours(Value: Double);
begin
  _AL_hours.Value := Value;
end;

procedure TPerson.SetBirthDate(Value: TDateTime);
begin
  _BirthDate.Value := Value;
end;

procedure TPerson.SetEmails(Index: Integer; Value: TEmail);
begin
  _Emails[Index] := Value;
end;

procedure TPerson.SetEmployed(Value: Boolean);
begin
  _Employed.Value := Value;
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

{ TPhone }

procedure TPerson.SetSalary(Value: Currency);
begin
  _Salary.Value := Value;
end;

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

{ TProject }

function TProject.GetName: string;
begin
  Result := _Name.Value;
end;

procedure TProject.SetName(const Value: string);
begin
  _Name.Value := Value;
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

procedure TCompany.SetNoOfBranches(Value: Integer);
begin
  _NoOfBranches.Value := Value;
end;

function TContact.AddExternalPart(ExternalPart: TPartsExternal): Integer;
begin
  Result := _ExternalParts.Add(ExternalPart);
end;

function TContact.AddPhone(Phone: TPhone): Integer;
begin
  Result := _Phones.Add(Phone);
end;

function TContact.AddProject(Project: TProject): Integer;
begin
  Result := _Projects.Add(Project);
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

procedure TContact.ClearExternalParts;
begin
  _ExternalParts.Clear;
end;

procedure TContact.ClearPhones;
begin
  _Phones.Clear;
end;

procedure TContact.ClearProjects;
begin
  _Projects.Clear;
end;

procedure TContact.DeleteExternalPart(Index: Integer);
begin
  _ExternalParts.Delete(Index);
end;

procedure TContact.DeletePhone(Index: Integer);
begin
  _Phones.Delete(Index);
end;

procedure TContact.DeleteProject(Index: Integer);
begin
  _Projects.Delete(Index);
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

function TContact.GetExternalPartCount: Integer;
begin
  Result := _ExternalParts.Count;
end;

function TContact.GetExternalParts(Index: Integer): TPartsExternal;
begin
  Result := _ExternalParts[Index] as TPartsExternal;
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

function TContact.GetPartExternal: TPartExternal;
begin
  Result := _PartExternal.Value as TPartExternal;
end;

function TContact.GetPhoneCount: Integer;
begin
  Result := _Phones.Count
end;

function TContact.GetPhones(Index: Integer): TPhone;
begin
  Result := _Phones[Index] as TPhone;
end;

function TContact.GetProjectCount: Integer;
begin
  Result := _Projects.Count;
end;

function TContact.GetProjects(Index: Integer): TProject;
begin
  Result := _Projects[Index] as TProject;
end;

function TContact.IndexOfExternalPart(ExternalPart: TPartsExternal): Integer;
begin
  Result := _ExternalParts.IndexOf(ExternalPart);
end;

function TContact.IndexOfPhone(Phone: TPhone): Integer;
begin
  Result := _Phones.IndexOf(Phone);
end;

function TContact.IndexOfProject(Project: TProject): Integer;
begin
  Result := _Projects.IndexOf(Project);
end;

procedure TContact.InsertExternalPart(Index: Integer; ExternalPart: TPartsExternal);
begin
  _ExternalParts.Insert(Index, ExternalPart);
end;

procedure TContact.InsertPhone(Index: Integer; Phone: TPhone);
begin
  _Phones.Insert(Index, Phone);
end;

procedure TContact.InsertProject(Index: Integer; Project: TProject);
begin
  _Projects.Insert(Index, Project);
end;

function TContact.RemoveExternalPart(ExternalPart: TPartsExternal): Integer;
begin
  Result := _ExternalParts.Remove(ExternalPart);
end;

function TContact.RemovePhone(Phone: TPhone): Integer;
begin
  Result := _Phones.Remove(Phone);
end;

function TContact.RemoveProject(Project: TProject): Integer;
begin
  Result := _Projects.Remove(Project);
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

procedure TContact.SetExternalParts(Index: Integer; Value: TPartsExternal);
begin
  _ExternalParts[Index] := Value;
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

procedure TContact.SetPartExternal(Value: TPartExternal);
begin
  _PartExternal.Value := Value;
end;

procedure TContact.SetPhones(Index: Integer; Value: TPhone);
begin
  _Phones[Index] := Value;
end;

{ TContactFilter }

procedure TContact.SetProjects(Index: Integer; Value: TProject);
begin
  _Projects[Index] := Value;
end;

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
  Result := _Employees.Add(Employee)
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
  Result := _Employees.Count
end;

function TCompany.GetEmployees(Index: Integer): TPerson;
begin
  Result := _Employees[Index] as TPerson;
end;

function TCompany.GetNoOfBranches: Integer;
begin
  Result := _NoOfBranches.Value;
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

{ TPartExternal }

function TPartExternal.GetName: string;
begin
  Result := _Name.Value;
end;

procedure TPartExternal.SetName(const Value: string);
begin
  _Name.Value := Value;
end;

{ TPartsExternal }

function TPartsExternal.GetName: string;
begin
  Result := _Name.Value;
end;

procedure TPartsExternal.SetName(const Value: string);
begin
  _Name.Value := Value;
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
    TPartExternal,
    TPartsExternal,
    TPerson,
    TPhone,
    TProject
  ]);

end.
