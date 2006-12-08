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
  TDBField = class;
  TDBFieldPair = class;
  TDBForeignKey = class;
  TDBPrimaryKey = class;
  TDBTable = class;
  TEmail = class;
  TExternalAddress = class;
  TExternalPhone = class;
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
  {IOMETADATA Name: String(20);
    Name: String(30);
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
    ExternalAddress: Part(TExternalAddress) external;
    ExternalPhones: Parts(TExternalPhone) external 'Contact_ExternalPhones'; }
    _Address: TInstantPart;
    _Category: TInstantReference;
    _City: TInstantString;
    _ExternalPhones: TInstantParts;
    _Name: TInstantString;
    _ExternalAddress: TInstantPart;
    _Phones: TInstantParts;
    _Projects: TInstantReferences;
  private
    function GetAddress: TAddress;
    function GetCategory: TCategory;
    function GetCity: string;
    function GetExternalPartCount: Integer;
    function GetExternalPhones(Index: Integer): TExternalPhone;
    function GetMainPhoneNumber: string;
    function GetName: string;
    function GeTExternalAddress: TExternalAddress;
    function GetPhoneCount: Integer;
    function GetPhones(Index: Integer): TPhone;
    function GetProjectCount: Integer;
    function GetProjects(Index: Integer): TProject;
    procedure SetAddress(Value: TAddress);
    procedure SetCategory(Value: TCategory);
    procedure SetCity(const Value: string);
    procedure SetExternalPhones(Index: Integer; Value: TExternalPhone);
    procedure SetMainPhoneNumber(const Value: string);
    procedure SetName(const Value: string);
    procedure SeTExternalAddress(Value: TExternalAddress);
    procedure SetPhones(Index: Integer; Value: TPhone);
    procedure SetProjects(Index: Integer; Value: TProject);
  protected
    procedure AfterCreate; override;
    procedure BeforeStore; override;
    function GetCaption: string; override;
  public
    function AddExternalPart(ExternalPart: TExternalPhone): Integer;
    function AddPhone(Phone: TPhone): Integer;
    function AddProject(Project: TProject): Integer;
    procedure ClearExternalPhones;
    procedure ClearPhones;
    procedure ClearProjects;
    procedure DeleteExternalPart(Index: Integer);
    procedure DeletePhone(Index: Integer);
    procedure DeleteProject(Index: Integer);
    function IndexOfExternalPart(ExternalPart: TExternalPhone): Integer;
    function IndexOfPhone(Phone: TPhone): Integer;
    function IndexOfProject(Project: TProject): Integer;
    procedure InsertExternalPart(Index: Integer; ExternalPart: TExternalPhone);
    procedure InsertPhone(Index: Integer; Phone: TPhone);
    procedure InsertProject(Index: Integer; Project: TProject);
    function RemoveExternalPart(ExternalPart: TExternalPhone): Integer;
    function RemovePhone(Phone: TPhone): Integer;
    function RemoveProject(Project: TProject): Integer;
    property ExternalPartCount: Integer read GetExternalPartCount;
    property ExternalPhones[Index: Integer]: TExternalPhone read GetExternalPhones write SetExternalPhones;
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
    property ExternalAddress: TExternalAddress read GeTExternalAddress write SeTExternalAddress;
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
    AL_hours: Float;
    EmploymentDate: Date;
    BirthTime: Time; }
    _AL_hours: TInstantFloat;
    _BirthDate: TInstantDateTime;
    _BirthTime: TInstantTime;
    _Emails: TInstantParts;
    _Employed: TInstantBoolean;
    _Employer: TInstantReference;
    _EmploymentDate: TInstantDate;
    _Picture: TInstantGraphic;
    _Salary: TInstantCurrency;
  private
    function GetAL_hours: Double;
    function GetBirthDate: TDateTime;
    function GetBirthTime: TDateTime;
    function GetEmailCount: Integer;
    function GetEmails(Index: Integer): TEmail;
    function GetEmployed: Boolean;
    function GetEmployer: TCompany;
    function GetEmploymentDate: TDateTime;
    function GetMainEmailAddress: string;
    function GetPicture: string;
    function GetSalary: Currency;
    procedure SetAL_hours(Value: Double);
    procedure SetBirthDate(Value: TDateTime);
    procedure SetBirthTime(Value: TDateTime);
    procedure SetEmails(Index: Integer; Value: TEmail);
    procedure SetEmployed(Value: Boolean);
    procedure SetEmployer(const Value: TCompany);
    procedure SetEmploymentDate(Value: TDateTime);
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
    property BirthTime: TDateTime read GetBirthTime write SetBirthTime;
    property Employed: Boolean read GetEmployed write SetEmployed;
    property Employer: TCompany read GetEmployer write SetEmployer;
    property EmploymentDate: TDateTime read GetEmploymentDate write SetEmploymentDate;
    property MainEmailAddress: string read GetMainEmailAddress write SetMainEmailAddress;
    property Picture: string read GetPicture write SetPicture;
    property Salary: Currency read GetSalary write SetSalary;
  end;

  TCompany = class(TContact)
  {IOMETADATA stored;
    Employees: References(TPerson);
    NoOfBranches: Integer;
    Subsidiaries: References(TCompany); }
    _Employees: TInstantReferences;
    _NoOfBranches: TInstantInteger;
    _Subsidiaries: TInstantReferences;
  private
    function GetEmployeeCount: Integer;
    function GetEmployees(Index: Integer): TPerson;
    function GetNoOfBranches: Integer;
    function GetSubsidiaries(Index: Integer): TCompany;
    function GetSubsidiaryCount: Integer;
    procedure SetNoOfBranches(Value: Integer);
    procedure SetSubsidiaries(Index: Integer; Value: TCompany);
  public
    function AddEmployee(Employee: TPerson): Integer;
    function AddSubsidiary(Subsidiary: TCompany): Integer;
    procedure ClearEmployees;
    procedure ClearSubsidiaries;
    procedure DeleteEmployee(Index: Integer);
    procedure DeleteSubsidiary(Index: Integer);
    function IndexOfEmployee(Employee: TPerson): Integer;
    function IndexOfSubsidiary(Subsidiary: TCompany): Integer;
    procedure InsertEmployee(Index: Integer; Employee: TPerson);
    procedure InsertSubsidiary(Index: Integer; Subsidiary: TCompany);
    function RemoveEmployee(Employee: TPerson): Integer;
    function RemoveSubsidiary(Subsidiary: TCompany): Integer;
    property EmployeeCount: Integer read GetEmployeeCount;
    property Employees[Index: Integer]: TPerson read GetEmployees;
    property Subsidiaries[Index: Integer]: TCompany read GetSubsidiaries write SetSubsidiaries;
    property SubsidiaryCount: Integer read GetSubsidiaryCount;
  published
    property NoOfBranches: Integer read GetNoOfBranches write SetNoOfBranches;
  end;

  TProject = class(TInstantObject)
  {IOMETADATA stored 'Projects';
    Name: String(30);
    SubProjects: Parts(TProject) external 'Project_SubProjects';
    Addresses: Parts(TExternalAddress) external 'Project_Addresses';
    Manager: Reference(TContact);
    Participants: References(TContact) external 'Project_Participants'; }
    _Addresses: TInstantParts;
    _Manager: TInstantReference;
    _Name: TInstantString;
    _Participants: TInstantReferences;
    _SubProjects: TInstantParts;
  private
    function GetAddressCount: Integer;
    function GetAddresses(Index: Integer): TExternalAddress;
    function GetManager: TContact;
    function GetName: string;
    function GetParticipantCount: Integer;
    function GetParticipants(Index: Integer): TContact;
    function GetSubProjectCount: Integer;
    function GetSubProjects(Index: Integer): TProject;
    procedure SetAddresses(Index: Integer; Value: TExternalAddress);
    procedure SetManager(Value: TContact);
    procedure SetName(const Value: string);
    procedure SetParticipants(Index: Integer; Value: TContact);
    procedure SetSubProjects(Index: Integer; Value: TProject);
  public
    function AddAddress(Address: TExternalAddress): Integer;
    function AddParticipant(Participant: TContact): Integer;
    function AddSubProject(SubProject: TProject): Integer;
    procedure ClearAddresses;
    procedure ClearParticipants;
    procedure ClearSubProjects;
    procedure DeleteAddress(Index: Integer);
    procedure DeleteParticipant(Index: Integer);
    procedure DeleteSubProject(Index: Integer);
    function IndexOfAddress(Address: TExternalAddress): Integer;
    function IndexOfParticipant(Participant: TContact): Integer;
    function IndexOfSubProject(SubProject: TProject): Integer;
    procedure InsertAddress(Index: Integer; Address: TExternalAddress);
    procedure InsertParticipant(Index: Integer; Participant: TContact);
    procedure InsertSubProject(Index: Integer; SubProject: TProject);
    function RemoveAddress(Address: TExternalAddress): Integer;
    function RemoveParticipant(Participant: TContact): Integer;
    function RemoveSubProject(SubProject: TProject): Integer;
    property AddressCount: Integer read GetAddressCount;
    property Addresses[Index: Integer]: TExternalAddress read GetAddresses write SetAddresses;
    property ParticipantCount: Integer read GetParticipantCount;
    property Participants[Index: Integer]: TContact read GetParticipants write SetParticipants;
    property SubProjectCount: Integer read GetSubProjectCount;
    property SubProjects[Index: Integer]: TProject read GetSubProjects write SetSubProjects;
  published
    property Manager: TContact read GetManager write SetManager;
    property Name: string read GetName write SetName;
  end;

  TExternalAddress = class(TInstantObject)
  {IOMETADATA stored 'ExternalAddresses';
    Name: String(30);
    Category: Reference(TCategory);
    Site_Contact: Reference(TPerson); }
    _Category: TInstantReference;
    _Name: TInstantString;
    _Site_Contact: TInstantReference;
  private
    function GetCategory: TCategory;
    function GetName: string;
    function GetSite_Contact: TPerson;
    procedure SetCategory(Value: TCategory);
    procedure SetName(const Value: string);
    procedure SetSite_Contact(Value: TPerson);
  published
    property Category: TCategory read GetCategory write SetCategory;
    property Name: string read GetName write SetName;
    property Site_Contact: TPerson read GetSite_Contact write SetSite_Contact;
  end;

  TExternalPhone = class(TInstantObject)
  {IOMETADATA stored 'ExternalPhones';
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

  TDBTable = class(TInstantObject)
  {IOMETADATA stored;
    PrimaryKey: Part(TDBPrimaryKey);
    Name: String;
    ForeignKeys: Parts(TDBForeignKey); }
    _ForeignKeys: TInstantParts;
    _Name: TInstantString;
    _PrimaryKey: TInstantPart;
  private
    function GetForeignKeyCount: Integer;
    function GetForeignKeys(Index: Integer): TDBForeignKey;
    function GetName: string;
    function GetPrimaryKey: TDBPrimaryKey;
    procedure SetForeignKeys(Index: Integer; Value: TDBForeignKey);
    procedure SetName(const Value: string);
    procedure SetPrimaryKey(Value: TDBPrimaryKey);
  public
    function AddForeignKey(ForeignKey: TDBForeignKey): Integer;
    procedure ClearForeignKeys;
    procedure DeleteForeignKey(Index: Integer);
    function IndexOfForeignKey(ForeignKey: TDBForeignKey): Integer;
    procedure InsertForeignKey(Index: Integer; ForeignKey: TDBForeignKey);
    function RemoveForeignKey(ForeignKey: TDBForeignKey): Integer;
    property ForeignKeyCount: Integer read GetForeignKeyCount;
    property ForeignKeys[Index: Integer]: TDBForeignKey read GetForeignKeys write SetForeignKeys;
  published
    property Name: string read GetName write SetName;
    property PrimaryKey: TDBPrimaryKey read GetPrimaryKey write SetPrimaryKey;
  end;

  TDBField = class(TInstantObject)
  {IOMETADATA stored;
    Table: Reference(TDBTable);
    Name: String; }
    _Name: TInstantString;
    _Table: TInstantReference;
  private
    function GetName: string;
    function GetTable: TDBTable;
    procedure SetName(const Value: string);
    procedure SetTable(Value: TDBTable);
  published
    property Name: string read GetName write SetName;
    property Table: TDBTable read GetTable write SetTable;
  end;

  TDBPrimaryKey = class(TInstantObject)
  {IOMETADATA stored;
    Fields: References(TDBField);
    Name: String; }
    _Fields: TInstantReferences;
    _Name: TInstantString;
  private
    function GetFieldCount: Integer;
    function GetFields(Index: Integer): TDBField;
    function GetName: string;
    procedure SetFields(Index: Integer; Value: TDBField);
    procedure SetName(const Value: string);
  public
    function AddField(Field: TDBField): Integer;
    procedure ClearFields;
    procedure DeleteField(Index: Integer);
    function IndexOfField(Field: TDBField): Integer;
    procedure InsertField(Index: Integer; Field: TDBField);
    function RemoveField(Field: TDBField): Integer;
    property FieldCount: Integer read GetFieldCount;
    property Fields[Index: Integer]: TDBField read GetFields write SetFields;
  published
    property Name: string read GetName write SetName;
  end;

  TDBFieldPair = class(TINstantObject)
  {IOMETADATA Field: Reference(TDBField);
    ForeignField: Reference(TDBField); }
    _Field: TInstantReference;
    _ForeignField: TInstantReference;
  private
    function GetField: TDBField;
    function GetForeignField: TDBField;
    procedure SetField(Value: TDBField);
    procedure SetForeignField(Value: TDBField);
  published
    property Field: TDBField read GetField write SetField;
    property ForeignField: TDBField read GetForeignField write SetForeignField;
  end;

  TDBForeignKey = class(TInstantObject)
  {IOMETADATA FieldPairs: Parts(TDBFieldPair);
    Name: String; }
    _FieldPairs: TInstantParts;
    _Name: TInstantString;
  private
    function GetFieldPairCount: Integer;
    function GetFieldPairs(Index: Integer): TDBFieldPair;
    function GetName: string;
    procedure SetFieldPairs(Index: Integer; Value: TDBFieldPair);
    procedure SetName(const Value: string);
  public
    function AddFieldPair(FieldPair: TDBFieldPair): Integer;
    procedure ClearFieldPairs;
    procedure DeleteFieldPair(Index: Integer);
    function IndexOfFieldPair(FieldPair: TDBFieldPair): Integer;
    procedure InsertFieldPair(Index: Integer; FieldPair: TDBFieldPair);
    function RemoveFieldPair(FieldPair: TDBFieldPair): Integer;
    property FieldPairCount: Integer read GetFieldPairCount;
    property FieldPairs[Index: Integer]: TDBFieldPair read GetFieldPairs write SetFieldPairs;
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

procedure TExternalPhone.SetNumber(const Value: string);
begin
  _Number.Value := Value;
end;

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

function TPerson.GetBirthTime: TDateTime;
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

function TPerson.GetEmployed: Boolean;
begin
  Result := _Employed.Value;
end;

function TPerson.GetEmployer: TCompany;
begin
  Result := _Employer.Value as TCompany;
end;

function TPerson.GetEmploymentDate: TDateTime;
begin
  Result := _EmploymentDate.Value;
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

procedure TPerson.SetBirthTime(Value: TDateTime);
begin
  _BirthTime.Value := Value;
end;

procedure TPerson.SetEmails(Index: Integer; Value: TEmail);
begin
  _Emails[Index] := Value;
end;

procedure TPerson.SetEmployed(Value: Boolean);
begin
  _Employed.Value := Value;
end;

procedure TPerson.SetEmployer(const Value: TCompany);
begin
  _Employer.Value := Value;
end;

procedure TPerson.SetEmploymentDate(Value: TDateTime);
begin
  _EmploymentDate.Value := Value;
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

function TProject.AddAddress(Address: TExternalAddress): Integer;
begin
  Result := _Addresses.Add(Address);
end;

function TProject.AddParticipant(Participant: TContact): Integer;
begin
  Result := _Participants.Add(Participant);
end;

function TProject.AddSubProject(SubProject: TProject): Integer;
begin
  Result := _SubProjects.Add(SubProject);
end;

procedure TProject.ClearAddresses;
begin
  _Addresses.Clear;
end;

procedure TProject.ClearParticipants;
begin
  _Participants.Clear;
end;

procedure TProject.ClearSubProjects;
begin
  _SubProjects.Clear;
end;

procedure TProject.DeleteAddress(Index: Integer);
begin
  _Addresses.Delete(Index);
end;

function TProject.GetAddressCount: Integer;
begin
  Result := _Addresses.Count;
end;

procedure TProject.DeleteParticipant(Index: Integer);
begin
  _Participants.Delete(Index);
end;

procedure TProject.DeleteSubProject(Index: Integer);
begin
  _SubProjects.Delete(Index);
end;

function TProject.GetAddresses(Index: Integer): TExternalAddress;
begin
  Result := _Addresses[Index] as TExternalAddress;
end;

function TProject.GetManager: TContact;
begin
  Result := _Manager.Value as TContact;
end;

function TProject.GetName: string;
begin
  Result := _Name.Value;
end;

function TProject.GetParticipantCount: Integer;
begin
  Result := _Participants.Count;
end;

function TProject.GetParticipants(Index: Integer): TContact;
begin
  Result := _Participants[Index] as TContact;
end;

function TProject.GetSubProjectCount: Integer;
begin
  Result := _SubProjects.Count;
end;

function TProject.GetSubProjects(Index: Integer): TProject;
begin
  Result := _SubProjects[Index] as TProject;
end;

function TProject.IndexOfAddress(Address: TExternalAddress): Integer;
begin
  Result := _Addresses.IndexOf(Address);
end;

function TProject.IndexOfParticipant(Participant: TContact): Integer;
begin
  Result := _Participants.IndexOf(Participant);
end;

function TProject.IndexOfSubProject(SubProject: TProject): Integer;
begin
  Result := _SubProjects.IndexOf(SubProject);
end;

procedure TProject.InsertAddress(Index: Integer; Address: TExternalAddress);
begin
  _Addresses.Insert(Index, Address);
end;

procedure TProject.InsertParticipant(Index: Integer; Participant: TContact);
begin
  _Participants.Insert(Index, Participant);
end;

procedure TProject.InsertSubProject(Index: Integer; SubProject: TProject);
begin
  _SubProjects.Insert(Index, SubProject);
end;

function TProject.RemoveAddress(Address: TExternalAddress): Integer;
begin
  Result := _Addresses.Remove(Address);
end;

function TProject.RemoveParticipant(Participant: TContact): Integer;
begin
  Result := _Participants.Remove(Participant);
end;

function TProject.RemoveSubProject(SubProject: TProject): Integer;
begin
  Result := _SubProjects.Remove(SubProject);
end;

procedure TProject.SetAddresses(Index: Integer; Value: TExternalAddress);
begin
  _Addresses[Index] := Value;
end;

procedure TProject.SetManager(Value: TContact);
begin
  _Manager.Value := Value;
end;

procedure TProject.SetName(const Value: string);
begin
  _Name.Value := Value;
end;

procedure TProject.SetParticipants(Index: Integer; Value: TContact);
begin
  _Participants[Index] := Value;
end;

procedure TProject.SetSubProjects(Index: Integer; Value: TProject);
begin
  _SubProjects[Index] := Value;
end;

{ TEmail }

{ TDBTable }

procedure TDBPrimaryKey.SetName(const Value: string);
begin
  _Name.Value := Value;
end;

function TDBTable.AddForeignKey(ForeignKey: TDBForeignKey): Integer;
begin
  Result := _ForeignKeys.Add(ForeignKey);
end;

procedure TDBTable.ClearForeignKeys;
begin
  _ForeignKeys.Clear;
end;

procedure TDBTable.DeleteForeignKey(Index: Integer);
begin
  _ForeignKeys.Delete(Index);
end;

function TDBTable.GetForeignKeyCount: Integer;
begin
  Result := _ForeignKeys.Count;
end;

function TDBTable.GetForeignKeys(Index: Integer): TDBForeignKey;
begin
  Result := _ForeignKeys[Index] as TDBForeignKey;
end;

function TDBTable.GetName: string;
begin
  Result := _Name.Value;
end;

function TDBTable.GetPrimaryKey: TDBPrimaryKey;
begin
  Result := _PrimaryKey.Value as TDBPrimaryKey;
end;

function TDBTable.IndexOfForeignKey(ForeignKey: TDBForeignKey): Integer;
begin
  Result := _ForeignKeys.IndexOf(ForeignKey);
end;

procedure TDBTable.InsertForeignKey(Index: Integer; ForeignKey: TDBForeignKey);
begin
  _ForeignKeys.Insert(Index, ForeignKey);
end;

function TDBTable.RemoveForeignKey(ForeignKey: TDBForeignKey): Integer;
begin
  Result := _ForeignKeys.Remove(ForeignKey);
end;

procedure TDBTable.SetForeignKeys(Index: Integer; Value: TDBForeignKey);
begin
  _ForeignKeys[Index] := Value;
end;

procedure TDBTable.SetName(const Value: string);
begin
  _Name.Value := Value;
end;

procedure TDBTable.SetPrimaryKey(Value: TDBPrimaryKey);
begin
  _PrimaryKey.Value := Value;
end;

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

function TCompany.RemoveSubsidiary(Subsidiary: TCompany): Integer;
begin
  Result := _Subsidiaries.Remove(Subsidiary);
end;

procedure TCompany.SetNoOfBranches(Value: Integer);
begin
  _NoOfBranches.Value := Value;
end;

procedure TCompany.SetSubsidiaries(Index: Integer; Value: TCompany);
begin
  _Subsidiaries[Index] := Value;
end;

function TContact.AddExternalPart(ExternalPart: TExternalPhone): Integer;
begin
  Result := _ExternalPhones.Add(ExternalPart);
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

procedure TContact.ClearExternalPhones;
begin
  _ExternalPhones.Clear;
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
  _ExternalPhones.Delete(Index);
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
  Result := _ExternalPhones.Count;
end;

function TContact.GetExternalPhones(Index: Integer): TExternalPhone;
begin
  Result := _ExternalPhones[Index] as TExternalPhone;
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

function TContact.GeTExternalAddress: TExternalAddress;
begin
  Result := _ExternalAddress.Value as TExternalAddress;
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

function TContact.IndexOfExternalPart(ExternalPart: TExternalPhone): Integer;
begin
  Result := _ExternalPhones.IndexOf(ExternalPart);
end;

function TContact.IndexOfPhone(Phone: TPhone): Integer;
begin
  Result := _Phones.IndexOf(Phone);
end;

function TContact.IndexOfProject(Project: TProject): Integer;
begin
  Result := _Projects.IndexOf(Project);
end;

procedure TContact.InsertExternalPart(Index: Integer; ExternalPart: TExternalPhone);
begin
  _ExternalPhones.Insert(Index, ExternalPart);
end;

procedure TContact.InsertPhone(Index: Integer; Phone: TPhone);
begin
  _Phones.Insert(Index, Phone);
end;

procedure TContact.InsertProject(Index: Integer; Project: TProject);
begin
  _Projects.Insert(Index, Project);
end;

function TContact.RemoveExternalPart(ExternalPart: TExternalPhone): Integer;
begin
  Result := _ExternalPhones.Remove(ExternalPart);
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

procedure TContact.SetExternalPhones(Index: Integer; Value: TExternalPhone);
begin
  _ExternalPhones[Index] := Value;
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

procedure TContact.SeTExternalAddress(Value: TExternalAddress);
begin
  _ExternalAddress.Value := Value;
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

function TCompany.AddSubsidiary(Subsidiary: TCompany): Integer;
begin
  Result := _Subsidiaries.Add(Subsidiary);
end;

procedure TCompany.ClearEmployees;
begin
  _Employees.Clear;
end;

procedure TCompany.ClearSubsidiaries;
begin
  _Subsidiaries.Clear;
end;

procedure TCompany.DeleteEmployee(Index: Integer);
begin
  _Employees.Delete(Index);
end;

procedure TCompany.DeleteSubsidiary(Index: Integer);
begin
  _Subsidiaries.Delete(Index);
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

function TCompany.GetSubsidiaries(Index: Integer): TCompany;
begin
  Result := _Subsidiaries[Index] as TCompany;
end;

function TCompany.GetSubsidiaryCount: Integer;
begin
  Result := _Subsidiaries.Count;
end;

function TCompany.IndexOfEmployee(Employee: TPerson): Integer;
begin
  Result := _Employees.IndexOf(Employee);
end;

function TCompany.IndexOfSubsidiary(Subsidiary: TCompany): Integer;
begin
  Result := _Subsidiaries.IndexOf(Subsidiary);
end;

procedure TCompany.InsertEmployee(Index: Integer; Employee: TPerson);
begin
  _Employees.Insert(Index, Employee);
end;

procedure TCompany.InsertSubsidiary(Index: Integer; Subsidiary: TCompany);
begin
  _Subsidiaries.Insert(Index, Subsidiary);
end;

function TCompany.RemoveEmployee(Employee: TPerson): Integer;
begin
  Result := _Employees.Remove(Employee);
end;

{ TExternalAddress }

function TExternalAddress.GetCategory: TCategory;
begin
  Result := _Category.Value as TCategory;
end;

function TExternalAddress.GetName: string;
begin
  Result := _Name.Value;
end;

function TExternalAddress.GetSite_Contact: TPerson;
begin
  Result := _Site_Contact.Value as TPerson;
end;

procedure TExternalAddress.SetCategory(Value: TCategory);
begin
  _Category.Value := Value;
end;

procedure TExternalAddress.SetName(const Value: string);
begin
  _Name.Value := Value;
end;

procedure TExternalAddress.SetSite_Contact(Value: TPerson);
begin
  _Site_Contact.Value := Value;
end;

{ TExternalPhone }

function TExternalPhone.GetName: string;
begin
  Result := _Name.Value;
end;

function TExternalPhone.GetNumber: string;
begin
  Result := _Number.Value;
end;

procedure TExternalPhone.SetName(const Value: string);
begin
  _Name.Value := Value;
end;

{ TDBField }

function TDBField.GetName: string;
begin
  Result := _Name.Value;
end;

function TDBField.GetTable: TDBTable;
begin
  Result := _Table.Value as TDBTable;
end;

procedure TDBField.SetName(const Value: string);
begin
  _Name.Value := Value;
end;

procedure TDBField.SetTable(Value: TDBTable);
begin
  _Table.Value := Value;
end;

{ TDBPrimaryKey }

procedure TDBForeignKey.SetName(const Value: string);
begin
  _Name.Value := Value;
end;

function TDBPrimaryKey.AddField(Field: TDBField): Integer;
begin
  Result := _Fields.Add(Field);
end;

procedure TDBPrimaryKey.ClearFields;
begin
  _Fields.Clear;
end;

procedure TDBPrimaryKey.DeleteField(Index: Integer);
begin
  _Fields.Delete(Index);
end;

function TDBPrimaryKey.GetFieldCount: Integer;
begin
  Result := _Fields.Count;
end;

function TDBPrimaryKey.GetFields(Index: Integer): TDBField;
begin
  Result := _Fields[Index] as TDBField;
end;

function TDBPrimaryKey.GetName: string;
begin
  Result := _Name.Value;
end;

function TDBPrimaryKey.IndexOfField(Field: TDBField): Integer;
begin
  Result := _Fields.IndexOf(Field);
end;

procedure TDBPrimaryKey.InsertField(Index: Integer; Field: TDBField);
begin
  _Fields.Insert(Index, Field);
end;

function TDBPrimaryKey.RemoveField(Field: TDBField): Integer;
begin
  Result := _Fields.Remove(Field);
end;

procedure TDBPrimaryKey.SetFields(Index: Integer; Value: TDBField);
begin
  _Fields[Index] := Value;
end;

{ TDBFieldPair }

function TDBFieldPair.GetField: TDBField;
begin
  Result := _Field.Value as TDBField;
end;

function TDBFieldPair.GetForeignField: TDBField;
begin
  Result := _ForeignField.Value as TDBField;
end;

procedure TDBFieldPair.SetField(Value: TDBField);
begin
  _Field.Value := Value;
end;

procedure TDBFieldPair.SetForeignField(Value: TDBField);
begin
  _ForeignField.Value := Value;
end;

{ TDBForeignKey }

function TDBForeignKey.AddFieldPair(FieldPair: TDBFieldPair): Integer;
begin
  Result := _FieldPairs.Add(FieldPair);
end;

procedure TDBForeignKey.ClearFieldPairs;
begin
  _FieldPairs.Clear;
end;

procedure TDBForeignKey.DeleteFieldPair(Index: Integer);
begin
  _FieldPairs.Delete(Index);
end;

function TDBForeignKey.GetFieldPairCount: Integer;
begin
  Result := _FieldPairs.Count;
end;

function TDBForeignKey.GetFieldPairs(Index: Integer): TDBFieldPair;
begin
  Result := _FieldPairs[Index] as TDBFieldPair;
end;

function TDBForeignKey.GetName: string;
begin
  Result := _Name.Value;
end;

function TDBForeignKey.IndexOfFieldPair(FieldPair: TDBFieldPair): Integer;
begin
  Result := _FieldPairs.IndexOf(FieldPair);
end;

procedure TDBForeignKey.InsertFieldPair(Index: Integer; FieldPair: TDBFieldPair);
begin
  _FieldPairs.Insert(Index, FieldPair);
end;

function TDBForeignKey.RemoveFieldPair(FieldPair: TDBFieldPair): Integer;
begin
  Result := _FieldPairs.Remove(FieldPair);
end;

procedure TDBForeignKey.SetFieldPairs(Index: Integer; Value: TDBFieldPair);
begin
  _FieldPairs[Index] := Value;
end;

initialization
  InstantRegisterClasses([
    TAddress,
    TCategory,
    TCompany,
    TContact,
    TContactFilter,
    TCountry,
    TDBField,
    TDBFieldPair,
    TDBForeignKey,
    TDBPrimaryKey,
    TDBTable,
    TEmail,
    TExternalAddress,
    TExternalPhone,
    TPerson,
    TPhone,
    TProject
  ]);

end.
