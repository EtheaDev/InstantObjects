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
  InstantPersistence, InstantTypes
  {$IFDEF DELPHI_NEON}
  , Neon.Core.Types
  , Neon.Core.Nullables
  , Neon.Core.Attributes
  {$ENDIF}
  ;

type
  TAddress = class;
  TCategory = class;
  TCompany = class;
  TContact = class;
  TContactFilter = class;
  TCountry = class;
  TEmail = class;
  TExternalAddress = class;
  TExternalPhone = class;
  TPerson = class;
  TPhone = class;
  TProject = class;
  TProjectBox = class;
  TProjectItem = class;
  TProjectItems = class;
  TSampleClass = class;

  TAddress = class(TInstantObject)
  {IOMETADATA City: String(30) index;
    Country: Reference(TCountry);
    State: String(4);
    Street: Memo;
    Zip: String(10); }
    _City: TInstantString;
    [NeonInclude, NeonProperty('Country')]
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
    [NeonIgnore]
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
    [NeonInclude, NeonProperty('Category')]
    _Category: TInstantReference;
    _City: TInstantString;
    [NeonInclude, NeonProperty('ExternalPhones')]
    _ExternalPhones: TInstantParts;
    _Name: TInstantString;
    _ExternalAddress: TInstantPart;
    [NeonInclude, NeonProperty('Phones')]
    _Phones: TInstantParts;
    [NeonInclude, NeonProperty('Country')]
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
    [NeonIgnore]
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
    Salary: Currency valid ',.0..9€';
    Employed: Boolean;
    AL_hours: Float;
    EmploymentDate: Date;
    BirthTime: Time; }
    _AL_hours: TInstantFloat;
    _BirthDate: TInstantDateTime;
    _BirthTime: TInstantTime;
    [NeonInclude, NeonProperty('Emails')]
    _Emails: TInstantParts;
    _Employed: TInstantBoolean;
    [NeonInclude, NeonProperty('Employer')]
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
    [NeonIgnore]
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
    [NeonInclude, NeonProperty('Employees')]
    _Employees: TInstantReferences;
    _NoOfBranches: TInstantInteger;
    [NeonInclude, NeonProperty('Subsidiaries')]
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
    Participants: References(TContact) external 'Project_Participants';
    Items: Part(TProjectItems); }
    [NeonInclude, NeonProperty('Addresses')]
    _Addresses: TInstantParts;
    _Items: TInstantPart;
    [NeonInclude, NeonProperty('Manager')]
    _Manager: TInstantReference;
    _Name: TInstantString;
    [NeonInclude, NeonProperty('Participants')]
    _Participants: TInstantReferences;
    [NeonInclude, NeonProperty('Subprojects')]
    _SubProjects: TInstantParts;
  private
    function GetAddressCount: Integer;
    function GetAddresses(Index: Integer): TExternalAddress;
    function GetItems: TProjectItems;
    function GetManager: TContact;
    function GetName: string;
    function GetParticipantCount: Integer;
    function GetParticipants(Index: Integer): TContact;
    function GetSubProjectCount: Integer;
    function GetSubProjects(Index: Integer): TProject;
    procedure SetAddresses(Index: Integer; Value: TExternalAddress);
    procedure SetItems(Value: TProjectItems);
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
    property Items: TProjectItems read GetItems write SetItems;
    [NeonIgnore]
    property Manager: TContact read GetManager write SetManager;
    property Name: string read GetName write SetName;
  end;

  TExternalAddress = class(TInstantObject)
  {IOMETADATA stored 'ExternalAddresses';
    Name: String(30);
    Category: Reference(TCategory);
    Site_Contact: Reference(TPerson); }
    [NeonInclude, NeonProperty('Category')]
    _Category: TInstantReference;
    _Name: TInstantString;
    [NeonInclude, NeonProperty('Site_Contact')]
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
    [NeonIgnore]
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

  TProjectBox = class(TInstantObject)
  {IOMETADATA stored;
    Project: Part(TProject);
    RelatedProjectBoxes: References(TProjectBox); }
    _Project: TInstantPart;
    [NeonInclude, NeonProperty('RelatedProjectBoxes')]
    _RelatedProjectBoxes: TInstantReferences;
  private
    function GetProject: TProject;
    function GetRelatedProjectBoxCount: Integer;
    function GetRelatedProjectBoxes(Index: Integer): TProjectBox;
    procedure SetProject(Value: TProject);
    procedure SetRelatedProjectBoxes(Index: Integer; Value: TProjectBox);
  public
    function AddRelatedProjectBox(RelatedProjectBox: TProjectBox): Integer;
    procedure ClearRelatedProjectBoxes;
    procedure DeleteRelatedProjectBox(Index: Integer);
    function IndexOfRelatedProjectBox(RelatedProjectBox: TProjectBox): Integer;
    procedure InsertRelatedProjectBox(Index: Integer; RelatedProjectBox: TProjectBox);
    function RemoveRelatedProjectBox(RelatedProjectBox: TProjectBox): Integer;
    property RelatedProjectBoxCount: Integer read GetRelatedProjectBoxCount;
    property RelatedProjectBoxes[Index: Integer]: TProjectBox read GetRelatedProjectBoxes write SetRelatedProjectBoxes;
  published
    property Project: TProject read GetProject write SetProject;
  end;

  TProjectItem = class(TInstantObject)
  {IOMETADATA stored;
    Description: String(50);
    Country: Reference(TCountry); }
    [NeonInclude, NeonProperty('Country')]
    _Country: TInstantReference;
    _Description: TInstantString;
  private
    function GetCountry: TCountry;
    function GetDescription: string;
    procedure SetCountry(Value: TCountry);
    procedure SetDescription(const Value: string);
  published
    [NeonIgnore]
    property Country: TCountry read GetCountry write SetCountry;
    property Description: string read GetDescription write SetDescription;
  end;

  TProjectItems = class(TInstantObject)
  {IOMETADATA stored;
    Items: Parts(TProjectItem); }
    [NeonInclude, NeonProperty('Items')]
    _Items: TInstantParts;
  private
    function GetItemCount: Integer;
    function GetItems(Index: Integer): TProjectItem;
    procedure SetItems(Index: Integer; Value: TProjectItem);
  public
    function AddItem(Item: TProjectItem): Integer;
    procedure ClearItems;
    procedure DeleteItem(Index: Integer);
    function IndexOfItem(Item: TProjectItem): Integer;
    procedure InsertItem(Index: Integer; Item: TProjectItem);
    function RemoveItem(Item: TProjectItem): Integer;
    property ItemCount: Integer read GetItemCount;
    property Items[Index: Integer]: TProjectItem read GetItems write SetItems;
  end;

  TSampleClass = class(TInstantObject)
  {IOMETADATA stored;
    CharacterListAttribute: String(20) stored 'CHARLISTATTR' width 20 required;
    CharacterFileName: String(50) stored 'CHARFILENAME' width 50;
    CharacterDirAttribute: String(50) stored 'CHARDIRATTR' width 50;
    TimeAttribute: DateTime stored 'TIMEATTRIB' mask '!99:99;1; ' width 8 usenull;
    DateAttribute: DateTime stored 'DATEATTR' mask '!99/99/9999;1; ' width 10 usenull;
    DateTimeAttribute: DateTime stored 'DATETIMEATTR' mask '!99/99/9999 99:99;1; ' width 19 usenull;
    MemoAttribute: Memo stored 'MEMOATTR' width 100;
    ImageLinkAttribute: String(100) stored 'IMAGELINKATTR' width 100;
    BLobAttribute: Blob stored 'BLOBATTR' width 100;
    MemoHTMLAttribute: Memo stored 'MEMOHTMLATTR' width 200;
    SmallIntegerAttribute: Integer stored 'SMALLINTATTR' width 3;
    IntegerAttribute: Integer stored 'INTEGERATTR' width 10;
    BooleanAttribute: Boolean stored 'BOOLEANATTR' width 5 default "False" required;
    FloatingPointAttribute: Float stored 'FLOATATTR' width 14;
    CurrencyAttribute: Currency stored 'CURRENCYATTR' width 12;
    ColorAttribute: Integer stored 'COLORATTR' width 10;
    CalculatedAttribute: String(20) stored 'CALCATTR' width 20;
    MultiReference: String(200) stored 'MULTIREF' width 10 required;
    MultiLangDesc: String(100) stored 'MULTILANGDESC' width 100;
    MultiLangMemo: Memo stored 'MULTILANGMEMO'; }
    _CharacterListAttribute: TInstantString;
    _CharacterFileName: TInstantString;
    _CharacterDirAttribute: TInstantString;
    _TimeAttribute: TInstantDateTime;
    _DateAttribute: TInstantDateTime;
    _DateTimeAttribute: TInstantDateTime;
    _MemoAttribute: TInstantMemo;
    _ImageLinkAttribute: TInstantString;
    _BLobAttribute: TInstantBlob;
    _MemoHTMLAttribute: TInstantMemo;
    _SmallIntegerAttribute: TInstantInteger;
    _IntegerAttribute: TInstantInteger;
    _BooleanAttribute: TInstantBoolean;
    _FloatingPointAttribute: TInstantFloat;
    _CurrencyAttribute: TInstantCurrency;
    _ColorAttribute: TInstantInteger;
    _CalculatedAttribute: TInstantString;
    _MultiReference: TInstantString;
    _MultiLangDesc: TInstantString;
    _MultiLangMemo: TInstantMemo;
  private
  protected
    function GetCharacterListAttribute: string; virtual;
    function GetCharacterFileName: string; virtual;
    function GetCharacterDirAttribute: string; virtual;
    function GetTimeAttribute: TDateTime; virtual;
    function GetDateAttribute: TDateTime; virtual;
    function GetDateTimeAttribute: TDateTime; virtual;
    function GetMemoAttribute: string; virtual;
    function GetImageLinkAttribute: string; virtual;
    function GetBLobAttribute: string; virtual;
    function GetMemoHTMLAttribute: string; virtual;
    function GetSmallIntegerAttribute: Integer; virtual;
    function GetIntegerAttribute: Integer; virtual;
    function GetBooleanAttribute: Boolean; virtual;
    function GetFloatingPointAttribute: Double; virtual;
    function GetCurrencyAttribute: Currency; virtual;
    function GetColorAttribute: Integer; virtual;
    function GetCalculatedAttribute: string; virtual;
    function GetMultiReference: string; virtual;
    function GetMultiLangDesc: string; virtual;
    function GetMultiLangMemo: string; virtual;
    procedure SetCharacterListAttribute(const Value: string); virtual;
    procedure SetCharacterFileName(const Value: string); virtual;
    procedure SetCharacterDirAttribute(const Value: string); virtual;
    procedure SetTimeAttribute(Value: TDateTime); virtual;
    procedure SetDateAttribute(Value: TDateTime); virtual;
    procedure SetDateTimeAttribute(Value: TDateTime); virtual;
    procedure SetMemoAttribute(const Value: string); virtual;
    procedure SetImageLinkAttribute(const Value: string); virtual;
    procedure SetBLobAttribute(const Value: string); virtual;
    procedure SetMemoHTMLAttribute(const Value: string); virtual;
    procedure SetSmallIntegerAttribute(Value: Integer); virtual;
    procedure SetIntegerAttribute(Value: Integer); virtual;
    procedure SetBooleanAttribute(Value: Boolean); virtual;
    procedure SetFloatingPointAttribute(Value: Double); virtual;
    procedure SetCurrencyAttribute(Value: Currency); virtual;
    procedure SetColorAttribute(Value: Integer); virtual;
    procedure SetMultiReference(const Value: string); virtual;
    procedure SetMultiLangDesc(const Value: string); virtual;
    procedure SetMultiLangMemo(const Value: string); virtual;
    procedure AfterCreate; override;
  published
    property CharacterListAttribute: string read GetCharacterListAttribute write SetCharacterListAttribute;
    property CharacterFileName: string read GetCharacterFileName write SetCharacterFileName;
    property CharacterDirAttribute: string read GetCharacterDirAttribute write SetCharacterDirAttribute;
    property TimeAttribute: TDateTime read GetTimeAttribute write SetTimeAttribute;
    property DateAttribute: TDateTime read GetDateAttribute write SetDateAttribute;
    property DateTimeAttribute: TDateTime read GetDateTimeAttribute write SetDateTimeAttribute;
    property MemoAttribute: string read GetMemoAttribute write SetMemoAttribute;
    property ImageLinkAttribute: string read GetImageLinkAttribute write SetImageLinkAttribute;
    property BLobAttribute: string read GetBLobAttribute write SetBLobAttribute;
    property MemoHTMLAttribute: string read GetMemoHTMLAttribute write SetMemoHTMLAttribute;
    property SmallIntegerAttribute: Integer read GetSmallIntegerAttribute write SetSmallIntegerAttribute;
    property IntegerAttribute: Integer read GetIntegerAttribute write SetIntegerAttribute;
    property BooleanAttribute: Boolean read GetBooleanAttribute write SetBooleanAttribute;
    property FloatingPointAttribute: Double read GetFloatingPointAttribute write SetFloatingPointAttribute;
    property CurrencyAttribute: Currency read GetCurrencyAttribute write SetCurrencyAttribute;
    property ColorAttribute: Integer read GetColorAttribute write SetColorAttribute;
    property CalculatedAttribute: string read GetCalculatedAttribute;
    property MultiReference: string read GetMultiReference write SetMultiReference;
    property MultiLangDesc: string read GetMultiLangDesc write SetMultiLangDesc;
    property MultiLangMemo: string read GetMultiLangMemo write SetMultiLangMemo;
  end;


var
  TestUseUnicode: Boolean;

implementation

uses
  SysUtils, InstantUtils, InstantMetadata;

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

function TProject.GetItems: TProjectItems;
begin
  Result := _Items.Value as TProjectItems;
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

procedure TProject.SetItems(Value: TProjectItems);
begin
  _Items.Value := Value;
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
  Category := TCategory.Retrieve('CAT000', False, False, Connector);
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

{ TProjectBox }

function TProjectBox.AddRelatedProjectBox(RelatedProjectBox: TProjectBox): Integer;
begin
  Result := _RelatedProjectBoxes.Add(RelatedProjectBox);
end;

procedure TProjectBox.ClearRelatedProjectBoxes;
begin
  _RelatedProjectBoxes.Clear;
end;

procedure TProjectBox.DeleteRelatedProjectBox(Index: Integer);
begin
  _RelatedProjectBoxes.Delete(Index);
end;

function TProjectBox.GetProject: TProject;
begin
  Result := _Project.Value as TProject;
end;

function TProjectBox.GetRelatedProjectBoxCount: Integer;
begin
  Result := _RelatedProjectBoxes.Count;
end;

function TProjectBox.GetRelatedProjectBoxes(Index: Integer): TProjectBox;
begin
  Result := _RelatedProjectBoxes[Index] as TProjectBox;
end;

function TProjectBox.IndexOfRelatedProjectBox(RelatedProjectBox: TProjectBox): Integer;
begin
  Result := _RelatedProjectBoxes.IndexOf(RelatedProjectBox);
end;

procedure TProjectBox.InsertRelatedProjectBox(Index: Integer; RelatedProjectBox: TProjectBox);
begin
  _RelatedProjectBoxes.Insert(Index, RelatedProjectBox);
end;

function TProjectBox.RemoveRelatedProjectBox(RelatedProjectBox: TProjectBox): Integer;
begin
  Result := _RelatedProjectBoxes.Remove(RelatedProjectBox);
end;

procedure TProjectBox.SetProject(Value: TProject);
begin
  _Project.Value := Value;
end;

{ TProjectItem }

procedure TProjectBox.SetRelatedProjectBoxes(Index: Integer; Value: TProjectBox);
begin
  _RelatedProjectBoxes[Index] := Value;
end;

function TProjectItem.GetCountry: TCountry;
begin
  Result := _Country.Value as TCountry;
end;

function TProjectItem.GetDescription: string;
begin
  Result := _Description.Value;
end;

procedure TProjectItem.SetCountry(Value: TCountry);
begin
  _Country.Value := Value;
end;

procedure TProjectItem.SetDescription(const Value: string);
begin
  _Description.Value := Value;
end;

{ TProjectItems }

function TProjectItems.AddItem(Item: TProjectItem): Integer;
begin
  Result := _Items.Add(Item);
end;

procedure TProjectItems.ClearItems;
begin
  _Items.Clear;
end;

procedure TProjectItems.DeleteItem(Index: Integer);
begin
  _Items.Delete(Index);
end;

function TProjectItems.GetItemCount: Integer;
begin
  Result := _Items.Count;
end;

function TProjectItems.GetItems(Index: Integer): TProjectItem;
begin
  Result := _Items[Index] as TProjectItem;
end;

function TProjectItems.IndexOfItem(Item: TProjectItem): Integer;
begin
  Result := _Items.IndexOf(Item);
end;

procedure TProjectItems.InsertItem(Index: Integer; Item: TProjectItem);
begin
  _Items.Insert(Index, Item);
end;

function TProjectItems.RemoveItem(Item: TProjectItem): Integer;
begin
  Result := _Items.Remove(Item);
end;

procedure TProjectItems.SetItems(Index: Integer; Value: TProjectItem);
begin
  _Items[Index] := Value;
end;

{ TSampleClass }
function TSampleClass.GetCharacterListAttribute: string;
begin
  Result := _CharacterListAttribute.Value;
end;
function TSampleClass.GetCharacterFileName: string;
begin
  Result := _CharacterFileName.Value;
end;
function TSampleClass.GetCharacterDirAttribute: string;
begin
  Result := _CharacterDirAttribute.Value;
end;
function TSampleClass.GetTimeAttribute: TDateTime;
begin
  Result := _TimeAttribute.Value;
end;
function TSampleClass.GetDateAttribute: TDateTime;
begin
  Result := _DateAttribute.Value;
end;
function TSampleClass.GetDateTimeAttribute: TDateTime;
begin
  Result := _DateTimeAttribute.Value;
end;
function TSampleClass.GetMemoAttribute: string;
begin
  Result := _MemoAttribute.Value;
end;
function TSampleClass.GetImageLinkAttribute: string;
begin
  Result := _ImageLinkAttribute.Value;
end;
procedure TSampleClass.AfterCreate;
begin
  inherited;
  Id := InstantGenerateId;
end;

function TSampleClass.GetBLobAttribute: string;
begin
  Result := _BLobAttribute.Value;
end;
function TSampleClass.GetMemoHTMLAttribute: string;
begin
  Result := _MemoHTMLAttribute.Value;
end;
function TSampleClass.GetSmallIntegerAttribute: Integer;
begin
  Result := _SmallIntegerAttribute.Value;
end;

function TSampleClass.GetIntegerAttribute: Integer;
begin
  Result := _IntegerAttribute.Value;
end;
function TSampleClass.GetBooleanAttribute: Boolean;
begin
  Result := _BooleanAttribute.Value;
end;
function TSampleClass.GetFloatingPointAttribute: Double;
begin
  Result := _FloatingPointAttribute.Value;
end;
function TSampleClass.GetCurrencyAttribute: Currency;
begin
  Result := _CurrencyAttribute.Value;
end;
function TSampleClass.GetColorAttribute: Integer;
begin
  Result := _ColorAttribute.Value;
end;
function TSampleClass.GetCalculatedAttribute: string;
begin
  Result := _CalculatedAttribute.Value;
end;
function TSampleClass.GetMultiReference: string;
begin
  Result := _MultiReference.Value;
end;
function TSampleClass.GetMultiLangDesc: string;
begin
  Result := _MultiLangDesc.Value;
end;
function TSampleClass.GetMultiLangMemo: string;
begin
  Result := _MultiLangMemo.Value;
end;

procedure TSampleClass.SetCharacterListAttribute(const Value: string);
begin
  _CharacterListAttribute.Value := Value;
end;
procedure TSampleClass.SetCharacterFileName(const Value: string);
begin
  _CharacterFileName.Value := Value;
end;
procedure TSampleClass.SetCharacterDirAttribute(const Value: string);
begin
  _CharacterDirAttribute.Value := Value;
end;
procedure TSampleClass.SetTimeAttribute(Value: TDateTime);
begin
  _TimeAttribute.Value := Value;
end;
procedure TSampleClass.SetDateAttribute(Value: TDateTime);
begin
  _DateAttribute.Value := Value;
end;
procedure TSampleClass.SetDateTimeAttribute(Value: TDateTime);
begin
  _DateTimeAttribute.Value := Value;
end;
procedure TSampleClass.SetMemoAttribute(const Value: string);
begin
  _MemoAttribute.Value := Value;
end;
procedure TSampleClass.SetImageLinkAttribute(const Value: string);
begin
  _ImageLinkAttribute.Value := Value;
end;
procedure TSampleClass.SetBLobAttribute(const Value: string);
begin
  _BLobAttribute.Value := Value;
end;
procedure TSampleClass.SetMemoHTMLAttribute(const Value: string);
begin
  _MemoHTMLAttribute.Value := Value;
end;
procedure TSampleClass.SetSmallIntegerAttribute(Value: Integer);
begin
  _SmallIntegerAttribute.Value := Value;
end;

procedure TSampleClass.SetIntegerAttribute(Value: Integer);
begin
  _IntegerAttribute.Value := Value;
end;
procedure TSampleClass.SetBooleanAttribute(Value: Boolean);
begin
  _BooleanAttribute.Value := Value;
end;
procedure TSampleClass.SetFloatingPointAttribute(Value: Double);
begin
  _FloatingPointAttribute.Value := Value;
end;
procedure TSampleClass.SetCurrencyAttribute(Value: Currency);
begin
  _CurrencyAttribute.Value := Value;
end;
procedure TSampleClass.SetColorAttribute(Value: Integer);
begin
  _ColorAttribute.Value := Value;
end;
procedure TSampleClass.SetMultiReference(const Value: string);
begin
  _MultiReference.Value := Value;
end;
procedure TSampleClass.SetMultiLangDesc(const Value: string);
begin
  _MultiLangDesc.Value := Value;
end;
procedure TSampleClass.SetMultiLangMemo(const Value: string);
begin
  _MultiLangMemo.Value := Value;
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
    TExternalAddress,
    TExternalPhone,
    TPerson,
    TPhone,
    TProject,
    TProjectBox,
    TProjectItem,
    TProjectItems,
    TSampleClass
  ]);

  TestUseUnicode := True;

end.
