(*
 *   PrimerCross demo:
 *   Model.pas with "external storage" of Part and Parts
 *
 *)
unit Model;

{$I '..\..\Source\InstantDefines.inc'}

interface

uses
  InstantPersistence
  , InstantTypes
  , InstantClasses
  , System.Classes
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
  TPerson = class;
  TPhone = class;
  TProfile = class;
  TUser = class;

  TPhoneType = (ptHome, ptMobile, ptOffice, ptOther);


  TProfile = class(TInstantObject)
  {IOMETADATA stored 'APP_PROFILES';
    AccessRoles: String(100) stored 'ACCESS_ROLES';}
    _AccessRoles: TInstantString;
  private
    function GetAccessRoles: string;
    procedure SetAccessRoles(const Value: string);
  protected
  public
    {$IFDEF WINLINUX64}
    class function Retrieve(const AObjectId: string; CreateIfMissing: Boolean = False;
      ARefresh: Boolean = False; AConnector: TComponent = nil;
      const AObjectData: TInstantAbstractObjectData = nil): TProfile; reintroduce; virtual;
    {$ENDIF}
  published
    property AccessRoles: string read GetAccessRoles write SetAccessRoles;
  end;

  TUser = class(TInstantObject)
  {IOMETADATA stored 'APP_USERS';
    Password: String(50) stored 'USER_PASSWORD' width 40;
    Language: String(2) stored 'USER_LANGUAGE' required;
    Profile: Reference(TProfile) stored 'USER_PROFILE' required;
    Administrator: Boolean stored 'IS_ADMINISTRATOR' width 4;
    System: Boolean stored 'IS_SYSTEM' width 4;
    LastName: String(40) stored 'LAST_NAME' width 40;
    FirstName: String(40) stored 'FIRST_NAME' width 40;
    Email: String(100) stored 'EMAIL_ADDRESS' width 50;
    PhoneNumber: String(15) stored 'PHONE_NUMBER' width 15;
    AccessDenied: Boolean stored 'ACCESS_DENIED' width 4;
    MustChangePassword: Boolean stored 'MUST_CHANGE_PASSWORD' width 4;
    PrivacyConfirm: Boolean stored 'PRIVACY_CONFIRM' width 4;
    CreationDate: DateTime stored 'CREATION_DATE' mask '!99/99/9999 99:99;1; ' width 16 usenull;
    IPAddress: String(50) stored 'IP_ADDRESS' width 50; }
    _Password: TInstantString;
    [NeonInclude, NeonProperty('Profile')]
    _Language: TInstantString;
    _Profile: TInstantReference;
    _Administrator: TInstantBoolean;
    _System: TInstantBoolean;
    _LastName: TInstantString;
    _FirstName: TInstantString;
    _Email: TInstantString;
    _PhoneNumber: TInstantString;
    _AccessDenied: TInstantBoolean;
    _MustChangePassword: TInstantBoolean;
    _PrivacyConfirm: TInstantBoolean;
    _CreationDate: TInstantDateTime;
    _IPAddress: TInstantString;
  private
  protected
    function GetPassword: string; virtual;
    function GetLanguage: string; virtual;
    function GetProfile: TProfile; virtual;
    function GetAdministrator: Boolean; virtual;
    function GetSystem: Boolean; virtual;
    function GetLastName: string; virtual;
    function GetFirstName: string; virtual;
    function GetEmail: string; virtual;
    function GetPhoneNumber: string; virtual;
    function GetAccessDenied: Boolean; virtual;
    function GetMustChangePassword: Boolean; virtual;
    function GetPrivacyConfirm: Boolean; virtual;
    function GetCreationDate: TDateTime; virtual;
    function GetIPAddress: string; virtual;
    procedure SetPassword(const Value: string); virtual;
    procedure SetLanguage(const AValue: string); virtual;
    procedure SetProfile(Value: TProfile); virtual;
    procedure SetAdministrator(Value: Boolean); virtual;
    procedure SetSystem(Value: Boolean); virtual;
    procedure SetLastName(const Value: string); virtual;
    procedure SetFirstName(const Value: string); virtual;
    procedure SetEmail(const Value: string); virtual;
    procedure SetPhoneNumber(const Value: string); virtual;
    procedure SetAccessDenied(Value: Boolean); virtual;
    procedure SetMustChangePassword(Value: Boolean); virtual;
    procedure SetPrivacyConfirm(Value: Boolean); virtual;
    procedure SetCreationDate(Value: TDateTime); virtual;
    procedure SetIPAddress(const Value: string); virtual;

    procedure AfterCreate; override;
  public
    {$IFDEF WINLINUX64}
    class function Retrieve(const AObjectId: string; CreateIfMissing: Boolean = False;
      ARefresh: Boolean = False; AConnector: TComponent = nil;
      const AObjectData: TInstantAbstractObjectData = nil): TUser; reintroduce; virtual;
    {$ENDIF}
  published
    property Password: string read GetPassword write SetPassword;
    property Language: string read GetLanguage write SetLanguage;
    [NeonIgnore]
    property Profile: TProfile read GetProfile write SetProfile;
    property Administrator: Boolean read GetAdministrator write SetAdministrator;
    property System: Boolean read GetSystem write SetSystem;
    property LastName: string read GetLastName write SetLastName;
    property FirstName: string read GetFirstName write SetFirstName;
    property Email: string read GetEmail write SetEmail;
    property PhoneNumber: string read GetPhoneNumber write SetPhoneNumber;
    property AccessDenied: Boolean read GetAccessDenied write SetAccessDenied;
    property MustChangePassword: Boolean read GetMustChangePassword write SetMustChangePassword;
    property PrivacyConfirm: Boolean read GetPrivacyConfirm write SetPrivacyConfirm;
    property CreationDate: TDateTime read GetCreationDate write SetCreationDate;
    property IPAddress: string read GetIPAddress write SetIPAddress;
  end;

  TAddress = class(TInstantObject)
    {IOMETADATA stored;
    City: String(30) index;
    Country: Reference(TCountry);
    State: String(4);
    Street: Memo;
    Zip: String(10); }
    _City: TInstantString;
    {$IFDEF DELPHI_NEON}[NeonInclude, NeonProperty('Country')]{$ENDIF}
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
  public
    {$IFDEF WINLINUX64}
    class function Retrieve(const AObjectId: string; CreateIfMissing: Boolean = False;
      ARefresh: Boolean = False; AConnector: TComponent = nil;
      const AObjectData: TInstantAbstractObjectData = nil): TAddress; reintroduce; virtual;
    {$ENDIF}
  published
    property City: string read GetCity write SetCity;
    {$IFDEF DELPHI_NEON}[NeonIgnore]{$ENDIF}
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
  public
    {$IFDEF WINLINUX64}
    class function Retrieve(const AObjectId: string; CreateIfMissing: Boolean = False;
      ARefresh: Boolean = False; AConnector: TComponent = nil;
      const AObjectData: TInstantAbstractObjectData = nil): TCountry; reintroduce; virtual;
    {$ENDIF}
  published
    property Id;
    property Name: string read GetName write SetName;
  end;

  TPhone = class(TInstantObject)
  {IOMETADATA stored;
    Name: String(20);
    Number: String(20) mask '(000) 000-0000;0;_';
    PhoneType: Enum(TPhoneType); }
    _Name: TInstantString;
    _Number: TInstantString;
    _PhoneType: TInstantEnum;
  private
    function GetName: string;
    function GetNumber: string;
    function GetPhoneType: TPhoneType;
    procedure SetName(const Value: string);
    procedure SetNumber(const Value: string);
    procedure SetPhoneType(Value: TPhoneType);
  public
    {$IFDEF WINLINUX64}
    class function Retrieve(const AObjectId: string; CreateIfMissing: Boolean = False;
      ARefresh: Boolean = False; AConnector: TComponent = nil;
      const AObjectData: TInstantAbstractObjectData = nil): TPhone; reintroduce; virtual;
    {$ENDIF}
  published
    property Name: string read GetName write SetName;
    property Number: string read GetNumber write SetNumber;
    property PhoneType: TPhoneType read GetPhoneType write SetPhoneType;
  end;

  TEmail = class(TInstantObject)
  {IOMETADATA stored;
    Address: String(100); }
    _Address: TInstantString;
  private
    function GetAddress: string;
    procedure SetAddress(const Value: string);
  public
    {$IFDEF WINLINUX64}
    class function Retrieve(const AObjectId: string; CreateIfMissing: Boolean = False;
      ARefresh: Boolean = False; AConnector: TComponent = nil;
      const AObjectData: TInstantAbstractObjectData = nil): TEmail; reintroduce; virtual;
    {$ENDIF}
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
  public
    {$IFDEF WINLINUX64}
    class function Retrieve(const AObjectId: string; CreateIfMissing: Boolean = False;
      ARefresh: Boolean = False; AConnector: TComponent = nil;
      const AObjectData: TInstantAbstractObjectData = nil): TCategory; reintroduce; virtual;
    {$ENDIF}
    constructor Create(AConnector: TInstantConnector = nil); override;
  published
    property Name: string read GetName write SetName;
  end;

  TContact = class(TInstantObject)
  {IOMETADATA stored;
    Address: Part(TAddress) external;
    Category: Reference(TCategory);
    City: String(30) index;
    Name: String(50) index;
    Phones: Parts(TPhone) external 'Contact_Phones'; }
    _Address: TInstantPart;
    {$IFDEF DELPHI_NEON}[NeonInclude, NeonProperty('Category')]{$ENDIF}
    _Category: TInstantReference;
    _City: TInstantString;
    _Name: TInstantString;
    {$IFDEF DELPHI_NEON}[NeonInclude, NeonProperty('Phones')]{$ENDIF}
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
    {$IFDEF WINLINUX64}
    class function Retrieve(const AObjectId: string; CreateIfMissing: Boolean = False;
      ARefresh: Boolean = False; AConnector: TComponent = nil;
      const AObjectData: TInstantAbstractObjectData = nil): TContact; reintroduce; virtual;
    {$ENDIF}
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
    {$IFDEF DELPHI_NEON}[NeonIgnore]{$ENDIF}
    property Category: TCategory read GetCategory write SetCategory;
    property City: string read GetCity write SetCity;
    property MainPhoneNumber: string read GetMainPhoneNumber write SetMainPhoneNumber;
    property Name: string read GetName write SetName;
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
    Emails: Parts(TEmail) external 'Person_Emails';
    Employer: Reference(TCompany);
    Picture: Graphic;
    Salary: Currency;
    BirthDate: Date;
    BirthTime: Time; }
    _BirthDate: TInstantDate;
    _BirthTime: TInstantTime;
    {$IFDEF DELPHI_NEON}[NeonInclude, NeonProperty('Emails')]{$ENDIF}
    _Emails: TInstantParts;
    {$IFDEF DELPHI_NEON}[NeonInclude, NeonProperty('Employer')]{$ENDIF}
    _Employer: TInstantReference;
    {$IFDEF DELPHI_NEON}[NeonInclude, NeonProperty('Picture')]{$ENDIF}
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
    {$IFDEF WINLINUX64}
    class function Retrieve(const AObjectId: string; CreateIfMissing: Boolean = False;
      ARefresh: Boolean = False; AConnector: TComponent = nil;
      const AObjectData: TInstantAbstractObjectData = nil): TPerson; reintroduce; virtual;
    {$ENDIF}
    procedure FreeInstance; override;
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
    [NeonInclude(IncludeIf.NotDefault)]
    property BirthDate: TDate read GetBirthDate write SetBirthDate;
    property BirthTime: TTime read GetBirthTime write SetBirthTime;
    {$IFDEF DELPHI_NEON}[NeonIgnore]{$ENDIF}
    property Employer: TCompany read GetEmployer;
    property MainEmailAddress: string read GetMainEmailAddress write SetMainEmailAddress;
    {$IFDEF DELPHI_NEON}[NeonIgnore]{$ENDIF}
    property Picture: string read GetPicture write SetPicture;
    property Salary: Currency read GetSalary write SetSalary;
  end;

  TCompany = class(TContact)
  {IOMETADATA stored;
    Employees: References(TPerson) external 'Company_Employees'; }
    {$IFDEF DELPHI_NEON}[NeonInclude, NeonProperty('Employees')]{$ENDIF}
    _Employees: TInstantReferences;
  private
    function GetEmployeeCount: Integer;
    function GetEmployees(Index: Integer): TPerson;
  public
    {$IFDEF WINLINUX64}
    class function Retrieve(const AObjectId: string; CreateIfMissing: Boolean = False;
      ARefresh: Boolean = False; AConnector: TComponent = nil;
      const AObjectData: TInstantAbstractObjectData = nil): TCompany; reintroduce; virtual;
    {$ENDIF}
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
  SysUtils, InstantUtils, InstantMetadata;

{ TProfile }
{$IFDEF WINLINUX64}
class function TProfile.Retrieve(const AObjectId: string; CreateIfMissing: Boolean = False;
  ARefresh: Boolean = False; AConnector: TComponent = nil;
  const AObjectData: TInstantAbstractObjectData = nil): TProfile;
begin
  Result := inherited Retrieve(AObjectId,
    CreateIfMissing, ARefresh, AConnector, AObjectData) as TProfile;
end;
{$ENDIF}

function TProfile.GetAccessRoles: string;
begin
  Result := _AccessRoles.Value;
end;
procedure TProfile.SetAccessRoles(const Value: string);
begin
  _AccessRoles.Value := Value;
end;

{ TUser }
{$IFDEF WINLINUX64}
class function TUser.Retrieve(const AObjectId: string; CreateIfMissing: Boolean = False;
  ARefresh: Boolean = False; AConnector: TComponent = nil;
  const AObjectData: TInstantAbstractObjectData = nil): TUser;
begin
  Result := inherited Retrieve(AObjectId,
    CreateIfMissing, ARefresh, AConnector, AObjectData) as TUser;
end;
{$ENDIF}

function TUser.GetPassword: string;
begin
  Result := _Password.Value;
end;
function TUser.GetProfile: TProfile;
begin
  Result := _Profile.Value as TProfile;
end;
function TUser.GetLanguage: string;
begin
  Result := _Language.Value;
end;
function TUser.GetAdministrator: Boolean;
begin
  Result := _Administrator.Value;
end;
function TUser.GetSystem: Boolean;
begin
  Result := _System.Value;
end;
function TUser.GetLastName: string;
begin
  Result := _LastName.Value;
end;
function TUser.GetFirstName: string;
begin
  Result := _FirstName.Value;
end;
function TUser.GetEmail: string;
begin
  Result := _Email.Value;
end;
function TUser.GetPhoneNumber: string;
begin
  Result := _PhoneNumber.Value;
end;
procedure TUser.AfterCreate;
begin
  inherited;
  CreationDate := Now;
  MustChangePassword := False;
end;

function TUser.GetAccessDenied: Boolean;
begin
  Result := _AccessDenied.Value;
end;
function TUser.GetMustChangePassword: Boolean;
begin
  Result := _MustChangePassword.Value;
end;
function TUser.GetPrivacyConfirm: Boolean;
begin
  Result := _PrivacyConfirm.Value;
end;
function TUser.GetCreationDate: TDateTime;
begin
  Result := _CreationDate.Value;
end;
function TUser.GetIPAddress: string;
begin
  Result := _IPAddress.Value;
end;
procedure TUser.SetPassword(const Value: string);
begin
  _Password.Value := Value;
end;
procedure TUser.SetProfile(Value: TProfile);
begin
  _Profile.Value := Value;
  if Assigned(Value) then
  begin
    Administrator := Value.Id = 'ADMIN';
    System := Value.Id = 'SYSTEM';
  end;
end;
procedure TUser.SetLanguage(const AValue: string);
begin
  _Language.Value := AValue;
end;
procedure TUser.SetAdministrator(Value: Boolean);
begin
  _Administrator.Value := Value;
end;
procedure TUser.SetSystem(Value: Boolean);
begin
  _System.Value := Value;
end;
procedure TUser.SetLastName(const Value: string);
begin
  _LastName.Value := Value;
end;
procedure TUser.SetFirstName(const Value: string);
begin
  _FirstName.Value := Value;
end;
procedure TUser.SetEmail(const Value: string);
begin
  _Email.Value := Value;
end;
procedure TUser.SetPhoneNumber(const Value: string);
begin
  _PhoneNumber.Value := Value;
end;
procedure TUser.SetAccessDenied(Value: Boolean);
begin
  _AccessDenied.Value := Value;
end;
procedure TUser.SetMustChangePassword(Value: Boolean);
begin
  _MustChangePassword.Value := Value;
end;
procedure TUser.SetPrivacyConfirm(Value: Boolean);
begin
  _PrivacyConfirm.Value := Value;
end;
procedure TUser.SetCreationDate(Value: TDateTime);
begin
  _CreationDate.Value := Value;
end;

procedure TUser.SetIPAddress(const Value: string);
begin
  _IPAddress.Value := Value;
end;

{ TAddress }
{$IFDEF WINLINUX64}
class function TAddress.Retrieve(const AObjectId: string; CreateIfMissing: Boolean = False;
  ARefresh: Boolean = False; AConnector: TComponent = nil;
  const AObjectData: TInstantAbstractObjectData = nil): TAddress;
begin
  Result := inherited Retrieve(AObjectId,
    CreateIfMissing, ARefresh, AConnector, AObjectData) as TAddress;
end;
{$ENDIF}

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

{$IFDEF WINLINUX64}
class function TCountry.Retrieve(const AObjectId: string; CreateIfMissing: Boolean = False;
  ARefresh: Boolean = False; AConnector: TComponent = nil;
  const AObjectData: TInstantAbstractObjectData = nil): TCountry;
begin
  Result := inherited Retrieve(AObjectId,
    CreateIfMissing, ARefresh, AConnector, AObjectData) as TCountry;
end;
{$ENDIF}

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

{$IFDEF WINLINUX64}
class function TPerson.Retrieve(const AObjectId: string; CreateIfMissing: Boolean = False;
  ARefresh: Boolean = False; AConnector: TComponent = nil;
  const AObjectData: TInstantAbstractObjectData = nil): TPerson;
begin
  Result := inherited Retrieve(AObjectId,
    CreateIfMissing, ARefresh, AConnector, AObjectData) as TPerson;
end;
{$ENDIF}

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

procedure TPerson.FreeInstance;
begin
  inherited;
  //Avoid circular reference of object TCompany:
  //When releasing TPerson object that is the only reference to
  //the TCompany object we must dereference TCompany so it can be
  //freed from memory.
  if (RefCount <> 0) and (_Employer.Value is TInstantObject) and
    (TInstantObject(_Employer.Value).RefCount = RefCount) then
    _Employer.Value := nil;
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
  Result := _Emails.Count;
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

{$IFDEF WINLINUX64}
class function TPhone.Retrieve(const AObjectId: string; CreateIfMissing: Boolean = False;
  ARefresh: Boolean = False; AConnector: TComponent = nil;
  const AObjectData: TInstantAbstractObjectData = nil): TPhone;
begin
  Result := inherited Retrieve(AObjectId,
    CreateIfMissing, ARefresh, AConnector, AObjectData) as TPhone;
end;
{$ENDIF}

function TPhone.GetName: string;
begin
  Result := _Name.Value;
end;

function TPhone.GetNumber: string;
begin
  Result := _Number.Value;
end;

function TPhone.GetPhoneType: TPhoneType;
begin
  Result := TPhoneType(_PhoneType.Value);
end;

procedure TPhone.SetName(const Value: string);
begin
  _Name.Value := Value;
end;

procedure TPhone.SetNumber(const Value: string);
begin
  _Number.Value := Value;
end;

procedure TPhone.SetPhoneType(Value: TPhoneType);
begin
  _PhoneType.Value := Ord(Value);
end;

{ TEmail }

{$IFDEF WINLINUX64}
class function TEmail.Retrieve(const AObjectId: string; CreateIfMissing: Boolean = False;
  ARefresh: Boolean = False; AConnector: TComponent = nil;
  const AObjectData: TInstantAbstractObjectData = nil): TEmail;
begin
  Result := inherited Retrieve(AObjectId,
    CreateIfMissing, ARefresh, AConnector, AObjectData) as TEmail;
end;
{$ENDIF}

function TEmail.GetAddress: string;
begin
  Result := _Address.Value;
end;

procedure TEmail.SetAddress(const Value: string);
begin
  _Address.Value := Value;
end;

{ TCategory }

{$IFDEF WINLINUX64}
class function TCategory.Retrieve(const AObjectId: string; CreateIfMissing: Boolean = False;
  ARefresh: Boolean = False; AConnector: TComponent = nil;
  const AObjectData: TInstantAbstractObjectData = nil): TCategory;
begin
  Result := inherited Retrieve(AObjectId,
    CreateIfMissing, ARefresh, AConnector, AObjectData) as TCategory;
end;
{$ENDIF}

constructor TCategory.Create(AConnector: TInstantConnector = nil);
begin
  inherited;
  ;
end;

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

{$IFDEF WINLINUX64}
class function TContact.Retrieve(const AObjectId: string; CreateIfMissing: Boolean = False;
  ARefresh: Boolean = False; AConnector: TComponent = nil;
  const AObjectData: TInstantAbstractObjectData = nil): TContact;
begin
  Result := inherited Retrieve(AObjectId,
    CreateIfMissing, ARefresh, AConnector, AObjectData) as TContact;
end;
{$ENDIF}

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
  Result := _Phones.Count;
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

{$IFDEF WINLINUX64}
class function TCompany.Retrieve(const AObjectId: string; CreateIfMissing: Boolean = False;
  ARefresh: Boolean = False; AConnector: TComponent = nil;
  const AObjectData: TInstantAbstractObjectData = nil): TCompany;
begin
  Result := inherited Retrieve(AObjectId,
    CreateIfMissing, ARefresh, AConnector, AObjectData) as TCompany;
end;
{$ENDIF}

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
    TPhone,
    TProfile,
    TUser
  ]);

end.
