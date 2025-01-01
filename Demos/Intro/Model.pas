unit Model;

interface

{$IFDEF LINUX64}
{$I '../InstantDefines.inc'}
{$ELSE}
{$I '..\InstantDefines.inc'}
{$ENDIF}

uses
  System.Classes,
  InstantPersistence,
  InstantTypes,
  InstantClasses;

type
  TAddress = class;
  TCompany = class;
  TContact = class;
  TPerson = class;
  TPhone = class;

  TAddress = class(TInstantObject)
  {IOMETADATA City: String(30);
    PostalCode: String(8);
    Street: String(30); }
    _City: TInstantString;
    _PostalCode: TInstantString;
    _Street: TInstantString;
  private
    function GetCity: string;
    function GetPostalCode: string;
    function GetStreet: string;
    procedure SetCity(const Value: string);
    procedure SetPostalCode(const Value: string);
    procedure SetStreet(const Value: string);
  published
    property City: string read GetCity write SetCity;
    property PostalCode: string read GetPostalCode write SetPostalCode;
    property Street: string read GetStreet write SetStreet;
  end;

  TPhone = class(TInstantObject)
  {IOMETADATA Name: String(10);
    Number: String(10) mask '(999) 999-9999;0;_'; }
    _Name: TInstantString;
    _Number: TInstantString;
  private
    function GetName: string;
    function GetNumber: string;
    procedure SetName(const Value: string);
    procedure SetNumber(const Value: string);
  public
  published
    property Name: string read GetName write SetName;
    property Number: string read GetNumber write SetNumber;
  end;

  TContact = class(TInstantObject)
  {IOMETADATA stored;
    Address: Part(TAddress);
    Name: String(30);
    Phones: Parts(TPhone); }
    _Address: TInstantPart;
    _Name: TInstantString;
    _Phones: TInstantParts;
  private
    function GetAddress: TAddress;
    function GetName: string;
    function GetPhoneCount: Integer;
    function GetPhones(Index: Integer): TPhone;
    procedure SetAddress(Value: TAddress);
    procedure SetName(const Value: string);
    procedure SetPhones(Index: Integer; Value: TPhone);
  public
    function AddPhone(Phone: TPhone): Integer;
    procedure ClearPhones;
    procedure DeletePhone(Index: Integer);
    function IndexOfPhone(Phone: TPhone): Integer;
    procedure InsertPhone(Index: Integer; Phone: TPhone);
    function RemovePhone(Phone: TPhone): Integer;
    property PhoneCount: Integer read GetPhoneCount;
    property Phones[Index: Integer]: TPhone read GetPhones write SetPhones;
  public
  published
    property Address: TAddress read GetAddress write SetAddress;
    property Name: string read GetName write SetName;
  end;

  TPerson = class(TContact)
  {IOMETADATA stored;
    DateOfBirth: DateTime; }
    _DateOfBirth: TInstantDateTime;
  private
    function GetDateOfBirth: TDateTime;
    procedure SetDateOfBirth(Value: TDateTime);
  public
    {$IFDEF WINLINUX64}
    class function Retrieve(const AObjectId: string; CreateIfMissing: Boolean = False;
      ARefresh: Boolean = False; AConnector: TComponent = nil;
      const AObjectData: TInstantAbstractObjectData = nil): TPerson; reintroduce; virtual;
    {$ENDIF}
  published
    property DateOfBirth: TDateTime read GetDateOfBirth write SetDateOfBirth;
  end;

  TCompany = class(TContact)
  {IOMETADATA stored;
    ContactPerson: Reference(TPerson); }
    _ContactPerson: TInstantReference;
  private
    function GetContactPerson: TPerson;
    procedure SetContactPerson(Value: TPerson);
  public
    {$IFDEF WINLINUX64}
    class function Retrieve(const AObjectId: string; CreateIfMissing: Boolean = False;
      ARefresh: Boolean = False; AConnector: TComponent = nil;
      const AObjectData: TInstantAbstractObjectData = nil): TCompany; reintroduce; virtual;
    {$ENDIF}
  published
    property ContactPerson: TPerson read GetContactPerson write SetContactPerson;
  end;

implementation

uses
  InstantMetadata;

{ TAddress }

function TAddress.GetCity: string;
begin
  Result := _City.Value;
end;

function TAddress.GetPostalCode: string;
begin
  Result := _PostalCode.Value;
end;

function TAddress.GetStreet: string;
begin
  Result := _Street.Value;
end;

procedure TAddress.SetCity(const Value: string);
begin
  _City.Value := Value;
end;

procedure TAddress.SetPostalCode(const Value: string);
begin
  _PostalCode.Value := Value;
end;

procedure TAddress.SetStreet(const Value: string);
begin
  _Street.Value := Value;
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

{ TContact }

function TContact.AddPhone(Phone: TPhone): Integer;
begin
  Result := _Phones.Add(Phone);
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

procedure TContact.SetName(const Value: string);
begin
  _Name.Value := Value;
end;

procedure TContact.SetPhones(Index: Integer; Value: TPhone);
begin
  _Phones[Index] := Value;
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

function TPerson.GetDateOfBirth: TDateTime;
begin
  Result := _DateOfBirth.Value;
end;

procedure TPerson.SetDateOfBirth(Value: TDateTime);
begin
  _DateOfBirth.Value := Value;
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

function TCompany.GetContactPerson: TPerson;
begin
  Result := _ContactPerson.Value as TPerson;
end;

procedure TCompany.SetContactPerson(Value: TPerson);
begin
  _ContactPerson.Value := Value;
end;

initialization
  InstantRegisterClasses([
    TAddress,
    TCompany,
    TContact,
    TPerson,
    TPhone
  ]);

end.
