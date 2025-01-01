(*
 *   InstantObjects
 *   Classes
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
 * The Original Code is: Seleqt InstantObjects
 *
 * The Initial Developer of the Original Code is: Seleqt
 *
 * Portions created by the Initial Developer are Copyright (C) 2001-2003
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Carlo Barazzetta, Adrea Petrelli, Marco Cantù, Nando Dessena, Uberto Barbini,
 * Riceball Lee, Brian Andersen
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantClasses;

{$IFDEF LINUX64}
{$I '../InstantDefines.inc'}
{$ELSE}
{$I '..\InstantDefines.inc'}
{$ENDIF}

interface

uses
  WinApi.Windows
  , System.Classes
  , InstantConsts
  , System.SysUtils
  ;

const
  InstantBufferSize = 4096;

type
  TChars = set of AnsiChar;

  EInstantError = class(Exception)
  private
    FOriginalException: TObject;
    procedure Initialize(E: TObject);
  public
    constructor CreateRes(ResStringRec: PResStringRec; E: TObject = nil);
    constructor CreateResFmt(ResStringRec: PResStringRec;
      const Args: array of const; E: TObject = nil);
    constructor Create(const Msg: string; E: TObject = nil);
    constructor CreateFmt(const Msg: string;
      const Args: array of const; E: TObject = nil);
    destructor Destroy; override;
    property OriginalException: TObject read FOriginalException;
  end;

  TAbstractWriter = class(TWriter);
  TAbstractReader = class(TReader);

  TInstantReader = class;
  TInstantWriter = class;

  TInstantProcessObjectEvent = procedure(Sender: TObject; AObject: TPersistent) of object;

  TInstantTextToBinaryConverter = class;
  TInstantBinaryToTextConverter = class;

  TInstantStreamableClass = class of TInstantStreamable;

  TInstantStreamable = class(TPersistent)
  protected
    class procedure ConvertToBinary(Converter: TInstantTextToBinaryConverter); overload; virtual;
    class procedure ConvertToText(Converter: TInstantBinaryToTextConverter); virtual;
    class function CreateInstance(Arg: Pointer = nil): TInstantStreamable; virtual;
    procedure ReadObject(Reader: TInstantReader); virtual;
    procedure WriteObject(Writer: TInstantWriter); virtual;
  public
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromStream(Stream: TStream;
      ProcessEvent: TInstantProcessObjectEvent = nil);
    procedure SaveToStream(Stream: TStream;
      ProcessEvent: TInstantProcessObjectEvent = nil);
  end;

  TInstantNamedList = class(TList)
  protected
    function GetName: string; virtual;
    procedure SetName(const Name: string); virtual;
  public
    property Name: string read GetName write SetName;
  end;

  TInstantCollectionItemClass = class of TInstantCollectionItem;

  TInstantCollectionItem = class(TCollectionItem)
  private
    FName: string;
  protected
    class procedure ConvertToBinary(Converter: TInstantTextToBinaryConverter); overload; virtual;
    class procedure ConvertToText(Converter: TInstantBinaryToTextConverter); virtual;
    class function CreateInstance(Arg: Pointer = nil): TInstantCollectionItem; virtual;
    function GetDisplayName: string; override;
    function GetName: string; virtual;
    procedure ReadObject(Reader: TInstantReader); virtual;
    procedure SetDisplayName(const Value: string); override;
    procedure SetName(const Value: string); virtual;
    procedure WriteObject(Writer: TInstantWriter); virtual;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Name: string read GetName write SetName;
  end;

  TInstantCollectionClass = class of TInstantCollection;

  TInstantCollection = class(TCollection)
  protected
    class procedure ConvertToBinary(Converter: TInstantTextToBinaryConverter); overload; virtual;
    class procedure ConvertToText(Converter: TInstantBinaryToTextConverter); virtual;
    class function CreateInstance(Arg: Pointer = nil): TInstantCollection; virtual;
    procedure ReadObject(Reader: TInstantReader); virtual;
    procedure WriteObject(Writer: TInstantWriter); virtual;
  public
    constructor Create(ItemClass: TInstantCollectionItemClass);
    function Find(const AName: string): TInstantCollectionItem;
    function Remove(Item: TInstantCollectionItem): Integer;
    procedure GetItemNames(List: TStrings);
    function IndexOf(const AName: string): Integer; overload;
    function IndexOf(Item: TInstantCollectionItem): Integer; overload;
  end;

  TInstantOwnedCollection = class(TInstantCollection)
  private
    FOwner: TPersistent;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TInstantCollectionItemClass);
    procedure Assign(Source: TPersistent); override;
    function Owner: TPersistent;
  end;

  TInstantReader = class(TAbstractReader)
  private
    FStream: TStream;
  protected
    procedure SkipBytes(Count: Integer);
    procedure ProcessObject(AObject: TPersistent);
  public
    constructor Create(Stream: TStream; BufSize: Integer = InstantBufferSize);
    procedure ReadBinary(ReadData: TStreamProc);
    function ReadObject(AObject: TPersistent = nil;
      Arg: Pointer = nil): TPersistent; virtual;
    procedure ReadProperties(AObject: TPersistent);
    procedure SkipValue;
    property Stream: TStream read FStream;
  end;

  TInstantWriter = class(TAbstractWriter)
  private
    FStream: TStream;
  protected
    procedure ProcessObject(AObject: TPersistent);
  public
    constructor Create(Stream: TStream; BufSize: Integer = InstantBufferSize);
    procedure WriteBinary(WriteData: TStreamProc);
    procedure WriteObject(AObject: TPersistent); virtual;
    procedure WriteProperties(AObject: TPersistent);
    procedure WriteValue(Value: TValueType);
    property Stream: TStream read FStream;
  end;

  TInstantStream = class(TStream)
  private
    FFreeSource: Boolean;
    FSource: TStream;
    FOnProcessObject: TInstantProcessObjectEvent;
    function GetSource: TStream;
    procedure SetSource(Value: TStream);
  protected
    procedure DoProcessObject(AObject: TPersistent); virtual;
    procedure SetSize(Value: Integer); override;
  public
    constructor Create(ASource: TStream = nil; AFreeSource: Boolean = False);
    destructor Destroy; override;
    procedure AlignStream;
    function Read(var Buffer; Count: Integer): Integer; override;
    function ReadObject(AObject: TPersistent = nil; Arg: Pointer = nil): TPersistent;
    function ReadObjectRes(AObject: TPersistent = nil; Arg: Pointer = nil): TPersistent;
    procedure ReadResHeader;
    procedure ReadResourceFileHeader;
    function Seek(Offset: Integer; Origin: Word): Integer; override;
    function Write(const Buffer; Count: Integer): Integer; override;
    procedure WriteObject(AObject: TPersistent);
    procedure WriteObjectRes(const ResName: string; AObject: TPersistent);
    procedure WriteResourceFileHeader;
    procedure WriteResourceHeader(const ResName: string;
      DataSize: Cardinal);
    property FreeSource: Boolean read FFreeSource write FFreeSource;
    property Source: TStream read GetSource write SetSource;
    property OnProcessObject: TInstantProcessObjectEvent read FOnProcessObject write FOnProcessObject;
  end;

  TInstantResourceStream = class(TInstantStream)
  public
    constructor Create(Instance: THandle; const ResName: string; ResType: PChar);
    constructor CreateFromId(Instance: THandle; ResID: Integer; ResType: PChar);
  end;

  TInstantFileStream = class(TInstantStream)
  public
    constructor Create(const FileName: string; Mode: Word);
  end;

  TInstantStringStream = class(TStringStream)
  public
    constructor Create(AString: string);
  end;

  TInstantXMLToken = (xtNone, xtStartTag, xtEndTag, xtAnyTag, xtData);

  TInstantXMLProducer = class(TObject)
  private
    FStream: TStream;
    FTagStack: TStringList;
    FWriter: TAbstractWriter;
    FIndentationSize: Integer;
    FLastToken: TInstantXMLToken;
    function GetCurrentTag: string;
    function GetEof: Boolean;
    function GetPosition: Integer;
    function GetTagStack: TStringList;
    function GetWriter: TAbstractWriter;
    procedure SetPosition(Value: Integer);
  protected
    function IsPrettyXMLEnabled: Boolean;
    procedure WriteString(const S: string);
    property TagStack: TStringList read GetTagStack;
    property Writer: TAbstractWriter read GetWriter;
    procedure WriteIndentation;
    procedure WriteLineBreak;
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;
    procedure WriteEscapedData(const Data: string);
    procedure WriteData(const Data: string);
    procedure WriteEndTag;
    procedure WritePrologue;
    procedure WriteStartTag(const Tag: string);
    property CurrentTag: string read GetCurrentTag;
    property Eof: Boolean read GetEof;
    property Position: Integer read GetPosition write SetPosition;
    property Stream: TStream read FStream;
  end;

  TInstantXMLProcessor = class(TObject)
  private
    FReader: TAbstractReader;
    FStream: TStream;
    function GetEof: Boolean;
    function GetPosition: Integer;
    function GetReader: TAbstractReader;
    function GetToken: TInstantXMLToken;
    function ReadEscapedChar: Char;
    procedure SetPosition(const Value: Integer);
  protected
    procedure CheckToken(AToken: TInstantXMLToken);
    function PeekChar: Char;
    function PeekCharSkippingBlanks: Char;
    function ReadChar: Char;
    procedure SkipBlanks;
    property Reader: TAbstractReader read GetReader;
    function IsProcessingInstruction(const ATagName: string): Boolean;
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;
    function PeekTag: string;
    function PeekTagName: string;
    procedure ReadData(Stream: TStream); overload;
    function ReadData: string; overload;
    function ReadTag: string;
    function ReadTagName: string;
    procedure Skip;
    procedure SkipProcessingInstruction;
    property Eof: Boolean read GetEof;
    property Position: Integer read GetPosition write SetPosition;
    property Stream: TStream read FStream;
    property Token: TInstantXMLToken read GetToken;
  end;

  TInstantConverter = class(TObject)
  private
    FObjectClassList: TList;
    function GetObjectClassName: string;
    function GetObjectClass: TClass;
    function GetObjectClassList: TList;
  protected
    function GetInput: TStream; virtual; abstract;
    function GetOutput: TStream; virtual; abstract;
    procedure InternalConvert; virtual; abstract;
    procedure InternalConvertProperties; virtual; abstract;
    procedure PopObjectClass;
    procedure PushObjectClass(AObjectClass: TClass);
    property Input: TStream read GetInput;
    property Output: TStream read GetOutput;
    property ObjectClassList: TList read GetObjectClassList;
  public
    destructor Destroy; override;
    procedure Convert;
    procedure ConvertProperties;
    property ObjectClass: TClass read GetObjectClass;
    property ObjectClassName: string read GetObjectClassName;
  end;

  TInstantBinaryToTextConverter = class(TInstantConverter)
  private
    FProducer: TInstantXMLProducer;
    FReader: TInstantReader;
  protected
    function GetInput: TStream; override;
    function GetOutput: TStream; override;
    procedure InternalConvert; override;
    procedure InternalConvertProperties; override;
  public
    constructor Create(Input, Output: TStream);
    destructor Destroy; override;
    property Producer: TInstantXMLProducer read FProducer;
    property Reader: TInstantReader read FReader;
  end;

  TInstantTextToBinaryConverter = class(TInstantConverter)
  private
    FProcessor: TInstantXMLProcessor;
    FWriter: TInstantWriter;
    procedure DoConvertProperties(const StopTag: string);
  protected
    function GetInput: TStream; override;
    function GetOutput: TStream; override;
    procedure InternalConvert; override;
    procedure InternalConvertProperties; override;
  public
    constructor Create(Input, Output: TStream);
    destructor Destroy; override;
    procedure ConvertProperties(const StopTag: string); overload;
    property Processor: TInstantXMLProcessor read FProcessor;
    property Writer: TInstantWriter read FWriter;
  end;

  EInstantStreamError = class(EInstantError)
  end;

  EInstantValidationError = class(EInstantError)
  end;

  EInstantRangeError = class(EInstantError)
  end;

  EInstantConversionError = class(EInstantError)
  end;

  TInstantAbstractObjectClass = class of TInstantAbstractObject;

  TInstantAbstractObjectData = class(TComponent);

  TInstantAbstractObject = class(TInstantStreamable)
  private
    FConnector: TComponent;
  protected
    function GetConnector: TComponent;
    procedure SetConnector(AConnector: TComponent);
  public
    constructor Retrieve(const AObjectId: string; CreateIfMissing: Boolean = False;
      Refresh: Boolean = False; AConnector: TComponent = nil;
      const AObjectData: TInstantAbstractObjectData = nil); virtual; abstract;
  end;

  TInstantAbstractAttributeClass = class of TInstantAbstractAttribute;

  TInstantAbstractAttribute = class(TInstantStreamable)
  private
    FMetadata: TInstantCollectionItem;
    FOwner: TInstantAbstractObject;
  protected
    function GetOwner: TInstantAbstractObject; reintroduce; virtual;
    procedure SetOwner(AOwner: TInstantAbstractObject);
    function GetMetadata: TInstantCollectionItem;
    procedure SetMetadata(AMetadata: TInstantCollectionItem);
    procedure Initialize; virtual; abstract;
  public
    constructor Create(AOwner: TInstantAbstractObject;
      AMetadata: TInstantCollectionItem); virtual;
  end;

  TInstantStreamFormat = (
    sfBinary
    , sfXML
    {$IFDEF DELPHI_NEON}
    , sfJSON
    {$ENDIF}
    );

const
  AInstantStreamFormatStr : Array[TInstantStreamFormat] of string =
   ('Binary format'
    ,'XML format'
    {$IFDEF DELPHI_NEON}
    ,'JSON format'
    {$ENDIF}
    );

function InstantBuildEndTag(const TagName: string): string;
function InstantBuildStartTag(const TagName: string): string;
procedure InstantCheckClass(AClass: TClass; MinimumClass: TClass);
procedure InstantObjectBinaryToText(Input, Output: TStream);
procedure InstantObjectTextToBinary(Input, Output: TStream);
function InstantReadObject(Stream: TStream; Format: TInstantStreamFormat;
  AObject: TPersistent = nil): TPersistent;
function InstantReadObjectFromStream(Stream: TStream;
  AObject: TPersistent = nil; ProcessEvent: TInstantProcessObjectEvent = nil;
  Arg: Pointer = nil): TPersistent;
procedure InstantReadObjects(Stream: TStream; Format: TInstantStreamFormat; Objects: TList);
procedure InstantWriteObject(Stream: TStream; Format: TInstantStreamFormat; AObject: TPersistent);
procedure InstantWriteObjectToStream(Stream: TStream; AObject: TPersistent;
  ProcessEvent: TInstantProcessObjectEvent = nil);
procedure InstantWriteObjects(Stream: TStream; Format: TInstantStreamFormat; Objects: TList);

implementation

uses
  System.TypInfo
  , System.StrUtils
  //Deactivate {$DEFINE DELPHI_NEON} directive into InstantDefines.inc if you don't want JSON support
  {$IFDEF DELPHI_NEON}
  , Instant.Neon.Serializers
  {$ENDIF}
  , InstantUtils
  , InstantRtti;

const
  ResourceHeader : packed array[0..31] of Byte = ($00,$00,$00,$00,$20,$00,$00,
    $00,$FF,$FF,$00,$00,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$00);

type
  PRaiseFrame = ^TRaiseFrame;
  TRaiseFrame = record
    NextRaise: PRaiseFrame;
    ExceptAddr: Pointer;
    ExceptObject: TObject;
    ExceptionRecord: PExceptionRecord;
  end;

{ Global routines }

function InstantBuildEndTag(const TagName: string): string;
begin
  Result := Format(InstantEndTagFormat, [TagName]);
end;

function InstantBuildStartTag(const TagName: string): string;
begin
  Result := Format(InstantStartTagFormat, [TagName]);
end;

procedure InstantCheckClass(AClass: TClass; MinimumClass: TClass);
begin
  if Assigned(AClass) and Assigned(MinimumClass) then
    if not AClass.InheritsFrom(MinimumClass) then
      raise EInstantError.CreateFmt(SInvalidClass,
        [AClass.ClassName, MinimumClass.ClassName]);
end;

procedure InstantObjectBinaryToText(Input, Output: TStream);
begin
  with TInstantBinaryToTextConverter.Create(Input, Output) do
  try
    Convert;
  finally
    Free;
  end;
end;

procedure InstantObjectTextToBinary(Input, Output: TStream);
begin
  with TInstantTextToBinaryConverter.Create(Input, Output) do
  try
    Processor.SkipProcessingInstruction;
    Convert;
  finally
    Free;
  end;
end;

function InstantReadObject(Stream: TStream; Format: TInstantStreamFormat;
  AObject: TPersistent = nil): TPersistent;
var
  MemoryStream: TMemoryStream;
  StringStream: TStringStream;
  UStringStream: TStringStream;
begin
  if Format = sfBinary then
    Result := InstantReadObjectFromStream(Stream, AObject)
  else if Format = sfXML then
  begin
    MemoryStream := TMemoryStream.Create;
    try
      StringStream := TStringStream.Create('', TEncoding.UTF8);
      try
        StringStream.CopyFrom(Stream, Stream.Size);
        UStringStream := TStringStream.Create(StringStream.DataString, TEncoding.Unicode);
        try
          UStringStream.Position := 0;
          InstantObjectTextToBinary(UStringStream, MemoryStream);
          MemoryStream.Position := 0;
          Result := InstantReadObjectFromStream(MemoryStream, AObject);
        finally
          UStringStream.Free;
        end;
      finally
        StringStream.Free;
      end;
    finally
      MemoryStream.Free;
    end;
  {$IFDEF DELPHI_NEON}
  end
  else if Format = sfJSON then
  begin
    StringStream := TStringStream.Create('', TEncoding.UTF8);
    try
      StringStream.CopyFrom(Stream, Stream.Size);
      Result := JSONToPersistentObject(StringStream.datastring, AObject);
    finally
      StringStream.Free;
    end;
  {$ENDIF}
  end
  else
    Result := nil;
end;

function InstantReadObjectFromStream(Stream: TStream;
  AObject: TPersistent; ProcessEvent: TInstantProcessObjectEvent;
  Arg: Pointer): TPersistent;
begin
  if Stream is TInstantStream then
    with TInstantStream(Stream) do
    begin
     OnProcessObject := ProcessEvent;
     Result := ReadObject(AObject, Arg)
    end
  else if Assigned(Stream) then
    with TInstantStream.Create(Stream) do
    try
      OnProcessObject := ProcessEvent;
      Result := ReadObject(AObject, Arg);
    finally
      Free;
    end
  else
    Result := nil;
end;

procedure InstantReadObjects(Stream: TStream; Format: TInstantStreamFormat; Objects: TList);
begin
  if Assigned(Stream) and Assigned(Objects) then
    while Stream.Position < Stream.Size do
      Objects.Add(InstantReadObject(Stream, Format));
end;

procedure InstantWriteObject(Stream: TStream; Format: TInstantStreamFormat;
  AObject: TPersistent);
var
  MemoryStream: TMemoryStream;
begin
  if Format = sfBinary then
    InstantWriteObjectToStream(Stream, AObject)
  else if Format = sfXML then
  begin
    MemoryStream := TMemoryStream.Create;
    try
      InstantWriteObjectToStream(MemoryStream, AObject);
      MemoryStream.Position := 0;
      InstantObjectBinaryToText(MemoryStream, Stream);
    finally
      MemoryStream.Free;
    end;
  {$IFDEF DELPHI_NEON}
  end
  else if Format = sfJSON then
  begin
    PersistentObjectToStream(AObject, Stream);
  {$ENDIF}
  end;
end;

procedure InstantWriteObjectToStream(Stream: TStream;
  AObject: TPersistent; ProcessEvent: TInstantProcessObjectEvent);
begin
  if Stream is TInstantStream then
    with TInstantStream(Stream) do
    begin
      OnProcessObject := ProcessEvent;
      WriteObject(AObject)
    end
  else if Assigned(Stream) then
    with TInstantStream.Create(Stream) do
    try
      OnProcessObject := ProcessEvent;
      WriteObject(AObject);
    finally
      Free;
    end
end;

procedure InstantWriteObjects(Stream: TStream; Format: TInstantStreamFormat; Objects: TList);
var
  I: Integer;
begin
  if Assigned(Stream) and Assigned(Objects) then
    for I := 0 to Pred(Objects.Count) do
      InstantWriteObject(Stream, Format, Objects[I]);
end;

{ EInstantError }

constructor EInstantError.Create(const Msg: string; E: TObject);
begin
  inherited Create(Msg);
  Initialize(E);
end;

constructor EInstantError.CreateFmt(const Msg: string;
  const Args: array of const; E: TObject);
begin
  inherited CreateFmt(Msg, Args);
  Initialize(E);
end;

constructor EInstantError.CreateRes(ResStringRec: PResStringRec;
  E: TObject);
begin
  inherited CreateRes(ResStringRec);
  Initialize(E);
end;

constructor EInstantError.CreateResFmt(ResStringRec: PResStringRec;
  const Args: array of const; E: TObject);
begin
  inherited CreateResFmt(ResStringRec, Args);
  Initialize(E);
end;

destructor EInstantError.Destroy;
begin
  FreeAndNil(FOriginalException);
  inherited;
end;

procedure EInstantError.Initialize(E: TObject);
begin
  if ((ExceptObject <> nil) and (ExceptObject = E)) then
    FOriginalException := AcquireExceptionObject;
end;

{ TInstantStreamable }

procedure TInstantStreamable.Assign(Source: TPersistent);
begin
  if not (Source is TInstantStreamable) then
    inherited;
end;

class procedure TInstantStreamable.ConvertToBinary(
  Converter: TInstantTextToBinaryConverter);
begin
end;

class procedure TInstantStreamable.ConvertToText(
  Converter: TInstantBinaryToTextConverter);
begin
end;

class function TInstantStreamable.CreateInstance(
  Arg: Pointer): TInstantStreamable;
begin
  Result := Create;
end;

procedure TInstantStreamable.LoadFromStream(Stream: TStream;
  ProcessEvent: TInstantProcessObjectEvent);
begin
  if Assigned(Stream) then
    InstantReadObjectFromStream(Stream, Self, ProcessEvent);
end;

procedure TInstantStreamable.ReadObject(Reader: TInstantReader);
begin
end;

procedure TInstantStreamable.SaveToStream(Stream: TStream;
  ProcessEvent: TInstantProcessObjectEvent);
begin
  if Assigned(Stream) then
    InstantWriteObjectToStream(Stream, Self, ProcessEvent);
end;

procedure TInstantStreamable.WriteObject(Writer: TInstantWriter);
begin
end;

{ TInstantNamedList }

function TInstantNamedList.GetName: string;
begin
  Result := '';
end;

procedure TInstantNamedList.SetName(const Name: string);
begin
end;

{ TInstantCollectionItem }

procedure TInstantCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TInstantCollectionItem then
    Name := TInstantCollectionItem(Source).Name
  else
    inherited;
end;

class procedure TInstantCollectionItem.ConvertToBinary(
  Converter: TInstantTextToBinaryConverter);
begin
  Converter.ConvertProperties;
end;

class procedure TInstantCollectionItem.ConvertToText(
  Converter: TInstantBinaryToTextConverter);
begin
  Converter.ConvertProperties;
end;

class function TInstantCollectionItem.CreateInstance(
  Arg: Pointer): TInstantCollectionItem;
begin
  if Assigned(Arg) then
    Result := Create(TInstantCollection(Arg))
  else
    Result := Create(nil)
end;

function TInstantCollectionItem.GetDisplayName: string;
begin
  Result := Name
end;

function TInstantCollectionItem.GetName: string;
begin
  Result := FName;
end;

procedure TInstantCollectionItem.ReadObject(Reader: TInstantReader);
begin
  Reader.ReadProperties(Self);
end;

procedure TInstantCollectionItem.SetDisplayName(const Value: string);
begin
  Name := Value;
  inherited;
end;

procedure TInstantCollectionItem.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TInstantCollectionItem.WriteObject(Writer: TInstantWriter);
begin
  Writer.WriteProperties(Self);
end;

{ TInstantCollection }

class procedure TInstantCollection.ConvertToBinary(Converter: TInstantTextToBinaryConverter);
var
  ObjectEnd: string;
begin
  ObjectEnd := InstantBuildEndTag(Converter.ObjectClassName);
  while not SameText(Converter.Processor.PeekTag, ObjectEnd) do
    Converter.Convert;
  Converter.Writer.WriteListEnd;
end;

class procedure TInstantCollection.ConvertToText(Converter: TInstantBinaryToTextConverter);
begin
  with Converter do
  begin
    while not Reader.EndOfList do
      Convert;
    Reader.ReadListEnd;
  end;
end;

constructor TInstantCollection.Create(ItemClass: TInstantCollectionItemClass);
begin
  inherited Create(ItemClass)
end;

class function TInstantCollection.CreateInstance(
  Arg: Pointer): TInstantCollection;
begin
  if Assigned(Arg) then
    Result := Create(TInstantCollectionItemClass(Arg))
  else
    Result := Create(TInstantCollectionItem);
end;

function TInstantCollection.Find(const AName: string): TInstantCollectionItem;
var
  I: Integer;
begin
  I := IndexOf(AName);
  if I = -1 then
    Result := nil
  else
    Result := TInstantCollectionItem(Items[I]);
end;

procedure TInstantCollection.GetItemNames(List: TStrings);
var
  I: Integer;
begin
  List.BeginUpdate;
  try
    List.Clear;
    for I := 0 to Pred(Count) do
      with TInstantCollectionItem(Items[I]) do
        if Name <> '' then List.Add(Name);
  finally
    List.EndUpdate;
  end;
end;

function TInstantCollection.IndexOf(const AName: string): Integer;
begin
  for Result := 0 to Pred(Count) do
    if SameText(TInstantCollectionItem(Items[Result]).Name, AName) then
      Exit;
  Result := -1;
end;

function TInstantCollection.IndexOf(Item: TInstantCollectionItem): Integer;
begin
  if Assigned(Item) and (Item.Collection = Self) then
    Result := Item.Index
  else
    Result := -1;
end;

procedure TInstantCollection.ReadObject(Reader: TInstantReader);
begin
  Clear;
  with Reader do
  begin
    while not EndOfList do
      ReadObject(nil, Self);
    ReadListEnd;
  end;
end;

function TInstantCollection.Remove(Item: TInstantCollectionItem): Integer;
begin
  Result := IndexOf(Item);
  if Result <> -1 then
    Delete(Result);
end;

procedure TInstantCollection.WriteObject(Writer: TInstantWriter);
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do
    Writer.WriteObject(Items[I]);
  Writer.WriteListEnd;
end;

{ TInstantOwnedCollection }

procedure TInstantOwnedCollection.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TInstantOwnedCollection then
    FOwner := TInstantOwnedCollection(Source).FOwner;
end;

constructor TInstantOwnedCollection.Create(AOwner: TPersistent;
  ItemClass: TInstantCollectionItemClass);
begin
  inherited Create(ItemClass);
  FOwner := AOwner;
end;

function TInstantOwnedCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TInstantOwnedCollection.Owner: TPersistent;
begin
  Result := GetOwner;
end;

{ TInstantReader }

constructor TInstantReader.Create(Stream: TStream; BufSize: Integer);
begin
  inherited Create(Stream, BufSize);
  FStream := Stream;
end;

procedure TInstantReader.ProcessObject(AObject: TPersistent);
begin
  if Stream is TInstantStream then
    TInstantStream(Stream).DoProcessObject(AObject);
end;

procedure TInstantReader.ReadBinary(ReadData: TStreamProc);
var
  Count: Integer;
  Stream: TMemoryStream;
begin
  if ReadValue <> vaBinary then
  begin
    Position := Position - 1;
    SkipValue;
    raise EReadError.Create(SInvalidPropertyValue);
  end;
  Stream := TMemoryStream.Create;
  try
    Read(Count, SizeOf(Count));
    Stream.SetSize(Count);
    Read(Stream.Memory^, Count);
    ReadData(Stream);
  finally
    Stream.Free;
  end;
end;

function TInstantReader.ReadObject(AObject: TPersistent;
  Arg: Pointer): TPersistent;

  function CreateObject(ObjectClass: TPersistentClass): TPersistent;
  begin
    if ObjectClass.InheritsFrom(TInstantStreamable) then
      Result := TInstantStreamableClass(ObjectClass).CreateInstance(Arg)
    else if ObjectClass.InheritsFrom(TInstantCollection) then
      Result := TInstantCollectionClass(ObjectClass).CreateInstance(Arg)
    else if ObjectClass.InheritsFrom(TInstantCollectionItem) then
      Result := TInstantCollectionItemClass(ObjectClass).CreateInstance(Arg)
    else
      raise EInstantStreamError.CreateFmt(SClassNotStreamable,
        [ObjectClass.ClassName]);
  end;

var
  ObjectClassName: string;
  ObjectClass: TPersistentClass;
begin
  ObjectClassName := ReadStr;
  ObjectClass := FindClass(ObjectClassName);
  if Assigned(AObject) then
    if ObjectClass.InheritsFrom(AObject.ClassType) then
      Result := AObject
    else
      raise EInstantStreamError.CreateFmt(SUnexpectedClass,
        [ObjectClassName, AObject.ClassName])
  else
    Result := CreateObject(ObjectClass);
  if Result is TInstantStreamable then
    TInstantStreamable(Result).ReadObject(Self)
  else if Result is TInstantCollection then
    TInstantCollection(Result).ReadObject(Self)
  else if Result is TInstantCollectionItem then
    TInstantCollectionItem(Result).ReadObject(Self);
  ReadListEnd;
  ProcessObject(Result);
end;

procedure TInstantReader.ReadProperties(AObject: TPersistent);
begin
  while not EndOfList do
    ReadProperty(AObject);
  ReadListEnd;
end;

procedure TInstantReader.SkipBytes(Count: Integer);
var
  Bytes: array[0..255] of Char;
begin
  while Count > 0 do
    if Count > SizeOf(Bytes) then
    begin
      Read(Bytes, SizeOf(Bytes));
      Dec(Count, SizeOf(Bytes));
    end else
    begin
      Read(Bytes, Count);
      Count := 0;
    end;
end;

procedure TInstantReader.SkipValue;
begin
  inherited;
end;

{ TInstantWriter }

constructor TInstantWriter.Create(Stream: TStream; BufSize: Integer);
begin
  inherited Create(Stream, BufSize);
  FStream := Stream;
end;

procedure TInstantWriter.ProcessObject(AObject: TPersistent);
begin
  if Stream is TInstantStream then
    TInstantStream(Stream).DoProcessObject(AObject);
end;

procedure TInstantWriter.WriteBinary(WriteData: TStreamProc);
begin
  inherited WriteBinary(WriteData);
end;

procedure TInstantWriter.WriteObject(AObject: TPersistent);
begin
  WriteUTF8Str(AObject.ClassName);
  if AObject is TInstantStreamable then
    TInstantStreamable(AObject).WriteObject(Self)
  else if AObject is TInstantCollection then
    TInstantCollection(AObject).WriteObject(Self)
  else if AObject is TInstantCollectionItem then
    TInstantCollectionItem(AObject).WriteObject(Self)
  else
    raise EInstantStreamError.CreateFmt(SClassNotStreamable,
      [AObject.ClassName]);
  WriteListEnd;
  ProcessObject(AObject);
end;

procedure TInstantWriter.WriteProperties(AObject: TPersistent);
begin
  inherited WriteProperties(AObject);
  WriteListEnd;
end;

procedure TInstantWriter.WriteValue(Value: TValueType);
begin
  inherited WriteValue(Value);
end;

{ TInstantStream }

procedure TInstantStream.AlignStream;
const
  Padding: packed array[0..2] of Byte = ($00,$00,$00);
var
  Offset: Integer;
begin
  { Align stream to 32-bit boundary }
  Offset := (Source.Position mod 4);
  if Offset <> 0 then
    if Source.Position = Source.Size then
      Source.Write(Padding, 4 - Offset)
    else
      Source.Seek(4 - Offset, soFromCurrent);
end;

constructor TInstantStream.Create(ASource: TStream; AFreeSource: Boolean);
begin
  inherited Create;
  FSource := ASource;
  FFreeSource := AFreeSource;
end;

destructor TInstantStream.Destroy;
begin
  Source := nil;
  inherited;
end;

procedure TInstantStream.DoProcessObject(AObject: TPersistent);
begin
  if Assigned(FOnProcessObject) then
    FOnProcessObject(Self, AObject);
end;

function TInstantStream.GetSource: TStream;
begin
  if not Assigned(FSource) then
  begin
    FSource := TMemoryStream.Create;
    FFreeSource := True;
  end;
  Result := FSource;
end;

function TInstantStream.Read(var Buffer; Count: Integer): Integer;
begin
  Result := Source.Read(Buffer, Count);
end;

function TInstantStream.ReadObject(AObject: TPersistent;
  Arg: Pointer): TPersistent;
begin
  with TInstantReader.Create(Self) do
  try
    Result := ReadObject(AObject, Arg);
  finally
    Free;
  end;
end;

function TInstantStream.ReadObjectRes(AObject: TPersistent;
  Arg: Pointer): TPersistent;
begin
   ReadResHeader;
   Result := ReadObject(AObject, Arg);
   AlignStream;
end;

procedure TInstantStream.ReadResHeader;
var
  HeaderSize: Cardinal;
begin
  Position := Position + SizeOf(Cardinal);
  Read(HeaderSize, SizeOf(Cardinal));
  Position := Position + HeaderSize - 2 * SizeOf(Cardinal);
end;

procedure TInstantStream.ReadResourceFileHeader;
begin
  Source.Seek(SizeOf(ResourceHeader), soFromCurrent);
end;

function TInstantStream.Seek(Offset: Integer; Origin: Word): Integer;
begin
  Result := Source.Seek(OffSet, Origin);
end;

procedure TInstantStream.SetSize(Value: Integer);
begin
  Source.Size := Value;
end;

procedure TInstantStream.SetSource(Value: TStream);
begin
  if FFreeSource then
    Source.Free;
  FSource := Value;
  FFreeSource := False;
end;

function TInstantStream.Write(const Buffer; Count: Integer): Integer;
begin
  Result := Source.Write(Buffer, Count);
end;

procedure TInstantStream.WriteObject(AObject: TPersistent);
begin
  with TInstantWriter.Create(Self) do
  try
    WriteObject(AObject);
  finally
    Free;
  end;
end;

procedure TInstantStream.WriteObjectRes(const ResName: string;
  AObject: TPersistent);
var
  ObjectStream: TInstantStream;
begin
  ObjectStream := TInstantStream.Create;
  try
    ObjectStream.WriteObject(AObject);
    WriteResourceHeader(UpperCase(ResName), ObjectStream.Size);
    Write(TMemoryStream(ObjectStream.Source).Memory^, ObjectStream.Size);
    AlignStream;
  finally
    ObjectStream.Free;
  end;
end;

procedure TInstantStream.WriteResourceFileHeader;
begin
  Source.Write(Resourceheader, SizeOf(ResourceHeader));
end;

procedure TInstantStream.WriteResourceHeader(const ResName: string;
  DataSize: Cardinal);
const
  ResourceType: packed array[0..3] of Byte  = ($FF,$FF,$0A,$00); // RT_RCDATA
  DataVersion: Cardinal = 0;
  Flags: Word = $30; // Pure and moveable
  Language: Word = 1030;
  Version: Cardinal = 0;
  Characteristics: Cardinal = 0;

var
  NameBufferSize: Integer;
  HeaderSize: Integer;
begin
  NameBufferSize := (Length(ResName) + 1) * SizeOf(WideChar);
  HeaderSize := 28 + NameBufferSize;

  { include padding for name alignment in header }
  if (NameBufferSize mod SizeOf(Cardinal)) <> 0 then
    HeaderSize := HeaderSize + SizeOf(Word);

  Write(DataSize, SizeOf(Cardinal));
  Write(HeaderSize, SizeOf(Cardinal));
  Write(ResourceType, SizeOf(Cardinal));
  Write(Pointer(Widestring(ResName))^, NameBufferSize);
  AlignStream;
  Write(DataVersion, SizeOf(Cardinal));
  Write(Flags, SizeOf(Word));
  Write(Language, SizeOf(Word));
  Write(Version, SizeOf(Cardinal));
  Write(Characteristics, SizeOf(Cardinal));
end;

{ TInstantResourceStream }

constructor TInstantResourceStream.Create(Instance: THandle;
  const ResName: string; ResType: PChar);
begin
  inherited Create(TResourceStream.Create(Instance, UpperCase(ResName),
    ResType), True);
end;

constructor TInstantResourceStream.CreateFromId(Instance: THandle;
  ResID: Integer; ResType: PChar);
begin
  inherited Create(TResourceStream.CreateFromID(Instance, ResID, ResType),
    True);
end;

{ TInstantFileStream }

constructor TInstantFileStream.Create(const FileName: string; Mode: Word);
begin
  inherited Create(TFileStream.Create(FileName, Mode), True);
end;

{ TInstantStringStream }

constructor TInstantStringStream.Create(AString: string);
begin
  inherited Create(AString, TEncoding.Unicode);
end;

{ TInstantXMLProducer }

function TInstantXMLProducer.IsPrettyXMLEnabled: Boolean;
begin
  Result := FIndentationSize > 0;
end;

constructor TInstantXMLProducer.Create(Stream: TStream);
begin
  inherited Create;
  FStream := Stream;
  FLastToken := xtNone;
  FIndentationSize := InstantXMLIndentationSize;
end;

destructor TInstantXMLProducer.Destroy;
begin
  FWriter.Free;
  FTagStack.Free;
  inherited;
end;

function TInstantXMLProducer.GetCurrentTag: string;
begin
  Result := TagStack[TagStack.Count - 1];
end;

function TInstantXMLProducer.GetEof: Boolean;
begin
  Writer.FlushBuffer;
  Result := Writer.Position = Stream.Position;
end;

function TInstantXMLProducer.GetPosition: Integer;
begin
  Result := Writer.Position;
end;

function TInstantXMLProducer.GetTagStack: TStringList;
begin
  if not Assigned(FTagStack) then
    FTagStack := TStringList.Create;
  Result := FTagStack;
end;

function TInstantXMLProducer.GetWriter: TAbstractWriter;
begin
  if not Assigned(FWriter) then
    FWriter := TAbstractWriter.Create(Stream, InstantBufferSize);
  Result := FWriter;
end;

procedure TInstantXMLProducer.SetPosition(Value: Integer);
begin
  Writer.Position := Value;
end;

procedure TInstantXMLProducer.WriteData(const Data: string);
begin
  WriteString(Data);
  FLastToken := xtData;
end;

procedure TInstantXMLProducer.WriteStartTag(const Tag: string);
begin
  if IsPrettyXMLEnabled then
  begin
    if FLastToken = xtStartTag then
      WriteLineBreak;
    WriteIndentation;
  end;
  WriteString(InstantBuildStartTag(Tag));
  TagStack.Add(Tag);
  FLastToken := xtStartTag;
end;

procedure TInstantXMLProducer.WriteEndTag;
var
  LTagName: string;
begin
  LTagName := TagStack[TagStack.Count - 1];
  TagStack.Delete(TagStack.Count - 1);
  if IsPrettyXMLEnabled then
  begin
    if FLastToken = xtStartTag then
      WriteLineBreak;
    if FLastToken in [xtStartTag, xtEndTag] then
      WriteIndentation;
  end;
  WriteString(InstantBuildEndTag(LTagName));
  if IsPrettyXMLEnabled then
    WriteLineBreak;
  FLastToken := xtEndTag;
end;

procedure TInstantXMLProducer.WriteEscapedData(const Data: string);
const
  EscStr = '&%s;';
var
  I: Integer;
  Esc: string;
  C: Char;
  LString: string;
begin
  LString := '';
  for I := 1 to Length(Data) do
  begin
    C := Data[I];
    if InstantCharInSet(C, [#34, #38, #39, #60, #62]) then
    begin
      case C of
        #34:
          Esc := 'quot';
        #38:
          Esc := 'amp';
        #39:
          Esc := 'apos';
        #60:
          Esc := 'lt';
        #62:
          Esc := 'gt';
      end;
      Esc := Format(EscStr, [Esc]);
      LString := LString + Esc;
    end
    else
      LString := LString + C;
  end;
  WriteString(LString);
  FLastToken := xtData;
end;

procedure TInstantXMLProducer.WriteLineBreak;
begin
  WriteString(sLineBreak);
end;

procedure TInstantXMLProducer.WritePrologue;
begin
  WriteString(InstantPrologue + sLineBreak);
end;

procedure TInstantXMLProducer.WriteIndentation;
begin
  WriteString(DupeString(' ' , TagStack.Count * FIndentationSize));
end;

procedure TInstantXMLProducer.WriteString(const S: string);
var
  U: UTF8String;
begin
  U := UTF8String(S);
  Writer.Write(Pointer(U)^, Length(U));
end;

{ TInstantXMLProcessor }

procedure TInstantXMLProcessor.CheckToken(AToken: TInstantXMLToken);
begin
  if not (Token = AToken) then
    raise EInstantError.CreateFmt(SInvalidToken,
      [GetEnumName(TypeInfo(TInstantXMLToken), Ord(Token))]);
end;

constructor TInstantXMLProcessor.Create(Stream: TStream);
begin
  inherited Create;
  FStream := Stream;
end;

destructor TInstantXMLProcessor.Destroy;
begin
  FReader.Free;
  inherited;
end;

function TInstantXMLProcessor.GetEof: Boolean;
begin
  Result := Reader.Position = Stream.Size;
end;

function TInstantXMLProcessor.GetPosition: Integer;
begin
  Result := Reader.Position;
end;

function TInstantXMLProcessor.GetReader: TAbstractReader;
begin
  if not Assigned(FReader) then
    FReader := TAbstractReader.Create(Stream, InstantBufferSize);
  Result := Freader;
end;

function TInstantXMLProcessor.GetToken: TInstantXMLToken;
begin
  if PeekCharSkippingBlanks = InstantTagStart then
    Result := xtAnyTag
  else
    Result := xtData;
end;

function TInstantXMLProcessor.PeekChar: Char;
var
  Pos: Integer;
begin
  Pos := Position;
  try
    Result := ReadChar;
  finally
    Position := Pos;
  end;
end;

function TInstantXMLProcessor.PeekCharSkippingBlanks: Char;
var
  Pos: Integer;
begin
  Pos := Position;
  try
    SkipBlanks;
    Result := ReadChar;
  finally
    Position := Pos;
  end;
end;

function TInstantXMLProcessor.PeekTag: string;
var
  Pos: Integer;
begin
  Pos := Position;
  try
    Result := ReadTag;
  finally
    Position := Pos;
  end;
end;

function TInstantXMLProcessor.PeekTagName: string;
var
  Pos: Integer;
begin
  Pos := Position;
  try
    Result := ReadTagName;
  finally
    Position := Pos;
  end;
end;

function TInstantXMLProcessor.ReadChar: Char;
begin
  Reader.Read(Result, SizeOf(Char));
end;

function TInstantXMLProcessor.ReadData: string;
begin
  Result := '';
  while not (PeekChar = InstantTagStart) do
  begin
    Result := Result + ReadEscapedChar;
  end;
end;

procedure TInstantXMLProcessor.ReadData(Stream: TStream);
var
  C: Char;
  CharSize: Integer;
begin
  CheckToken(xtData);
  CharSize := SizeOf(Char);
  while not (PeekChar = InstantTagStart) do
  begin
    C := ReadEscapedChar;
    Stream.Write(C, CharSize);
  end;
end;

function TInstantXMLProcessor.ReadEscapedChar: Char;

  procedure UnEscape;
  var
    S: string;
  begin
    S := '';
    Result := ReadChar;
    while Result <> ';' do
    begin
      S := S + Result;
      Result := ReadChar;
    end;
    if S[1] = '#' then
      Result := Char(StrToInt(Copy(S, 2, Length(S) - 1)))
    else if S = 'quot' then
      Result := #34
    else if S = 'amp' then
      Result := #38
    else if S = 'apos' then
      Result := #39
    else if S = 'lt' then
      Result := #60
    else if S = 'gt' then
      Result := #62;
  end;

begin
  Result := ReadChar;
  if Result = '&' then
    UnEscape;
end;

function TInstantXMLProcessor.ReadTag: string;
var
  C: Char;
  Pos: Integer;
begin
  Pos := Position;
  try
    SkipBlanks;
    CheckToken(xtAnyTag);
  except
    Position := Pos;
    raise;
  end;
  Result := '';
  repeat
    C := ReadChar;
    Result := Result + C;
  until (C = InstantTagEnd) or (C = ' ');
  // Skip attributes.
  if C = ' ' then
  begin
    Delete(Result, Length(Result), 1);
    Result := Result + InstantTagEnd;
    repeat
      C := ReadChar;
    until C = InstantTagEnd;
  end;
end;

function TInstantXMLProcessor.ReadTagName: string;
begin
  Result := ReadTag;
  Result := Copy(Result, 2, Length(Result) - 2);
end;

procedure TInstantXMLProcessor.SetPosition(const Value: Integer);
begin
  Reader.Position := Value;
end;

procedure TInstantXMLProcessor.Skip;
var
  TagName, StartTag, EndTag: string;
  Level: Integer;
begin
  TagName := ReadTagName;
  StartTag := InstantBuildStartTag(TagName);
  EndTag := InstantBuildEndTag(TagName);
  Level := 1;
  repeat
    if Token = xtAnyTag then
    begin
      TagName := ReadTag;
      if SameText(TagName, StartTag) then
        Inc(Level)
      else if SameText(TagName, EndTag) then
        Dec(Level);
    end else
      ReadData;
  until (TagName = EndTag) and (Level = 0);
end;

procedure TInstantXMLProcessor.SkipBlanks;
begin
  while InstantCharInSet(PeekChar, [#0..#32]) do
    ReadChar;
end;

procedure TInstantXMLProcessor.SkipProcessingInstruction;
var
  LNextTagName: string;
begin
  LNextTagName := PeekTagName;
  if IsProcessingInstruction(LNextTagName) then
    ReadTag;
end;

function TInstantXMLProcessor.IsProcessingInstruction(const ATagName: string): Boolean;
begin
  Result := (ATagName <> '') and (ATagName[1] = InstantProcessingInstructionStart);
end;

{ TInstantConverter }

procedure TInstantConverter.Convert;
begin
  InternalConvert;
end;

procedure TInstantConverter.ConvertProperties;
begin
  InternalConvertProperties;
end;

destructor TInstantConverter.Destroy;
begin
  FObjectClassList.Free;
  inherited;
end;

function TInstantConverter.GetObjectClass: TClass;
begin
  Result := TClass(ObjectClassList.Last);
end;

function TInstantConverter.GetObjectClassList: TList;
begin
  if not Assigned(FObjectClassList) then
    FObjectClassList := TList.Create;
  Result := FObjectClassList;
end;

function TInstantConverter.GetObjectClassName: string;
begin
  if Assigned(ObjectClass) then
    Result := ObjectClass.ClassName
  else
    Result := '';
end;

procedure TInstantConverter.PopObjectClass;
begin
  with ObjectClassList do
    Delete(Pred(Count));
end;

procedure TInstantConverter.PushObjectClass(AObjectClass: TClass);
begin
  ObjectClassList.Add(AObjectClass);
end;

{ TInstantBinaryToTextConverter }

constructor TInstantBinaryToTextConverter.Create(Input, Output: TStream);
begin
  inherited Create;
  FProducer := TInstantXMLProducer.Create(Output);
  FReader := TInstantReader.Create(Input);
end;

destructor TInstantBinaryToTextConverter.Destroy;
begin
  FProducer.Free;
  FReader.Free;
  inherited;
end;

function TInstantBinaryToTextConverter.GetInput: TStream;
begin
  Result := Reader.Stream;
end;

function TInstantBinaryToTextConverter.GetOutput: TStream;
begin
  Result := Producer.Stream;
end;

procedure TInstantBinaryToTextConverter.InternalConvert;
var
  LClass: TPersistentClass;
  LFirstClass: Boolean;
begin
  LClass := FindClass(Reader.ReadStr);
  LFirstClass := ObjectClassList.Count = 0;
  PushObjectClass(LClass);
  try
    if LFirstClass then
      Producer.WritePrologue;
    Producer.WriteStartTag(ObjectClassName);
    if ObjectClass.InheritsFrom(TInstantStreamable) then
      TInstantStreamableClass(ObjectClass).ConvertToText(Self)
    else if ObjectClass.InheritsFrom(TInstantCollection) then
      TInstantCollectionClass(ObjectClass).ConvertToText(Self)
    else if ObjectClass.InheritsFrom(TInstantCollectionItem) then
      TInstantCollectionItemClass(ObjectClass).ConvertToText(Self);
    Reader.ReadListEnd;
    Producer.WriteEndTag;
  finally
    PopObjectClass;
  end;
end;

procedure TInstantBinaryToTextConverter.InternalConvertProperties;

  procedure ConvertPropertyValue;
  var
    S, SetStr: string;
  begin
    case Reader.NextValue of
      vaInt8, vaInt16, vaInt32:
        Producer.WriteData(IntToStr(Reader.ReadInteger));
      vaExtended:
        Producer.WriteData(FloatToStr(Reader.ReadFloat));
      vaCurrency:
        Producer.WriteData(CurrToStr(Reader.ReadCurrency));
      vaDate:
        Producer.WriteData(InstantDateTimeToStr(Reader.ReadDate));
      vaString, vaLString, vaUTF8String, vaWString:
        Producer.WriteEscapedData(Reader.ReadString);
      vaSet:
        begin
          Reader.ReadValue;
          SetStr := '';
          repeat
            S := Reader.ReadStr;
            if S <> '' then
            begin
              if SetStr <> '' then
                SetStr := SetStr + ', ';
              SetStr := SetStr + S;
            end;
          until S = '';
          Producer.WriteEscapedData(SetStr);
        end;
(*
      vaList: //CB:
        begin
          Reader.ReadListBegin;
          while not Reader.EndOfList do
          begin
            Producer.WriteStartTag('ListItem');
            ConvertPropertyValue;
            Producer.WriteEndTag;
          end;
          Reader.ReadListEnd;
        end;
*)
      vaIdent:
        Producer.WriteEscapedData(Reader.ReadIdent);
      vaFalse:
        begin
          Reader.ReadValue;
          Producer.WriteData(InstantFalseString);
        end;
      vaTrue:
        begin
          Reader.ReadValue;
          Producer.WriteData(InstantTrueString);
        end;
      else
        raise EInstantStreamError.Create(SInvalidValueType);
      end;
  end;

begin
  while not Reader.EndOfList do
  begin
    Producer.WriteStartTag(Reader.ReadStr);
    ConvertPropertyValue;
    Producer.WriteEndTag;
  end;
  Reader.ReadListEnd;
end;

{ TInstantToTextToBinaryConverter }

procedure TInstantTextToBinaryConverter.ConvertProperties(
  const StopTag: string);
begin
  DoConvertProperties(StopTag);
end;

constructor TInstantTextToBinaryConverter.Create(Input, Output: TStream);
begin
  inherited Create;
  FProcessor := TInstantXMLProcessor.Create(Input);
  FWriter := TInstantWriter.Create(Output);
end;

destructor TInstantTextToBinaryConverter.Destroy;
begin
  FProcessor.Free;
  FWriter.Free;
  inherited;
end;

procedure TInstantTextToBinaryConverter.DoConvertProperties(
  const StopTag: string);

(*  procedure ConvertOrdValue(PropType: PTypeInfo; Value: Integer);
  begin
    case PropType^.Kind of
      tkInteger:
        Writer.WriteInteger(Value);
      tkChar:
        Writer.WriteChar(Chr(Value));
      tkEnumeration:
        Writer.WriteIdent(GetEnumName(PropType, Value));
    end;
  end;
*)
  procedure ConvertProperty(PropInfo: PPropInfo);
  var
    I: Integer;
    PropName, ValueStr: string;
    S: TStringList;
  begin
    PropName := Processor.ReadTagName;
    ValueStr := Processor.ReadData;
    Writer.WriteUTF8Str(PropName);
    case GetTypeInfo(PropInfo)^.Kind of //PropInfo^.PropType^^.Kind of
      tkInteger:
        Writer.WriteInteger(StrToInt(ValueStr));
      tkFloat:
      begin
        if GetTypeData(GetTypeInfo(PropInfo)(*PropInfo^.PropType^*)).FloatType = ftCurr then
          Writer.WriteCurrency(StrToCurr(ValueStr))
        else
          Writer.WriteFloat(StrToFloat(ValueStr));
      end;
      tkString, tkLString, tkChar:
        Writer.WriteString(ValueStr);
      tkUString:
        Writer.WriteString(ValueStr);
      tkEnumeration:
        Writer.WriteIdent(ValueStr);
      tkSet:
        begin
          Writer.WriteValue(vaSet);
          S := TStringList.Create;
          try
            InstantStrToList(ValueStr, S, [',']);
            for I := 0 to Pred(S.Count) do
              Writer.WriteUTF8Str(S[I]);
          finally
            S.Free;
          end;
          Writer.WriteStr('');
       end;
    else
      raise EInstantStreamError.Create(SInvalidValueType);
    end;
    Processor.ReadTag;
  end;

var
  PropInfo: PPropInfo;
  TagName, PropertiesEnd: string;
begin
  if StopTag = '' then
    PropertiesEnd := InstantBuildEndTag(ObjectClassName)
  else
    PropertiesEnd := StopTag;
  while not SameText(Processor.PeekTag, PropertiesEnd) do
  begin
    TagName := Processor.PeekTagName;
    PropInfo := InstantGetPropInfo(ObjectClass, TagName);
    if Assigned(PropInfo) then
      ConvertProperty(PropInfo)
    else
      Processor.Skip;
  end;
  Writer.WriteListEnd;
end;

function TInstantTextToBinaryConverter.GetInput: TStream;
begin
  Result := Processor.Stream;
end;

function TInstantTextToBinaryConverter.GetOutput: TStream;
begin
  Result := Writer.Stream;
end;

procedure TInstantTextToBinaryConverter.InternalConvert;
begin
  PushObjectClass(FindClass(Processor.ReadTagName));
  try
    Writer.WriteUTF8Str(ObjectClassName);
    if ObjectClass.InheritsFrom(TInstantStreamable) then
      TInstantStreamableClass(ObjectClass).ConvertToBinary(Self)
    else if ObjectClass.InheritsFrom(TInstantCollection) then
      TInstantCollectionClass(ObjectClass).ConvertToBinary(Self)
    else if ObjectClass.InheritsFrom(TInstantCollectionItem) then
      TInstantCollectionItemClass(ObjectClass).ConvertToBinary(Self);
    Processor.ReadTag;
    Writer.WriteListEnd;
  finally
    PopObjectClass;
  end;
end;

procedure TInstantTextToBinaryConverter.InternalConvertProperties;
begin
  DoConvertProperties('');
end;

{ TInstantAbstractObject }

function TInstantAbstractObject.GetConnector: TComponent;
begin
  Result := FConnector;
end;

procedure TInstantAbstractObject.SetConnector(AConnector: TComponent);
begin
  FConnector := AConnector;
end;

{ TInstantAbstractAttribute }

function TInstantAbstractAttribute.GetMetadata: TInstantCollectionItem;
begin
  Result := FMetadata;
end;

function TInstantAbstractAttribute.GetOwner: TInstantAbstractObject;
begin
  Result := FOwner;
end;

procedure TInstantAbstractAttribute.SetMetadata(
  AMetadata: TInstantCollectionItem);
begin
  FMetadata := AMetadata;
  Initialize;
end;

procedure TInstantAbstractAttribute.SetOwner(AOwner: TInstantAbstractObject);
begin
  FOwner := AOwner;
end;

constructor TInstantAbstractAttribute.Create(AOwner: TInstantAbstractObject;
    AMetadata: TInstantCollectionItem);
begin
  inherited Create;
  FOwner := AOwner;
  SetMetadata(AMetadata);
end;

end.

