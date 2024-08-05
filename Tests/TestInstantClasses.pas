(*
 *   InstantObjects Test Suite
 *   TestInstantClasses
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
 * The Original Code is: InstantObjects Test Suite/TestInstantClasses
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

unit TestInstantClasses;

{$IFDEF LINUX64}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

interface
uses
  Classes, SysUtils,
  InstantClasses,
  {$IFNDEF DUNITX_TESTS}testregistry, fpcunit,{$ELSE}InstantTest,{$ENDIF}
  DUnitX.TestFramework;

type
  [TestFixture]
  TInstantGuineaPig = class(TInstantCollectionItem)
  private
    FPigName: string;
    FWeight: double;
    FAge: integer;
    procedure SetPigName(const Value: string);
    procedure SetAge(const Value: integer);
    procedure SetWeight(const Value: double);
  public
    constructor Create(Collection: TCollection); override;
  published
    property Age: integer read FAge write SetAge;
    property Weight: double read FWeight write SetWeight;
    property PigName: string read FPigName write SetPigName;
  end;


  { TTestInstantClasses }
  [TestFixture]
  TTestInstantClasses = class({$IFNDEF DUNITX_TESTS}TTestCase{$ELSE}TInstantTestCase{$ENDIF})
  published
    [Test]
    procedure TestInstantCollection;
    procedure TestInstantWriter;
    procedure TestInstantReader;
    procedure TestInstantReadWriteProperty;
    procedure TestInstantReadWriteClass;
    procedure TestInstantReadWriteObjectToStream;
    procedure TestInstantConverters;
    procedure TestInstantXMLProducer;
  end;

implementation

uses
  StrUtils, InstantConsts;

{ TTestInstantClasses }

procedure TTestInstantClasses.TestInstantCollection;
var
  i: TInstantCollectionItem;
  c: TInstantCollection;
begin
  c := TInstantCollection.Create(TInstantCollectionItem);
  try
    AssertNotNull(c);
    AssertEquals(0, c.Count);
    i := c.add as TInstantCollectionItem;
    AssertEquals(1, c.Count);
    i.Name := 'pippo';
    AssertTrue(i = c.Find('pippo'));
  finally
    c.Free;
  end;
end;

procedure TTestInstantClasses.TestInstantWriter;
var
  ms: TStringStream;
  iw: TInstantWriter;
  goofy, mickeymouse: string;
begin
  goofy := 'goofy';
  mickeymouse := 'mickeymouse';

  // string (long strings)
  ms := TStringStream.Create('');
  try
    iw := TInstantWriter.Create(ms);
    try
      iw.WriteString(goofy);
      iw.WriteString(mickeymouse);
      iw.FlushBuffer;
      AssertEquals(20, ms.Position);
      AssertEquals(#6#5 + goofy + #6#11 + mickeymouse, ms.DataString);
    finally
      iw.Free;
    end;
  finally
    ms.Free;
  end;

  // str (short strings only)
  ms := TStringStream.Create('');
  try
    iw := TInstantWriter.Create(ms);
    try
      iw.WriteStr('DonaldDuck');
      iw.FlushBuffer;
      AssertEquals(11, ms.Position);
      AssertEquals(#10'DonaldDuck', ms.DataString);
    finally
      iw.Free;
    end;
  finally
    ms.Free;
  end;

  // boolean
  ms := TStringStream.Create('');
  try
    iw := TInstantWriter.Create(ms);
    try
      iw.WriteBoolean(False);
      iw.FlushBuffer;
      AssertEquals(1, ms.Position);
      AssertEquals('8', IntToStr(Ord(ms.DataString[1])));
    finally
      iw.Free;
    end;
  finally
    ms.Free;
  end;

  // float (check first 4 bytes only)
  ms := TStringStream.Create('');
  try
    iw := TInstantWriter.Create(ms);
    try
      iw.WriteFloat(3.14);
      iw.FlushBuffer;
      AssertEquals(11, ms.Position);
      {$IFNDEF WIN64}
      AssertEquals('5', IntToStr(Ord(ms.DataString[1])));
      AssertEquals('195', IntToStr(Ord(ms.DataString[2])));
      AssertEquals('245', IntToStr(Ord(ms.DataString[3])));
      AssertEquals('40', IntToStr(Ord(ms.DataString[4])));
      {$ELSE}
      AssertEquals('5', IntToStr(Ord(ms.DataString[1])));
      AssertEquals('0', IntToStr(Ord(ms.DataString[2])));
      AssertEquals('248', IntToStr(Ord(ms.DataString[3])));
      AssertEquals('40', IntToStr(Ord(ms.DataString[4])));
      {$ENDIF}

    finally
      iw.Free;
    end;
  finally
    ms.Free;
  end;

  // integer
  ms := TStringStream.Create('');
  try
    iw := TInstantWriter.Create(ms);
    try
      iw.WriteInteger(123);
      iw.FlushBuffer;
      AssertEquals(2, ms.Position);
      AssertEquals('2', IntToStr(Ord(ms.DataString[1])));
      AssertEquals('123', IntToStr(Ord(ms.DataString[2])));
    finally
      iw.Free;
    end;
  finally
    ms.Free;
  end;

  // integer long

{ TODO : UNICODE: this fails. should be reworked }
  {ms := TStringStream.Create;
  try
    iw := TInstantWriter.Create(ms);
    try
      iw.WriteInteger(1234567);
      iw.FlushBuffer;
      AssertEquals(5, ms.Position);
      AssertEquals('4', IntToStr(Ord(ms.DataString[1])));
      AssertEquals('135', IntToStr(Ord(ms.DataString[2])));
      AssertEquals('214', IntToStr(Ord(ms.DataString[3])));
      AssertEquals('18', IntToStr(Ord(ms.DataString[4])));
      AssertEquals('0', IntToStr(Ord(ms.DataString[5])));
    finally
      iw.Free;
    end;
  finally
    ms.Free;
  end;}
end;

procedure TTestInstantClasses.TestInstantReader;
var
  ms: TStringStream;
  ir: TInstantReader;
  iw: TInstantWriter;
  s: string;
begin
  s := '';
  ms := TStringStream.Create(s);
  ir := TInstantReader.Create(ms);
  iw := TInstantWriter.Create(ms);
  try
    // string
    iw.WriteString('goofy');
    iw.WriteString('mickeymouse');
    iw.FlushBuffer;
    ms.Position := 0;
    AssertEquals('goofy', ir.ReadString);
    AssertEquals('mickeymouse', ir.ReadString);
    ms.Size := 0;

    //str
    iw.Position := 0;
    iw.WriteStr('DonaldDuck');
    iw.FlushBuffer;
    ms.Position := 0;
    AssertEquals('DonaldDuck', ir.ReadStr);
    ms.Size := 0;

    //boolean
    iw.Position := 0;
    iw.WriteBoolean(False);
    iw.FlushBuffer;
    ms.Position := 0;
    AssertEquals(False, ir.ReadBoolean);
    ms.Size := 0;

    //float (controllo solo i primi 4 bytes)
    iw.Position := 0;
    iw.WriteFloat(3.14);
    iw.FlushBuffer;
    ms.Position := 0;
    AssertEquals(3.14, ir.ReadFloat);
    ms.Size := 0;

  finally
    iw.Free;
    ir.Free;
    ms.Free;
  end;
end;

procedure TTestInstantClasses.TestInstantReadWriteProperty;
var
  ms: TStringStream;
  ir: TInstantReader;
  iw: TInstantWriter;
  s: string;
  c: TInstantGuineaPig;
begin
  s := '';
  ms := TStringStream.Create(s);
  ir := TInstantReader.Create(ms);
  iw := TInstantWriter.Create(ms);
  c := TInstantGuineaPig.Create(nil);
  try
    iw.WriteProperties(c);
    iw.FlushBuffer;
    ms.Position := 0;

    c.PigName := 'croton';
    ir.ReadProperties(c);
    AssertEquals('Miss piggy', c.PigName);
  finally
    c.Free;
    iw.Free;
    ir.Free;
    ms.Free;
  end;
end;

procedure TTestInstantClasses.TestInstantReadWriteClass;
var
  ms: TStringStream;
  ir: TInstantReader;
  iw: TInstantWriter;
  s: string;
  c: TInstantGuineaPig;
begin
  s := '';
  ms := TStringStream.Create(s);
  ir := TInstantReader.Create(ms);
  iw := TInstantWriter.Create(ms);
  c := TInstantGuineaPig.Create(nil);
  try
    c.PigName := 'AZazòèìù !$';
    c.Age := 123456;
    c.Weight := -1.2345789;

    //write class
    iw.WriteObject(c);
    iw.FlushBuffer;
    
    c.PigName := '';
    c.Age := 0;
    c.Weight := 0;
    ms.Position := 0;

    ir.ReadObject(c);
    AssertEquals('AZazòèìù !$', c.PigName);
    AssertEquals(123456, c.Age);
    AssertEquals(-1.2345789, c.Weight);

  finally
    c.Free;
    iw.Free;
    ir.Free;
    ms.Free;
  end;
end;

procedure TTestInstantClasses.TestInstantReadWriteObjectToStream;
var
  ms: TStringStream;
  ir: TInstantReader;
  iw: TInstantWriter;
  s: string;
  c: TInstantGuineaPig;
begin
  s := '';
  ms := TStringStream.Create(s);
  ir := TInstantReader.Create(ms);
  iw := TInstantWriter.Create(ms);
  c := TInstantGuineaPig.Create(nil);
  try
    c.PigName := 'AZazòèìù !$';
    c.Age := 123456;
    c.Weight := -1.2345789;

    //write class
    InstantWriteObjectToStream(ms, c, nil);
    ms.Position := 0;

    c.PigName := '';
    c.Age := 0;
    c.Weight := 0;
    
    //read class
    InstantReadObjectFromStream(ms, c, nil);
    AssertEquals('AZazòèìù !$', c.PigName);
    AssertEquals(123456, c.Age);
    AssertEquals(-1.2345789, c.Weight);

  finally
    c.Free;
    iw.Free;
    ir.Free;
    ms.Free;
  end;
end;

procedure TTestInstantClasses.TestInstantConverters;
var
  ins: TStringStream;
  outs: TStringStream;
  c: TInstantGuineaPig;
  ic: TInstantBinaryToTextConverter;
  LLineBreak: string;
  LIndentation: string;
begin
  if InstantXMLIndentationSize > 0 then
    LLineBreak := sLineBreak
  else
    LLineBreak := '';
  LIndentation := DupeString(' ', InstantXMLIndentationSize);

  ins := TStringStream.Create('');
  outs := TStringStream.Create('');
  c := TInstantGuineaPig.Create(nil);
  ic := TInstantBinaryToTextConverter.Create(ins, outs);
  try
    InstantWriteObjectToStream(ins, c, nil);
    ins.Position := 0;

    AssertEquals('TInstantGuineaPig', ic.Reader.ReadStr); //la stringa con il classname
    c.ConvertToText(ic);
    ic.Producer.eof; //to flush the buffer
    AssertEquals('ConvertToText', '<Age>2</Age>' + LLineBreak + '<Weight>1' +
      FormatSettings.DecimalSeparator +
      '123</Weight>' + LLineBreak + '<PigName>Miss piggy</PigName>' + LLineBreak, outs.DataString);
  finally
    ic.Free;
    c.Free;
    ins.Free;
    outs.Free;
  end;

  ins := TStringStream.Create('');
  outs := TStringStream.Create('');
  c := TInstantGuineaPig.Create(nil);
  ic := TInstantBinaryToTextConverter.Create(ins, outs);
  try
    InstantWriteObjectToStream(ins, c, nil);
    ins.Position := 0;

    InstantObjectBinaryToText(ins, outs);
    AssertEquals('InstantObjectBinaryToText',
      InstantPrologue + LLineBreak +
      '<TInstantGuineaPig>' + LLineBreak + LIndentation + '<Age>2</Age>' + LLineBreak + LIndentation +
      '<Weight>1' + FormatSettings.DecimalSeparator +
        '123</Weight>' + LLineBreak + LIndentation +
      '<PigName>Miss piggy</PigName>' + LLineBreak + '</TInstantGuineaPig>' + LLineBreak,
      outs.DataString);
  finally
    ic.Free;
    c.Free;
    ins.Free;
    outs.Free;
  end;
end;

procedure TTestInstantClasses.TestInstantXMLProducer;
var
  ms: TStringStream;
  s: string;
  c: TInstantGuineaPig;
  LLineBreak: string;
  LIndentation: string;
begin
  if InstantXMLIndentationSize > 0 then
    LLineBreak := sLineBreak
  else
    LLineBreak := '';
  LIndentation := DupeString(' ', InstantXMLIndentationSize);

  s := '';
  ms := TStringStream.Create(s);
  c := TInstantGuineaPig.Create(nil);
  try
    InstantWriteObject(ms, sfXML, c);
    AssertEquals(Length(InstantPrologue) + 102 + (Length(LLineBreak) * 6) + (Length(LIndentation) * 3), ms.Position);

    AssertEquals('InstantWriteObject(sfXML)',
      InstantPrologue + LLineBreak +
      '<TInstantGuineaPig>' + LLineBreak + LIndentation + '<Age>2</Age>' + LLineBreak + LIndentation +
      '<Weight>1' + FormatSettings.DecimalSeparator +
        '123</Weight>' + LLineBreak + LIndentation +
      '<PigName>Miss piggy</PigName>' + LLineBreak + '</TInstantGuineaPig>' + LLineBreak,
      ms.DataString);
  finally
    c.Free;
    ms.Free;
  end;
end;


{ TInstantGuineaPig }

constructor TInstantGuineaPig.Create;
begin
  inherited Create(Collection);
  PigName := 'Miss piggy';
  Age := 2;
  Weight := 1.123;
end;

procedure TInstantGuineaPig.SetAge(const Value: integer);
begin
  FAge := Value;
end;

procedure TInstantGuineaPig.SetPigName(const Value: string);
begin
  FPigName := Value;
end;

procedure TInstantGuineaPig.SetWeight(const Value: double);
begin
  FWeight := Value;
end;

initialization
  RegisterClass(TInstantGuineaPig);

  // Register any test cases with the test runner (old version)
{$IFNDEF DUNITX_TESTS}
  RegisterTests([TTestInstantClasses]);
{$ENDIF}
end.
