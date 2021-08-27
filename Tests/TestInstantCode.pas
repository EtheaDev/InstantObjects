(*
 *   InstantObjects Test Suite
 *   TestMockBroker
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
 * The Original Code is: InstantObjects Test Suite/TestMockBroker
 *
 * The Initial Developer of the Original Code is: Uberto Barbini
 *
 * Portions created by the Initial Developer are Copyright (C) 2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Nando Dessena
 *
 * ***** END LICENSE BLOCK ***** *)

unit TestInstantCode;

{$IFDEF LINUX}
{$I '../InstantDefines.inc'}
{$ELSE}
{$I '..\InstantDefines.inc'}
{$ENDIF}

interface

uses
  InstantCode, {$IFNDEF DUNITX_TESTS}testregistry, fpcunit,{$ELSE}InstantTest,{$ENDIF} InstantMock, TestModel,
  DUnitX.TestFramework;

type
  [TestFixture]
  TTestInstantCodeModel = class(TInstantTestCase)
  published
    [Test]
    {
      This test uses TInstantCodeModel to: read the actual TestModel.pas unit
      from disk; parse it; rename class TCategory to TCategory2, and checks
      that the rename succeeded. The modified source code is not saved back to
      disk.
    }
    procedure TestRenameModelClass;
  end;

implementation

uses
  SysUtils, Classes;

{ TTestInstantCodeModel }

procedure TTestInstantCodeModel.TestRenameModelClass;
var
  LCodeModel: TInstantCodeModel;
  LFile: TStream;
  LSource: TStringStream;
  LClass: TInstantCodeClass;
  LChangeInfo: TInstantCodeClassChangeInfo;
  LSourceCode: string;
  LChangedAttrs: TStringList;
  LNewAttrs: TList;
begin
  LCodeModel := TInstantCodeModel.Create;
  try
    LFile := TFileStream.Create(ExtractFilePath(ParamStr(0)) + 'TestModel.pas', fmOpenRead);
    try
      LSource := TStringStream.Create('');
      try
        LSource.CopyFrom(LFile, LFile.Size);
        LCodeModel.LoadModule(LSource.DataString);

        AssertNotNull(LCodeModel.FindModule('TestModel'));
        AssertNotNull(LCodeModel.FindClass('TCategory'));

        LClass := LCodeModel.FindClass('TCategory');

        LChangedAttrs := TStringList.Create;
        try
          LNewAttrs := TList.Create;
          try
            LClass.Name := 'TCategory2';
            LChangeInfo := TInstantCodeClassChangeInfo.Create(LClass, ctEdit,
              'TCategory', LChangedAttrs, LNewAttrs);
            try
              LSourceCode := LSource.DataString;
              LClass.ApplyToSource(LSourceCode, LChangeInfo);

              LCodeModel.Clear;
              LCodeModel.LoadModule(LSourceCode);

              AssertNotNull('Renamed class not found', LCodeModel.FindClass('TCategory2'));
              AssertNull('Old class is still there', LCodeModel.FindClass('TCategory'));
            finally
              LChangeInfo.Free;
            end;
          finally
            LNewAttrs.Free;
          end;
        finally
          LChangedAttrs.Free;
        end;
      finally
        LSource.Free;
      end;
    finally
      LFile.Free;
    end;
  finally
    LCodeModel.Free;
  end;
end;

initialization
{$IFNDEF DUNITX_TESTS}
  RegisterTests([TTestInstantCodeModel]);
{$ENDIF}
end.
