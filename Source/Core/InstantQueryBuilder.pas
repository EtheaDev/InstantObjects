(*
 *   InstantObject
 *   InstantQueryBuilder
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
 * The Initial Developer of the Original Code is: Carlo Barazzetta
 *
 * Contributor(s):
 * Carlo Barazzetta, Nicola Tambascia
 *
 * ***** END LICENSE BLOCK ***** *)
unit InstantQueryBuilder;

interface

uses
  //Delphi
  System.SysUtils
  , System.Classes
  //InstantObjects
  , InstantClasses
  , InstantBrokers
  , InstantPersistence
  ;

type
  TInstantQueryBuilder = class
  private
    FInstantObjectClass: TInstantObjectClass;
    FWhereClause: string;
    FOrderByClause: string;
    FChangedFrom: TDateTime;
    FChangedTo: TDateTime;
    FQuery: TInstantSQLQuery;
    FPageCount: integer;
    FRecordCount: integer;
    procedure SetInstantObjectClass(AValue: TInstantObjectClass);
    procedure SetWhere(Value: string);
    procedure SetOrderByClause(Value: string);
    procedure SetChangedFrom(Value: TDateTime);
    function GetQuery: TInstantSQLQuery;
    procedure CalcCommand;
    procedure SetChangedTo(const Value: TDateTime);
  public
    constructor Create(AConnector: TInstantConnector); overload;
    destructor Destroy; override;
    procedure UpdateCommand(const AClass: TInstantObjectClass;
      const AWhere, AOrderBy: string;
      const AChangedFrom, AChangedTo: TDateTime;
      PageCount, RecordCount : integer); overload;
    procedure UpdateCommand(const AClass: TInstantObjectClass;
      const AWhere, AOrderBy,
      AChangedFrom, AChangedTo: string;
      PageCount, RecordCount : integer); overload;
    property InstantObjectClass: TInstantObjectClass read FInstantObjectClass write SetInstantObjectClass;
    property WhereClause: string read FWhereClause write SetWhere;
    property OrderByClause: string read FOrderByClause write SetOrderByClause;
    property ChangedFrom: TDateTime read FChangedFrom write SetChangedFrom;
    property ChangedTo: TDateTime read FChangedTo write SetChangedTo;
    property PageCount: integer read FPageCount write FPageCount;
    property RecordCount: integer read FRecordCount write FRecordCount;
    property Query: TInstantSQLQuery read GetQuery;
  end;

implementation

uses
  System.DateUtils
  , InstantObjects.MARS.Server.Resources.Base
  ;

{ TInstantQueryBuilder }

procedure TInstantQueryBuilder.CalcCommand;
var
  LCommandText : string;
  LWhereCommand: string;
  IOClassName: string;
  LPosOfChangedFromParam, LPosOfChangedToParam: Integer;
begin
  Assert(Assigned(FInstantObjectClass));
  IOClassName := FInstantObjectClass.ClassName;

  LCommandText := Format('SELECT * FROM %s',[IOClassName]);

  //Composizione WHERE
  LWhereCommand := '';
  if WhereClause <> '' then
    LWhereCommand := Format('(%s)', [WhereClause]);

  LPosOfChangedFromParam := Pos(':ChangedFrom', LWhereCommand);
  if (LPosOfChangedFromParam <= 0) and (ChangedFrom <> 0) then
  begin
    if LWhereCommand <> '' then
      LWhereCommand := LWhereCommand + ' AND ';
    LWhereCommand := LWhereCommand + '(UpdateTimeStamp >= :ChangedFrom)';
  end;

  LPosOfChangedToParam := Pos(':ChangedTo', LWhereCommand);
  if (LPosOfChangedToParam <= 0) and (ChangedTo <> 0) then
  begin
    if LWhereCommand <> '' then
      LWhereCommand := LWhereCommand + ' AND ';
    LWhereCommand := LWhereCommand + ' (UpdateTimeStamp <= :ChangedTo)';
  end;

  //per la paginazione SQL è neccessario la order by
  if RecordCount <> 0 then
  begin
    if OrderByClause = '' then
      OrderByClause := 'Id';
  end;

  if LWhereCommand <> '' then
    LCommandText := LCommandText + Format(' WHERE %s', [LWhereCommand]);

  if OrderByClause <> '' then
    LCommandText := LCommandText + Format(' ORDER BY %s', [OrderByClause]);

  FQuery.Command := LCommandText;

  FQuery.FetchParams(FQuery.Command, FQuery.Params);

  if Pos(':ChangedFrom', LWhereCommand) > 0 then
    FQuery.Params.ParamByName('ChangedFrom').AsDateTime := ChangedFrom;

  if Pos(':ChangedTo', LWhereCommand) > 0  then
    FQuery.Params.ParamByName('ChangedTo').AsDateTime := ChangedTo;

  //aggiungo PageCount e RecordCount per la paginazione
  FQuery.PageCount := PageCount;
  FQuery.RecordCount := RecordCount;
end;

constructor TInstantQueryBuilder.Create(AConnector: TInstantConnector);
begin
  FQuery := AConnector.CreateQuery as TInstantSQLQuery;
end;

destructor TInstantQueryBuilder.Destroy;
begin
  FQuery.Free;
  inherited;
end;

function TInstantQueryBuilder.GetQuery: TInstantSQLQuery;
begin
  Result := FQuery;
end;

procedure TInstantQueryBuilder.SetInstantObjectClass(AValue: TInstantObjectClass);
begin
  if FInstantObjectClass <> AValue then
  begin
    FInstantObjectClass := AValue;
    CalcCommand;
  end;
end;

procedure TInstantQueryBuilder.SetChangedFrom(Value: TDateTime);
begin
  if FChangedFrom <> Value then
  begin
    FChangedFrom := Value;
    CalcCommand;
  end;
end;

procedure TInstantQueryBuilder.SetChangedTo(const Value: TDateTime);
begin
  if FChangedTo <> Value then
  begin
    FChangedTo := Value;
    CalcCommand;
  end;
end;

procedure TInstantQueryBuilder.SetOrderByClause(Value: string);
begin
  if FOrderByClause <> Value then
  begin
    FOrderByClause := Value;
    CalcCommand;
  end;
end;

procedure TInstantQueryBuilder.SetWhere(Value: string);
begin
  if FWhereClause <> Value then
  begin
    FWhereClause := Value;
    CalcCommand;
  end;
end;

procedure TInstantQueryBuilder.UpdateCommand(
  const AClass: TInstantObjectClass;
  const AWhere, AOrderBy: string;
  const AChangedFrom, AChangedTo: TDateTime;
  PageCount, RecordCount : integer);
begin
  FInstantObjectClass := AClass;
  FWhereClause := AWhere;
  FOrderByClause := AOrderBy;
  FChangedFrom := AChangedFrom;
  FChangedTo := AChangedTo;
  FPageCount := PageCount;
  FRecordCount := RecordCount;
  if (FChangedFrom <> 0) and (AOrderBy = '') then
    FOrderByClause := 'Id';
  CalcCommand;
end;

procedure TInstantQueryBuilder.UpdateCommand(
  const AClass: TInstantObjectClass;
  const AWhere, AOrderBy,
  AChangedFrom, AChangedTo: string;
  PageCount, RecordCount : integer);
var
  LChangedFrom, LChangedTo: TDateTime;
begin
  if AChangedFrom <> '' then
    LChangedFrom := ISO8601ToDate(AChangedFrom)
  else
    LChangedFrom := 0;
  if AChangedTo <> '' then
    LChangedTo := ISO8601ToDate(AChangedTo)
  else
    LChangedTo := 0;
  UpdateCommand(AClass, AWhere, AOrderBy,
    LChangedFrom, LChangedTo,
    PageCount, RecordCount);
end;

end.
