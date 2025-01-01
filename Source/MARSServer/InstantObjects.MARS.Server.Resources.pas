(*
 *   InstantObject with MARS Curiosity REST Library
 *   Server.Resources
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
unit InstantObjects.MARS.Server.Resources;

interface

uses
  //Delphi
  Classes
  
  , System.SysUtils
  , Generics.Collections
  , System.JSON
  //MARS
  , MARS.Core.Registry
  , MARS.Core.Attributes
  , MARS.Core.MediaType
  , MARS.Core.JSON
  , MARS.Core.MessageBodyWriters
  , MARS.Core.MessageBodyReaders
  , InstantObjects.MARS.Data
  , MARS.Core.RequestAndResponse.Interfaces
  , MARS.Metadata.Attributes
  , InstantObjects.MARS.Server.Resources.Base
  //InstantObjects
  , InstantPersistence
;

type
  //metadata
  [Path('/classmetadata')
    , Produces(TMediaType.TEXT_PLAIN)
    {$IFNDEF BYPASS_TOKEN}, RolesAllowed('standard'){$ENDIF}
    ]
  TInstantObjectMetadataResource = class(TBaseResource)
  public
    [GET, Path('/Model')]
    function RetrieveModel: string;
    [GET, Path('/{AClassName}')]
    function RetrieveMetadata([PathParam] AClassName: string): string;
  end;

  //Servizi di query generici
  [Path('/data'), Produces(TMediaType.APPLICATION_JSON)]
  [MetaDescription('Resturns object data of a resource')]
  TInstantObjectResource = class(TBaseResource)
  public
    [GET, Path('/{AClassName}/{AId}')
    {$IFNDEF BYPASS_TOKEN}, RolesAllowed('reader'){$ENDIF}]
    function RetrieveInstantObject([PathParam] AClassName: string;
      [PathParam] AId: string): TInstantObject; override;

    [GET, Path('/{AClassName}')
    {$IFNDEF BYPASS_TOKEN}, RolesAllowed('reader'){$ENDIF}]
    function RetrieveInstantObjectList([PathParam] AClassName: string;
      [QueryParam] Where: string = '';
      [QueryParam] OrderBy: string = '';
      [QueryParam] ChangedFrom: string = '';
      [QueryParam] ChangedTo: string = '';
      [QueryParam] PageCount: integer = 0;
      [QueryParam] RecordCount: integer = 0):
        TInstantObjectList<TInstantObject>; override;

    [POST, Path('/{AClassName}/{AId}')
    , Consumes(TMediaType.APPLICATION_JSON)
    , Produces(TMediaType.APPLICATION_JSON)
    {$IFNDEF BYPASS_TOKEN}, RolesAllowed('standard'){$ENDIF}]
    function Post([PathParam] const AId: string;
      [BodyParam] AObject: TInstantObject;
      const ACheckIfExists: boolean = True): TInstantObject; override;

    [PUT, Path('/{AClassName}/{AId}')
    , Consumes(TMediaType.APPLICATION_JSON)
    , Produces(TMediaType.APPLICATION_JSON)
    {$IFNDEF BYPASS_TOKEN}, RolesAllowed('standard'){$ENDIF}]
    function Put([PathParam] const AId: string;
      [BodyParam] AObject: TInstantObject): TInstantObject; override;

    [DELETE, Path('/{AClassName}/{AId}')
    , Produces(TMediaType.APPLICATION_JSON)
    {$IFNDEF BYPASS_TOKEN}, RolesAllowed('standard'){$ENDIF}]
    function Delete([PathParam] AClassName: string;
      [PathParam] AId: string): TInstantObject; override;
  end;

  [Path('/keys'), Produces(TMediaType.APPLICATION_JSON)
    {$IFNDEF BYPASS_TOKEN}, RolesAllowed('reader'){$ENDIF}
    ]
  [MetaDescription('Resturns object keys of a resource')]
  TInstantObjectReferencesResource = class(TBaseResource)
  public
    [GET, Path('/{AClassName}')]
    function RetrieveInstantObjectReferenceList([PathParam] AClassName: string;
      [QueryParam] Where: string = '';
      [QueryParam] OrderBy: string = '';
      [QueryParam] ChangedFrom: string = '';
      [QueryParam] ChangedTo: string = '';
      [QueryParam] PageCount: integer = 0;
      [QueryParam] RecordCount: integer = 0): TList<TInstantObjectReference>; override;
  end;

  [Path('/count'), Produces(TMediaType.APPLICATION_JSON)
    {$IFNDEF BYPASS_TOKEN}, RolesAllowed('reader'){$ENDIF}
    ]
  [MetaDescription('Resturns objects count of a resource')]
  TInstantObjectCountResource = class(TBaseResource)
  public
    [GET, Path('/{AClassName}')]
    function GetInstantObjectReferenceCount([PathParam] AClassName: string;
      [QueryParam] Where: string = '';
      [QueryParam] OrderBy: string = '';
      [QueryParam] ChangedFrom: string = '';
      [QueryParam] ChangedTo: string = ''): Integer; override;
  end;

implementation

uses
  //Delphi
  System.StrUtils
  , System.TypInfo
  , System.Variants
  //MARS
  , MARS.Core.Exceptions
  , MARS.Data.FireDAC
  //MARS Server
  , InstantObjects.MARS.Server.Consts
  , InstantObjects.MARS.Server.Exceptions
  //InstantObject
  , InstantFireDAC
  , InstantClasses
  , InstantMetaData
  ;

{ TInstantObjectMetadataResource }

function TInstantObjectMetadataResource.RetrieveMetadata(AClassName: string): string;
var
  LIOMetadata: TInstantClassMetadata;
  LStringStream: TStringStream;
begin
  LIOMetadata := InstantFindClassMetadata(AClassName);
  if Assigned(LIOMetadata) then
  begin
    LStringStream := TStringStream.Create;
    try
      InstantWriteObject(LStringStream, sfJSON, LIOMetadata);
      Result := LStringStream.DataString;
    finally
      LStringStream.Free;
    end;
  end
  else
  begin
    raise EMARSServerException.CreateError(http_404_NotFound,
      Format('Metadata for Class %s not found', [AClassName]));

  end;
end;

function TInstantObjectMetadataResource.RetrieveModel: string;
var
  LIOMetadatas: TInstantClassMetadatas;
  LStringStream: TStringStream;
  LClassName: string;
begin
  LIOMetadatas := InstantModel.ClassMetadatas;
  if Assigned(LIOMetadatas) then
  begin
    LStringStream := TStringStream.Create;
    try
      InstantWriteObject(LStringStream, sfJSON, LIOMetadatas);
      Result := LStringStream.DataString;
    finally
      LStringStream.Free;
    end;
  end
  else
  begin
    raise EMARSServerException.CreateError(http_404_NotFound,
      Format('Metadata for Class %s not found', [LClassName]));
  end;
end;

{ TInstantObjectResource }

function TInstantObjectResource.Delete(AClassName, AId: string): TInstantObject;
begin
  Result := inherited Delete(AClassName, AId);
end;

function TInstantObjectResource.Post(const AId: string;
  AObject: TInstantObject;
  const ACheckIfExists: boolean = True): TInstantObject;
begin
  Result := inherited Post(AId, AObject, ACheckIfExists);
end;

function TInstantObjectResource.Put(const AId: string;
  AObject: TInstantObject): TInstantObject;
begin
  Result := inherited Put(AId, AObject);
end;

function TInstantObjectResource.RetrieveInstantObject(AClassName, AId: string): TInstantObject;
begin
  Result := inherited RetrieveInstantObject(AClassName, AId);
end;

function TInstantObjectResource.RetrieveInstantObjectList(AClassName, Where,
  OrderBy, ChangedFrom, ChangedTo: string;
  PageCount, RecordCount : integer): TInstantObjectList<TInstantObject>;
begin
  Result := inherited RetrieveInstantObjectList(AClassName, Where, OrderBy,
    ChangedFrom, ChangedTo, PageCount, RecordCount);
end;

{ TInstantObjectReferencesResource }

function TInstantObjectReferencesResource.RetrieveInstantObjectReferenceList(
  AClassName, Where, OrderBy,
  ChangedFrom, ChangedTo: string;
  PageCount, RecordCount : integer): TList<TInstantObjectReference>;
begin
  Result := inherited RetrieveInstantObjectReferenceList(AClassName,
    Where, OrderBy, ChangedFrom, ChangedTo,
    PageCount, RecordCount);
end;

{ TInstantObjectCountResource }

function TInstantObjectCountResource.GetInstantObjectReferenceCount(
  AClassName, Where, OrderBy, ChangedFrom, ChangedTo: string): Integer;
begin
  Result := inherited GetInstantObjectReferenceCount(AClassName,
    Where, OrderBy, ChangedFrom, ChangedTo);
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<TInstantObjectMetadataResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TInstantObjectResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TInstantObjectReferencesResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TInstantObjectCountResource>;

end.
