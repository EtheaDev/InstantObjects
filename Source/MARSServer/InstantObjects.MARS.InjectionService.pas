{*******************************************************************}
{                                                                   }
{   InstantSolutions Framework - MARS Server                        }
{                                                                   }
{   Copyright (c) 2005-2024 Ethea S.r.l.                            }
{   ALL RIGHTS RESERVED / TUTTI I DIRITTI RISERVATI                 }
{                                                                   }
{*******************************************************************}
{                                                                   }
{   The entire contents of this file is protected by                }
{   International Copyright Laws. Unauthorized reproduction,        }
{   reverse-engineering, and distribution of all or any portion of  }
{   the code contained in this file is strictly prohibited and may  }
{   result in severe civil and criminal penalties and will be       }
{   prosecuted to the maximum extent possible under the law.        }
{                                                                   }
{   RESTRICTIONS                                                    }
{                                                                   }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED      }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE        }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE       }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT  }
{   AND PERMISSION FROM ETHEA S.R.L.                                }
{                                                                   }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON       }
{   ADDITIONAL RESTRICTIONS.                                        }
{                                                                   }
{*******************************************************************}
{                                                                   }
{   Il contenuto di questo file è protetto dalle leggi              }
{   internazionali sul Copyright. Sono vietate la riproduzione, il  }
{   reverse-engineering e la distribuzione non autorizzate di tutto }
{   o parte del codice contenuto in questo file. Ogni infrazione    }
{   sarà perseguita civilmente e penalmente a termini di legge.     }
{                                                                   }
{   RESTRIZIONI                                                     }
{                                                                   }
{   SONO VIETATE, SENZA IL CONSENSO SCRITTO DA PARTE DI             }
{   ETHEA S.R.L., LA COPIA, LA VENDITA, LA DISTRIBUZIONE E IL       }
{   TRASFERIMENTO A TERZI, A QUALUNQUE TITOLO, DEL CODICE SORGENTE  }
{   CONTENUTO IN QUESTO FILE E ALTRI FILE AD ESSO COLLEGATI.        }
{                                                                   }
{   SI FACCIA RIFERIMENTO ALLA LICENZA D'USO PER INFORMAZIONI SU    }
{   EVENTUALI RESTRIZIONI ULTERIORI.                                }
{                                                                   }
{*******************************************************************}
unit InstantObjects.MARS.InjectionService;

{$I MARS.inc}

interface

uses
  System.Classes, System.SysUtils, System.Rtti, System.Types
  , MARS.Core.Injection
  , MARS.Core.Injection.Interfaces
  , MARS.Core.Injection.Types
  , MARS.Core.Activation.Interfaces
;

type
  TInstantObjectInjectionService = class(TInterfacedObject, IMARSInjectionService)
  public
    procedure GetValue(const ADestination: TRttiObject;
      const AActivation: IMARSActivation; out AValue: TInjectionValue);
  end;

  {$IFDEF FWINSTANTOBJECT}
  TFWInstantObjectInjectionService = class(TInterfacedObject, IMARSInjectionService)
  public
    procedure GetValue(const ADestination: TRttiObject;
      const AActivation: IMARSActivation; out AValue: TInjectionValue);
  end;
{$ENDIF}

implementation

uses
  MARS.Rtti.Utils
  , MARS.Data.FireDAC
  , InstantObjects.MARS.Data
;

{ TInstantObjectInjectionService }

procedure TInstantObjectInjectionService.GetValue(const ADestination: TRttiObject;
  const AActivation: IMARSActivation; out AValue: TInjectionValue);
var
  LType: TRttiType;
  LEnvironment: TMARSInstantObjects;
  {$IFDEF FWINSTANTOBJECT}
  LFWEnvironment: TMARSFWInstantObject;
  {$ENDIF}
  LValue: TInjectionValue;
begin
  LValue.Clear;
  LType := ADestination.GetRttiType;
  if (LType.IsObjectOfType(TMARSInstantObjects)) then
  begin
    LEnvironment := TMARSInstantObjects.Create(AActivation);
    LValue := TInjectionValue.Create(LEnvironment, False);
  {$IFDEF FWINSTANTOBJECT}
  end
  else if (LType.IsObjectOfType(TMARSFWInstantObject)) then
  begin
    LFWEnvironment := TMARSFWInstantObjects.Create(AActivation);
    LValue := TInjectionValue.Create(LFWEnvironment, False);
  {$ENDIF}
  end;

  AValue := LValue;
end;

procedure RegisterServices;
begin
  TMARSInjectionServiceRegistry.Instance.RegisterService(
    function :IMARSInjectionService
    begin
      Result := TInstantObjectInjectionService.Create;
    end
  , function (const ADestination: TRttiObject): Boolean
    var
      LType: TRttiType;
    begin
      Result := ((ADestination is TRttiParameter) or
        (ADestination is TRttiField) or
        (ADestination is TRttiProperty));
      if Result then
      begin
        LType := ADestination.GetRttiType;
        Result := LType.IsObjectOfType(TMARSInstantObjects);
      end;
    end
  );

{$IFDEF FWINSTANTOBJECT}
  TMARSInjectionServiceRegistry.Instance.RegisterService(
    function :IMARSInjectionService
    begin
      Result := TFWInstantObjectInjectionService.Create;
    end
  , function (const ADestination: TRttiObject): Boolean
    var
      LType: TRttiType;
    begin
      Result := ((ADestination is TRttiParameter) or
        (ADestination is TRttiField) or
        (ADestination is TRttiProperty));
      if Result then
      begin
        LType := ADestination.GetRttiType;
        Result := LType.IsObjectOfType(TMARSFWInstantObject);
      end;
    end
  );
{$ENDIF}
end;

{$IFDEF FWINSTANTOBJECT}
{ TFWInstantObjectInjectionService }

procedure TFWInstantObjectInjectionService.GetValue(
  const ADestination: TRttiObject; const AActivation: IMARSActivation;
  out AValue: TInjectionValue);
var
  LType: TRttiType;
  LEnvironment: TMARSFWInstantObject;
  LValue: TInjectionValue;
begin
  LValue.Clear;
  LType := ADestination.GetRttiType;
  if (LType.IsObjectOfType(TMARSFWInstantObject)) then
  begin
    LEnvironment := TMARSFWInstantObjects.Create(AActivation);
    LValue := TInjectionValue.Create(LEnvironment, False);
  end;

  AValue := LValue;

end;
{$ENDIF}

initialization

  RegisterServices;

end.