unit OFOptions;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  InstantEdit, Db, InstantPresentation, StdCtrls, ExtCtrls, ComCtrls, Mask,
  DBCtrls;

type
  TOFOptions = class(TPersistent)
  private
    FProjectFileName: string;
  public
    procedure Load;
    procedure Save;
  published
    property ProjectFileName: string read FProjectFileName write FProjectFileName;
  end;

  TOFOptionsForm = class(TInstantEditForm)
    PageControl: TPageControl;
    ProjectSheet: TTabSheet;
    ProjectFileEdit: TLabel;
    ProjectFileNameEdit: TDBEdit;
    ProjectFileButton: TButton;
  private
    function GetSubject: TOFOptions;
    procedure SetSubject(const Value: TOFOptions);
  public
    property Subject: TOFOptions read GetSubject write SetSubject;
  end;

implementation

uses
  MMToolsAPI;

{$R *.DFM}

{ TOFOptions }

procedure TOFOptions.Load;
begin

end;

procedure TOFOptions.Save;
begin

end;

{ TOFOptionsForm }

function TOFOptionsForm.GetSubject: TOFOptions;
begin
  Result := inherited Subject as TOFOptions;
end;

procedure TOFOptionsForm.SetSubject(const Value: TOFOptions);
begin
  inherited Subject := Value;
end;

end.
