unit InstantXXXConnectionDefEdit;

interface

uses
  Forms, StdCtrls, Controls, ExtCtrls, Classes, InstantXXX;

type
  TInstantXXXConnectionDefEditForm = class(TForm)
    BottomBevel: TBevel;
    BottomPanel: TPanel;
    CancelButton: TButton;
    ClientPanel: TPanel;
    OkButton: TButton;
  public
    procedure LoadData(ConnectionDef: TInstantXXXConnectionDef);
    procedure SaveData(ConnectionDef: TInstantXXXConnectionDef);
  end;

implementation

{$R *.DFM}

{ TInstantXXXConnectionDefEditForm }

procedure TInstantXXXConnectionDefEditForm.LoadData(
  ConnectionDef: TInstantXXXConnectionDef);
begin
  { TODO: Copy data from ConnectionDef to edit controls }
end;

procedure TInstantXXXConnectionDefEditForm.SaveData(
  ConnectionDef: TInstantXXXConnectionDef);
begin
  { TODO: Copy data from edit controls to ConnectionDef }
end;

end.
