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
    StreamFormatLabel: TLabel;
    StreamFormatComboBox: TComboBox;
    procedure FormCreate(Sender: TObject);
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
  StreamFormatComboBox.ItemIndex := Ord(ConnectionDef.BlobStreamFormat); //CB
  { TODO: Copy data from ConnectionDef to edit controls }
end;

procedure TInstantXXXConnectionDefEditForm.SaveData(
  ConnectionDef: TInstantXXXConnectionDef);
begin
  ConnectionDef.BlobStreamFormat := TInstantStreamFormat(StreamFormatComboBox.ItemIndex); //CB
  { TODO: Copy data from edit controls to ConnectionDef }
end;

procedure TInstantXXXConnectionDefEditForm.FormCreate(Sender: TObject);
begin
  AssignInstantStreamFormat(StreamFormatComboBox.Items); //CB
end;

end.
