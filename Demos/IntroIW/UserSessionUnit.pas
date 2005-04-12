unit UserSessionUnit;

{
  This is a DataModule where you can add components or declare fields that are specific to 
  ONE user. Instead of creating global variables, it is better to use this datamodule. You can then
  access the it using UserSession.
}
interface

uses
  IWUserSessionBase, SysUtils, Classes, InstantPersistence, DB,
  InstantADO, ADODB;

type
  TIWUserSession = class(TIWUserSessionBase)
    ADOConnection1: TADOConnection;
    InstantADOConnector1: TInstantADOConnector;
    procedure IWUserSessionBaseCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    sAppId : String;
    bOk : Boolean;   // used to simulate MODAL Forms
  end;

implementation

{$R *.dfm}

procedure TIWUserSession.IWUserSessionBaseCreate(Sender: TObject);
begin
  InstantADOConnector1.Connect;
end;

end.
