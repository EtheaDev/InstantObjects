unit InstantXXXReg;

interface

procedure Register;

implementation

uses
  Classes, InstantXXX;

procedure Register;
begin
  RegisterComponents('InstantObjects', [TInstantXXXConnector]);
end;

end.
