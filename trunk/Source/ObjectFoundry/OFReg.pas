unit OFReg;

interface

uses
  MMToolsAPI;

procedure InitializeExpert(const Srv: IMMToolServices); stdcall;
procedure FinalizeExpert; stdcall;
function ExpertVersion: LongInt; stdcall;

exports
  InitializeExpert name MMExpertEntryProcName,
  FinalizeExpert name MMExpertExitProcName,
  ExpertVersion name MMExpertVersionProcName;

implementation

uses
  Forms, OFExpert, OFNotify, OFCritic;

procedure InitializeExpert(const Srv: IMMToolServices); stdcall;
begin
  // Copy interface pointer to initialize global var in MMToolsApi.pas
  MMToolServices := Srv;
  // Register the expert
  Srv.AddExpert(TObjectFoundryExpert.Create);
  // Create a project notifier and register it
  // It will work independent - MM will control it's life cycle.
  Srv.AddProjectNotifier(TProjectNotifier.Create);
  // Add an ObjectFoundry design critic which performs some basic naming convention checks
  // It will work independent - MM will control it's life cycle.
  MMToolServices.CriticManager.AddCritic(TObjectFoundryCritic.Create);
  // now sync with parent window, if we omit this, modal dialogs won't be really modal and
  // modeless forms will behave even worse.
  Application.Handle := Srv.GetParentHandle;
end;

procedure FinalizeExpert; stdcall;
begin
  // there's no need to export this function is there's nothing to clean up
  // In this demo expert all the cleaning-up is done by the Expert.Destroyed
end;

function ExpertVersion: LongInt; stdcall;
begin
  // This funciton and it's implementation are mandatory, if this function is not
  // exported or the version mismatches the version in ModelMaker, the expert is not
  // loaded
  Result := MMToolsApiVersion;
end;

end.
