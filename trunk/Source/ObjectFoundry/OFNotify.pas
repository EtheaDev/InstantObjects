unit OFNotify;

interface

uses SysUtils, Classes, MMEngineDefs, MMToolsApi;

type
  TProjectNotifier = class (TInterfacedObject, IUnknown, IMMNotifier, IMMProjectNotifier)
  private
    procedure CheckModel;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AfterNew; safecall;
    procedure AfterOpen(const FileName: WideString); safecall;
    procedure AfterSave(const FileName: WideString); safecall;
    procedure AfterStartup; safecall;
    procedure BeforeNew(var Cancel: Boolean); safecall;
    procedure BeforeOpen(const FileName: WideString; var Cancel: Boolean); safecall;
    procedure BeforeSave(const FileName: WideString; var Cancel: Boolean); safecall;
    procedure BeforeShutDown; safecall;
    procedure CommitEditors; safecall;
    procedure Destroyed; safecall;
    procedure ExpertsLoaded; safecall;
  end;

implementation

uses
  OFDefs;

{
************************************ TProjectNotifier ************************************
}
constructor TProjectNotifier.Create;
begin
  inherited Create;
end;

destructor TProjectNotifier.Destroy;
begin
  inherited Destroy;
end;

procedure TProjectNotifier.AfterNew;
begin
  CheckModel;
end;

procedure TProjectNotifier.AfterOpen(const FileName: WideString);
begin
  CheckModel;
end;

procedure TProjectNotifier.AfterSave(const FileName: WideString);
begin
end;

procedure TProjectNotifier.AfterStartup;
begin
end;

procedure TProjectNotifier.BeforeNew(var Cancel: Boolean);
begin
end;

procedure TProjectNotifier.BeforeOpen(const FileName: WideString; var Cancel: Boolean);
begin
end;

procedure TProjectNotifier.BeforeSave(const FileName: WideString; var Cancel: Boolean);
begin
end;

procedure TProjectNotifier.BeforeShutDown;
begin
end;

procedure TProjectNotifier.Destroyed;
begin
end;

procedure TProjectNotifier.CommitEditors;
begin
end;

procedure TProjectNotifier.CheckModel;
var
  Index: Integer;
  Root: IMMClass;
begin
  // TODO : The insertion of TInstantObject could be / should be optional ?
  // If optional: if TInstantObject is in the model => assign it as ClassPersistencyRoot

  // This method should check the code model
  // insert required template classes and assign a PersistencyRoot for the framework IsPersistent evaluation rule
  if MMToolServices.CodeModel.FindClass('TInstantObject', Index) then
  begin
    if MMToolServices.CodeModel.Classes[Index].IsInterface then
    begin
      EXIT;
      // TODO: CONFLICT!
      // MMToolServices.MessageServer.CreateMessage();
    end
    else // Yes it is a class!
      Root := MMToolServices.CodeModel.Classes[Index] as IMMClass;
  end
  else
  begin
    // create the class, insert TPersistent the same way if required
    Root := MMToolServices.CodeModel.AddClass;
    Root.Name := 'TInstantObject';
    Root.Options[classPlaceHolder] := True;
    // TODO : what else should be added?
    // add method as placeholder
    // M := Root.AddMethod;
    // set Root.ancestor etc
  end;
  // and finally assign the persistency root: this will invoke the
  // AutoDetect rule: class inherits from TInstantObject => Persistent
  (MMToolServices.CodeModel as IMMV9CodeModel).ClassPersistencyRoot := Root;
  // TODO : maybe we need to be able to block the user editing the class / methods?
end;

procedure TProjectNotifier.ExpertsLoaded;
begin
end;

end.
