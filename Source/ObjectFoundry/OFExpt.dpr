library OFExpt;

{$R *.RES}                     
{$R ..\Design\iodesimages.res}   

uses
  InstantAttributeEditor in '..\Design\InstantAttributeEditor.pas' {InstantAttributeEditorForm},
  InstantDesignUtils in '..\Design\InstantDesignUtils.pas',
  InstantEdit in '..\Design\InstantEdit.pas' {InstantEditForm},
  InstantCode in '..\Core\InstantCode.pas',
  OFClasses in 'OFClasses.pas',
  OFClassRegWizard in 'OFClassRegWizard.pas' {ClassRegWizardForm},
  OFCritic in 'OFCritic.pas',
  OFDefs in 'OFDefs.pas',
  OFExpert in 'OFExpert.pas',
  OFNotify in 'OFNotify.pas',
  OFOptions in 'OFOptions.pas' {OFOptionsForm},
  OFReg in 'OFReg.pas',
  OFUtils in 'OFUtils.pas';

end.
