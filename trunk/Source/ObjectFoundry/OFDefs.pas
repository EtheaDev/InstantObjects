unit OFDefs;

interface


{$IFDEF MM7}
uses
  MMToolsAPI, MMDiagramAPI;

type
  IMMV9ClassBase = IMMClassBase;
  IMMV9CodeModel = IMMCodeModel;
{$ELSE}
uses
  MMToolsAPI;

type
  TMMActionData = record
    Caption: WideString; // ModelMaker provides a defaults name based on to the menu item name
    ImageIndex: Integer; // Default = -1; Only used for toolbuttons, ignored for menu items
    Hint: WideString; // Default = ''
    Checked: Boolean; // Default = False
    Enabled: Boolean; // Default = True
    Visible: Boolean; // Default = True
    Updated: Boolean; // Default = False. Setting Updated = True lets MM call GetActionData
                      // before the menu item is being shown
    ShortCut: Word; // Mapped on TShortCut, Default = 0 (none) ShortCut is static and not updated.
  end;
{$ENDIF}

type
  IOFReference = IMMReference;
  IOFEntityReference = IMMEntityReference;

implementation

end.
