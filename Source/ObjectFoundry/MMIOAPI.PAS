{

ModelMaker 7 ObjectFoundry integration services.
Copyright (c) 2003 ModelMaker Tools BV 
Netherlands
http://www.modelmakertools.com

This unit defines the interfaces that allow the ObjectFoundry expert 
to access the native code model thus enabling InstantObjects integration.
ObjectFoundry was (previously) created and maintained by Seleqt software.
The only reason for using this unit is to be able to recompile the 
Object Foundry expert.

This unit is provided for free and as-is, no responsibility on it's
functioning and maintenance is taken whatsoever.

Using these interfaces it at your OWN RISK, and if used in any other 
way than designed (as in MM7.07 status), using these interfaces can 
cause serious and unrecoverable model integratity problems.

19 Nov 2003
Gerrit Beuze
ModelMaker Tools

}


unit MMIOAPI;

interface

uses SysUtils, Classes, MMToolsAPI, MMEngineDefs;

{ This could be the transport structure in case we need a IMMIOAttribute.GetAssociatedMembers
type
  PMemberIDArray = ^TMemberIDArray;
  TMemberIDArray = array[0..8191] of Integer;
  TMemberIDList = record
    Size: Integer;
    MemberIDs: PMemberIDArray;
  end;
}

const
{
// bits defining locked member attributes, can be combined by or-ing
  lmaNone           = 0;    // no locked attributes
  lmaName           = 1;
  lmaReserved1      = 2;
  lmaReserved2      = 4;
  lmaDataType       = 8;
  lmaVisibility     = $10;
  lmaOptions        = $20;  // class method, call inheritd etc.
  lmaReserved4      = $40;
  lmaReserved5      = $80;
  lmaMethodKind     = $100;
  lmaBindingKind    = $200;
  lmaParameters     = $400;
  lmaPropIndex      = $1000; // property only
  lmaPropLinks      = $2000; // disables read/write access
  lmaEventDispatch  = $4000; // event only

// Set of locked attributes for IO access method. Use in LinkMember()
  IOAccessMethodAttrMask = lmaName or lmaDataType or lmaOptions or lmaMethodKind or lmaParameters;
}

// tagged value names required in MM
  IOClassStorageName = 'IO.StorageName'; // updated by class editor
  IOAttrGetterEmpty = 'IO.GetterEmpty';  // updated (set to IOBoolTVTrue) if no matching locked code was found. Never reset by MM
  IOAttrSetterEmpty = 'IO.SetterEmpty';  // updated (set to IOBoolTVTrue) if no matching locked code was found. Never reset by MM

  IOBoolTVFalse = 'False';
  IOBoolTVTrue = 'True';


type
// used in GetAttributeAccessCode to specify getter/setter.
// Current implementation assumes SINGLE LINE code only.
  TIOAccessMethodType = (ioamGetter, ioamSetter);

// used in Auto attribute Visualization
  TIOAttributeType = (ioaNoAttribute, ioaSimple, ioaPart, ioaParts, ioaReference, ioaReferences);


  IInstantObjectsExpert = interface (IUnknown)
    ['{2D20DBE7-82D0-4C00-903D-ECAEF910DC12}']
    {:
    When the user creates a new or edits an existing attribute MM will call this method.
    Return True is dialog accepted, False is cancelled. This is used only to know whether
    a new attribute should be marked alive or deleted. A new attribute already exists and
    is linked to a IOStateField plus all the other relevant attributes are set to their
    correct values. The dialog should simply change the IMMProperty attributes if the
    changes are accepted.
    }
    function EditAttribute(const P: IMMProperty): Boolean; stdcall;
    {:
    If the importer does not find meta data for a class inheriting from TInstantObject
    EraseMetaData is called to make sure all meta data is removed.
    }
    procedure EraseMetaData(const C: IMMClass); stdcall;
    {:
    Return the attribute getter or setter access code for the
    Function results: 0 = success;
    -1 = no access code available
    >0 = buffer too small, the actual value is the size of the buffer required.
    MM allocates the memory for the buffer and passes the
    BufLen including the space for terminating zero.
    The space for the terminating zero is included in BufLen
    NOTE: single line code ONLY.
    }
    function GetAttributeAccessCode(const P: IMMProperty; MethodType: TIOAccessMethodType;
            Buf: PChar; BufLen: Integer): Integer; stdcall;
    {:
    Return type of attribute for visualization.
    Fast evaluation required.
    }
    function GetAttributeType(const P: IMMProperty): TIOAttributeType; stdcall;
    function GetAvailable: Boolean; stdcall;
    {:
    Called by the uses clause wizard if any class in the unit is descending from
    PersistencyRoot
    Return values:
    0 = success; Buf contains a #0 terminated string containing a comma separated list of
    module names
    to be included in the unit uses clause (at this time: 'InstantPersistency').
    -1 = do not insert unit names;
    >0 = buffer too small, the actual value is the size of the buffer required.
    MM allocates the memry for the buffer and passes the
    BufLen including the space for terminating zero.
    The space for the terminating zero is included in BufLen
    Initial value for BufLen is 256.
    }
    function GetIOUsesClause(Buf: PChar; BufLen: Integer): Integer; stdcall;
    {:
    Called when generating a class that inherits from TInstantObject. Function results: 0
    = success; -1 = do not insert meta data; >0 = buffer too small, the actual value is
    the size of the buffer required. MM allocates the memry for the buffer and passes the
    BufLen including the space for terminating zero.
    The space for the terminating zero is included in BufLen
    }
    function GetMetaData(const C: IMMClass; Buf: PChar; BufLen: Integer): Integer; 
            stdcall;
    {:
    Called after all members of a class have been imported. After importing members the
    expert is responsible for updating the IOAttribute properties and storing the meta
    data. With respect to the interaction with MM the main responsibility is to link /
    unlink the IOStateFields to the properties making them attributes.
    }
    procedure SetMetaData(const C: IMMClass; const MetaData: PChar); stdcall;
    {:
    A property has changed (name, data type etc.) IE should now update the IOStateField
    and the read/write access code through IMMIOAttribute. Note that this method can be
    called very often and that the IOStateField or the ReadMethod/WriteMethod can be nil (
    temporarily) because this method is also called during constrution of a property.
    }
    procedure UpdateAttributeMembers(const P: IMMProperty); stdcall;
    {:
    Gives the opportunity to disable InstantObjects integration.
    }
    property Available: Boolean read GetAvailable;
  end;
  
  IMMIOAttribute = interface (IUnknown)
    ['{496694D4-B26F-4649-8B09-91A3470CDC04}']
    function GetIOStateField: IMMField; stdcall;
    function GetIsIOAttribute: Boolean; stdcall;
    {:
    Links field F to the property, this turns the property into a IOAttribute. Any
    exsiting IOStateField is unlinked and reduced to user owned. If linking fails (for
    example because already owned by another entity) False is returned. True is returned
    if the field was linked to the property.
    }
    function LinkIOStateField(const F: IMMField): Boolean; stdcall;
    {:
    Links member with MemberID to the property.
    Typically used to associate IOAccessMethods to the property.
    Linked members will be included in cut/copy/delete/move operations
    No other updates. User can edit all settings for the member.
    Do not pass IOStateField and MM property read/write method.
    Member must be in the same class as the property.
    Also, members owned by other entities cannot be linked.
    Returns True if member was linked successfully, else False
    LockedAttr is a bit set that defines what attributes should be disabled
    in the MM member dialog. Unlinking a member will auto reset the locked attributes to 
    lmaNone
    }
    function LinkMember(MemberID: Integer): Boolean; stdcall;
    {:
    Unlinks the current IOStateField. This reduces the field to user owned (it can be
    deleted now through the MMToolsAPI or by the user). If no state field was linked
    nothing happens.
    }
    procedure UnlinkIOStateField; stdcall;
    {:
    Unlinks member with ID from the property. Reducing it to a user owned member.
    Call UnlinkMember before deleting a member!
    Do not pass IOStateField and MM property read/write method.
    }
    procedure UnlinkMember(MemberID: Integer); stdcall;
    {:
    All attributes of a IOStateField are blocked for editing. The only way of changing
    them is through this method. This method is usually called as a response on the
    IInstantObjectsExpert.UpdateAttributeMembers.
    }
    procedure UpdateIOStateField(const NewName, NewDataName: PChar); stdcall;
    {:
    The read/write access code in the Attribute's read and write method is blocked for
    editing. The only way of changing it is through this method. This method is usually
    called as a response on the IInstantObjectsExpert.UpdateAttributeMembers.
    }
    procedure UpdateMethodAccessCode(const ReadCode, WriteCode: PChar); stdcall;
    {:
    Retruns the currently linked IO state field.
    }
    property IOStateField: IMMField read GetIOStateField;
    {:
    Returns True if the property is linked to a state field.
    }
    property IsIOAttribute: Boolean read GetIsIOAttribute;
  end;
  

implementation

end.