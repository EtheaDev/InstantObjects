{$IF DEFINED(WIN64) OR DEFINED(LINUX64)}
  {$DEFINE WINLINUX64}
{$ENDIF}

//Used to force Selector/Exposers to create TLargeintField, also at Design-Time
//Remove this directive if you want to use only 32Bit applications
//and want to use always TIntegerField form 'Self' and 'References' fields
{$DEFINE USE_LARGEINT_FIELD_FOR_REF}

//Deactivate this directive if you don't want to use Serialization with Delphi-Neon library
{.$DEFINE DELPHI_NEON}

//Activate this directive if you want to not trim right spaces in attributes values
{.$DEFINE IO_DISABLE_STRING_TRIM}

{$A8}
{$B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T+,U-,V+,W-,X+,Y+,Z1}
{$MINSTACKSIZE $00004000}
{$MAXSTACKSIZE $00100000}
{$IMAGEBASE $00400000}

  {$WARN SYMBOL_DEPRECATED ON}
  {$WARN SYMBOL_LIBRARY ON}
  {$WARN SYMBOL_PLATFORM ON}
  {$WARN UNIT_LIBRARY ON}
  {$WARN UNIT_PLATFORM ON}
  {$WARN UNIT_DEPRECATED ON}
  {$WARN HRESULT_COMPAT ON}
  {$WARN HIDING_MEMBER ON}
  {$WARN HIDDEN_VIRTUAL ON}
  {$WARN GARBAGE ON}
  {$WARN BOUNDS_ERROR ON}
  {$WARN ZERO_NIL_COMPAT ON}
  {$WARN STRING_CONST_TRUNCED ON}
  {$WARN FOR_LOOP_VAR_VARPAR ON}
  {$WARN TYPED_CONST_VARPAR ON}
  {$WARN ASG_TO_TYPED_CONST ON}
  {$WARN CASE_LABEL_RANGE ON}
  {$WARN FOR_VARIABLE ON}
  {$WARN CONSTRUCTING_ABSTRACT ON}
  {$WARN COMPARISON_FALSE ON}
  {$WARN COMPARISON_TRUE ON}
  {$WARN COMPARING_SIGNED_UNSIGNED ON}
  {$WARN COMBINING_SIGNED_UNSIGNED ON}
  {$WARN UNSUPPORTED_CONSTRUCT ON}
  {$WARN FILE_OPEN ON}
  {$WARN FILE_OPEN_UNITSRC ON}
  {$WARN BAD_GLOBAL_SYMBOL ON}
  {$WARN DUPLICATE_CTOR_DTOR ON}
  {$WARN INVALID_DIRECTIVE ON}
  {$WARN PACKAGE_NO_LINK ON}
  {$WARN PACKAGED_THREADVAR ON}
  {$WARN IMPLICIT_IMPORT ON}
  {$WARN HPPEMIT_IGNORED ON}
  {$WARN NO_RETVAL ON}
  {$WARN USE_BEFORE_DEF ON}
  {$WARN FOR_LOOP_VAR_UNDEF ON}
  {$WARN UNIT_NAME_MISMATCH ON}
  {$WARN NO_CFG_FILE_FOUND ON}
  {$WARN MESSAGE_DIRECTIVE ON}
  {$WARN IMPLICIT_VARIANTS ON}
  {$WARN UNICODE_TO_LOCALE ON}
  {$WARN LOCALE_TO_UNICODE ON}
  {$WARN IMAGEBASE_MULTIPLE ON}
  {$WARN SUSPICIOUS_TYPECAST ON}
  {$WARN PRIVATE_PROPACCESSOR ON}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}

{
  Define this symbol to cause InstantAllocMem and InstantFreeMem to pad
  allocated memory blocks with a guard area to detect buffer overruns.
  When this define is enabled. InstantFreeMem will check if the signature
  in the guard area has been changed and will raise an exception indicating
  memory corruption has occured. This was implemented to aid in identification
  of issues in the record buffer management in the InstantPresentation unit.
}
{.$DEFINE IO_MEM_OVERRUN_CHECK}

{
  Define this symbol to enable logging of all SQL statements to the debug
  console (via OutputDebugString - only on Windows) and to the procedure
  pointed by the InstantBrokers.InstantLogProc global variable.
}
{.$DEFINE IO_STATEMENT_LOGGING}

{
  Whenever a TInstantObject is destroyed, a check is performed to see if there
  are any objects that circularly refer to it, which should be destroyed as
  well. If you are sure that your object model doesn't allow circular
  references (as is the case with many object models, especially simple ones),
  you can define this symbol to gain a little speed in programs that create
  and destroy large quantities of objects.

  WARNING: If you disable the check, then every time your program frees an
  object with circular references, it will leak memory. You might not notice it
  unless you are using a memory leak detection tool such as FastMM.
  So please make sure your model doesn't allow any circular reference
  before enabling this define.

  Also, do it only if you actually need the speed gain, which is not significant
  in most cases.
}
{$DEFINE IO_NO_CIRCULAR_REFERENCE_CHECK}


