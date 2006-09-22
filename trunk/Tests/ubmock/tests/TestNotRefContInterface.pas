unit TestNotRefContInterface;

interface

uses
  Classes, SysUtils,
  fpcunit, testregistry,
  NotRefCountIObject;

type

  TTestNotRefCountIObject = class(TTestCase)
  private
  protected
  published
    procedure TestIFreeable;
    procedure TestGetObject;
    procedure TestFreeAtRelease;
  end;


implementation



procedure TTestNotRefCountIObject.TestGetObject;
var
  o: TNotRefCount;
  i: IFreeable;
begin
  o := TNotRefCount.Create;
  try
    i := o;
    AssertSame(o, i.GetObject);
  finally
    i := nil;
    o.Free;
  end;
end;

procedure TTestNotRefCountIObject.TestIFreeable;
var
  i: IFreeable;
  o: TObject;
begin
  i := TNotRefCount.Create;
  try
    AssertNotNull(i.GetObject);
    AssertEquals(TNotRefCount.ClassName, i.GetObject.ClassName);
  finally
    o := i.GetObject;
    i := nil;
    o.Free;
  end;
end;

procedure TTestNotRefCountIObject.TestFreeAtRelease;
var
  i: IFreeable;
  o: TObject;
  eav: Boolean;
  s: string;
begin
  i := TNotRefCount.Create;
  try
    AssertNotNull(i.GetObject);
    o := i.GetObject;
  finally
    FreeAndNilIntf(i);
  end;
  AssertNullIntf(i);

   //reset the top of the heap
  s := StringOfChar('?', 100);

  eav := False;
  try
    o.Free; //try to refree the object
  except
    on e: EAccessViolation do
      eav := True;
  end;
  AssertTrue(eav);
  AssertEquals(StringOfChar('?', 100), s);
end;


initialization
  RegisterTests([TTestNotRefCountIObject]);

end.
