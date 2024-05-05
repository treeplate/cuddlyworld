{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit teststorage;

interface

uses
   sysutils, storable, lists;

procedure RunTests();

implementation

{$IFOPT C-} {$FATAL Tests require asserts enabled.} {$ENDIF}

type
   TTest = class(TStorable)
      FName: UTF8String;
      constructor Create(AName: UTF8String);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
   end;
   
type
   TTestList = specialize TStorableList<TTest>;

constructor TTest.Create(AName: UTF8String);
begin
   inherited Create();
   FName := AName;
end;

constructor TTest.Read(Stream: TReadStream);
begin
   inherited;
   FName := Stream.ReadString();
end;

procedure TTest.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteString(FName);
end;

procedure RunTests();
const
   Filename = 'test.$$$';
var
   S: UTF8String;
   L: TTestList;
   X: TTest;
begin
   // write data to disk
   L := TTestList.Create([slOwner]);
   X := TTest.Create('A');
   L.AppendItem(X);
   X := TTest.Create('B');
   L.AppendItem(X);
   X := TTest.Create('C');
   L.AppendItem(X);
   StoreObjectToFile(Filename, L, 1);
   S := '';
   for X in L do
      S := S + X.FName;
   L.Free();
   Assert(S = 'ABC');
   // read it back
   L := ReadObjectFromFile(Filename) as TTestList;
   S := '';
   for X in L do
      S := S + X.FName;
   L.Free();
   Assert(S = 'ABC');
   DeleteFile(Filename);
end;

initialization
   RegisterStorableClass(TTest);
   RegisterStorableClass(TTestList);
end.