{$IFNDEF PART} {$FATAL surfacelocations.inc must be imported with PART defined} {$ENDIF}
{$IFNDEF SUPERCLASS} {$FATAL surfacelocations.inc must be imported with SUPERCLASS defined} {$ENDIF}

{$IF SUPERCLASS=TNamedLocation}
 {$DEFINE SUBCLASS:=TSurfaceNamedLocation}
 {$DEFINE CONSTRUCTORARGUMENTDECLARATIONS:=Name, DefiniteName, IndefiniteName, Description: UTF8String;}
 {$DEFINE CONSTRUCTORARGUMENTNAMES:=Name, DefiniteName, IndefiniteName, Description}
{$ELSEIF SUPERCLASS=TProxyLocation}
 {$DEFINE SUBCLASS:=TSurfaceProxyLocation}
 {$DEFINE CONSTRUCTORARGUMENTDECLARATIONS:=Source: TThing; Position: TThingPosition;}
 {$DEFINE CONSTRUCTORARGUMENTNAMES:=Source, Position}
{$ELSE}
 {$FATAL Unknown SUPERCLASS value.}
{$ENDIF}

{$IF PART=Interface}

   SUBCLASS = class(SUPERCLASS)
    protected
      FSurface: TThing;
      class function CreateFromProperties(Properties: TTextStreamProperties): SUBCLASS; override;
    public
      constructor Create(CONSTRUCTORARGUMENTDECLARATIONS Ground: TThing);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      class procedure DescribeProperties(Describer: TPropertyDescriber); override;
      procedure EnumerateChildren(List: TThingList; const PositionFilter: TThingPositionFilter); override;
      function GetSurface(): TThing; override;
   end;

{$ELSEIF PART=Implementation}

constructor SUBCLASS.Create(CONSTRUCTORARGUMENTDECLARATIONS Ground: TThing);
begin
   inherited Create(CONSTRUCTORARGUMENTNAMES);
   FSurface := Ground;
   Add(FSurface, tpPartOfImplicit);
   AddLandmark(cdDown, FSurface, [loPermissibleNavigationTarget, loConsiderDirectionUnimportantWhenFindingChildren, loNotVisibleFromBehind]);
end;

constructor SUBCLASS.Read(Stream: TReadStream);
begin
   inherited;
   Stream.ReadReference(@Pointer(FSurface));
end;

procedure SUBCLASS.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteReference(FSurface);
end;

class function SUBCLASS.CreateFromProperties(Properties: TTextStreamProperties): SUBCLASS;
var
{$IF SUPERCLASS=TNamedLocation}
   Name: UTF8String;
   DefiniteName, IndefiniteName, Description: UTF8String;
{$ELSEIF SUPERCLASS=TProxyLocation}
   Source: TThing;
   Position: TThingPosition;
{$ELSE}
 {$FATAL Unknown SUPERCLASS value.}
{$ENDIF}
   Ground: TThing;
   StreamedLandmarks: TStreamedLandmarks;
   StreamedChildren: TStreamedChildren;
begin
   while (not Properties.Done) do
   begin
      if (
{$IF SUPERCLASS=TNamedLocation}
          Properties.HandleUniqueStringProperty(pnName, Name) and
          Properties.HandleUniqueStringProperty(pnDefiniteName, DefiniteName) and
          Properties.HandleUniqueStringProperty(pnIndefiniteName, IndefiniteName) and
          Properties.HandleUniqueStringProperty(pnDescription, Description) and
{$ELSEIF SUPERCLASS=TProxyLocation}
          TThing.HandleUniqueThingProperty(Properties, pnSource, Source, TThing) and {BOGUS Hint: Local variable "Source" does not seem to be initialized}
          Properties.specialize HandleUniqueEnumProperty<TThingPosition>(pnPosition, Position) and {BOGUS Hint: Local variable "Position" does not seem to be initialized}
{$ELSE}
 {$FATAL Unknown SUPERCLASS value.}
{$ENDIF}
          TThing.HandleUniqueThingProperty(Properties, pnGround, Ground, TThing) and {BOGUS Hint: Local variable "Ground" does not seem to be initialized}
          HandleLandmarkProperties(Properties, StreamedLandmarks) and
          HandleChildProperties(Properties, StreamedChildren)) then
       Properties.FailUnknownProperty();
   end;
{$IF SUPERCLASS=TNamedLocation}
   Properties.EnsureSeen([pnName, pnDefiniteName, pnIndefiniteName, pnDescription, pnGround]);
   Result := Create(Name, DefiniteName, IndefiniteName, Description, Ground);
{$ELSEIF SUPERCLASS=TProxyLocation}
   Properties.EnsureSeen([pnSource, pnPosition, pnGround]);
   Result := Create(Source, Position, Ground);
{$ELSE}
 {$FATAL Unknown SUPERCLASS value.}
{$ENDIF}
   StreamedLandmarks.Apply(Result);
   StreamedChildren.Apply(Result);
end;

class procedure SUBCLASS.DescribeProperties(Describer: TPropertyDescriber);
begin
{$IF SUPERCLASS=TNamedLocation}
   Describer.AddProperty(pnName, ptString);
   Describer.AddProperty(pnDefiniteName, ptString);
   Describer.AddProperty(pnIndefiniteName, ptString);
   Describer.AddProperty(pnDescription, ptString);
{$ELSEIF SUPERCLASS=TProxyLocation}
   Describer.AddProperty(pnSource, ptThing);
   Describer.AddProperty(pnPosition, ptThingPosition);
{$ELSE}
 {$FATAL Unknown SUPERCLASS value.}
{$ENDIF}
   Describer.AddProperty(pnGround, ptThing);
   Describer.AddProperty(pnLandmark, ptLandmark);
   Describer.AddProperty(pnChild, ptChild);
end;

procedure SUBCLASS.EnumerateChildren(List: TThingList; const PositionFilter: TThingPositionFilter);
begin
   inherited;
   FSurface.EnumerateChildren(List, PositionFilter);
end;

function SUBCLASS.GetSurface(): TThing;
begin
   Result := FSurface;
end;

{$ELSE}
 {$FATAL Unknown PART value}
{$ENDIF}

{$UNDEF SUBCLASS}
