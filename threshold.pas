{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit threshold;

interface

uses
   locations, things, thingdim, grammarian, matcher, storable, physics, messages, textstream, properties;

type
   TRelativePerspectivePosition = (rppFront, rppBack, rppHere);
   TVisibleSide = (vsFront, vsBack);
   TVisibleSides = set of TVisibleSide;

type
   TThresholdThing = class abstract(TScenery)
    protected
      FFrontSideFacesDirection: TCardinalDirection;
      function LocatePerspective(Perspective: TAvatar): TRelativePerspectivePosition;
      class function CreateFromProperties(Properties: TTextStreamProperties): TThresholdThing; override;
    public
      constructor Create(Name: UTF8String; Pattern: UTF8String; Description: UTF8String; FrontFacesDirection: TCardinalDirection);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      class procedure DescribeProperties(Describer: TPropertyDescriber); override;
      function CanTraverse(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar): Boolean; virtual;
      property FrontSideFacesDirection: TCardinalDirection read FFrontSideFacesDirection write FFrontSideFacesDirection;
   end;

   TStaticThresholdThing = class(TThresholdThing) // @RegisterStorableClass
    // This is for something that's always traversable, like an archway or something
    // note that we inherit from a class that defines Openable, so IsOpen() might be true or false
    // but that doesn't mean that when IsOpen() is false, we're not traversable
    protected
      FFrontSideDescription: UTF8String;
      FBackSideDescription: UTF8String;
      class function CreateFromProperties(Properties: TTextStreamProperties): TStaticThresholdThing; override;
    public
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      class procedure DescribeProperties(Describer: TPropertyDescriber); override;
      function GetDescriptionSelf(Perspective: TAvatar): UTF8String; override;
      property FrontSideDescription: UTF8String read FFrontSideDescription write FFrontSideDescription;
      property BackSideDescription: UTF8String read FBackSideDescription write FBackSideDescription;
   end;

   TDoor = class;

   TDoorWay = class(TThresholdThing) // @RegisterStorableClass
    protected // open state is stored in inherited FOpened boolean
      const tpConsiderForDoorPosition = tpIn; // if you try to use this, it'll turn into tpOfficialDoorPosition
      const tpOfficialDoorPosition = tpInstalledIn; // this must have at most one TDoor that is tpOfficialDoorPosition at any one time
      const tpOnGround = tpOn;
      class function CreateFromProperties(Properties: TTextStreamProperties): TDoorWay; override;
      function GetDoor(): TDoor; // or nil if there isn't one
      function GetCouldBeDoor(Thing: TThing; ThingPosition: TThingPosition): Boolean;
      procedure Removed(Thing: TThing); override;
    public
      constructor Create(Name: UTF8String; Pattern: UTF8String; Description: UTF8String; FrontFacesDirection: TCardinalDirection; Door: TDoor = nil);
      class procedure DescribeProperties(Describer: TPropertyDescriber); override;
      function IsClear(): Boolean; virtual;
      procedure EnumerateChildren(List: TThingList; const PositionFilter: TThingPositionFilter); override;
      procedure ProxiedEnumerateExplicitlyReferencedThings(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; FromOutside, FromFarAway: Boolean; Directions: TCardinalDirectionSet; Reporter: TThingReporter); override;
      procedure ProxiedFindMatchingThings(Perspective: TAvatar; Options: TFindMatchingThingsOptions; PositionFilter: TThingPositionFilter; PropertyFilter: TThingFeatures; List: TThingList); override;
      function ProxiedFindThingTraverser(Thing: TThing; Perspective: TAvatar; Options: TFindThingOptions): Boolean; override;
      function CanPut(Thing: TThing; ThingPosition: TThingPosition; Care: TPlacementStyle; Perspective: TAvatar; var Message: TMessage): Boolean; override;
      procedure Put(Thing: TThing; ThingPosition: TThingPosition; Care: TPlacementStyle; Perspective: TAvatar); override;
      procedure HandleAdd(Thing: TThing; Blame: TAvatar); override;
      procedure HandlePassedThrough(Traveller: TThing; AFrom, ATo: TAtom; AToPosition: TThingPosition; Perspective: TAvatar); override;
      function GetInside(var PositionOverride: TThingPosition): TThing; override;
      function CanInsideHold(const Manifest: TThingSizeManifest; const ManifestCount: Integer): Boolean; override;
      function GetDefaultDestination(var ThingPosition: TThingPosition): TThing; override;
      function GetLookIn(Perspective: TAvatar): UTF8String; override;
      function GetLookThrough(Perspective: TAvatar): UTF8String; virtual; // this gives the answer regardless of whether there's a door, it's open, or whatever
      function GetLookUnder(Perspective: TAvatar): UTF8String; override;
      function GetDescriptionRemoteBrief(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Direction: TCardinalDirection): UTF8String; override;
      function GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection; LeadingPhrase: UTF8String; Options: TLeadingPhraseOptions): UTF8String; override;
      function GetDescriptionObstacles(Perspective: TAvatar; NoObstacleMessage: UTF8String = ''): UTF8String; virtual;
      function GetDescriptionEmpty(Perspective: TAvatar): UTF8String; override;
      function GetDescriptionClosed(Perspective: TAvatar): UTF8String; override; // defers to the door
      function GetEntrance(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: TMessage; NotificationList: TAtomList): TAtom; override;
      function GetFeatures(): TThingFeatures; override;
      function CanSeeIn(): Boolean; override;
      function CanSeeThrough(): Boolean; virtual;
      function Open(Perspective: TAvatar; var Message: TMessage): Boolean; override;
      function Close(Perspective: TAvatar; var Message: TMessage): Boolean; override;
      function CanTraverse(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar): Boolean; override;
      function GetNavigationInstructions(Direction: TCardinalDirection; Child: TThing; Perspective: TAvatar; var Message: TMessage): TNavigationInstruction; override;
      property Door: TDoor read GetDoor; // can be nil, if there's no door
   end;

   TDoorSide = class;

   TDoor = class(TDescribedPhysicalThing) // @RegisterStorableClass
    // The description (settable via the .Description property) will override
    // using the sides when both sides are visible. This may be helpful if the
    // sides have identical descriptions (in which case the normal behaviour
    // of including both in the overall description is ugly).
    protected
      FFrontSide, FBackSide: TDoorSide;
      class function CreateFromProperties(Properties: TTextStreamProperties): TDoor; override;
      function GetDoorWay(): TDoorWay; // or nil if the door isn't in a doorway
      // GetLock() could work a similar way
      function IsChildTraversable(Child: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean; override;
      function GetMatcherFlags(Perspective: TAvatar): TMatcherFlags; override;
      function LocatePerspective(Perspective: TAvatar): TRelativePerspectivePosition;
      function DetermineVisibleSides(Perspective: TAvatar): TVisibleSides;
    public
      const mfOpen: TMatcherFlag = 1;
      const mfClosed: TMatcherFlag = 2;
      constructor Create(Name: UTF8String; Pattern: UTF8String; FrontSide, BackSide: TDoorSide; AMass: TThingMass = tmHeavy; ASize: TThingSize = tsMassive);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      class procedure DescribeProperties(Describer: TPropertyDescriber); override;
      function CanPut(Thing: TThing; ThingPosition: TThingPosition; Care: TPlacementStyle; Perspective: TAvatar; var Message: TMessage): Boolean; override;
      procedure HandlePassedThrough(Traveller: TThing; AFrom, ATo: TAtom; AToPosition: TThingPosition; Perspective: TAvatar); override;
      function GetDescriptionSelf(Perspective: TAvatar): UTF8String; override;
      function GetLookUnder(Perspective: TAvatar): UTF8String; override;
      function CanSeeUnder(Perspective: TAvatar): Boolean; virtual;
      function GetCannotSeeUnder(Perspective: TAvatar): UTF8String; virtual;
      function GetDescriptionClosed(Perspective: TAvatar): UTF8String; override;
      function GetEntrance(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: TMessage; NotificationList: TAtomList): TAtom; override;
      function GetFeatures(): TThingFeatures; override;
      function Open(Perspective: TAvatar; var Message: TMessage): Boolean; override;
      function Close(Perspective: TAvatar; var Message: TMessage): Boolean; override;
      property DoorWay: TDoorWay read GetDoorWay; // can be nil, if the door is not in a doorway
      property Description: UTF8String read FDescription write FDescription;
   end;

   TDoorSide = class(TDescribedPhysicalThing) // @RegisterStorableClass
    // description argument to constructor shouldn't have a capital first letter
    // it gets concatenated to leading clauses like "On the front side, "...
    protected
      function GetMatcherFlags(Perspective: TAvatar): TMatcherFlags; override;
      class function CreateFromProperties(Properties: TTextStreamProperties): TDoorSide; override;
    public
      constructor Create(Name: UTF8String; Pattern: UTF8String; Description: UTF8String; AMass: TThingMass = tmLight; ASize: TThingSize = tsSmall);
      class procedure DescribeProperties(Describer: TPropertyDescriber); override;
      const mfOtherSideVisible: TMatcherFlag = 1;
      function GetDescriptionSelf(Perspective: TAvatar): UTF8String; override;
      function GetDescriptionSelfSentenceFragment(Perspective: TAvatar): UTF8String; virtual;
      function GetRepresentative(): TAtom; override;
      function GetSurface(): TThing; override;
      function CanMove(Perspective: TAvatar; var Message: TMessage): Boolean; override;
      function CanPut(Thing: TThing; ThingPosition: TThingPosition; Care: TPlacementStyle; Perspective: TAvatar; var Message: TMessage): Boolean; override;
   end;

   TThresholdSurface = class(TSurface) // @RegisterStorableClass
    public
      function CanPut(Thing: TThing; ThingPosition: TThingPosition; Care: TPlacementStyle; Perspective: TAvatar; var Message: TMessage): Boolean; override;
   end;

   TThresholdLocation = class(TSurfaceProxyLocation) // @RegisterStorableClass
    protected
      class function CreateFromProperties(Properties: TTextStreamProperties): TThresholdLocation; override;
    public
      constructor Create(PassageWay: TThing; Surface: TThing);
      class procedure DescribeProperties(Describer: TPropertyDescriber); override;
      function GetTitle(Perspective: TAvatar): UTF8String; override;
      function GetLookTowardsDirectionDefault(Perspective: TAvatar; Direction: TCardinalDirection): UTF8String; override;
      function GetDescriptionHere(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Directions: TCardinalDirectionSet = cdAllDirections; Context: TAtom = nil): UTF8String; override;
      function GetDescriptionRemoteBrief(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Direction: TCardinalDirection): UTF8String; override;
      function GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection; LeadingPhrase: UTF8String; Options: TLeadingPhraseOptions): UTF8String; override;
      function GetContextFragment(Perspective: TAvatar; PertinentPosition: TThingPosition; Context: TAtom = nil): UTF8String; override;
      procedure GetNearbyThingsByClass(List: TThingList; FromOutside: Boolean; Filter: TThingClass); override;
      procedure EnumerateExplicitlyReferencedThings(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; FromOutside, FromFarAway: Boolean; Directions: TCardinalDirectionSet; Reporter: TThingReporter); override;
      function GetEntrance(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: TMessage; NotificationList: TAtomList): TAtom; override;
      procedure ProxiedFindMatchingThings(Perspective: TAvatar; Options: TFindMatchingThingsOptions; PositionFilter: TThingPositionFilter; PropertyFilter: TThingFeatures; List: TThingList); override;
      function ProxiedFindThingTraverser(Thing: TThing; Perspective: TAvatar; Options: TFindThingOptions): Boolean; override;
   end;

// XXX wall with a hole in it... wall without a hole in it... wall that can be hit to make a hole in it...

// Same as ConnectLocations but puts a threshold between them. Direction is determined from the Threshold thing.
//
// Return value must be added to the World. (If you see a memory leak on exit, you probably forgot to do that.)
// If you omit loAutoDescribe from the last argument, then the threshold won't be mentioned in descriptions of rooms that contain it.
// Flags will always contain loPermissibleNavigationTarget and loThreshold regardless of the provided argument.
function ConnectThreshold(FrontLocation, BackLocation: TLocation; Threshold: TThresholdThing; Surface: TThing = nil; Flags: TLandmarkOptions = [loAutoDescribe]): TThresholdLocation;

implementation

uses
   lists, exceptions, broadcast, typinfo;

function ConnectThreshold(FrontLocation, BackLocation: TLocation; Threshold: TThresholdThing; Surface: TThing; Flags: TLandmarkOptions = [loAutoDescribe]): TThresholdLocation;
begin
   if (not Assigned(Surface)) then
      Surface := TThresholdSurface.Create('floor', 'flat? (ground/grounds floor/floors)@', 'The floor is flat.');
   Result := TThresholdLocation.Create(Threshold, Surface);
   Flags := Flags + [loPermissibleNavigationTarget, loThreshold];
   FrontLocation.AddLandmark(cdReverse[Threshold.FrontSideFacesDirection], Result, Flags);
   Result.AddLandmark(Threshold.FrontSideFacesDirection, FrontLocation, [loAutoDescribe, loPermissibleNavigationTarget, loNotVisibleFromBehind]);
   BackLocation.AddLandmark(Threshold.FrontSideFacesDirection, Result, Flags);
   Result.AddLandmark(cdReverse[Threshold.FrontSideFacesDirection], BackLocation, [loAutoDescribe, loPermissibleNavigationTarget, loNotVisibleFromBehind]);
end;


constructor TThresholdThing.Create(Name: UTF8String; Pattern: UTF8String; Description: UTF8String; FrontFacesDirection: TCardinalDirection);
begin
   inherited Create(Name, Pattern, Description, tmHeavy, tsMassive);
   FFrontSideFacesDirection := FrontFacesDirection;
end;

constructor TThresholdThing.Read(Stream: TReadStream);
begin
   inherited;
   FFrontSideFacesDirection := TCardinalDirection(Stream.ReadCardinal());
end;

procedure TThresholdThing.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteCardinal(Cardinal(FFrontSideFacesDirection));
end;

class function TThresholdThing.CreateFromProperties(Properties: TTextStreamProperties): TThresholdThing;
var
   Name: UTF8String;
   Pattern: UTF8String;
   Description: UTF8String;
   FrontDirection: TCardinalDirection;
   StreamedChildren: TStreamedChildren;
begin
   while (not Properties.Done) do
   begin
      if (Properties.HandleUniqueStringProperty(pnName, Name) and
          Properties.HandleUniqueStringProperty(pnPattern, Pattern) and
          Properties.HandleUniqueStringProperty(pnDescription, Description) and
          Properties.specialize HandleUniqueEnumProperty<TCardinalDirection>(pnFrontDirection, FrontDirection) and {BOGUS Hint: Local variable "FrontDirection" does not seem to be initialized}
          HandleChildProperties(Properties, StreamedChildren)) then
       Properties.FailUnknownProperty();
   end;
   Properties.EnsureSeen([pnName, pnPattern, pnDescription, pnFrontDirection]);
   Result := Create(Name, Pattern, Description, FrontDirection);
   StreamedChildren.Apply(Result);
end;

class procedure TThresholdThing.DescribeProperties(Describer: TPropertyDescriber);
begin
   Describer.AddProperty(pnName, ptString);
   Describer.AddProperty(pnPattern, ptPattern);
   Describer.AddProperty(pnDescription, ptString);
   Describer.AddProperty(pnFrontDirection, ptDirection);
   Describer.AddProperty(pnChild, ptChild);
end;

function TThresholdThing.LocatePerspective(Perspective: TAvatar): TRelativePerspectivePosition;
var
   Ancestor: TAtom;
   SubjectiveInformation: TSubjectiveInformation;
   Direction, CandidateDirection: TCardinalDirection;
begin
   Ancestor := Self;
   Assert(Ancestor is TThing); // because we are a TThing
   repeat
      Ancestor := (Ancestor as TThing).Parent;
   until (not (Ancestor is TThing)) or (Ancestor = Perspective);
   if (Ancestor = Perspective) then
   begin
      Result := rppHere;
   end
   else
   begin
      SubjectiveInformation := Perspective.Locate(Self);
      if (PopCnt(Cardinal(SubjectiveInformation.Directions)) <> 1) then
      begin
         // e.g. if you're right there at the archway or whatever
         // or if there's a trapdoor on some object instead of it being a directional landmark
         // we assume that if we can't find Perspective at all, that we're here somehow
         Result := rppHere;
      end
      else
      begin
         for CandidateDirection in SubjectiveInformation.Directions do
            Direction := CandidateDirection; // there can only be one at this point, so this should be enough
         Assert(Direction in [FFrontSideFacesDirection, cdReverse[FFrontSideFacesDirection]]);
         if (Direction = FFrontSideFacesDirection) then
            Result := rppBack
         else
            Result := rppFront;
      end;
   end;
end;

function TDoorWay.GetNavigationInstructions(Direction: TCardinalDirection; Child: TThing; Perspective: TAvatar; var Message: TMessage): TNavigationInstruction;
begin
   Assert(Message.IsValid);
   if (Direction in [cdOut, cdDown]) then
   begin
      Result.TravelType := ttByPosition;
      Result.RequiredAbilities := [naWalk];
      Result.PositionTarget := FParent.GetSurface();
      Assert(Assigned(Result.PositionTarget)); // XXX handle a doorway being in an area with no surface? but what would that mean?
      Result.Position := tpOn;
   end
   else
   begin
      Result := inherited;
   end;
end;

function TThresholdThing.CanTraverse(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar): Boolean;
begin
   Result := True;
end;


constructor TStaticThresholdThing.Read(Stream: TReadStream);
begin
   inherited;
   FFrontSideDescription := Stream.ReadString();
   FBackSideDescription := Stream.ReadString();
end;

procedure TStaticThresholdThing.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteString(FFrontSideDescription);
   Stream.WriteString(FBackSideDescription);
end;

class function TStaticThresholdThing.CreateFromProperties(Properties: TTextStreamProperties): TStaticThresholdThing;
var
   Name: UTF8String;
   Pattern: UTF8String;
   Description, FrontDescription, BackDescription: UTF8String;
   FrontDirection: TCardinalDirection;
   StreamedChildren: TStreamedChildren;
begin
   while (not Properties.Done) do
   begin
      if (Properties.HandleUniqueStringProperty(pnName, Name) and
          Properties.HandleUniqueStringProperty(pnPattern, Pattern) and
          Properties.HandleUniqueStringProperty(pnDescription, Description) and
          Properties.HandleUniqueStringProperty(pnFrontDescription, FrontDescription) and
          Properties.HandleUniqueStringProperty(pnBackDescription, BackDescription) and
          Properties.specialize HandleUniqueEnumProperty<TCardinalDirection>(pnFrontDirection, FrontDirection) and {BOGUS Hint: Local variable "FrontDirection" does not seem to be initialized}
          HandleChildProperties(Properties, StreamedChildren)) then
       Properties.FailUnknownProperty();
   end;
   Properties.EnsureSeen([pnName, pnPattern, pnDescription, pnFrontDirection]);
   Result := Create(Name, Pattern, Description, FrontDirection);
   if (Properties.Seen(pnFrontDescription)) then
      Result.FrontSideDescription := FrontDescription;
   if (Properties.Seen(pnBackDescription)) then
      Result.BackSideDescription := BackDescription;
   StreamedChildren.Apply(Result);
end;

class procedure TStaticThresholdThing.DescribeProperties(Describer: TPropertyDescriber);
begin
   Describer.AddProperty(pnName, ptString);
   Describer.AddProperty(pnPattern, ptPattern);
   Describer.AddProperty(pnDescription, ptString);
   Describer.AddProperty(pnFrontDescription, ptString);
   Describer.AddProperty(pnBackDescription, ptString);
   Describer.AddProperty(pnFrontDescription, ptDirection);
   Describer.AddProperty(pnChild, ptChild);
end;

function TStaticThresholdThing.GetDescriptionSelf(Perspective: TAvatar): UTF8String;
begin
   case (LocatePerspective(Perspective)) of
      rppFront:
         begin
            if (FFrontSideDescription <> '') then
               Result := FFrontSideDescription
            else
               Result := inherited;
         end;
      rppBack:
         begin
            if (FBackSideDescription <> '') then
               Result := FBackSideDescription
            else
               Result := inherited;
         end;
      rppHere:
         Result := inherited;
   end;
end;


constructor TDoorWay.Create(Name: UTF8String; Pattern: UTF8String; Description: UTF8String; FrontFacesDirection: TCardinalDirection; Door: TDoor = nil);
begin
   inherited Create(Name, Pattern, Description, FrontFacesDirection);
   if (Assigned(Door)) then
   begin
      Add(Door, tpOfficialDoorPosition);
      Assert(Door.Position in tpArguablyInside);
   end;
end;

class function TDoorWay.CreateFromProperties(Properties: TTextStreamProperties): TDoorWay;
var
   Name: UTF8String;
   Pattern: UTF8String;
   Description: UTF8String;
   FrontDirection: TCardinalDirection;
   DoorValue: TThing = nil;
   StreamedChildren: TStreamedChildren;
begin
   while (not Properties.Done) do
   begin
      if (Properties.HandleUniqueStringProperty(pnName, Name) and
          Properties.HandleUniqueStringProperty(pnPattern, Pattern) and
          Properties.HandleUniqueStringProperty(pnDescription, Description) and
          Properties.specialize HandleUniqueEnumProperty<TCardinalDirection>(pnFrontDirection, FrontDirection) and {BOGUS Hint: Local variable "FrontDirection" does not seem to be initialized}
          TThing.HandleUniqueThingProperty(Properties, pnDoor, DoorValue, TDoor) and
          HandleChildProperties(Properties, StreamedChildren)) then
       Properties.FailUnknownProperty();
   end;
   Properties.EnsureSeen([pnName, pnPattern, pnDescription, pnFrontDirection]);
   Result := Create(Name, Pattern, Description, FrontDirection, DoorValue as TDoor);
   StreamedChildren.Apply(Result);
end;

class procedure TDoorWay.DescribeProperties(Describer: TPropertyDescriber);
begin
   Describer.AddProperty(pnName, ptString);
   Describer.AddProperty(pnPattern, ptPattern);
   Describer.AddProperty(pnDescription, ptString);
   Describer.AddProperty(pnFrontDirection, ptDirection);
   Describer.AddProperty(pnDoor, ptDoor);
   Describer.AddProperty(pnChild, ptChild);
end;

function TDoorWay.GetDoor(): TDoor;
var
   Child: TThing;
begin
   Result := nil;
   for Child in FChildren do
      if (GetCouldBeDoor(Child, Child.Position)) then
      begin
         Assert(Child.Position in tpArguablyInside);
         Assert(not Assigned(Result));
         Result := Child as TDoor;
         {$IFOPT C-} exit; {$ENDIF}
      end;
end;

function TDoorWay.GetCouldBeDoor(Thing: TThing; ThingPosition: TThingPosition): Boolean;
begin
   Result := (Thing is TDoor) and (ThingPosition in [tpOfficialDoorPosition, tpConsiderForDoorPosition]) and ((Thing as TDoor).Size = FSize);
end;

function TDoorWay.IsClear(): Boolean;
var
   List: TThingList;
begin
   List := GetChildren(tpObtrusive);
   Result := List.Length = 0;
   List.Free();
end;

procedure TDoorWay.EnumerateChildren(List: TThingList; const PositionFilter: TThingPositionFilter);
begin
   inherited;
   if (FParent is TThresholdLocation) then
      FParent.EnumerateChildren(List, PositionFilter);
end;

procedure TDoorWay.ProxiedEnumerateExplicitlyReferencedThings(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; FromOutside, FromFarAway: Boolean; Directions: TCardinalDirectionSet; Reporter: TThingReporter);
var
   Obstacles: TThingList;
   Obstacle: TThing;
begin
   inherited;
   Obstacles := GetChildren(tpObtrusive);
   try
      for Obstacle in Obstacles do // should we check IsChildTraversable() ?
         Obstacle.ProxiedEnumerateExplicitlyReferencedThings(Tokens, Start, Perspective, FromOutside, FromFarAway, Directions, Reporter);
   finally
      Obstacles.Free();
   end;
end;

procedure TDoorWay.ProxiedFindMatchingThings(Perspective: TAvatar; Options: TFindMatchingThingsOptions; PositionFilter: TThingPositionFilter; PropertyFilter: TThingFeatures; List: TThingList);
var
   Obstacles: TThingList;
   Obstacle: TThing;
begin
   inherited;
   Obstacles := GetChildren(tpObtrusive);
   try
      for Obstacle in Obstacles do // should we check IsChildTraversable() ?
         Obstacle.ProxiedFindMatchingThings(Perspective, Options, PositionFilter, PropertyFilter, List);
   finally
      Obstacles.Free();
   end;
end;

function TDoorWay.ProxiedFindThingTraverser(Thing: TThing; Perspective: TAvatar; Options: TFindThingOptions): Boolean;
var
   Obstacles: TThingList;
   Obstacle: TThing;
begin
   Result := inherited;
   if (Result) then
      exit;
   Obstacles := GetChildren(tpObtrusive);
   try
      for Obstacle in Obstacles do // should we check IsChildTraversable() ?
         if (Obstacle.ProxiedFindThingTraverser(Thing, Perspective, Options)) then
         begin
            Result := True;
            exit;
         end;
   finally
      Obstacles.Free();
   end;
end;

function TDoorWay.CanPut(Thing: TThing; ThingPosition: TThingPosition; Care: TPlacementStyle; Perspective: TAvatar; var Message: TMessage): Boolean;
var
   OldDoor: TDoor;
   CouldBeDoor: Boolean;
   DoorObstacles: TThingList;
begin
   Assert(Message.IsValid);
   OldDoor := GetDoor();
   if (ThingPosition = tpOn) then
   begin
      if (not Assigned(OldDoor)) then
         Message := TMessage.Create(mkClosed, '_ can''t put something on _.',
                                   [Capitalise(Perspective.GetDefiniteName(Perspective)), GetIndefiniteName(Perspective)])
      else
         Message := TMessage.Create(mkClosed, '_ can''t put something on _. Did you mean on _?',
                                   [Capitalise(Perspective.GetDefiniteName(Perspective)),
                                    GetIndefiniteName(Perspective),
                                    OldDoor.GetDefiniteName(Perspective)]);
      Result := False;
   end
   else
   if (ThingPosition = tpIn) then
   begin
      CouldBeDoor := GetCouldBeDoor(Thing, ThingPosition) and (Care = psCarefully);
      if (CouldBeDoor) then
      begin
         if (Assigned(OldDoor)) then
         begin
            // can't install a door when there's already a door
            Message := TMessage.Create(mkDuplicate, '_ already _ _.', [Capitalise(GetDefiniteName(Perspective)),
                                                                       TernaryConditional('has', 'have', IsPlural(Perspective)),
                                                                       OldDoor.GetIndefiniteName(Perspective)]);
            Result := False;
         end
         else
         begin
            DoorObstacles := Thing.GetChildren(tpObtrusive);
            try
               if (DoorObstacles.Length > 0) then
               begin
                  Message := TMessage.Create(mkBlocked, '_ cannot install _ _ _ while _ _ _ _.',
                                             [Capitalise(Perspective.GetDefiniteName(Perspective)),
                                              Thing.GetIndefiniteName(Perspective),
                                              ThingPositionToString(tpConsiderForDoorPosition),
                                              GetIndefiniteName(Perspective),
                                              DoorObstacles.GetIndefiniteString(Perspective, 'or'),
                                              IsAre(DoorObstacles.IsPlural(Perspective)),
                                              ThingPositionToString(DoorObstacles.First.Position),
                                              Thing.GetObjectPronoun(Perspective)]);
                  Result := False;
               end
               else
               begin
                  Message := TMessage.Create(mkSuccess, '_ _ _ _ _.',
                                             [Capitalise(Perspective.GetDefiniteName(Perspective)),
                                              TernaryConditional('installs', 'install', Perspective.IsPlural(Perspective)),
                                              Thing.GetIndefiniteName(Perspective),
                                              ThingPositionToString(tpConsiderForDoorPosition),
                                              GetDefiniteName(Perspective)]);
                  Result := True;
               end;
            finally
               DoorObstacles.Free();
            end;
         end;
      end
      else
      if (FParent is TThresholdLocation) then
      begin
         // just dump the junk in the threshold location
         Assert(Assigned(FParent.GetSurface()));
         Result := FParent.GetSurface().CanPut(Thing, tpOn, Care, Perspective, Message);
      end
      else
      if (Assigned(OldDoor) and not IsOpen()) then
      begin
         // can't put something inside a closed doorway, whether it could itself be a door or not
         Message := TMessage.Create(mkClosed, GetDescriptionClosed(Perspective));
         Result := False;
      end
      else
      begin
         // just dump the stuff in us
         Result := inherited;
      end;
   end
   else
      Assert(False); // CanPut only supports tpOn and tpIn
end;

procedure TDoorWay.Put(Thing: TThing; ThingPosition: TThingPosition; Care: TPlacementStyle; Perspective: TAvatar);
var
   Ground: TAtom;
begin
   if (((not GetCouldBeDoor(Thing, ThingPosition)) or (Care <> psCarefully) or (Assigned(GetDoor()))) and (FParent is TThresholdLocation)) then
   begin
      Ground := FParent.GetSurface();
      Assert(Assigned(Ground));
      DoBroadcast([Self, Ground], Perspective,
                  [C(M(@Perspective.GetDefiniteName)), SP, // You
                   MP(Perspective, M('drops'), M('drop')), SP, // drop
                   M(@Thing.GetDefiniteName), SP, // the door
                   M(ThingPositionToString(ThingPosition)), SP, // in
                   M(@GetDefiniteName), // the door way
                   M(', '),
                   M(ThingPositionToString(tpOnGround)), SP, // to
                   M(@Ground.GetDefiniteName), // the ground
                   M('.')]);
      Ground.Put(Thing, tpOnGround, Care, Perspective);
   end
   else
   begin
      Assert(Care = psCarefully);
      Assert(ThingPosition in [tpConsiderForDoorPosition, tpOfficialDoorPosition]);
      inherited Put(Thing, tpOfficialDoorPosition, psCarefully, Perspective);
   end;
end;

procedure TDoorWay.HandleAdd(Thing: TThing; Blame: TAvatar);
begin
   if (Thing = GetDoor()) then
   begin
      FOpened := not IsClear();
      DoBroadcast([Self, Blame], Blame,
                  [C(M(@Blame.GetDefiniteName)), SP, // You
                   MP(Blame, M('installs'), M('install')), SP, // install
                   M(@Thing.GetIndefiniteName), SP, // a thing
                   M(ThingPositionToString(tpConsiderForDoorPosition)), SP, // in
                   M(@GetDefiniteName), // the door way
                   M('. '),
                   C(M(@Thing.GetSubjectPronoun)), SP, // It
                   MP(Thing, M('is'), M('are')), SP, // is
                   M(TernaryConditional('closed', 'open', IsOpen())),
                   M('.')]);
   end;
   inherited;
end;

procedure TDoorWay.HandlePassedThrough(Traveller: TThing; AFrom, ATo: TAtom; AToPosition: TThingPosition; Perspective: TAvatar);
var
   TheDoor: TDoor;
begin
   TheDoor := GetDoor();
   if (Assigned(TheDoor)) then
      TheDoor.HandlePassedThrough(Traveller, AFrom, ATo, AToPosition, Perspective);
end;

procedure TDoorWay.Removed(Thing: TThing);
begin
   if (not Assigned(GetDoor())) then
      FOpened := True;
   inherited;
end;

function TDoorWay.GetInside(var PositionOverride: TThingPosition): TThing;
begin
   Result := Self;
end;

function TDoorWay.CanInsideHold(const Manifest: TThingSizeManifest; const ManifestCount: Integer): Boolean;
begin
   Result := (GetInsideSizeManifest() + Manifest) < FSize;
end;

function TDoorWay.GetDefaultDestination(var ThingPosition: TThingPosition): TThing;
begin
   if ((ThingPosition = tpOn) and (FParent is TThresholdLocation)) then
   begin
      Result := FParent.GetSurface();
   end
   else
   begin
      Assert(ThingPosition in [tpAt, tpIn]);
      ThingPosition := tpIn;
      Result := Self;
   end;
   Assert(Assigned(Result));
end;

function TDoorWay.GetLookIn(Perspective: TAvatar): UTF8String;
begin
   if (CanSeeThrough()) then
   begin
      Result := GetLookThrough(Perspective);
      if (Result = '') then
         Result := inherited;
   end
   else
      Result := inherited;
end;

function TDoorWay.GetLookUnder(Perspective: TAvatar): UTF8String;
var
   TheDoor: TDoor;
   Directions: TCardinalDirectionSet;
begin
   Result := '';
   TheDoor := GetDoor();
   if (Assigned(TheDoor)) then
   begin
      Directions := cdAllDirections;
      case (LocatePerspective(Perspective)) of
         rppFront: Exclude(Directions, cdReverse[FFrontSideFacesDirection]);
         rppBack: Exclude(Directions, FFrontSideFacesDirection);
         rppHere: ; // nothing to change
      end;
      Result := Capitalise(GetDefiniteName(Perspective)) + ' ' +
                TernaryConditional('contains', 'contain', IsPlural(Perspective)) + ' ' +
                TheDoor.GetIndefiniteName(Perspective) + '.' +
                WithSpaceIfNotEmpty(TheDoor.GetBasicDescription(Perspective, psThereIsAThingHere, Directions)) +
                WithNewlineIfNotEmpty(GetDescriptionObstacles(Perspective,
                         'Other than ' + TheDoor.GetDefiniteName(Perspective) + ', there is nothing in ' + GetDefiniteName(Perspective) + '.'));
   end
   else
   begin
      Result := GetDescriptionObstacles(Perspective, 'There is nothing in ' + GetDefiniteName(Perspective) + '.');
   end;
end;

function TDoorWay.GetLookThrough(Perspective: TAvatar): UTF8String;
begin
   Assert((not Assigned(GetDoor())) or
          IsOpen() or
          CanSeeThrough() or
          GetDoor().CanSeeUnder(Perspective));
   case (LocatePerspective(Perspective)) of
      rppFront: Result := GetLookTowardsDirection(Perspective, cdReverse[FFrontSideFacesDirection]);
      rppBack: Result := GetLookTowardsDirection(Perspective, FFrontSideFacesDirection);
      else
         Result := FParent.GetBasicDescription(Perspective, psThereIsAThingHere, cdAllDirections, Self); // we're probably at the threshold itself somehow
   end;
end;

function TDoorWay.GetDescriptionRemoteBrief(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Direction: TCardinalDirection): UTF8String;
var
   TheDoor: TDoor;
begin
   TheDoor := GetDoor();
   if (Assigned(TheDoor)) then
   begin
      Result := TheDoor.GetDescriptionRemoteBrief(Perspective, Mode, Direction);
   end
   else
   begin
      Result := inherited;
   end;
end;

function TDoorWay.GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection; LeadingPhrase: UTF8String; Options: TLeadingPhraseOptions): UTF8String;
var
   TheDoor: TDoor;
begin
   // this whole function is a weird mess of cases
   // we should rethink this all through
   // we should probably not bother using GetLookThrough either, since we know the direction we're looking
   TheDoor := GetDoor();
   if (not Assigned(TheDoor)) then
   begin
      if (CanSeeThrough()) then
      begin
         Perspective.AutoDisambiguated('looking through ' + GetDefiniteName(Perspective));
         Result := GetLookThrough(Perspective);
      end
      else
      begin
         // Defer to the default behavior but
         if ((lpMandatory in Options) or not (lpNamesTarget in Options)) then
            LeadingPhrase := LeadingPhrase + ','
         else
            LeadingPhrase := 'Looking';
         LeadingPhrase := LeadingPhrase + ' through ' + GetDefiniteName(Perspective);
         Include(Options, lpMandatory);
         Result := inherited;
      end;
   end
   else
   begin
      Exclude(Options, lpNamesTarget);
      if (IsOpen()) then
      begin
         if (CanSeeThrough()) then
         begin
            if (Direction <> cdOut) then
               Perspective.AutoDisambiguated('looking through the open ' + TheDoor.GetName(Perspective));
            Result := GetLookThrough(Perspective);
         end
         else
         begin
            Result := TheDoor.GetDescriptionRemoteDetailed(Perspective, Direction, LeadingPhrase, Options);
         end;
      end
      else
         Result := TheDoor.GetDescriptionRemoteDetailed(Perspective, Direction, LeadingPhrase, Options);
   end;
   /// XXX if we go through the no-door case above, and there's something blocking the door frame, this is ugly:
   Result := Result + WithSpaceIfNotEmpty(GetDescriptionObstacles(Perspective));
end;

function TDoorWay.GetDescriptionObstacles(Perspective: TAvatar; NoObstacleMessage: UTF8String = ''): UTF8String;
var
   Obstacles: TThingList;
begin
   Result := '';
   Obstacles := GetChildren(tpObtrusive);
   try
      if (Obstacles.Length > 0) then
         Result := 'Blocking ' +
                   GetDefiniteName(Perspective) + ' ' +
                   IsAre(Obstacles.IsPlural(Perspective)) + ' ' +
                   Obstacles.GetIndefiniteString(Perspective, 'and') + '.'
      else
         Result := NoObstacleMessage;
   finally
      Obstacles.Free();
   end;
end;

function TDoorWay.GetDescriptionEmpty(Perspective: TAvatar): UTF8String;
var
   TheDoor: TDoor;
begin
   TheDoor := GetDoor();
   if (Assigned(TheDoor)) then
      Result := 'Other than ' + TheDoor.GetDefiniteName(Perspective) + ', there is nothing in ' + GetDefiniteName(Perspective) + '.'
   else
      Result := inherited;
end;

function TDoorWay.GetDescriptionClosed(Perspective: TAvatar): UTF8String;
var
   TheDoor: TDoor;
begin
   TheDoor := GetDoor();
   Assert(Assigned(TheDoor) and not IsOpen());
   if (Assigned(TheDoor)) then
      Result := TheDoor.GetDescriptionClosed(Perspective)
   else
      Result := inherited;
end;

function TDoorWay.GetEntrance(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: TMessage; NotificationList: TAtomList): TAtom;
begin
   Assert(Message.IsValid);
   if (IsOpen() and (FParent is TThresholdLocation)) then
   begin
      if (Direction = cdIn) then
      begin
         case (LocatePerspective(Perspective)) of
            rppFront: Direction := cdReverse[FFrontSideFacesDirection];
            rppBack: Direction := FFrontSideFacesDirection;
            rppHere: ; // cdIn is fine
         end;
      end;
      PositionOverride := tpOn;
      Result := FParent.GetEntrance(Traveller, Direction, Perspective, PositionOverride, DisambiguationOpening, Message, NotificationList)
   end
   else
      Result := inherited;
end;

function TDoorWay.CanSeeIn(): Boolean;
begin
   Result := True;
end;

function TDoorWay.CanSeeThrough(): Boolean;
begin
   Result := (not Assigned(GetDoor())) or (IsOpen());
end;

function TDoorWay.Open(Perspective: TAvatar; var Message: TMessage): Boolean;
var
   TheDoor: TDoor;
begin
   Assert(Message.IsValid);
   TheDoor := GetDoor();
   if (Assigned(TheDoor)) then
   begin
      if (IsOpen()) then
      begin
         Message := TMessage.Create(mkRedundant, '_ in _ _ already open.',
                                                 [Capitalise(TheDoor.GetDefiniteName(Perspective)),
                                                  GetDefiniteName(Perspective),
                                                  IsAre(IsPlural(Perspective))]);
         Result := False;
      end
      else
      begin
         Assert(IsClear(), 'Something is mysteriously blocking the (closed!) door.');
         DoBroadcast([TheDoor, Perspective], Perspective, [C(M(@Perspective.GetDefiniteName)), SP,
                                                           MP(Perspective, M('opens'), M('open')), SP,
                                                           M(@TheDoor.GetDefiniteName), M('.')]);
         FOpened := True;
         Result := True;
      end;
   end
   else
   begin
      Assert(IsOpen());
      Message := TMessage.Create(mkNoDoor, '_ _ wide open.', [Capitalise(GetDefiniteName(Perspective)), IsAre(IsPlural(Perspective))]);
      Result := False;
   end;
end;

function TDoorWay.Close(Perspective: TAvatar; var Message: TMessage): Boolean;
var
   TheDoor: TDoor;
   Obstacles, MoreObstacles: TThingList;
begin
   Assert(Message.IsValid);
   TheDoor := GetDoor();
   if (Assigned(TheDoor)) then
   begin
      if (not IsOpen()) then
      begin
         Message := TMessage.Create(mkRedundant, '_ _ _ _ already closed.',
                                                 [Capitalise(TheDoor.GetDefiniteName(Perspective)),
                                                  ThingPositionToString(TheDoor.Position),
                                                  GetDefiniteName(Perspective),
                                                  IsAre(IsPlural(Perspective))]);
         Result := False;
      end
      else
      begin
         Obstacles := GetChildren(tpObtrusive);
         try
            MoreObstacles := TheDoor.GetChildren(tpObtrusive);
            try
               Obstacles.AdoptList(MoreObstacles);
            finally
               MoreObstacles.Free();
            end;
            if (Obstacles.Length > 0) then
            begin
               Message := TMessage.Create(mkBlocked, '_ cannot be closed; _ _ in the way.',
                                          [Capitalise(TheDoor.GetDefiniteName(Perspective)),
                                           Obstacles.GetDefiniteString(Perspective, 'and'),
                                           IsAre(Obstacles.IsPlural(Perspective))]);
               Result := False;
               exit;
            end;
         finally
            Obstacles.Free();
         end;
         DoBroadcast([TheDoor, Perspective], Perspective, [C(M(@Perspective.GetDefiniteName)), SP,
                                                           MP(Perspective, M('closes'), M('close')), SP,
                                                           M(@TheDoor.GetDefiniteName), M('.')]);
         Result := True;
         FOpened := False;
      end;
   end
   else
   begin
      Assert(IsOpen());
      Message := TMessage.Create(mkNoDoor, 'There is nothing in _ with which to close _.', [GetDefiniteName(Perspective), GetObjectPronoun(Perspective)]);
      Result := False;
   end;
end;

function TDoorWay.GetFeatures(): TThingFeatures;
begin
   Result := inherited;
   Result := Result + [tfCanHaveThingsPushedIn];
end;

function TDoorWay.CanTraverse(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar): Boolean;
begin
   Result := (not Assigned(GetDoor())) or (IsOpen());
end;


constructor TDoor.Create(Name: UTF8String; Pattern: UTF8String; FrontSide, BackSide: TDoorSide; AMass: TThingMass = tmHeavy; ASize: TThingSize = tsMassive);
begin
   inherited Create(Name, Pattern, '' { description }, AMass, ASize);
   Assert(Assigned(FrontSide));
   FFrontSide := FrontSide;
   Add(FrontSide, tpAmbiguousPartOfImplicit);
   Assert(Assigned(BackSide));
   FBackSide := BackSide;
   Add(BackSide, tpAmbiguousPartOfImplicit);
end;

constructor TDoor.Read(Stream: TReadStream);
begin
   inherited;
   Stream.ReadReference(@Pointer(FFrontSide));
   Stream.ReadReference(@Pointer(FBackSide));
end;

procedure TDoor.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteReference(FFrontSide);
   Stream.WriteReference(FBackSide);
end;

class function TDoor.CreateFromProperties(Properties: TTextStreamProperties): TDoor;
var
   Name: UTF8String;
   Pattern: UTF8String;
   FrontSide, BackSide: TThing;
   MassValue: TThingMass = tmHeavy;
   SizeValue: TThingSize = tsMassive;
   StreamedChildren: TStreamedChildren;
begin
   while (not Properties.Done) do
   begin
      if (Properties.HandleUniqueStringProperty(pnName, Name) and
          Properties.HandleUniqueStringProperty(pnPattern, Pattern) and
          TThing.HandleUniqueThingProperty(Properties, pnFrontSide, FrontSide, TDoorSide) and {BOGUS Hint: Local variable "FrontSide" does not seem to be initialized}
          TThing.HandleUniqueThingProperty(Properties, pnBackSide, BackSide, TDoorSide) and {BOGUS Hint: Local variable "BackSide" does not seem to be initialized}
          Properties.specialize HandleUniqueEnumProperty<TThingMass>(pnMass, MassValue) and
          Properties.specialize HandleUniqueEnumProperty<TThingSize>(pnSize, SizeValue) and
          HandleChildProperties(Properties, StreamedChildren)) then
       Properties.FailUnknownProperty();
   end;
   Properties.EnsureSeen([pnName, pnPattern, pnFrontSide, pnBackSide]);
   Result := Create(Name, Pattern, FrontSide as TDoorSide, BackSide as TDoorSide, MassValue, SizeValue);
   StreamedChildren.Apply(Result);
end;

class procedure TDoor.DescribeProperties(Describer: TPropertyDescriber);
begin
   Describer.AddProperty(pnName, ptString);
   Describer.AddProperty(pnPattern, ptPattern);
   Describer.AddProperty(pnFrontSide, ptDoorSide);
   Describer.AddProperty(pnBackSide, ptDoorSide);
   Describer.AddProperty(pnMass, ptMass);
   Describer.AddProperty(pnSize, ptSize);
   Describer.AddProperty(pnChild, ptChild);
end;

function TDoor.GetDoorWay(): TDoorWay;
begin
   if ((FParent is TDoorWay) and (FPosition = (FParent as TDoorWay).tpOfficialDoorPosition)) then
      Result := FParent as TDoorWay
   else
      Result := nil;
end;

function TDoor.IsChildTraversable(Child: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean;
var
   VisibleSides: TVisibleSides;
begin
   VisibleSides := DetermineVisibleSides(Perspective);
   if (Child = FBackSide) then
      Result := (vsBack in VisibleSides) and inherited
   else
   if (Child = FFrontSide) then
      Result := (vsFront in VisibleSides) and inherited
   else
      Result := inherited;
end;

function TDoor.GetMatcherFlags(Perspective: TAvatar): TMatcherFlags;
var
   TheDoorWay: TDoorWay;
begin
   Result := inherited;
   TheDoorWay := GetDoorWay();
   if (Assigned(TheDoorWay)) then
   begin
      if (TheDoorWay.IsOpen()) then
         Result := Result or (1 shl mfOpen) // $R-
      else
         Result := Result or (1 shl mfClosed); // $R-
   end;
end;

function TDoor.LocatePerspective(Perspective: TAvatar): TRelativePerspectivePosition;
var
   TheDoorWay: TDoorWay;
begin
   TheDoorWay := GetDoorWay();
   if (Assigned(TheDoorWay)) then
      Result := TheDoorWay.LocatePerspective(Perspective)
   else
      Result := rppHere;
end;

function TDoor.DetermineVisibleSides(Perspective: TAvatar): TVisibleSides;
var
   TheDoorWay: TDoorWay;
begin
   TheDoorWay := GetDoorWay();
   if (Assigned(TheDoorWay) and not TheDoorWay.IsOpen()) then
   begin
      case (LocatePerspective(Perspective)) of
         rppFront: Result := [vsFront];
         rppBack: Result := [vsBack];
       else
          Result := [vsFront, vsBack];
      end;
   end
   else
      Result := [vsFront, vsBack];
end;

function TDoor.CanPut(Thing: TThing; ThingPosition: TThingPosition; Care: TPlacementStyle; Perspective: TAvatar; var Message: TMessage): Boolean;
var
   TheDoorWay: TDoorWay;
begin
   Assert(Message.IsValid);
   if (ThingPosition = tpOn) then
   begin
      TheDoorWay := GetDoorWay();
      if (Assigned(TheDoorWay)) then
      begin
         if (not TheDoorWay.IsOpen()) then
         begin
            Message := TMessage.Create(mkCannotPutOnBecauseInstalled, TheDoorWay.GetDescriptionNoInside(Perspective));
            Result := False;
            exit;
         end;
      end;
   end;
   Result := inherited;
end;

procedure TDoor.HandlePassedThrough(Traveller: TThing; AFrom, ATo: TAtom; AToPosition: TThingPosition; Perspective: TAvatar);
var
   Obstacles: TThingList;
   TheDoorWay: TDoorWay;
   Ground: TAtom;
   Thing: TThing;
   Message: TMessage;
begin
   TheDoorWay := GetDoorWay();
   Assert(Assigned(TheDoorWay) and TheDoorWay.IsOpen());
   Obstacles := GetChildren(tpObtrusive);
   try
      if (Obstacles.Length > 0) then
      begin
         if (TheDoorWay.Parent is TThresholdLocation) then
         begin
            Ground := TheDoorWay.Parent.GetSurface();
            Assert(Assigned(Ground));
            // mkThingsFall
            DoBroadcastAll([Self, Ground, Traveller, Perspective],
                           [C(M(@Obstacles.GetDefiniteString, M('and'))), SP,
                            MP(@Obstacles.IsPlural, M('falls'), M('fall')), SP,
                            M(ThingPositionToDirectionString(TDoorWay.tpOnGround)), SP,
                            M(@Ground.GetDefiniteName), SP,
                            M('as'), SP,
                            M(@Traveller.GetDefiniteName), SP,
                            MP(Traveller, M('passes'), M('pass')), SP,
                            M('through'), SP,
                            M(@GetDefiniteName), M(','), SP,
                            M('barely missing'), SP,
                            M(@Traveller.GetObjectPronoun), SP,
                            M('on'), SP,
                            M(@Obstacles.GetPossessiveAdjective), SP,
                            M('way down.')]);
            // XXX things should actually hit the Traveller on the way down...
            for Thing in Obstacles do
            begin
               {$IFOPT C+}
                  Message := TMessage.Create();
                  Assert(Ground.CanPut(Thing, TDoorWay.tpOnGround, psRoughly, Perspective, Message));
                  Assert(Message.AsKind = mkSuccess);
                  Assert(Message.AsText = '');
               {$ENDIF}
               Ground.Put(Thing, TDoorWay.tpOnGround, psRoughly, Perspective);
            end;
         end
         else
         begin
            // You wade through the foo and the foo on your way through the door.
            DoBroadcastAll([Self, Traveller, Perspective],
                           [C(M(@Traveller.GetDefiniteName)), SP,
                           MP(Traveller, M('wades'), M('wade')), SP,
                           M('through'), SP,
                           M(@Obstacles.GetDefiniteString, M('and')), SP,
                           M('on'), SP,
                           M(@Traveller.GetPossessiveAdjective), SP,
                           M('way through'), SP,
                           M(@GetDefiniteName), M('.')]);
         end;
      end;
   finally
      Obstacles.Free();
   end;
end;

function TDoor.GetDescriptionSelf(Perspective: TAvatar): UTF8String;
var
   VisibleSides: TVisibleSides;
   TheDoorWay: TDoorWay;
begin
   VisibleSides := DetermineVisibleSides(Perspective);
   if (VisibleSides = [vsFront]) then
   begin
      Result := FFrontSide.GetDescriptionSelf(Perspective);
   end
   else
   if (VisibleSides = [vsBack]) then
   begin
      Result := FBackSide.GetDescriptionSelf(Perspective);
   end
   else
   begin
      Assert(VisibleSides = [vsFront, vsBack]);
      if (FDescription <> '') then
         Result := FDescription
      else
         Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + TernaryConditional('has', 'have', IsPlural(Perspective)) + ' two sides. On the front, ' + FFrontSide.GetDescriptionSelfSentenceFragment(Perspective) + ' On the back, ' + FBackSide.GetDescriptionSelfSentenceFragment(Perspective);
   end;
   Assert(Result <> '');
   TheDoorWay := GetDoorWay();
   if (Assigned(TheDoorWay)) then
   begin
      Result := Result + ' ' + Capitalise(GetDefiniteName(Perspective)) + ' ' + TernaryConditional('is', 'are', IsPlural(Perspective)) + ' ' + TernaryConditional('closed', 'open', TheDoorWay.IsOpen()) + '.';
   end;
end;

function TDoor.GetLookUnder(Perspective: TAvatar): UTF8String;
var
   TheDoorWay: TDoorWay;
begin
   TheDoorWay := GetDoorWay();
   if (Assigned(TheDoorWay)) then
   begin
      if (TheDoorWay.IsOpen()) then
         Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + TernaryConditional('is', 'are', IsPlural(Perspective)) + ' open.'
      else
      if (CanSeeUnder(Perspective)) then
      begin
         Assert(LocatePerspective(Perspective) in [rppFront, rppBack]);
         Result := TheDoorWay.GetLookThrough(Perspective);
         if (Result = '') then
            Result := inherited;
      end
      else
         Result := GetCannotSeeUnder(Perspective);
   end
   else
      Result := inherited;
end;

function TDoor.CanSeeUnder(Perspective: TAvatar): Boolean;
begin
   Result := True;
end;

function TDoor.GetCannotSeeUnder(Perspective: TAvatar): UTF8String;
begin
   Assert(not CanSeeUnder(Perspective));
   Result := Capitalise(Perspective.GetDefiniteName(Perspective)) + ' cannot see under ' + GetDefiniteName(Perspective) + '.';
end;

function TDoor.GetDescriptionClosed(Perspective: TAvatar): UTF8String;
var
   TheDoorWay: TDoorWay;
begin
   TheDoorWay := GetDoorWay();
   Assert(Assigned(TheDoorWay) and not TheDoorWay.IsOpen());
   if (Assigned(TheDoorWay) and not TheDoorWay.IsOpen()) then
      Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + IsAre(IsPlural(Perspective)) + ' closed.'
   else
      Result := inherited;
end;

function TDoor.GetEntrance(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: TMessage; NotificationList: TAtomList): TAtom;
var
   TheDoorWay: TThing;
begin
   Assert(Message.IsValid);
   TheDoorWay := GetDoorWay();
   if (Assigned(TheDoorWay)) then
      Result := TheDoorWay.GetEntrance(Traveller, Direction, Perspective, PositionOverride, DisambiguationOpening, Message, NotificationList)
   else
      Result := inherited;
end;

function TDoor.GetFeatures(): TThingFeatures;
var
   TheDoorWay: TThing;
begin
   Result := inherited;
   TheDoorWay := GetDoorWay();
   if (Assigned(TheDoorWay)) then
   begin
      if (TheDoorWay.IsOpen()) then
         Result := Result + [tfClosable]
      else
         Result := Result + [tfOpenable];
   end;
end;

function TDoor.Open(Perspective: TAvatar; var Message: TMessage): Boolean;
var
   TheDoorWay: TDoorWay;
begin
   Assert(Message.IsValid);
   TheDoorWay := GetDoorWay();
   if (Assigned(TheDoorWay)) then
   begin
      Result := TheDoorWay.Open(Perspective, Message);
   end
   else
   begin
      Message := TMessage.Create(mkBogus, '_ _ _ _, so there is no way to open _.',
                                 [Capitalise(GetDefiniteName(Perspective)),
                                  IsAre(IsPlural(Perspective)),
                                  ThingPositionToString(FPosition),
                                  FParent.GetDefiniteName(Perspective),
                                  GetObjectPronoun(Perspective)]);
      Result := False;
   end;
end;

function TDoor.Close(Perspective: TAvatar; var Message: TMessage): Boolean;
var
   TheDoorWay: TDoorWay;
begin
   Assert(Message.IsValid);
   TheDoorWay := GetDoorWay();
   if (Assigned(TheDoorWay)) then
   begin
      Result := TheDoorWay.Close(Perspective, Message);
   end
   else
   begin
      Message := TMessage.Create(mkBogus, '_ _ _ _, so there is no way to close _.',
                                 [Capitalise(GetDefiniteName(Perspective)),
                                  IsAre(IsPlural(Perspective)),
                                  ThingPositionToString(FPosition),
                                  FParent.GetDefiniteName(Perspective),
                                  GetObjectPronoun(Perspective)]);
      Result := False;
   end;
end;


constructor TDoorSide.Create(Name: UTF8String; Pattern: UTF8String; Description: UTF8String; AMass: TThingMass = tmLight; ASize: TThingSize = tsSmall);
begin
   { needed for default values }
   inherited;
end;

class function TDoorSide.CreateFromProperties(Properties: TTextStreamProperties): TDoorSide;
var
   Name: UTF8String;
   Pattern: UTF8String;
   Description: UTF8String;
   MassValue: TThingMass = tmLight;
   SizeValue: TThingSize = tsSmall;
   StreamedChildren: TStreamedChildren;
begin
   while (not Properties.Done) do
   begin
      if (Properties.HandleUniqueStringProperty(pnName, Name) and
          Properties.HandleUniqueStringProperty(pnPattern, Pattern) and
          Properties.HandleUniqueStringProperty(pnDescription, Description) and
          Properties.specialize HandleUniqueEnumProperty<TThingMass>(pnMass, MassValue) and
          Properties.specialize HandleUniqueEnumProperty<TThingSize>(pnSize, SizeValue) and
          HandleChildProperties(Properties, StreamedChildren)) then
         Properties.FailUnknownProperty();
   end;
   Properties.EnsureSeen([pnName, pnPattern, pnDescription]);
   Result := Create(Name, Pattern, Description);
   StreamedChildren.Apply(Result);
end;

class procedure TDoorSide.DescribeProperties(Describer: TPropertyDescriber);
begin
   Describer.AddProperty(pnName, ptString);
   Describer.AddProperty(pnPattern, ptPattern);
   Describer.AddProperty(pnDescription, ptString);
   Describer.AddProperty(pnMass, ptMass);
   Describer.AddProperty(pnSize, ptSize);
   Describer.AddProperty(pnChild, ptChild);
end;

function TDoorSide.GetMatcherFlags(Perspective: TAvatar): TMatcherFlags;
var
   TheDoorWay: TDoorWay;
begin
   Result := inherited;
   if (FParent is TDoor) then
   begin
      TheDoorWay := (FParent as TDoor).GetDoorWay();
      if (Assigned(TheDoorWay) and not TheDoorWay.IsOpen()) then
         exit;
   end;
   Result := Result or (1 shl mfOtherSideVisible); // $R-
end;

function TDoorSide.GetDescriptionSelf(Perspective: TAvatar): UTF8String;
begin
   Result := Capitalise(inherited);
end;

function TDoorSide.GetDescriptionSelfSentenceFragment(Perspective: TAvatar): UTF8String;
begin
   Result := inherited GetDescriptionSelf(Perspective);
end;

function TDoorSide.GetRepresentative(): TAtom;
begin
   Assert(Assigned(FParent));
   Result := FParent;
end;

function TDoorSide.GetSurface(): TThing;
begin
   Result := nil;
end;

function TDoorSide.CanMove(Perspective: TAvatar; var Message: TMessage): Boolean;
begin
   Assert(Message.IsValid);
   Result := False;
   Message := TMessage.Create(mkCannotMoveBecauseLocation, '_ _ _ _.',
                                                           [Capitalise(GetDefiniteName(Perspective)),
                                                            IsAre(IsPlural(Perspective)),
                                                            ThingPositionToString(FPosition),
                                                            FParent.GetDefiniteName(Perspective)]);
end;

function TDoorSide.CanPut(Thing: TThing; ThingPosition: TThingPosition; Care: TPlacementStyle; Perspective: TAvatar; var Message: TMessage): Boolean;
begin
   Assert(Message.IsValid);
   if ((ThingPosition = tpOn) and (FCannotPlaceExcuse = '')) then
   begin
      Result := False;
      Message := TMessage.Create(mkBogus, 'There does not seem to be any way to attach _ to _.', [Thing.GetIndefiniteName(Perspective), GetDefiniteName(Perspective)]);
   end
   else
   begin
      Result := inherited;
   end;
end;


function TThresholdSurface.CanPut(Thing: TThing; ThingPosition: TThingPosition; Care: TPlacementStyle; Perspective: TAvatar; var Message: TMessage): Boolean;
begin
   Assert(Message.IsValid);
   Result := inherited;
   if (Result and
          Assigned(FParent) and
          (FParent is TThresholdLocation) and
          ((FParent as TThresholdLocation).FSource is TDoorway) and
          Assigned(((FParent as TThresholdLocation).FSource as TDoorway).GetDoor()) and
          not ((FParent as TThresholdLocation).FSource as TDoorway).IsOpen()) then
   begin
      // can't put something inside a closed doorway, whether it could itself be a door or not
      Message := TMessage.Create(mkClosed, ((FParent as TThresholdLocation).FSource as TDoorway).GetDescriptionNoInside(Perspective));
      Result := False;
   end;
end;


constructor TThresholdLocation.Create(PassageWay: TThing; Surface: TThing);
begin
   inherited Create(PassageWay, tpAtImplicit, Surface);
end;

class function TThresholdLocation.CreateFromProperties(Properties: TTextStreamProperties): TThresholdLocation;
var
   PassageWay, Surface: TThing;
   StreamedLandmarks: TStreamedLandmarks;
   StreamedChildren: TStreamedChildren;
begin
   while (not Properties.Done) do
   begin
      if (TThing.HandleUniqueThingProperty(Properties, pnPassageWay, PassageWay, TThing) and {BOGUS Hint: Local variable "PassageWay" does not seem to be initialized}
          TThing.HandleUniqueThingProperty(Properties, pnSurface, Surface, TThing) and {BOGUS Hint: Local variable "Surface" does not seem to be initialized}
          HandleLandmarkProperties(Properties, StreamedLandmarks) and
          HandleChildProperties(Properties, StreamedChildren)) then
       Properties.FailUnknownProperty();
   end;
   Properties.EnsureSeen([pnPassageWay, pnSurface]);
   Result := Create(PassageWay, Surface);
   StreamedLandmarks.Apply(Result);
   StreamedChildren.Apply(Result);
end;

class procedure TThresholdLocation.DescribeProperties(Describer: TPropertyDescriber);
begin
   Describer.AddProperty(pnPassageWay, ptThing);
   Describer.AddProperty(pnSurface, ptThing);
   Describer.AddProperty(pnLandmark, ptLandmark);
   Describer.AddProperty(pnChild, ptChild);
end;

function TThresholdLocation.GetTitle(Perspective: TAvatar): UTF8String;
begin
   Result := GetName(Perspective) + WithSpaceIfNotEmpty(GetContextFragment(Perspective, tpAt, nil));
end;

function TThresholdLocation.GetLookTowardsDirectionDefault(Perspective: TAvatar; Direction: TCardinalDirection): UTF8String;
begin
   Result := FSource.GetPresenceStatement(Perspective, psThereIsAThingThere);
end;

function TThresholdLocation.GetDescriptionHere(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Directions: TCardinalDirectionSet = cdAllDirections; Context: TAtom = nil): UTF8String;
begin
   Assert(Assigned(FSource));
   if (Context <> FSource) then
      Result := FSource.GetDescriptionSelf(Perspective) + WithNewlineIfMultiline(inherited)
   else
      Result := inherited;
end;

function TThresholdLocation.GetDescriptionRemoteBrief(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Direction: TCardinalDirection): UTF8String;
begin
   Result := FSource.GetDescriptionRemoteBrief(Perspective, Mode, Direction);
end;

function TThresholdLocation.GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection; LeadingPhrase: UTF8String; Options: TLeadingPhraseOptions): UTF8String;
begin
   Exclude(Options, lpNamesTarget);
   Result := FSource.GetDescriptionRemoteDetailed(Perspective, Direction, LeadingPhrase, Options);
end;

function TThresholdLocation.GetContextFragment(Perspective: TAvatar; PertinentPosition: TThingPosition; Context: TAtom = nil): UTF8String;
const
   kNecessaryOptions = [loAutoDescribe, loPermissibleNavigationTarget];
var
   Direction: TCardinalDirection;
   Landmark: TDirectionalLandmark;
   List: TAtomList;
begin
   Assert(Context <> Self);
   List := TAtomList.Create([slDropDuplicates]);
   for Direction in TCardinalDirection do // XXX maybe use cdOrderedCardinalDirections?
      for Landmark in FLandmarks do // this is rather expensive, we're doing 12 loops over the landmarks
         if (Landmark.Direction = Direction) then
         begin
            Assert(Context <> Landmark.Atom);
            if (Landmark.Options * kNecessaryOptions = kNecessaryOptions) then
               List.AppendItem(Landmark.Atom);
         end;
   if (List.Length >= 2) then
      Result := 'between ' + List.GetDefiniteString(Perspective, 'and')
   else
   if (List.Length = 1) then
      Result := 'near ' + List.First.GetDefiniteName(Perspective)
   else
      Result := inherited;
   List.Free();
end;

procedure TThresholdLocation.GetNearbyThingsByClass(List: TThingList; FromOutside: Boolean; Filter: TThingClass);
var
   Landmark: TDirectionalLandmark;
begin
   // The original implementation of this only took the first landmark in each direction.
   // This was changed when landmarks were simplified. We may need to bring that logic back.
   for Landmark in FLandmarks do
      Landmark.Atom.GetNearbyThingsByClass(List, True, Filter);
end;

procedure TThresholdLocation.EnumerateExplicitlyReferencedThings(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; FromOutside, FromFarAway: Boolean; Directions: TCardinalDirectionSet; Reporter: TThingReporter);
begin
   if (FromFarAway) then
      FSource.ProxiedEnumerateExplicitlyReferencedThings(Tokens, Start, Perspective, True, True, Directions, Reporter);
   inherited;
end;

function TThresholdLocation.ProxiedFindThingTraverser(Thing: TThing; Perspective: TAvatar; Options: TFindThingOptions): Boolean;
begin
   Result := FSource.ProxiedFindThingTraverser(Thing, Perspective, Options);
end;

procedure TThresholdLocation.ProxiedFindMatchingThings(Perspective: TAvatar; Options: TFindMatchingThingsOptions; PositionFilter: TThingPositionFilter; PropertyFilter: TThingFeatures; List: TThingList);
begin
   FSource.ProxiedFindMatchingThings(Perspective, Options, PositionFilter, PropertyFilter, List);
end;

function TThresholdLocation.GetEntrance(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: TMessage; NotificationList: TAtomList): TAtom;
begin
   Assert(Message.IsValid);
   Assert(Assigned(FSource));
   if ((FSource is TThresholdThing) and (FSource as TThresholdThing).CanTraverse(Traveller, Direction, Perspective)) then
   begin
      DisambiguationOpening := FSource;
      NotificationList.AppendItem(FSource);
      Result := GetAtomForDirectionalNavigation(Direction);
      if (Assigned(Result)) then
         Result := Result.GetEntrance(Traveller, Direction, Perspective, PositionOverride, DisambiguationOpening, Message, NotificationList)
      else
         Result := inherited;
   end
   else
   begin
      Message := TMessage.Create(mkClosed, FSource.GetDescriptionNoInside(Perspective));
      Result := nil;
   end;
end;

initialization
{$INCLUDE registrations/threshold.inc}
end.
