{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit player;

interface

uses
   storable, physics, grammarian, messages, lists, thingdim, thingseeker, typinfo;

const
   MaxCarrySingleMass = tmPonderous; { inclusive }
   MaxCarryMass: TThingMassManifest = (0, 0, 2, 0); { exclusive }
   MaxCarrySize = tsGigantic; { exclusive }
   MaxPushMass = tmLudicrous; { exclusive }
   MaxPushSize = tsLudicrous; { exclusive }
   MaxShakeMass: TThingMassManifest = (0, 2, 0, 0); { exclusive }
   MaxShakeSize = tsGigantic; { exclusive }
   MaxCarryCount = 10; { inclusive }
   MaxBalanceCount = 1; { inclusive }

type
   TPronouns = (pHe, pShe, pSingularThey, pPluralThey, pIt, pZe);

   TTalkVolume = (tvWhispering, tvSpeaking, tvShouting);

   TActionVerb = (avNone,
                  avLook, avLookDirectional, avLookAt, avExamine, avRead, avLookUnder, avLookIn, avInventory, avFind,
                  avGo, avEnter, avClimbOn, avUseTransportation,
                  avTake, avPut, avRelocate, avMove, avPush, avRemove, avPress, avShake,
                  avDig, avDigDirection,
                  avOpen, avClose,
                  avTalk, avDance,
                  {$IFDEF DEBUG} avDebugStatus, avDebugLocation, avDebugLocations, avDebugThings, avDebugThing, avDebugTeleport,
                                 avDebugMake, avDebugConnect, avDebugListClasses, avDebugDescribeClass, avDebugDescribeEnum, {$ENDIF}
                  avPronouns, avHelp, avQuit);

   // When adding allocated objects to this record, remember to free them in TWorld.Perform (world.pas).
   TAction = record
     case Verb: TActionVerb of
      avNone: ();
      avLook: ();
      avLookDirectional: (LookDirection: TCardinalDirection);
      avLookAt: (LookAtSubject: TThing);
      avExamine: (ExamineSubject: TThing);
      avRead: (ReadSubject: TThing);
      avLookUnder: (LookUnder: TThing);
      avLookIn: (LookIn: TThing);
      avInventory: ();
      avFind: (FindSubject: TThing);
      avGo: (GoDirection: TCardinalDirection);
      avEnter: (EnterSubject: TThing; EnterRequiredAbilities: TNavigationAbilities);
      avClimbOn: (ClimbOnSubject: TThing; ClimbOnRequiredAbilities: TNavigationAbilities);
      avUseTransportation: (UseTransportationInstruction: TTransportationInstruction);
      avTake: (TakeSubject: TThingList);
      avPut: (PutSubject: TThingList; PutTarget: TAtom; PutPosition: TThingPosition; PutCare: TPlacementStyle);
      avRelocate: (RelocateSubject: TThingList);
      avMove: (MoveSubject: TThingList; MoveTarget: TThing; MovePosition: TThingPosition); // MovePosition should be tpOn, tpIn, or, if ambiguous, tpAt
      avPush: (PushSubject: TThingList; PushDirection: TCardinalDirection);
      avRemove: (RemoveSubject: TThingList; RemoveFromPosition: TThingPosition; RemoveFromObject: TThing);
      avPress: (PressSubject: TThingList);
      avShake: (ShakeSubject: TThingList);
      avDig {and avDigDirection}: (DigSpade: TThing; case TActionVerb of avDig: (DigTarget: TThing); avDigDirection: (DigDirection: TCardinalDirection));
      avOpen: (OpenTarget: TThing);
      avClose: (CloseTarget: TThing);
      avTalk: (TalkTarget: TThing; TalkMessage: PUTF8String; TalkVolume: TTalkVolume);
      avDance: ();
      avPronouns: (Pronouns: TPronouns);
      {$IFDEF DEBUG}
      avDebugStatus: ();
      avDebugLocations: ();
      avDebugLocation: ();
      avDebugThings: (DebugThings: TThingList);
      avDebugThing: (DebugThing: TThing);
      avDebugTeleport: (DebugTarget: TAtom);
      avDebugMake: (DebugMakeData: PUTF8String);
      avDebugConnect: (DebugConnectDirection: TCardinalDirection; DebugConnectSource: TLocation; DebugConnectTarget: TAtom; DebugConnectOptions: TLandmarkOptions; DebugConnectBidirectional: Boolean);
      avDebugListClasses: (DebugSuperclass: TClass);
      avDebugDescribeClass: (DebugDescribeClass: TAtomClass);
      avDebugDescribeEnum: (DebugDescribeEnumTypeInfo: PTypeInfo);
      {$ENDIF}
      avHelp: ();
      avQuit: ();
   end;

   TMessageEvent = procedure (Message: UTF8String) of object;
   TForceDisconnectEvent = procedure () of object;

   TPlayer = class(TAvatar) // @RegisterStorableClass
    protected
      FName, FPassword: UTF8String;
      FPronouns: TPronouns;
      FOnMessage: TMessageEvent; { transient }
      FOnForceDisconnect: TForceDisconnectEvent; { transient }
      FContext: UTF8String; { transient }
      function CanCarryThing(Thing: TThing; var Message: TMessage): Boolean;
      function CanPushThing(Thing: TThing; var Message: TMessage): Boolean;
      function CanShakeThing(Thing: TThing; var Message: TMessage): Boolean;
      function CanMoveWithoutLoops(Subject: TThing; Target: TAtom; ThingPosition: TThingPosition; var Message: TMessage): Boolean;
      function IsNotUnderUs(Subject: TThing; var Message: TMessage): Boolean;
      procedure DoPutInternal(CurrentSubject: TThing; Target: TAtom; ThingPosition: TThingPosition; Care: TPlacementStyle);
      procedure DoPushInternal(CurrentSubject: TThing; Destination: TAtom; ThingPosition: TThingPosition; Care: TPlacementStyle; DestinationMustBeReachable: Boolean; NotificationList: TAtomList);
      procedure SetContext(Context: UTF8String);
      procedure ResetContext();
    public
      constructor Create(AName: UTF8String; APassword: UTF8String; APronouns: TPronouns);
      destructor Destroy(); override;
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      procedure DoLook(); override;
      procedure DoInventory();
      procedure AvatarMessage(Message: TMessage); override;
      procedure SendRawMessage(Message: UTF8String);
      procedure SendMessage(Message: UTF8String);
      procedure AutoDisambiguated(Message: UTF8String); override;
      function GetImpliedThing(Scope: TAllImpliedScope; FeatureFilter: TThingFeatures): TThing;
      function HasAbilityToTravelTo(Destination: TAtom; RequiredAbilities: TNavigationAbilities; Perspective: TAvatar; var Message: TMessage): Boolean; override;
      function GetIntrinsicMass(): TThingMass; override;
      function GetIntrinsicSize(): TThingSize; override;
      function GetName(Perspective: TAvatar): UTF8String; override;
      function GetLongName(Perspective: TAvatar): UTF8String; override;
      function GetIndefiniteName(Perspective: TAvatar): UTF8String; override;
      function GetDefiniteName(Perspective: TAvatar): UTF8String; override;
      function GetLongDefiniteName(Perspective: TAvatar): UTF8String; override;
      function GetSubjectPronoun(Perspective: TAvatar): UTF8String; override; // I
      function GetObjectPronoun(Perspective: TAvatar): UTF8String; override; // me
      function GetReflexivePronoun(Perspective: TAvatar): UTF8String; override; // myself
      function GetPossessivePronoun(Perspective: TAvatar): UTF8String; override; // mine
      function GetPossessiveAdjective(Perspective: TAvatar): UTF8String; override; // my
      function IsPlural(Perspective: TAvatar): Boolean; override;
      function GetPresenceStatement(Perspective: TAvatar; Mode: TGetPresenceStatementMode): UTF8String; override;
      function GetDescriptionSelf(Perspective: TAvatar): UTF8String; override;
      procedure AnnounceAppearance(); override;
      procedure AnnounceDisappearance(); override;
      procedure AnnounceDeparture(Destination: TAtom; Direction: TCardinalDirection); override;
      procedure AnnounceDeparture(Destination: TAtom); override; {BOGUS Hint: Value parameter "Destination" is assigned but never used}
      procedure AnnounceArrival(Source: TAtom; Direction: TCardinalDirection); override; {BOGUS Hint: Value parameter "Source" is assigned but never used}
      procedure AnnounceArrival(Source: TAtom); override; {BOGUS Hint: Value parameter "Source" is assigned but never used}
      function CanSurfaceHold(const Manifest: TThingSizeManifest; const ManifestCount: Integer): Boolean; override;
      procedure HandleAdd(Thing: TThing; Blame: TAvatar); override;
      function HasConnectedPlayer(): Boolean; override;
      function IsReadyForRemoval(): Boolean; override;
      procedure RemoveFromWorld(); override;
      function IsExplicitlyReferencedThing(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; out Count: Cardinal; out GrammaticalNumber: TGrammaticalNumber): Boolean; override;
      function GetUsername(): UTF8String;
      function GetPassword(): UTF8String;
      procedure DoFind(Subject: TThing);
      procedure DoLookUnder(Subject: TThing);
      procedure DoNavigation(Target: TAtom; ThingPosition: TThingPosition; RequiredAbilities: TNavigationAbilities);
      procedure DoNavigation(Direction: TCardinalDirection);
      procedure DoNavigation(Instruction: TTransportationInstruction);
      procedure DoTake(Subject: TThingList);
      procedure DoPut(Subject: TThingList; Target: TAtom; ThingPosition: TThingPosition; Care: TPlacementStyle);
      procedure DoRelocate(Subject: TThingList);
      procedure DoMove(Subject: TThingList; Target: TThing; ThingPosition: TThingPosition);
      procedure DoPushDirectional(Subject: TThingList; Direction: TCardinalDirection);
      procedure DoRemove(Subject: TThingList; RequiredPosition: TThingPosition; RequiredParent: TThing);
      procedure DoPress(Subject: TThingList);
      procedure DoShake(Subject: TThingList);
      procedure DoDig(Target: TThing; Spade: TThing);
      procedure DoDig(Direction: TCardinalDirection; Spade: TThing);
      procedure DoOpen(Subject: TThing);
      procedure DoClose(Subject: TThing);
      procedure DoTalk(Target: TThing; Message: UTF8String; Volume: TTalkVolume);
      procedure DoDance();
      procedure DoSetPronouns(Pronouns: TPronouns);
      {$IFDEF DEBUG}
      procedure DoDebugStatus();
      procedure DoDebugLocation();
      procedure DoDebugThings(Things: TThingList);
      procedure DoDebugThing(Thing: TThing);
      procedure DoDebugTeleport(Target: TAtom);
      procedure DoDebugListClasses(Superclass: TClass);
      function DebugGetCurrentLocation(): TLocation; // used by parser.inc
      {$ENDIF}
      procedure Adopt(AOnMessage: TMessageEvent; AOnForceDisconnect: TForceDisconnectEvent);
      procedure Abandon();
      property Name: UTF8String read FName;
      property Pronouns: TPronouns read FPronouns write FPronouns;
   end;

   TPlayerList = specialize TStorableList<TPlayer>; // @RegisterStorableClass

{$IFDEF DEBUG}
type
   TStatusReportProc = procedure (Perspective: TPlayer) of object;
var
   StatusReport: TStatusReportProc = nil;
{$ENDIF}

implementation

uses
   sysutils, exceptions, broadcast, things;

constructor TPlayer.Create(AName: UTF8String; APassword: UTF8String; APronouns: TPronouns);
var
   Bag: TBag;
begin
   inherited Create();
   FName := AName;
   FPassword := APassword;
   FPronouns := APronouns;
   Bag := TBag.Create('bag of holding', '(embroidered (bag/bags (of holding)?) (labeled ' + Capitalise(AName) + '))&', 'The bag has the name "' + Capitalise(AName) + '" embroidered around its rim.', tsLudicrous);
   Bag.Add(TFeature.Create('rim', '(rim/rims (bag? rim/rims))@', 'Around the bag''s rim is embroidered the name "' + Capitalise(AName) + '".'), tpAmbiguousPartOfImplicit); { the weird pattern is to avoid putting "bag" in the canonical description, as in, "the bag rim of the bag of holding" }
   Add(Bag, tpCarried);
end;

destructor TPlayer.Destroy();
begin
   if (Assigned(FOnForceDisconnect)) then
      FOnForceDisconnect();
   inherited;
end;

constructor TPlayer.Read(Stream: TReadStream);
begin
   inherited;
   FName := Stream.ReadString();
   FPassword := Stream.ReadString();
   FPronouns := TPronouns(Stream.ReadCardinal());
end;

procedure TPlayer.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteString(FName);
   Stream.WriteString(FPassword);
   Stream.WriteCardinal(Cardinal(FPronouns));
end;

procedure TPlayer.DoLook();
begin
   Assert((not (FPosition in tpContained)) or (FParent.GetRepresentative() = FParent)); // otherwise we'd look outside our parent
   Assert(FPosition in tpPlayerPositions);
   SendMessage(FParent.GetRepresentative().GetLook(Self));
end;

procedure TPlayer.DoInventory();
var
   Contents: UTF8String;
begin
   Contents := GetInventory(Self);
   if (Length(Contents) = 0) then
      Contents := 'You are not carrying anything.';
   SendMessage(Contents);
end;

{$IFDEF DEBUG}
procedure TPlayer.DoDebugStatus();
begin
   SendMessage('Debug build.');
   if (Assigned(StatusReport)) then
      StatusReport(Self);
end;

function TPlayer.DebugGetCurrentLocation(): TLocation;
var
   Location: TAtom;
begin
   Location := Self;
   while (Assigned(Location) and (Location is TThing)) do
      Location := (Location as TThing).Parent;
   if (not Assigned(Location)) then
      Fail('Player is in an orphan TThing tree!');
   if (not (Location is TLocation)) then
      Fail('Player is in a TThing tree that is not rooted by a TLocation!');
   Result := TLocation(Location);
end;

procedure TPlayer.DoDebugLocation();
begin
   SendMessage(DebugGetCurrentLocation().Debug(Self));
end;

procedure TPlayer.DoDebugThings(Things: TThingList);
var
   Thing: TThing;
   Collect, FromOutside: Boolean;
   Root: TAtom;
   FindMatchingThingsOptions: TFindMatchingThingsOptions;
begin
   Collect := not Assigned(Things);
   try
      if (Collect) then
      begin
         Root := GetSurroundingsRoot(FromOutside);
         Things := TThingList.Create();
         FindMatchingThingsOptions := [fomIncludePerspectiveChildren, fomIncludeNonImplicits];
         if (FromOutside) then
            Include(FindMatchingThingsOptions, fomFromOutside);
         Root.FindMatchingThings(Self, FindMatchingThingsOptions, tpEverything, [], Things);
      end;
      Assert(Things.Length > 0); { there's always at least one thing: us }
      SendMessage('Things:');
      for Thing in Things do
         SendMessage(' - ' + Thing.GetName(Self) + ': ' + Thing.GetLongDefiniteName(Self) + WithSpaceIfNotEmpty(ParentheticallyIfNotEmpty(Thing.GetPresenceStatement(Self, psOnThatSpecialThing))));
   finally
      if (Collect) then
         Things.Free();
   end;
end;

procedure TPlayer.DoDebugThing(Thing: TThing);
begin
   SendMessage(Thing.Debug(Self));
end;

procedure TPlayer.DoDebugTeleport(Target: TAtom);
var
   Ancestor: TAtom;
begin
   Ancestor := Target;
   while ((Ancestor <> Self) and (Ancestor is TThing)) do
      Ancestor := (Ancestor as TThing).Parent;
   if (Ancestor = Self) then
   begin
      SendMessage('Can''t teleport an actor onto the actor itself or any of its descendants.');
   end
   else
   begin
      AnnounceDisappearance();
      Target.Add(Self, tpOn);
      AnnounceAppearance();
      DoLook();
   end;
end;

procedure TPlayer.DoDebugListClasses(Superclass: TClass);
var
   RegisteredClassName: RawByteString;
begin
   SendMessage('The following classes are known:');
   for RegisteredClassName in GetRegisteredClasses(Superclass) do // $R-
      SendMessage(' - ' + RegisteredClassName);
end;
{$ENDIF}

procedure TPlayer.SetContext(Context: UTF8String);
begin
   FContext := Context;
end;

procedure TPlayer.ResetContext();
begin
   FContext := '';
end;

procedure TPlayer.AvatarMessage(Message: TMessage);
begin
   SendMessage(Message.AsText);
end;

procedure TPlayer.SendRawMessage(Message: UTF8String);
begin
   if (Assigned(FOnMessage)) then
      FOnMessage(Message);
end;

procedure TPlayer.SendMessage(Message: UTF8String);
begin
   if (Assigned(FOnMessage)) then
   begin
      if (FContext <> '') then
      begin
         if (Pos(#10, Message) > 0) then
            Message := FContext + ':' + #10 + Message
         else
            Message := FContext + ': ' + Message;
      end;
      FOnMessage(Message);
   end;
end;

procedure TPlayer.AutoDisambiguated(Message: UTF8String);
begin
   SendMessage('(' + Message + ')');
end;

function TPlayer.HasAbilityToTravelTo(Destination: TAtom; RequiredAbilities: TNavigationAbilities; Perspective: TAvatar; var Message: TMessage): Boolean;
begin
   Assert(Message.IsValid);
   if ((RequiredAbilities - [naWalk, naJump]) <> []) then
   begin
      Assert((RequiredAbilities - [naFly, naDebugTeleport]) = []);
      if (naFly in RequiredAbilities) then
      begin
         Result := False;
         Message := TMessage.Create(mkCannotFly, '_ cannot fly.', [Capitalise(GetDefiniteName(Perspective))]);
      end
      else
      if (RequiredAbilities - [naDebugTeleport] = []) then
      begin
         Result := False;
         Message := TMessage.Create(mkNotReachable, '_ cannot reach _.', [Capitalise(GetDefiniteName(Perspective)), Destination.GetDefiniteName(Perspective)]);
      end
      else
      begin
         Result := inherited;
      end;
   end
   else
   begin
      Result := True;
   end;
end;

function TPlayer.GetIntrinsicMass(): TThingMass;
begin
   Result := tmPonderous;
end;

function TPlayer.GetIntrinsicSize(): TThingSize;
begin
   Result := tsBig;
end;

function TPlayer.GetName(Perspective: TAvatar): UTF8String;
begin
   if (Perspective = Self) then
      Result := 'you'
   else
      Result := Capitalise(FName);
end;

function TPlayer.GetLongName(Perspective: TAvatar): UTF8String;
begin
   if (Perspective = Self) then
      Result := 'you'
   else
      Result := 'other player named ' + GetName(Perspective);
end;

function TPlayer.GetIndefiniteName(Perspective: TAvatar): UTF8String;
begin
   Result := GetName(Perspective);
end;

function TPlayer.GetDefiniteName(Perspective: TAvatar): UTF8String;
begin
   Result := GetName(Perspective);
end;

function TPlayer.GetLongDefiniteName(Perspective: TAvatar): UTF8String;
begin
   if (Perspective = Self) then
      Result := 'you'
   else
      Result := 'the other player named ' + GetDefiniteName(Perspective);
end;

function TPlayer.GetSubjectPronoun(Perspective: TAvatar): UTF8String;
begin
   if (Perspective = Self) then
      Result := 'you'
   else
   case FPronouns of
     pHe: Result := 'he';
     pShe: Result := 'she';
     pSingularThey: Result := 'they';
     pPluralThey: Result := 'they';
     pIt: Result := 'it';
     pZe: Result := 'ze';
   end;
end;

function TPlayer.GetObjectPronoun(Perspective: TAvatar): UTF8String;
begin
   if (Perspective = Self) then
      Result := 'you'
   else
   case FPronouns of
     pHe: Result := 'him';
     pShe: Result := 'her';
     pSingularThey: Result := 'them';
     pPluralThey: Result := 'them';
     pIt: Result := 'it';
     pZe: Result := 'zer';
   end;
end;

function TPlayer.GetReflexivePronoun(Perspective: TAvatar): UTF8String;
begin
   if (Perspective = Self) then
      Result := 'yourself'
   else
   case FPronouns of
     pHe: Result := 'himself';
     pShe: Result := 'herself';
     pSingularThey: Result := 'themself';
     pPluralThey: Result := 'themselves';
     pIt: Result := 'itself';
     pZe: Result := 'zerself';
   end;
end;

function TPlayer.GetPossessivePronoun(Perspective: TAvatar): UTF8String;
begin
   if (Perspective = Self) then
      Result := 'yours'
   else
   case FPronouns of
     pHe: Result := 'his';
     pShe: Result := 'hers';
     pSingularThey: Result := 'theirs';
     pPluralThey: Result := 'theirs';
     pIt: Result := 'its';
     pZe: Result := 'zirs';
   end;
end;

function TPlayer.GetPossessiveAdjective(Perspective: TAvatar): UTF8String;
begin
   if (Perspective = Self) then
      Result := 'your'
   else
   case FPronouns of
     pHe: Result := 'his';
     pShe: Result := 'her';
     pSingularThey: Result := 'their';
     pPluralThey: Result := 'their';
     pIt: Result := 'its';
     pZe: Result := 'zer';
   end;
end;

function TPlayer.IsPlural(Perspective: TAvatar): Boolean;
begin
   if (Perspective = Self) then
      Result := True
   else
      Result := FPronouns in [pPluralThey];
end;

function TPlayer.GetPresenceStatement(Perspective: TAvatar; Mode: TGetPresenceStatementMode): UTF8String;
begin
   Assert(FPosition in tpPlayerPositions);
   if (Mode = psThereIsAThingHere) then
      Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + IsAre(IsPlural(Perspective)) + ' here.'
   else
   if (Mode = psThereIsAThingThere) then
      Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + IsAre(IsPlural(Perspective)) + ' there.'
   else
   if ((Mode = psOnThatThingIsAThing) or (Mode = psTheThingIsOnThatThing)) then
      Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + IsAre(IsPlural(Perspective)) + ' ' +
                           ThingPositionToString(FPosition) + ' ' + FParent.GetLongDefiniteName(Perspective) + '.'
   else
      Result := inherited;
end;

function TPlayer.GetDescriptionSelf(Perspective: TAvatar): UTF8String;
begin
   Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + TernaryConditional('is', 'are', IsPlural(Perspective)) + ' a player.';
   if (not HasConnectedPlayer) then
   begin
      Result := Result + ' ' + Capitalise(GetPossessiveAdjective(Perspective)) + ' eyes look into the distance, as if ' + GetSubjectPronoun(Perspective) + ' ' + TernaryConditional('isn''t', 'aren''t', IsPlural(Perspective)) + ' really here.';
   end;
end;

procedure TPlayer.AnnounceAppearance();
begin
   DoBroadcast([Self], Self, [C(M(@GetIndefiniteName)), SP, MP(Self, M('appears.'), M('appear.'))]);
end;

procedure TPlayer.AnnounceDisappearance();
begin
   DoBroadcast([Self], Self, [C(M(@GetIndefiniteName)), SP, MP(Self, M('disappears.'), M('disappear.'))]);
end;

procedure TPlayer.AnnounceDeparture(Destination: TAtom; Direction: TCardinalDirection);
begin
   if (Destination is TLocation) then
      DoBroadcast([Self], Self, [C(M(@GetDefiniteName)), SP, MP(Self, M('goes'), M('go')), SP, M(CardinalDirectionToString(Direction) + '.')])
   else
      DoBroadcast([Self], Self, [C(M(@GetDefiniteName)), SP, MP(Self, M('enters'), M('enter')), SP, M(@Destination.GetDefiniteName), M('.')]);
end;

procedure TPlayer.AnnounceDeparture(Destination: TAtom);
begin
   DoBroadcast([Self], Self, [C(M(@GetDefiniteName)), SP, MP(Self, M('enters'), M('enter')), SP, M(@Destination.GetDefiniteName), M('.')]);
end;

procedure TPlayer.AnnounceArrival(Source: TAtom; Direction: TCardinalDirection);
begin
   // XXX this relies on the rooms being symmetric
   DoBroadcast([Self], Self, [C(M(@GetDefiniteName)), SP, MP(Self, M('arrives'), M('arrive')), M(' from '), M(@Source.GetDefiniteName), SP, M(CardinalDirectionToDirectionString(Direction)), M('.')]);
end;

procedure TPlayer.AnnounceArrival(Source: TAtom);
begin
   // XXX could be more intelligent by querying the current location
   // e.g. "enters from" when the current location has an exit and "arrives from" when it doesn't
   DoBroadcast([Self], Self, [C(M(@GetDefiniteName)), SP, MP(Self, M('arrives'), M('arrive')), M(' from '), M(@Source.GetDefiniteName), M('.')]);
end;

procedure TPlayer.DoFind(Subject: TThing);
var
   SubjectiveInformation: TSubjectiveInformation;
   Message, ExtraMessage: UTF8String;
   UseCommas: Boolean;
   Count, Index: Cardinal;
   Direction: TCardinalDirection;
begin
   SubjectiveInformation := Locate(Subject as TThing);
   if (SubjectiveInformation.Directions <> []) then
   begin
      Message := Capitalise(Subject.GetDefiniteName(Self)) + ' ' + IsAre(Subject.IsPlural(Self)) + ' ';
      Count := 0;
      for Direction := Low(SubjectiveInformation.Directions) to High(SubjectiveInformation.Directions) do
         if (Direction in SubjectiveInformation.Directions) then
            Inc(Count);
      Assert(Count > 0);
      UseCommas := Count > 2;
      Index := 0;
      for Direction := Low(SubjectiveInformation.Directions) to High(SubjectiveInformation.Directions) do
      begin
         if (Direction in SubjectiveInformation.Directions) then
         begin
            Inc(Index);
            if (Index > 1) then
            begin
               if (UseCommas) then
                  Message := Message + ',';
               if (Count = Index) then
                  Message := Message + ' and';
               Message := Message + ' ';
            end;
            Message := Message + CardinalDirectionToDirectionString(Direction);
         end;
      end;
      ExtraMessage := Subject.GetPresenceStatement(Self, psOnThatSpecialThing);
      if (ExtraMessage <> '') then
         Message := Message + ', ' + ExtraMessage;
      Message := Message + '.';
      SendMessage(Message);
   end
   else
      SendMessage(Subject.GetPresenceStatement(Self, psTheThingIsOnThatThing));
end;

procedure TPlayer.DoLookUnder(Subject: TThing);
var
   SubjectiveInformation: TSubjectiveInformation;
begin
   SubjectiveInformation := Locate(Subject);
   if (SubjectiveInformation.Directions = [cdUp]) then
   begin
      SendMessage('You are under ' + Subject.GetDefiniteName(Self) + '.');
   end
   else
      SendMessage(Subject.GetLookUnder(Self));
end;

procedure TPlayer.DoNavigation(Target: TAtom; ThingPosition: TThingPosition; RequiredAbilities: TNavigationAbilities);
var
   Message: TMessage;
begin
   Assert(ThingPosition in [tpIn, tpOn], 'DoNavigation called with a ThingPosition that isn''t supported by ForceTravel');
   Message := TMessage.Create();
   if (not CanReach(Target, Self, Message)) then
   begin
      Assert(Message.AsKind <> mkSuccess);
      Assert(Message.AsText <> '');
      AvatarMessage(Message);
   end
   else
   begin
      Assert(Message.AsKind = mkSuccess);
      Assert(Message.AsText = '');
      if (not HasAbilityToTravelTo(Target, RequiredAbilities, Self, Message)) then
      begin
         Assert(Message.AsKind <> mkSuccess);
         Assert(Message.AsText <> '');
         AvatarMessage(Message);
      end
      else
      begin
         Assert(Message.AsKind = mkSuccess);
         Assert(Message.AsText = '');
         ForceTravel(Self, Target, ThingPosition, Self);
      end;
   end;
end;

procedure TPlayer.DoNavigation(Direction: TCardinalDirection);
var
   Message: TMessage;
   Instructions: TNavigationInstruction;
begin
   Message := TMessage.Create();
   Instructions := FParent.GetNavigationInstructions(Direction, Self, Self, Message);
   if (Instructions.TravelType = ttNone) then
   begin
      Assert(Message.AsKind <> mkSuccess);
      Assert(Message.AsText <> '');
      AvatarMessage(Message);
   end
   else
   begin
      Assert(Message.AsKind = mkSuccess);
      Assert(Message.AsText = '');
      Assert(Instructions.DirectionTarget = Instructions.PositionTarget);
      if (not HasAbilityToTravelTo(Instructions.DirectionTarget, Instructions.RequiredAbilities, Self, Message)) then
      begin
         Assert(Message.AsKind <> mkSuccess);
         Assert(Message.AsText <> '');
         AvatarMessage(Message);
      end
      else
      begin
         Assert(Message.AsKind = mkSuccess);
         Assert(Message.AsText = '');
         case Instructions.TravelType of
            ttByPosition: ForceTravel(Self, Instructions.PositionTarget, Instructions.Position, Self);
            ttByDirection: ForceTravel(Self, Instructions.DirectionTarget, Instructions.Direction, Self);
         else
            Assert(False);
         end;
      end;
   end;
end;

procedure TPlayer.DoNavigation(Instruction: TTransportationInstruction);
begin
   case Instruction.TravelType of
      ttNone: Assert(False);
      ttByPosition: DoNavigation(Instruction.TargetThing, Instruction.Position, Instruction.RequiredAbilities);
      ttByDirection: DoNavigation(Instruction.Direction);
   end;
end;

procedure TPlayer.DoTake(Subject: TThingList);
var
   Multiple: Boolean;
   Success: Boolean;
   Message: TMessage;
   TargetSurface: TAtom;
   CurrentSubject: TThing;
begin
   Assert(Assigned(Subject));
   Assert(Subject.Length > 0);
   Multiple := Subject.Length > 1;
   try
      for CurrentSubject in Subject do
      begin
         if (Multiple) then
            SetContext(Capitalise(CurrentSubject.GetName(Self)));
         Message := TMessage.Create();
         if (not CanReach(CurrentSubject, Self, Message)) then
         begin
            Assert(Message.AsKind <> mkSuccess);
            SendMessage(Message.AsText);
         end
         else
         if (CurrentSubject.Parent = Self) then
         begin
            SendMessage('You already have that.');
         end
         else
         if (CurrentSubject = Self) then
         begin
            TargetSurface := FParent.GetSurface();
            if (Assigned(TargetSurface)) then
            begin
               DoBroadcast([Self], Self, [C(M(@GetDefiniteName)), SP,
                                          MP(Self, M('tries to'), M('try to')), SP,
                                          M('pick'), SP,
                                          M(@GetReflexivePronoun), SP,
                                          M('up, but'), SP,
                                          MP(Self, M('ends up'), M('end up')), SP,
                                          M(ThingPositionToString(Position)), SP,
                                          M(@TargetSurface.GetDefiniteName), M('.')]);
               SendMessage('You try to pick yourself up but end up ' + ThingPositionToString(Position) + ' ' + TargetSurface.GetDefiniteName(Self) + '.');
            end
            else
               SendMessage('How can one pick oneself up?');
         end
         else
         begin
            Message := TMessage.Create(mkSuccess, 'Taken.');
            Success := IsNotUnderUs(CurrentSubject, Message) and
                       CanCarryThing(CurrentSubject, Message);
            Assert((Message.AsKind = mkSuccess) = Success);
            SendMessage(Message.AsText);
            if (Success) then
               Self.Put(CurrentSubject, tpCarried, psCarefully, Self);
         end;
      end;
   finally
      if (Multiple) then
         ResetContext();
   end;
end;

procedure TPlayer.DoPut(Subject: TThingList; Target: TAtom; ThingPosition: TThingPosition; Care: TPlacementStyle);
var
   Multiple: Boolean;
   CurrentSubject: TThing;
begin
   Assert(Assigned(Subject));
   Assert(Subject.Length > 0);
   Assert(Assigned(Target));
   Assert((ThingPosition = tpIn) or (ThingPosition = tpOn));
   Multiple := Subject.Length > 1;
   try
      for CurrentSubject in Subject do
      begin
         if (Multiple) then
            SetContext(Capitalise(CurrentSubject.GetName(Self)));
         DoPutInternal(CurrentSubject, Target, ThingPosition, Care);
      end;
   finally
      if (Multiple) then
         ResetContext();
   end;
end;

procedure TPlayer.DoPutInternal(CurrentSubject: TThing; Target: TAtom; ThingPosition: TThingPosition; Care: TPlacementStyle);
var
   Message: TMessage;
   SurrogateTarget: TAtom;
   SingleThingList: TThingList;
   Success: Boolean;
   MessageText: UTF8String;
begin
   Message := TMessage.Create();
   SurrogateTarget := Target.GetDefaultDestination(ThingPosition);
   if (not CanMoveWithoutLoops(CurrentSubject, SurrogateTarget, ThingPosition, Message)) then
   begin
      Assert(Message.AsKind <> mkSuccess);
      SendMessage(Message.AsText);
   end
   else
   begin
      if (CurrentSubject.Parent <> Self) then
      begin
         AutoDisambiguated('first taking ' + CurrentSubject.GetDefiniteName(Self));
         SingleThingList := TThingList.Create();
         try
            SingleThingList.AppendItem(CurrentSubject);
            DoTake(SingleThingList);
         finally
            SingleThingList.Free();
         end;
      end;
      if (CurrentSubject.Parent = Self) then
      begin
         if (not CanReach(CurrentSubject, Self, Message)) then
         begin
            Assert(Message.AsKind <> mkSuccess);
            SendMessage(Message.AsText);
         end
         else
         if (not CanReach(Target, Self, Message)) then
         begin
            Assert(Message.AsKind <> mkSuccess);
            SendMessage(Message.AsText);
         end
         else
         if ((Target = CurrentSubject) or (SurrogateTarget = CurrentSubject)) then
         begin
            SendMessage('You can''t move something ' + ThingPositionToDirectionString(ThingPosition) + ' itself, however hard you try.');
         end
         else
         begin
            Assert(CurrentSubject.Parent = Self);
            Assert(Assigned(SurrogateTarget));
            case (Care) of
              psCarefully: MessageText := 'Placed';
              psRoughly: MessageText := 'Dropped';
            end;
            if (SurrogateTarget <> FParent.GetSurface()) then
               MessageText := MessageText + ' ' + ThingPositionToString(ThingPosition) + ' ' + SurrogateTarget.GetDefiniteName(Self);
            MessageText := MessageText + '.';
            Message := TMessage.Create(mkSuccess, MessageText);
            Success := Target.CanPut(CurrentSubject, ThingPosition, Care, Self, Message) and
                       SurrogateTarget.CanPut(CurrentSubject, ThingPosition, Care, Self, Message);
            Assert((Message.AsKind = mkSuccess) = Success);
            SendMessage(Message.AsText);
            if (Success) then
            begin
               SurrogateTarget.Put(CurrentSubject, ThingPosition, Care, Self);
            end;
         end;
      end;
   end;
end;

procedure TPlayer.DoRelocate(Subject: TThingList);
var
   Multiple: Boolean;
   SingleThingList: TThingList;
   Ancestor, LocationSurface: TAtom;
   CurrentSubject: TThing;
   {$IFOPT C+} PreviousParent: TAtom; {$ENDIF}
   Message: TMessage;
begin
   Assert(Assigned(Subject));
   Assert(Subject.Length > 0);
   Multiple := Subject.Length > 1;
   try
      for CurrentSubject in Subject do
      begin
         if (Multiple) then
            SetContext(Capitalise(CurrentSubject.GetName(Self)));
         Message := TMessage.Create();
         if (CurrentSubject = Self) then
         begin
            {$IFOPT C+} PreviousParent := FParent; {$ENDIF}
            DoDance();
            {$IFOPT C+} Assert(FParent = PreviousParent); {$ENDIF}
         end
         else
         if (not CanReach(CurrentSubject, Self, Message) or
             not IsNotUnderUs(CurrentSubject, Message)) then
         begin
            Assert(Message.AsKind <> mkSuccess);
            SendMessage(Message.AsText);
         end
         else
         if (CurrentSubject.Parent = Self) then
         begin
            SingleThingList := TThingList.Create();
            try
               SingleThingList.AppendItem(CurrentSubject);
               DoShake(SingleThingList);
            finally
               SingleThingList.Free();
            end;
         end
         else
         begin
            // Find the subject's location.
            Ancestor := CurrentSubject.Parent;
            while (Ancestor is TThing) do
               Ancestor := (Ancestor as TThing).Parent;
            Assert(Assigned(Ancestor));
            // Find the location's surface.
            LocationSurface := Ancestor.GetSurface(); // might be nil
            // If the subject is directly in the location or at the surface, then just shake it.
            Assert(Assigned(CurrentSubject.Parent));
            if ((CurrentSubject.Parent = Ancestor) or
                (CurrentSubject.Parent = LocationSurface) or
                (not (CurrentSubject.Position in [tpOn, tpIn]))) then
            begin
               SingleThingList := TThingList.Create();
               try
                  SingleThingList.AppendItem(CurrentSubject);
                  DoShake(SingleThingList);
               finally
                  SingleThingList.Free();
               end;
            end
            else
            begin
               SingleThingList := TThingList.Create();
               try
                  SingleThingList.AppendItem(CurrentSubject);
                  /// xxxx replace with push (tpOn) or take (tpIn)
                  DoRemove(SingleThingList, CurrentSubject.Position, nil);
               finally
                  SingleThingList.Free();
               end;
            end;
         end;
      end;
   finally
      if (Multiple) then
         ResetContext();
   end;
end;

procedure TPlayer.DoMove(Subject: TThingList; Target: TThing; ThingPosition: TThingPosition);

   function CanBePushedTo(Thing: TThing; Surface: TAtom): Boolean; { Thing is being pushed onto Surface }
   var
      Ancestor: TAtom;
   begin
      if (Thing.Position <> tpOn) then
      begin
         { There is no way you can push something onto something else if it's not already on something }
         Result := False;
         exit;
      end;
      { can always slide onto something we're holding }
      if ((Surface is TThing) and ((Surface as TThing).Parent = Self) and ((Surface as TThing).Position = tpCarried)) then
      begin
         Result := True;
         exit;
      end;
      { next see if we're trying to move the thing off something else onto it (i.e. pushing onto an ancestor) }
      { note we have to stop if we get to something that is not on something, e.g. you can't push from on a plate that is in a bag onto the table the bag is on }
      { nor can you push it from on the plate in the bag onto the bag itself }
      Ancestor := Thing;
      repeat
         Ancestor := (Ancestor as TThing).Parent;
         if ((Ancestor = Surface) or
             ((Surface is TThing) and ((Surface as TThing).Parent = Ancestor) and (tfCanHaveThingsPushedOn in (Surface as TThing).GetFeatures()))) then
         begin
            Result := True;
            exit;
         end;
      until ((not (Ancestor is TThing)) or
             (((Ancestor as TThing).Position in tpSeparate) and { tpIn is ok in the case where things can be pushed in/out from/to parent }
              (not (((Ancestor as TThing).Position in tpContained) and (tfCanHaveThingsPushedIn in (Ancestor as TThing).GetFeatures())))));
      Result := False;
   end;

   function CanBePushedInto(Thing: TThing; Surface: TAtom): Boolean; { Thing is being pushed into something whose inside is Surface }
   var
      Ancestor: TAtom;
   begin
      if (Thing.Position <> tpOn) then
      begin
         { There is no way you can push something into something else if it's not on something }
         Result := False;
         exit;
      end;
      { can slide into something we're holding }
      if ((Surface is TThing) and ((Surface as TThing).Parent = Self) and ((Surface as TThing).Position = tpCarried)) then
      begin
         Result := True;
         exit;
      end;
      { next see if we're trying to move the thing off something else into it (i.e. pushing into an ancestor) }
      { note we have to stop if we get to something that is not on something, e.g. you can't push from on a plate that is in a bag into the chest that the bag is in }
      { (but you _can_ push it into the bag) }
      Ancestor := Thing;
      repeat
         Ancestor := (Ancestor as TThing).Parent;
         if ((Ancestor = Surface) or
             ((Surface is TThing) and ((Surface as TThing).Parent = Ancestor) and (tfCanHaveThingsPushedIn in (Surface as TThing).GetFeatures()))) then
         begin
            Result := True;
            exit;
         end;
      until ((not (Ancestor is TThing)) or
             (((Ancestor as TThing).Position in tpSeparate) and { tpIn is ok in the case where things can be pushed in/out from/to parent }
              (not (((Ancestor as TThing).Position in tpContained) and (tfCanHaveThingsPushedIn in (Thing as TThing).GetFeatures())))));
      Result := False;
   end;

var
   Multiple, NavigateToTarget, DisambiguateExplicitly: Boolean;
   SurrogateTarget: TAtom;
   SurrogateThingPosition: TThingPosition;
   CurrentSubject: TThing;
   Message: TMessage;
   Details: TSubjectiveInformation;
begin
   Assert(Assigned(Subject));
   Assert(Subject.Length > 0);
   Assert(Assigned(Target));
   Assert(ThingPosition in [tpIn, tpOn, tpAt]);
   Multiple := Subject.Length > 1;
   NavigateToTarget := False;
   DisambiguateExplicitly := ThingPosition = tpAt;
   SurrogateThingPosition := ThingPosition;
   SurrogateTarget := Target.GetDefaultDestination(SurrogateThingPosition);
   Assert(Assigned(SurrogateTarget));
   Assert(SurrogateThingPosition in [tpIn, tpOn]);
   if (SurrogateTarget = Self) then
   begin
      DoTake(Subject);
   end
   else
   begin
      if (DisambiguateExplicitly or (SurrogateTarget <> Target)) then
         AutoDisambiguated(ThingPositionToString(SurrogateThingPosition) + ' ' + SurrogateTarget.GetDefiniteName(Self));
      try
         for CurrentSubject in Subject do
         begin
            if (Multiple) then
               SetContext(Capitalise(CurrentSubject.GetName(Self)));
            Message := TMessage.Create();
            if (CurrentSubject = Self) then
            begin
               NavigateToTarget := True;
            end
            else
            if (CurrentSubject = Target) then
            begin
               SendMessage('You can''t move something ' + ThingPositionToDirectionString(SurrogateThingPosition) + ' itself, however hard you try.');
            end
            else
            if (not CanReach(CurrentSubject, Self, Message) or
                not CanReach(Target, Self, Message) or
                not IsNotUnderUs(CurrentSubject, Message)) then
            begin
               Assert(Message.AsKind <> mkSuccess);
               SendMessage(Message.AsText);
            end
            else
            if ((SurrogateTarget = CurrentSubject.Parent) and (SurrogateThingPosition = CurrentSubject.Position)) then
            begin
               SendMessage(Capitalise(CurrentSubject.GetDefiniteName(Self) + ' ' + IsAre(CurrentSubject.IsPlural(Self)) + ' already ' + ThingPositionToString(CurrentSubject.Position) + ' ' + SurrogateTarget.GetDefiniteName(Self) + '.'));
            end
            else
            if (((ThingPosition = tpOn) and CanBePushedTo(CurrentSubject, Target)) or
                ((ThingPosition = tpIn) and CanBePushedInto(CurrentSubject, Target)) or
                ((SurrogateThingPosition = tpOn) and CanBePushedTo(CurrentSubject, SurrogateTarget)) or
                ((SurrogateThingPosition = tpIn) and CanBePushedInto(CurrentSubject, SurrogateTarget))) then
            begin
               DoPushInternal(CurrentSubject, SurrogateTarget, SurrogateThingPosition, psRoughly, True, nil);
            end
            else
            begin
               DoPutInternal(CurrentSubject, SurrogateTarget, SurrogateThingPosition, psCarefully);
            end;
         end;
         if (NavigateToTarget) then
         begin
            if (Multiple) then
               SetContext(Capitalise(GetName(Self)));
            if (SurrogateTarget is TThing) then
            begin
               Details := Locate(SurrogateTarget as TThing);
            end
            else
            begin
               Details := Locate(Target);
            end;
            DoNavigation(SurrogateTarget, SurrogateThingPosition, Details.RequiredAbilitiesToNavigate);
         end;
      finally
         if (Multiple) then
            ResetContext();
      end;
   end;
end;

procedure TPlayer.DoPushInternal(CurrentSubject: TThing; Destination: TAtom; ThingPosition: TThingPosition; Care: TPlacementStyle; DestinationMustBeReachable: Boolean; NotificationList: TAtomList);
var
   Success: Boolean;
   CurrentNotifiee: TAtom;
   Message: TMessage;
begin
   Message := TMessage.Create(mkSuccess, 'Pushed.');
   Assert(Message.AsKind = mkSuccess);
   Success := CanPushThing(CurrentSubject, Message) and
              Destination.CanPut(CurrentSubject, ThingPosition, Care, Self, Message) and
              CanMoveWithoutLoops(CurrentSubject, Destination, ThingPosition, Message) and
              CanReach(CurrentSubject, Self, Message) and
              ((not DestinationMustBeReachable) or CanReach(Destination, Self, Message));
   Assert((Message.AsKind = mkSuccess) = Success);
   SendMessage(Message.AsText);
   if (Success) then
   begin
      if (Assigned(NotificationList)) then
         for CurrentNotifiee in NotificationList do
            CurrentNotifiee.HandlePassedThrough(CurrentSubject, CurrentSubject.Parent, Destination, ThingPosition, Self);
      Destination.Put(CurrentSubject, ThingPosition, Care, Self);
   end;
end;

procedure TPlayer.DoPushDirectional(Subject: TThingList; Direction: TCardinalDirection);
var
   Multiple: Boolean;
   CurrentSubject, DisambiguationOpening: TThing;
   Destination: TAtom;
   Location: TLocation;
   ThingPosition: TThingPosition;
   Message: TMessage;
   NotificationList: TAtomList;
begin
   Assert(Assigned(Subject));
   Assert(Subject.Length > 0);
   Multiple := Subject.Length > 1;
   try
      for CurrentSubject in Subject do
      begin
         if (Multiple) then
            SetContext(Capitalise(CurrentSubject.GetName(Self)));
         Message := TMessage.Create();
         if (not CanReach(CurrentSubject, Self, Message) or
             not IsNotUnderUs(CurrentSubject, Message)) then
         begin
            Assert(Message.AsKind <> mkSuccess);
            SendMessage(Message.AsText);
         end
         else
         begin
            Assert(Assigned(CurrentSubject.Parent));
            Destination := CurrentSubject.Parent.GetRepresentative(); // This gets the current room (not the destination).
            Message := TMessage.Create();
            if (CurrentSubject = Self) then
            begin
               SendMessage('You try to push yourself but find that a closed system cannot have any unbalanced internal forces.');
            end
            else
            if (Destination is TLocation) then
            begin
               Location := Destination as TLocation;
               Destination := Location.GetAtomForDirectionalNavigation(Direction);
               if (not Assigned(Destination)) then
               begin
                  Location.FailNavigation(Direction, Self, Message);
                  Assert(Message.AsKind <> mkSuccess);
                  Assert(Message.AsText <> '');
                  AvatarMessage(Message);
               end
               else
               begin
                  DisambiguationOpening := nil;
                  NotificationList := TAtomList.Create();
                  try
                     ThingPosition := tpOn;
                     Assert(Message.AsKind = mkSuccess);
                     Destination := Destination.GetEntrance(CurrentSubject, Direction, Self, ThingPosition, DisambiguationOpening, Message, NotificationList);
                     if (Assigned(DisambiguationOpening) and (DisambiguationOpening <> Destination)) then
                        AutoDisambiguated('through ' + DisambiguationOpening.GetDefiniteName(Self));
                     if (Assigned(Destination)) then
                     begin
                        Assert(Message.AsKind = mkSuccess);
                        DoPushInternal(CurrentSubject, Destination, ThingPosition, psRoughly, False, NotificationList);
                     end
                     else
                     begin
                        Assert(Message.AsKind <> mkSuccess);
                        SendMessage(Message.AsText);
                     end;
                  finally
                     NotificationList.Free();
                  end;
               end;
            end
            else
            if (CurrentSubject.Position = tpOn) then
            begin
               SendMessage('You would have to push ' + CurrentSubject.GetDefiniteName(Self) + ' off ' + CurrentSubject.Parent.GetDefiniteName(Self) + ' first.');
            end
            else
            if (CurrentSubject.Position = tpIn) then
            begin
               SendMessage('You would have to move ' + CurrentSubject.GetDefiniteName(Self) + ' out of ' + CurrentSubject.Parent.GetDefiniteName(Self) + ' first.');
            end
            else
            begin
               SendMessage(Capitalise(CurrentSubject.GetDefiniteName(Self)) + ' ' + IsAre(CurrentSubject.IsPlural(Self)) + ' ' + ThingPositionToString(CurrentSubject.Position) + ' ' + CurrentSubject.Parent.GetDefiniteName(Self) + '. It is not clear how to move ' + CurrentSubject.GetObjectPronoun(Self) + '.');
            end;
         end;
      end;
   finally
      if (Multiple) then
         ResetContext();
   end;
end;

procedure TPlayer.DoRemove(Subject: TThingList; RequiredPosition: TThingPosition; RequiredParent: TThing);

   function IsNotOnesSelf(CurrentSubject: TThing; var Message: TMessage): Boolean;
   begin
      if (CurrentSubject = Self) then
      begin
         Message := TMessage.Create(mkBogus, 'You can''t just remove yourself from somewhere... where do you want to go instead?');
         Result := False;
      end
      else
         Result := True;
   end;

   function WhereExpected(CurrentSubject: TThing; RequiredParent: TThing; RequiredPosition: TThingPosition; var Message: TMessage): Boolean;
   begin
      if (Assigned(RequiredParent) and ((CurrentSubject.Parent <> RequiredParent) or (CurrentSubject.Position <> RequiredPosition))) then
      begin
         // "The subject is not on the required parent, it is on the subject's parent."
         Message := TMessage.Create(mkBogus, '_ _ not _ _, _ _ _ _.',
                                             [Capitalise(CurrentSubject.GetDefiniteName(Self)),
                                              IsAre(CurrentSubject.IsPlural(Self)),
                                              ThingPositionToString(RequiredPosition),
                                              RequiredParent.GetDefiniteName(Self),
                                              CurrentSubject.GetSubjectPronoun(Self),
                                              IsAre(CurrentSubject.IsPlural(Self)),
                                              ThingPositionToString(CurrentSubject.Position),
                                              CurrentSubject.Parent.GetDefiniteName(Self)]);
         Result := False;
      end
      else
      if (CurrentSubject.Position <> RequiredPosition) then
      begin
         // "The subject is not on anything, the subject is in the pot."
         Message := TMessage.Create(mkBogus, '_ _ not _ anything, _ _ _ _.',
                                             [Capitalise(CurrentSubject.GetDefiniteName(Self)),
                                              IsAre(CurrentSubject.IsPlural(Self)),
                                              ThingPositionToString(RequiredPosition),
                                              CurrentSubject.GetSubjectPronoun(Self),
                                              IsAre(CurrentSubject.IsPlural(Self)),
                                              ThingPositionToString(CurrentSubject.Position),
                                              CurrentSubject.Parent.GetDefiniteName(Self)]);
         Result := False;
      end
      else
         Result := True;
   end;

var
   Multiple: Boolean;
   SingleThingList: TThingList;
   Message: TMessage;
   CurrentSubject, Target: TThing;
begin
   // this is for "move flowers off table" and the like
   Assert(Assigned(Subject));
   Assert(Subject.Length > 0);
   Assert((RequiredPosition = tpOn) or (RequiredPosition = tpIn));
   Multiple := Subject.Length > 1;
   try
      for CurrentSubject in Subject do
      begin
         if (Multiple) then
            SetContext(Capitalise(CurrentSubject.GetName(Self)));
         Assert(Assigned(CurrentSubject.Parent));
         Message := TMessage.Create();
         if ((not IsNotOnesSelf(CurrentSubject, Message)) or
             (not CanReach(CurrentSubject, Self, Message)) or
             (Assigned(RequiredParent) and (not CanReach(RequiredParent, Self, Message))) or
             (not WhereExpected(CurrentSubject, RequiredParent, RequiredPosition, Message) or
             (not IsNotUnderUs(CurrentSubject, Message)))) then
         begin
            Assert(Message.AsKind <> mkSuccess);
            SendMessage(Message.AsText);
         end
         else
         begin
            if (not Assigned(RequiredParent)) then
            begin
               if (RequiredPosition = tpIn) then
               begin
                  AutoDisambiguated('out of ' + CurrentSubject.Parent.GetDefiniteName(Self));
               end
               else
               begin
                  Assert(RequiredPosition = tpOn);
                  AutoDisambiguated('off ' + CurrentSubject.Parent.GetDefiniteName(Self));
               end;
            end;
            if (not CanPushThing(CurrentSubject, Message)) then
            begin
                SendMessage(Message.AsText);
            end
            else
            begin
               if (CurrentSubject.Parent is TThing) then
               begin
                  Assert(Assigned((CurrentSubject.Parent as TThing).Parent));
                  Target := (CurrentSubject.Parent as TThing).Parent.GetSurface();
               end
               else
               begin
                  Target := nil;
               end;
               if ((not Assigned(Target)) or { parent must be a TLocation - it's directly in the room - only way to remove it is to pick it up }
                   (Target = CurrentSubject.Parent) or { can't push it if pushing it off its parent would just make it fall onto the current parent (e.g. when the parent is the ground) }
                   (CurrentSubject.Position <> tpOn)) then { if it's not on something, then can't push it off the thing }
               begin
                  AutoDisambiguated('by taking ' + CurrentSubject.GetDefiniteName(Self));
                  SingleThingList := TThingList.Create();
                  try
                     SingleThingList.AppendItem(CurrentSubject);
                     DoTake(SingleThingList);
                  finally
                     SingleThingList.Free();
                  end;
               end
               else
               begin
                  DoPushInternal(CurrentSubject, Target, tpOn, psRoughly, False, nil);
               end;
            end;
         end;
      end;
   finally
      if (Multiple) then
         ResetContext();
   end;
end;

procedure TPlayer.DoPress(Subject: TThingList);
var
   Multiple: Boolean;
   CurrentSubject: TThing;
   Message: TMessage;
begin
   Assert(Assigned(Subject));
   Assert(Subject.Length > 0);
   Multiple := Subject.Length > 1;
   try
      for CurrentSubject in Subject do
      begin
         if (Multiple) then
            SetContext(Capitalise(CurrentSubject.GetName(Self)));
         Message := TMessage.Create();
         if (not CanReach(CurrentSubject, Self, Message)) then
         begin
            Assert(Message.AsKind <> mkSuccess);
            SendMessage(Message.AsText);
         end
         else
         if (CurrentSubject = Self) then
         begin
            DoBroadcast([Self], Self, [C(M(@GetDefiniteName)), MP(Self, M(' presses '), M(' press ')), M(@GetReflexivePronoun), M('.')]);
            Press(Self);
         end
         else
         begin
            DoBroadcast([CurrentSubject, Self], Self, [C(M(@GetDefiniteName)), MP(Self, M(' presses '), M(' press ')), M(@CurrentSubject.GetDefiniteName), M('.')]);
            CurrentSubject.Press(Self);
         end;
      end;
   finally
      if (Multiple) then
         ResetContext();
   end;
end;

procedure TPlayer.DoShake(Subject: TThingList);
var
   Multiple: Boolean;
   Message: TMessage;
   CurrentSubject: TThing;
begin
   Assert(Assigned(Subject));
   Assert(Subject.Length > 0);
   Multiple := Subject.Length > 1;
   try
      for CurrentSubject in Subject do
      begin
         if (Multiple) then
            SetContext(Capitalise(CurrentSubject.GetName(Self)));
         Message := TMessage.Create();
         if (not CanReach(CurrentSubject, Self, Message)) then
         begin
            Assert(Message.AsKind <> mkSuccess);
            SendMessage(Message.AsText);
         end
         else
         if (CurrentSubject = Self) then
         begin
            DoBroadcast([Self], Self, [C(M(@GetDefiniteName)), MP(Self, M(' shakes '), M(' shake ')), M(@GetReflexivePronoun), M('.')]);
            SendMessage('You shake yourself.');
         end
         else
         begin
            Message := TMessage.Create(mkImmovable, 'You can''t shake _.', [CurrentSubject.GetDefiniteName(Self)]);
            if (CanShakeThing(CurrentSubject, Message)) then
            begin
               DoBroadcast([CurrentSubject, Self], Self, [C(M(@GetDefiniteName)), MP(Self, M(' shakes '), M(' shake ')), M(@CurrentSubject.GetDefiniteName), M('.')]);
               SendMessage(Capitalise(GetDefiniteName(Self)) + ' ' + TernaryConditional('shakes', 'shake', IsPlural(Self)) + ' ' + CurrentSubject.GetDefiniteName(Self) + '.');
               CurrentSubject.Shake(Self);
            end
            else
            begin
               Assert(Message.AsKind <> mkSuccess);
               SendMessage(Message.AsText);
            end;
         end;
      end;
   finally
      if (Multiple) then
         ResetContext();
   end;
end;

procedure TPlayer.DoDig(Target: TThing; Spade: TThing);
var
   SingleThingList: TThingList;
   Success: Boolean;
   Message: TMessage;
begin
   Assert(Assigned(Spade));
   Assert(Assigned(Spade.Parent));
   Assert(Assigned(Target));
   if (Spade.Parent <> Self) then
   begin
      AutoDisambiguated('first taking ' + Spade.GetDefiniteName(Self));
      SingleThingList := TThingList.Create();
      try
         SingleThingList.AppendItem(Spade);
         DoTake(SingleThingList);
      finally
         SingleThingList.Free();
      end;
   end;
   if (Spade.Parent = Self) then
   begin
      Message := TMessage.Create(mkSuccess, 'Dug.');
      Success := Spade.CanDig(Target, Self, Message) and Target.Dig(Spade, Self, Message);
      Assert((Message.AsKind = mkSuccess) = Success);
      if (Success) then
         Spade.Dug(Target, Self, Message);
      SendMessage(Message.AsText);
   end;
end;

procedure TPlayer.DoDig(Direction: TCardinalDirection; Spade: TThing);
var
   DefaultParent, Destination: TAtom;
   CurrentLocation: TLocation;
   Message: TMessage;
begin
   Assert(Assigned(Spade));
   Assert(Assigned(FParent));
   DefaultParent := FParent.GetRepresentative();
   if (DefaultParent is TLocation) then
   begin
      CurrentLocation := DefaultParent as TLocation;
      Destination := CurrentLocation.GetAtomForDirectionalNavigation(Direction);
      if (not Assigned(Destination)) then
      begin
         // could give a better message?
         CurrentLocation.FailNavigation(Direction, Self, Message);
         Assert(Message.AsKind <> mkSuccess);
         Assert(Message.AsText <> '');
         AvatarMessage(Message);
      end
      else
      if (Destination is TThing) then
      begin
         DoDig(Destination as TThing, Spade);
      end
      else
      begin
         SendMessage('You cannot dig ' + Destination.GetDefiniteName(Self) + '.'); // XXX "from here", maybe?
      end;
   end
   else
   begin
      if ((Direction = cdDown) and (FParent is TThing)) then
         DoDig(FParent as TThing, Spade)
      else
         SendMessage('You cannot dig ' + CardinalDirectionToString(Direction) + ' while you are ' + ThingPositionToString(FPosition) + ' ' + FParent.GetDefiniteName(Self) + '.');
   end;
end;

procedure TPlayer.DoOpen(Subject: TThing);
var
   Message: TMessage;
begin
   Message := TMessage.Create(mkSuccess, 'Opened.');
   Subject.Open(Self, Message);
   SendMessage(Message.AsText);
end;

procedure TPlayer.DoClose(Subject: TThing);
var
   Message: TMessage;
begin
   Message := TMessage.Create(mkSuccess, 'Closed.');
   Subject.Close(Self, Message);
   SendMessage(Message.AsText);
end;

procedure TPlayer.DoTalk(Target: TThing; Message: UTF8String; Volume: TTalkVolume);
var
   SingularVerb, PluralVerb: UTF8String;
begin
   Assert(Assigned(FParent));
   case Volume of
    tvWhispering: begin SingularVerb := 'whispers'; PluralVerb := 'whisper'; end;
    tvSpeaking: begin SingularVerb := 'says'; PluralVerb := 'say'; end;
    tvShouting: begin SingularVerb := 'shouts'; PluralVerb := 'shout'; end;
   end;
   if (Assigned(Target)) then
   begin
      // XXX notify NPCs
      if (Volume <> tvWhispering) then
         DoBroadcast([Self], Self, [C(M(@GetDefiniteName)), SP, MP(Self, M(SingularVerb), M(PluralVerb)), SP, M(Message), M(' to '), M(@Target.GetDefiniteName), M('.')])
      else
      if (Target is TPlayer) then
         (Target as TPlayer).SendMessage(Capitalise(GetDefiniteName(Target as TAvatar)) + ' ' + TernaryConditional(SingularVerb, PluralVerb, IsPlural(Target as TAvatar)) + ' ' + Message + ' to ' + Target.GetDefiniteName(Target as TAvatar) + '.');
      SendMessage(Capitalise(GetDefiniteName(Self)) + ' ' + TernaryConditional(SingularVerb, PluralVerb, IsPlural(Self)) + ' ' + Message + ' to ' + Target.GetDefiniteName(Self) + '.')
   end
   else
   begin
      DoBroadcast([Self], Self, [C(M(@GetDefiniteName)), SP, MP(Self, M(SingularVerb), M(PluralVerb)), SP, M(Message), M('.')]);
      SendMessage(Capitalise(GetDefiniteName(Self)) + ' ' + TernaryConditional(SingularVerb, PluralVerb, IsPlural(Self)) + ' ' + Message + '.');
   end;
end;

procedure TPlayer.DoDance();
begin
//   XXX; // need a general emoting mechanic
   DoBroadcast([Self], Self, [C(M(@GetDefiniteName)), SP, MP(Self, M('dances'), M('dance')), M('.')]);
   SendMessage(Capitalise(GetDefiniteName(Self)) + ' ' + TernaryConditional('dances', 'dance', IsPlural(Self)) + '.');
end;

procedure TPlayer.DoSetPronouns(Pronouns: TPronouns);
begin
   FPronouns := Pronouns;
   SendMessage('Noted.');
end;

function TPlayer.CanSurfaceHold(const Manifest: TThingSizeManifest; const ManifestCount: Integer): Boolean;
var
   Count: Cardinal;
   Child: TThing;
begin
   Assert(GetSurface() = Self);
   Count := 0;
   for Child in FChildren do
   begin
      if (Child.Position in [tpOn]) then
      begin
         Assert(Count < High(Count)); // this would be a ludicrous number of objects
         Count := Count + 1; // $R-
      end;
   end;
   if (Count + ManifestCount > MaxBalanceCount) then
   begin
      Result := False;
   end
   else
   begin
      Result := inherited;
   end;
end;

procedure TPlayer.HandleAdd(Thing: TThing; Blame: TAvatar);

   procedure CheckInventory();
   var
      Masses, CandidateMass, ThisMass: TThingMassManifest;
      Sizes, CandidateSize, ThisSize: TThingSizeManifest;
      Count: Cardinal;
      Child: TThing;
      Candidate: TThing;
   begin
      // TODO: check if Thing.Position is not tpCarried
      Zero(Masses);
      Zero(Sizes);
      Count := 0;
      for Child in FChildren do
      begin
         if (Child.Position in [tpCarried]) then
         begin
            Masses := Masses + Child.GetMassManifest();
            Sizes := Sizes + Child.GetOutsideSizeManifest();
            Assert(Count < High(Count)); // this would be a ludicrous number of objects
            Count := Count + 1; // $R-
         end;
      end;
      while ((Masses >= MaxCarryMass) or (Sizes >= MaxCarrySize) or (Count > MaxCarryCount) or ((Masses > MaxCarrySingleMass) and (Count > 1))) do
      begin
         Assert(Assigned(FChildren));
         Zero(CandidateMass);
         Zero(CandidateSize);
         for Child in FChildren do
         begin
            ThisMass := Child.GetMassManifest();
            ThisSize := Child.GetOutsideSizeManifest();
            if ((ThisMass > CandidateMass) or (ThisSize > CandidateSize)) then
            begin
               Candidate := Child;
               CandidateMass := ThisMass;
               CandidateSize := ThisSize;
            end;
         end;
         Assert(Assigned(Candidate));
         Masses := Masses - CandidateMass;
         Sizes := Sizes - CandidateSize;
         Assert(MaxCarryCount >= 0);
         Count := Count - 1; // can't go negative since Count > MaxCarryCount and MaxCarryCount >= 0 // $R-
         DoBroadcast([Self], nil, [C(M(@GetDefiniteName)), SP, MP(Self, M('fumbles'), M('fumble')), SP, M(@Candidate.GetDefiniteName), M('.')]);
         Assert(FPosition in tpPlayerPositions);
         FParent.Put(Candidate, FPosition, psRoughly, Self);
      end;
   end;

begin
   CheckInventory();
   inherited;
end;

function TPlayer.CanCarryThing(Thing: TThing; var Message: TMessage): Boolean;
begin
   Assert(Message.IsValid);
   if (not Thing.CanTake(Self, Message)) then
   begin
      Result := False;
   end
   else
   if (Thing.GetMassManifest() >= MaxCarryMass) then
   begin
      Result := False;
      Message := TMessage.Create(mkTooHeavy, '_ _ far too heavy.',
                                             [Capitalise(Thing.GetDefiniteName(Self)),
                                              IsAre(Thing.IsPlural(Self))]);
   end
   else
   if (Thing.GetOutsideSizeManifest() >= MaxCarrySize) then
   begin
      Result := False;
      Message := TMessage.Create(mkTooBig, '_ _ far too big.',
                                           [Capitalise(Thing.GetDefiniteName(Self)),
                                            IsAre(Thing.IsPlural(Self))]);
   end
   else
   begin
      Result := True;
   end;
end;

function TPlayer.CanPushThing(Thing: TThing; var Message: TMessage): Boolean;
begin
   Assert(Message.IsValid);
   if (not Thing.CanMove(Self, Message)) then
   begin
      Result := False;
   end
   else
   if (Thing.GetMassManifest() >= MaxPushMass) then
   begin
      Result := False;
      Message := TMessage.Create(mkTooHeavy, '_ _ far too heavy.',
                                             [Capitalise(Thing.GetDefiniteName(Self)),
                                              IsAre(Thing.IsPlural(Self))]);
   end
   else
   if (Thing.GetOutsideSizeManifest() >= MaxPushSize) then
   begin
      Result := False;
      Message := TMessage.Create(mkTooBig, '_ _ far too big.',
                                           [Capitalise(Thing.GetDefiniteName(Self)),
                                            IsAre(Thing.IsPlural(Self))]);
   end
   else
      Result := True;
end;

function TPlayer.CanShakeThing(Thing: TThing; var Message: TMessage): Boolean;
begin
   Assert(Message.IsValid);
   if (not Thing.CanShake(Self, Message)) then
   begin
      Result := False;
   end
   else
   if (Thing.GetMassManifest() >= MaxShakeMass) then
   begin
      Result := False;
      Message := TMessage.Create(mkTooHeavy, '_ _ too heavy to shake.',
                                             [Capitalise(Thing.GetDefiniteName(Self)),
                                              IsAre(Thing.IsPlural(Self))]);
   end
   else
   if (Thing.GetOutsideSizeManifest() >= MaxShakeSize) then
   begin
      Result := False;
      Message := TMessage.Create(mkTooBig, '_ _ too big to shake.',
                                           [Capitalise(Thing.GetDefiniteName(Self)),
                                            IsAre(Thing.IsPlural(Self))]);
   end
   else
      Result := True;
end;

function TPlayer.CanMoveWithoutLoops(Subject: TThing; Target: TAtom; ThingPosition: TThingPosition; var Message: TMessage): Boolean;
var
   Ancestor: TAtom;
   MessageText: UTF8String;
begin
   if (Target = Subject) then
   begin
      Message := Message.Create(mkCannotMoveBecauseLocation, 'You can''t move something ' + ThingPositionToDirectionString(ThingPosition) + ' itself, however hard you try.');
      Result := False;
   end
   else
   begin
      Ancestor := Target;
      while ((Ancestor is TThing) and (Ancestor <> Subject)) do
      begin
         Assert(Assigned((Ancestor as TThing).Parent));
         Ancestor := (Ancestor as TThing).Parent;
      end;
      if (Ancestor = Subject) then
      begin
         { the target is on the thing }
         Assert(Target is TThing);
         Assert(Assigned((Target as TThing).Parent));
         MessageText := (Target as TThing).GetDefiniteName(Self) + ' ' + IsAre(Target.IsPlural(Self)) + ' ' + ThingPositionToString((Target as TThing).Position) + ' ';
         Ancestor := (Target as TThing).Parent;
         while (Ancestor <> Subject) do
         begin
            Assert(Ancestor is TThing);
            Assert(Assigned((Ancestor as TThing).Parent));
            MessageText := MessageText + Ancestor.GetDefiniteName(Self) + ', which ' + IsAre(Ancestor.IsPlural(Self)) + ' ' + ThingPositionToString((Ancestor as TThing).Position) + ' ';
            Ancestor := (Ancestor as TThing).Parent;
         end;
         Assert(Ancestor = Subject);
         Message := TMessage.Create(mkCannotMoveBecauseLocation, 'That would be difficult, since ' + MessageText + Ancestor.GetDefiniteName(Self) + '.');
         Result := False;
      end
      else
         Result := True;
   end;
end;

function TPlayer.IsNotUnderUs(Subject: TThing; var Message: TMessage): Boolean;
var
   Ancestor: TAtom;
begin
   Ancestor := FParent;
   while ((Ancestor is TThing) and (Ancestor <> Subject)) do
      Ancestor := (Ancestor as TThing).Parent;
   if (Ancestor = Subject) then
   begin
      { we're (possibly indirectly) standing on it }
      Message := TMessage.Create(mkCannotMoveBecauseLocation, 'Given your current position, that would be quite difficult.');
      Result := False;
   end
   else
   begin
      Result := True;
   end;
end;

function TPlayer.GetImpliedThing(Scope: TAllImpliedScope; FeatureFilter: TThingFeatures): TThing;
var
   Root: TAtom;
   List: TThingList;
   FromOutside: Boolean;
   FindMatchingThingsOptions: TFindMatchingThingsOptions;
begin
   Assert(Assigned(FParent));
   Assert((aisSelf in Scope) or (aisSurroundings in Scope));
   List := TThingList.Create();
   try
      FindMatchingThingsOptions := [];
      if (aisSelf in Scope) then
         Include(FindMatchingThingsOptions, fomIncludePerspectiveChildren);
      if (aisSurroundings in Scope) then
      begin
         Root := GetSurroundingsRoot(FromOutside);
         if (FromOutside) then
            Include(FindMatchingThingsOptions, fomFromOutside);
      end
      else
      begin
         Root := Self;
         Include(FindMatchingThingsOptions, fomFromOutside);
      end;
      Root.FindMatchingThings(Self, FindMatchingThingsOptions, tpEverything, FeatureFilter, List);
      if (List.Length > 0) then
      begin
         // XXX should implement some kind of prioritisation scheme
         Result := List.First;
      end
      else
      begin
         Result := nil;
      end;
   finally
      List.Free();
   end;
end;

function TPlayer.HasConnectedPlayer(): Boolean;
begin
   Assert(Assigned(FOnMessage) = Assigned(FOnForceDisconnect));
   Result := Assigned(FOnMessage);
end;

function TPlayer.IsReadyForRemoval(): Boolean;
begin
   Result := False;
end;

procedure TPlayer.RemoveFromWorld();
var
   ChildrenCopy: TThingList;
begin
   if (FChildren.Length > 0) then
   begin
      ChildrenCopy := TThingList.Clone(FChildren);
      try
         // XXX this should probably be more aggressive about making this work
         DoPut(ChildrenCopy, FParent, tpOn, psRoughly);
      finally
         ChildrenCopy.Free();
      end;
   end;
   AnnounceDisappearance();
   inherited;
end;

function TPlayer.IsExplicitlyReferencedThing(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; out Count: Cardinal; out GrammaticalNumber: TGrammaticalNumber): Boolean;
var
   Word: UTF8String;

   // Return true and move to next word if this is a mtch.
   // Sets Aborted to true on last word.
   function Consume(const Candidate: UTF8String; out Aborted: Boolean): Boolean;
   begin
      Result := False;
      Aborted := False;
      if (Word = Candidate) then
      begin
         Result := True;
         Inc(Count);
         if (Start + Count >= Length(Tokens)) then
         begin
            Aborted := True;
         end
         else
         begin
            Word := Tokens[Start+Count];
         end;
      end;
   end;

   // Return true and move to next word if this is a mtch.
   // Sets Aborted to true on last word.
   // Updates GrammaticalNumber to WouldBeGrammaticalNumber if this is a match.
   function Consume(const Candidate: UTF8String; out Aborted: Boolean; const WouldBeGrammaticalNumber: TGrammaticalNumber): Boolean;
   begin
      Result := Consume(Candidate, Aborted);
      if (Result) then
         GrammaticalNumber := WouldBeGrammaticalNumber;
   end;

   // Move to next word if word is a match, return true if that was the last word.
   function ConsumeAndEnd(const Candidate: UTF8String): Boolean;
   begin
      Consume(Candidate, Result);
   end;

   // Move to next word if word is a match, return true if that was the last word, updating GrammaticalNumber to WouldBeGrammaticalNumber.
   function ConsumeAndEnd(const Candidate: UTF8String; const WouldBeGrammaticalNumber: TGrammaticalNumber): Boolean;
   begin
      if (Consume(Candidate, Result)) then
         GrammaticalNumber := WouldBeGrammaticalNumber;
   end;

   // Return true if word is a match, updating GrammaticalNumber to WouldBeGrammaticalNumber.
   // Caller must not attempt to consume additional words if this returns true.
   function ConsumeTerminal(const Candidate: UTF8String; const WouldBeGrammaticalNumber: TGrammaticalNumber): Boolean;
   begin
      if (Word = Candidate) then
      begin
         Inc(Count);
         GrammaticalNumber := WouldBeGrammaticalNumber;
         Result := True;
      end
      else
         Result := False;
   end;

   // Return true if word is a match and this is not the last word.
   function ConsumeNonTerminal(const Candidate: UTF8String): Boolean;
   begin
      Result := False;
      if (Start + Count + 1 < Length(Tokens)) then
      begin
         if (Word = Candidate) then
         begin
            Result := True;
            Inc(Count);
            Word := Tokens[Start+Count];
         end;
      end;
   end;

   procedure InternalParse();
   var
      Aborted: Boolean;
   begin
      Count := 0;
      Word := Tokens[Start];
      if (ConsumeTerminal('us', [gnPlural])) then
         exit
      else
      if (Perspective = Self) then
      begin
         if (ConsumeNonTerminal('other')) then
            exit;
         if (ConsumeTerminal('me', [gnSingular])) then
            exit;
      end
      else
      begin
         Assert(Perspective <> Self);
         if (ConsumeTerminal('them', [gnPlural]) or ConsumeTerminal('others', [gnPlural])) then
            exit;
         ConsumeNonTerminal('other');
      end;
      Assert(GrammaticalNumber = []);
      if (ConsumeTerminal(Canonicalise(FName), [gnSingular])) then
         exit;
      Aborted := False;
      if (Consume('human', Aborted, [gnSingular]) or
          Consume('humans', Aborted, [gnPlural]) or
          ((FPronouns = pHe) and
           (Consume('boy', Aborted, [gnSingular]) or
            Consume('boys', Aborted, [gnPlural]) or
            Consume('man', Aborted, [gnSingular]) or
            Consume('men', Aborted, [gnPlural]) or
            Consume('male', Aborted, [gnSingular]) or
            Consume('males', Aborted, [gnPlural]))) or
          ((FPronouns = pShe) and
           (Consume('girl', Aborted, [gnSingular]) or
            Consume('girls', Aborted, [gnPlural]) or
            Consume('woman', Aborted, [gnSingular]) or
            Consume('women', Aborted, [gnPlural]) or
            Consume('female', Aborted, [gnSingular]) or
            Consume('females', Aborted, [gnPlural])))) then
      begin
         if (Aborted) then
            exit;
      end;
      Assert(not Aborted);
      if (GrammaticalNumber <> [gnPlural]) then
      begin
         if (Consume('person', Aborted, [gnSingular]) or
             Consume('persons', Aborted, [gnPlural]) or
             Consume('people', Aborted, [gnPlural]) or
             Consume('player', Aborted, [gnSingular]) or
             Consume('players', Aborted, [gnPlural])) then
         begin
            if (Aborted) then
               exit;
         end;
      end;
      Assert(not Aborted);
      if ((GrammaticalNumber <> []) and (ConsumeNonTerminal('named') or ConsumeNonTerminal('called'))) then
      begin
         if (ConsumeTerminal(Canonicalise(FName), GrammaticalNumber)) then
            exit;
         GrammaticalNumber := [];
      end;
   end;

begin
   GrammaticalNumber := [];
   InternalParse();
   Result := GrammaticalNumber <> [];
end;

function TPlayer.GetUsername(): UTF8String;
begin
   Result := FName;
end;

function TPlayer.GetPassword(): UTF8String;
begin
   Result := FPassword;
end;

procedure TPlayer.Adopt(AOnMessage: TMessageEvent; AOnForceDisconnect: TForceDisconnectEvent);
begin
   Assert(Assigned(AOnMessage));
   Assert(Assigned(AOnForceDisconnect));
   if (HasConnectedPlayer()) then
   begin
      Assert(Assigned(FOnForceDisconnect));
      FOnForceDisconnect();
   end;
   FOnMessage := AOnMessage;
   FOnForceDisconnect := AOnForceDisconnect;
end;

procedure TPlayer.Abandon();
begin
   FOnMessage := nil;
   FOnForceDisconnect := nil;
end;

initialization
   {$INCLUDE registrations/player.inc}
   Assert(MaxCarrySingleMass < MaxCarryMass);
end.
