{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit physics;

interface

uses
   storable, lists, grammarian, thingdim, messages, textstream, properties;

type
   TAtom = class;
   TAtomClass = class of TAtom;
   TInternalAtomList = specialize TStorableList<TAtom>;
   TThing = class;
   TInternalThingList = specialize TStorableList<TThing>;
   TThingClass = class of TThing;
   TLocation = class;
   TLocationList = specialize TStorableList<TLocation>; // @RegisterStorableClass
   TAvatar = class;

type
   PAtomList = ^TAtomList;
   TAtomList = class(TInternalAtomList) // @RegisterStorableClass
      function GetIndefiniteString(Perspective: TAvatar; const Conjunction: UTF8String): UTF8String;
      function GetDefiniteString(Perspective: TAvatar; const Conjunction: UTF8String): UTF8String;
      function GetLongDefiniteString(Perspective: TAvatar; const Conjunction: UTF8String): UTF8String;
      function IsPlural(Perspective: TAvatar): Boolean;
      function GetPossessiveAdjective(Perspective: TAvatar): UTF8String;
   end;

   TThingList = class(TInternalThingList) // @RegisterStorableClass
      function GetIndefiniteString(Perspective: TAvatar; const Conjunction: UTF8String): UTF8String;
      function GetDefiniteString(Perspective: TAvatar; const Conjunction: UTF8String): UTF8String;
      function GetLongDefiniteString(Perspective: TAvatar; const Conjunction: UTF8String): UTF8String;
      function IsPlural(Perspective: TAvatar): Boolean;
      function GetPossessiveAdjective(Perspective: TAvatar): UTF8String;
   end;

function GetIndefiniteString(const List: array of TAtom; StartIndex, EndIndex: Cardinal; Perspective: TAvatar; const Conjunction: UTF8String): UTF8String;
function GetDefiniteString(const List: array of TAtom; StartIndex, EndIndex: Cardinal; Perspective: TAvatar; const Conjunction: UTF8String): UTF8String;
function GetLongDefiniteString(const List: array of TAtom; StartIndex, EndIndex: Cardinal; Perspective: TAvatar; const Conjunction: UTF8String): UTF8String;

type
   TReachablePosition = (rpReachable, rpNotReachable);
   TReachablePositionSet = set of TReachablePosition;
   TNavigationAbility = (naWalk, naJump, naFly, naDebugTeleport);
   TNavigationAbilities = set of TNavigationAbility;
   TSubjectiveInformation = record
      Directions: TCardinalDirectionSet;
      Reachable: TReachablePositionSet;
      RequiredAbilitiesToNavigate: TNavigationAbilities;
      procedure Reset();
   end;
   TTravelType = (ttNone, ttByPosition, ttByDirection);
   TNavigationInstruction = record
      RequiredAbilities: TNavigationAbilities;
      case TravelType: TTravelType of
       ttNone: ();
       ttByPosition: (PositionTarget: TAtom; Position: TThingPosition);
       ttByDirection: (DirectionTarget: TAtom; Direction: TCardinalDirection);
   end;
   TTransportationInstruction = record
      case TravelType: TTravelType of
       ttNone: ();
       ttByPosition: (TargetThing: TThing; Position: TThingPosition; RequiredAbilities: TNavigationAbilities);
       ttByDirection: (Direction: TCardinalDirection);
   end;

type
   TThingFeature = (tfDiggable, tfCanDig, tfExaminingReads, tfOpenable, tfClosable,
                    tfCanHaveThingsPushedOn, { e.g. it has a ramp, or a surface flush with its container -- e.g. holes can have things pushed onto them }
                    tfCanHaveThingsPushedIn); { e.g. it has its entrance flush with its base, or has a lip flush with its container -- holes, bags; but not boxes }
   TThingFeatures = set of TThingFeature; // as returned by GetFeatures()
   TFindMatchingThingsOption = (fomIncludePerspectiveChildren, // whether to include the player's children; e.g. not used by "take all"
                                fomIncludeNonImplicits, // whether to include things normally excluded because IsImplicitlyReferenceable returns true; e.g. used by "debug things" to make the avatars be included in the list; not used by "take all" so that avatars aren't picked up
                                fomFromOutside); // if set, the search is from the perspective of something outside the atom (e.g. it's parent, or something on it); otherwise, it's from the perspective of something inside (tpContained in) the atom
   TFindMatchingThingsOptions = set of TFindMatchingThingsOption;
   TFindThingOption = (foFindAnywhere, // treat loAutoDescribe as loThreshold - used when locating a perspective so that doorways can find us even when we can't implicitly see them
                       foFromOutside,
                       foWithPreciseDirections); // indicate we want to ignore loConsiderDirectionUnimportantWhenFindingChildren, we really want to know the direction
   TFindThingOptions = set of TFindThingOption;
   TLeadingPhraseOption = (lpMandatory, lpNamesTarget);
   TLeadingPhraseOptions = set of TLeadingPhraseOption;

const
   tfEverything = []; { an empty TThingFeatures set, because thing features _limit_ what can be returned }

type
   TGetDescriptionOnOptions = set of (optDeepOn, optPrecise);
   TGetDescriptionChildrenOptions = set of (optDeepChildren, optFar, optThorough, optOmitPerspective); { deep = bag and inside bag; far = door and inside door }
   TGetPresenceStatementMode = (psThereIsAThingHere { look },
                                psThereIsAThingThere { look north },
                                psOnThatThingIsAThing { nested look },
                                psTheThingIsOnThatThing { find },
                                psOnThatSpecialThing { find (something far away) -- only if parent is TThing, not TLocation });

type
   TStreamedChildren = record
    private
      FChildren: array of TThing;
      FPositions: array of TThingPosition;
    public
      procedure AddChild(Child: TThing; Position: TThingPosition);
      procedure Apply(Parent: TAtom);
   end;

   TLandmarkOption = (loAutoDescribe, // include landmark in descriptions of the room (works for locations; works for things if their position is in tpAutoDescribeDirectional) [1]
                      loPermissibleNavigationTarget, // whether you can travel that way; direction navigation uses the first landmark with this flag
                      loThreshold, // whether FindThing and FindMatchingThings should traverse (ExplicitlyReferenced logic doesn't use this since it looks everywhere) [1]
                      loVisibleFromFarAway, // whether to traverse multiple locations; for e.g. mountains
                      loNotVisibleFromBehind, // for e.g. surfaces so that they're not visible from below
                      loConsiderDirectionUnimportantWhenFindingChildren); // so that "find hole" doesn't say "below" and "find stairs" doesn't say "above" (ignored if foWithPreciseDirections)
   // [1]: Don't include a landmark in more than one direction at a time if they are marked with either of these
   TLandmarkOptions = set of TLandmarkOption;

   TStreamedLandmarks = record
    private
      FDirections: array of TCardinalDirection;
      FLandmarks: array of TAtom;
      FOptions: array of TLandmarkOptions;
    public
      procedure AddLandmark(Direction: TCardinalDirection; Atom: TAtom; Options: TLandmarkOptions);
      procedure Apply(Parent: TLocation);
   end;

type
   TPlacementStyle = (psRoughly, psCarefully);
   TThingReporter = procedure (Thing: TThing; Count: Cardinal; GrammaticalNumber: TGrammaticalNumber) of object;
   TThingCallback = procedure (Thing: TThing) is nested;

   TAtom = class(TStorable)
    {$IFDEF DEBUG}
    private
      FIntegritySelf: PtrUInt; // set to nil on allocation, PtrUInt(Self) by constructors, High(PtrUInt) by destructor
    {$ENDIF}
    protected
      FChildren: TThingList;
      class function CreateFromProperties(Properties: TTextStreamProperties): TAtom; virtual; abstract;
      class function HandleChildProperties(Properties: TTextStreamProperties; var Values: TStreamedChildren): Boolean;
      procedure Removed(Thing: TThing); virtual;
      function IsChildTraversable(Child: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean; virtual;
      {$IFOPT C+} procedure AssertChildPositionOk(Thing: TThing; Position: TThingPosition); virtual; {$ENDIF}
    public
      constructor Create();
      destructor Destroy(); override;
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      class procedure DescribeProperties(Describer: TPropertyDescriber); virtual;

      // Moving things around
      function GetChildren(const PositionFilter: TThingPositionFilter): TThingList; // these are the children that can be shaken loose
      procedure EnumerateChildren(List: TThingList; const PositionFilter: TThingPositionFilter); virtual;
      procedure Add(Thing: TThing; Position: TThingPosition); // use Put() if it's during gameplay
      procedure Add(Thing: TThingList.TEnumerator; Position: TThingPosition);
      procedure Remove(Thing: TThing);
      procedure Remove(Thing: TThingList.TEnumerator);
      function CanPut(Thing: TThing; ThingPosition: TThingPosition; Care: TPlacementStyle; Perspective: TAvatar; var Message: TMessage): Boolean; virtual; // whether Thing can be put on/in this Atom (only supports tpOn, tpIn) without things falling
      procedure Put(Thing: TThing; Position: TThingPosition; Care: TPlacementStyle; Perspective: TAvatar); virtual;
      function GetMassManifest(): TThingMassManifest; virtual; { self and children }
      function GetOutsideSizeManifest(): TThingSizeManifest; virtual; { external size of the object (e.g. to decide if it fits inside another): self and children that are tpOutside; add tpContained children if container is flexible }
      function GetInsideSizeManifest(): TThingSizeManifest; virtual; { only children that are tpContained }
      function GetSurfaceSizeManifest(): TThingSizeManifest; virtual; { children that are tpSurface (e.g. to decide if something else can be added to the object's surface or if the surface is full already) }
      function GetRepresentative(): TAtom; virtual; { the TAtom that is responsible for high-level dealings for this one (opposite of GetSurface, maybe a TLocation); this must not be a descendant, since otherwise we'll loop forever in GetContext }
      function GetSurface(): TThing; virtual; { the TThing that is responsible for the minutiae of where things dropped on this one actually go (opposite of GetRepresentative); can be null, e.g. for a wall or the air }
      function CanSurfaceHold(const Manifest: TThingSizeManifest; const ManifestCount: Integer): Boolean; virtual; abstract;
      function GetInside(var PositionOverride: TThingPosition): TThing; virtual; { returns nil if there's no inside to speak of }
      function CanInsideHold(const Manifest: TThingSizeManifest; const ManifestCount: Integer): Boolean; virtual;
      function GetDefaultDestination(var Position: TThingPosition): TAtom; virtual; // resolve object to actual destination for moving things; Position must be tpOn, tpIn, or tpAt when it's ambiguous (e.g. "move bar to foo")
      procedure HandlePassedThrough(Traveller: TThing; AFrom, ATo: TAtom; AToPosition: TThingPosition; Perspective: TAvatar); virtual; { use this for magic doors, falling down tunnels, etc }
      procedure HandleAdd(Thing: TThing; Perspective: TAvatar); virtual; { use this to fumble things or to cause things to fall off other things }
      function GetNavigationInstructions(Direction: TCardinalDirection; Child: TThing; Perspective: TAvatar; var Message: TMessage): TNavigationInstruction; virtual; abstract; // Perspective is the traveller; Child is the ancestor of Perspective (originally Perspective itself) that's a child of Self that we're dealing with now
      function GetEntrance(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: TMessage; NotificationList: TAtomList): TAtom; virtual; abstract;

      // Finding things
      procedure GetNearbyThingsByClass(List: TThingList; FromOutside: Boolean; Filter: TThingClass); virtual;
      function GetSurroundingsRoot(out FromOutside: Boolean): TAtom; virtual;
      procedure FindMatchingThings(Perspective: TAvatar; Options: TFindMatchingThingsOptions; PositionFilter: TThingPositionFilter; PropertyFilter: TThingFeatures; List: TThingList); virtual;
      procedure ProxiedFindMatchingThings(Perspective: TAvatar; Options: TFindMatchingThingsOptions; PositionFilter: TThingPositionFilter; PropertyFilter: TThingFeatures; List: TThingList); virtual; // see note [proxy]
      function FindThing(Thing: TThing; Perspective: TAvatar; Options: TFindThingOptions; out SubjectiveInformation: TSubjectiveInformation): Boolean; virtual; // used by CanReach() and Locate()
      function FindThingTraverser(Thing: TThing; Perspective: TAvatar; Options: TFindThingOptions): Boolean; virtual;
      function ProxiedFindThingTraverser(Thing: TThing; Perspective: TAvatar; Options: TFindThingOptions): Boolean; virtual; // see note [proxy]
      procedure EnumerateExplicitlyReferencedThings(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; FromOutside, FromFarAway: Boolean; Directions: TCardinalDirectionSet; Reporter: TThingReporter); virtual;
      procedure ProxiedEnumerateExplicitlyReferencedThings(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; FromOutside, FromFarAway: Boolean; Directions: TCardinalDirectionSet; Reporter: TThingReporter); virtual; // see note [proxy]

      // Atom identity
      function IsPlural(Perspective: TAvatar): Boolean; virtual; abstract;
      function GetName(Perspective: TAvatar): UTF8String; virtual; abstract;
      function GetLongName(Perspective: TAvatar): UTF8String; virtual; { if you reply to other terms, put as many as possible here; this is shown to disambiguate }
      function GetIndefiniteName(Perspective: TAvatar): UTF8String; virtual; abstract;
      function GetDefiniteName(Perspective: TAvatar): UTF8String; virtual;
      function GetLongDefiniteName(Perspective: TAvatar): UTF8String; virtual;
      function GetSubjectPronoun(Perspective: TAvatar): UTF8String; virtual; // I
      function GetObjectPronoun(Perspective: TAvatar): UTF8String; virtual; // me
      function GetReflexivePronoun(Perspective: TAvatar): UTF8String; virtual; // myself
      function GetPossessivePronoun(Perspective: TAvatar): UTF8String; virtual; // mine
      function GetPossessiveAdjective(Perspective: TAvatar): UTF8String; virtual; // my
      function GetTitle(Perspective: TAvatar): UTF8String; virtual;
      function GetContext(Perspective: TAvatar): UTF8String; virtual;
      function GetContextFragment(Perspective: TAvatar; PertinentPosition: TThingPosition; Context: TAtom = nil): UTF8String; virtual; // see note [context]
      function GetPresenceStatementFragment(Perspective: TAvatar; PertinentPosition: TThingPosition): UTF8String; virtual; // used by children to implement psTheThingIsOnThatThing in GetPresenceStatement

      // Atom descriptions
      // comment gives the default implementation, if it's not the empty string or abstract
      function GetLook(Perspective: TAvatar): UTF8String; virtual; // title + basic + horizon + on + children
      function GetLookTowardsDirection(Perspective: TAvatar; Direction: TCardinalDirection): UTF8String; virtual; abstract;
      function GetBasicDescription(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Directions: TCardinalDirectionSet = cdAllDirections; Context: TAtom = nil): UTF8String; virtual; // self + state + here // see note [context]
      function GetHorizonDescription(Perspective: TAvatar): UTF8String; virtual; // component of GetLook, typically defers to parent's GetDescriptionRemoteHorizon
      function GetDescriptionSelf(Perspective: TAvatar): UTF8String; virtual; abstract;
      function GetDescriptionState(Perspective: TAvatar): UTF8String; virtual; { e.g. 'The bottle is open.' }
      function GetDescriptionHere(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Directions: TCardinalDirectionSet = cdAllDirections; Context: TAtom = nil): UTF8String; virtual; abstract; // see note [context]
      function GetDescriptionOn(Perspective: TAvatar; Options: TGetDescriptionOnOptions; Prefix: UTF8String = ''): UTF8String; virtual; // presence + state for each child, plus on for each child if optDeepOn
      function GetDescriptionChildren(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions; Prefix: UTF8String = ''): UTF8String; virtual;
      function GetDescriptionRemoteHorizon(Perspective: TAvatar; Context: TThing; Depth: Cardinal): UTF8String; virtual; // what to display in a Depth-deep descendant's GetLook for the horizon component (0=child) // see note [context]
      function GetDescriptionRemoteBrief(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Direction: TCardinalDirection): UTF8String; virtual; abstract; // used by locations to include their loAutoDescribe landmarks in their Here description
      function GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection; LeadingPhrase: UTF8String; Options: TLeadingPhraseOptions): UTF8String; virtual; abstract; // used for things like "look north"

      {$IFDEF DEBUG} function Debug(Perspective: TAvatar): UTF8String; virtual; {$ENDIF}
      procedure WalkChildren(Callback: TThingCallback); virtual;
   end;

   TThing = class(TAtom)
    protected
      FParent: TAtom;
      FPosition: TThingPosition;
      function IsChildTraversable(Child: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean; override;
      function IsImplicitlyReferenceable(Perspective: TAvatar; PropertyFilter: TThingFeatures): Boolean; virtual;
      function IsExplicitlyReferenceable(Perspective: TAvatar): Boolean; virtual;
    public
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      class function HandleUniqueThingProperty(Properties: TTextStreamProperties; PossibleName: UTF8String; var Value: TThing; Superclass: TThingClass): Boolean;

      // Moving things around
      procedure GetNearbyThingsByClass(List: TThingList; FromOutside: Boolean; Filter: TThingClass); override;
      function GetSurroundingsRoot(out FromOutside: Boolean): TAtom; override;
      function CanReach(Subject: TAtom; Perspective: TAvatar; var Message: TMessage): Boolean; virtual; // whether we can manipulate Subject
      function HasAbilityToTravelTo(Destination: TAtom; RequiredAbilities: TNavigationAbilities; Perspective: TAvatar; var Message: TMessage): Boolean; virtual; // [travel]
      function CanPut(Thing: TThing; ThingPosition: TThingPosition; Care: TPlacementStyle; Perspective: TAvatar; var Message: TMessage): Boolean; override;
      function CanMove(Perspective: TAvatar; var Message: TMessage): Boolean; virtual; // Perspective can be nil for internal checks
      function CanTake(Perspective: TAvatar; var Message: TMessage): Boolean; virtual; // Perspective can be nil for internal checks
      function CanShake(Perspective: TAvatar; var Message: TMessage): Boolean; virtual;
      function GetIntrinsicMass(): TThingMass; virtual; abstract;
      function GetIntrinsicSize(): TThingSize; virtual; abstract;
      function GetMassManifest(): TThingMassManifest; override;
      function GetOutsideSizeManifest(): TThingSizeManifest; override;
      function GetSurface(): TThing; override;
      function CanSurfaceHold(const Manifest: TThingSizeManifest; const ManifestCount: Integer): Boolean; override;
      procedure HandleAdd(Thing: TThing; Perspective: TAvatar); override;
      function GetDefaultDestination(var Position: TThingPosition): TAtom; override;
      function GetEntrance(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: TMessage; NotificationList: TAtomList): TAtom; override;
      function GetTransportationDestination(Perspective: TAvatar): TTransportationInstruction; virtual; // for "take stairs" or "take train" or "travel by boat", where do we end up? nil means this TThing is not a form of transportation.

      // Identification
      function GetIndefiniteName(Perspective: TAvatar): UTF8String; override;
      function GetDefiniteName(Perspective: TAvatar): UTF8String; override;
      function GetLongDefiniteName(Perspective: TAvatar): UTF8String; override;
      function IsPlural(Perspective: TAvatar): Boolean; override;
      function GetTitle(Perspective: TAvatar): UTF8String; override;

      // Descriptions
      function GetHorizonDescription(Perspective: TAvatar): UTF8String; override; // defers to parent's GetDescriptionRemoteHorizon
      function GetDescriptionRemoteHorizon(Perspective: TAvatar; Context: TThing; Depth: Cardinal): UTF8String; override; // what to display in a Depth-deep descendant's GetLook (0=child) // see note [context]
      function GetExamine(Perspective: TAvatar): UTF8String; virtual; // basic + writing + on + children
      function GetLookUnder(Perspective: TAvatar): UTF8String; virtual; // by default, just gives the name of the parent
      function GetLookTowardsDirection(Perspective: TAvatar; Direction: TCardinalDirection): UTF8String; override; // various
      function GetLookIn(Perspective: TAvatar): UTF8String; virtual; // various
      function GetLookAt(Perspective: TAvatar): UTF8String; virtual; // basic + on + children
      function GetDescriptionEmpty(Perspective: TAvatar): UTF8String; virtual; // '...empty' { only called for optThorough searches }
      function GetDescriptionNoInside(Perspective: TAvatar): UTF8String; virtual; // 'can't get in' if no inside, else ...Closed() { used both from inside and outside }
      function GetDescriptionClosed(Perspective: TAvatar): UTF8String; virtual; // '...closed' { used both from inside and outside }
      function GetInventory(Perspective: TAvatar): UTF8String; virtual; // carried
      function GetDescriptionHere(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Directions: TCardinalDirectionSet = cdAllDirections; Context: TAtom = nil): UTF8String; override; // presence statement and state for each child // see note [context]
      function GetDescriptionDirectional(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Direction: TCardinalDirection): UTF8String; virtual; // name + state; used by GetDescriptionRemoteBrief to do the self-description.
      function GetDescriptionChildren(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions; Prefix: UTF8String = ''): UTF8String; override; // in + carried
      function GetDescriptionRemoteBrief(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Direction: TCardinalDirection): UTF8String; override; // various; includes GetDescriptionDirectional and children.
      function GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection; LeadingPhrase: UTF8String; Options: TLeadingPhraseOptions): UTF8String; override; // basic
      function GetDescriptionIn(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions; Prefix: UTF8String = ''): UTF8String; virtual; // in title, plus name and in of each child
      function GetDescriptionInTitle(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions): UTF8String; virtual; // '...contains:'
      function GetDescriptionCarried(Perspective: TAvatar; DeepCarried: Boolean; Prefix: UTF8String = ''): UTF8String; virtual; // carried title, plus name, on, children for each child
      function GetDescriptionCarriedTitle(Perspective: TAvatar; DeepCarried: Boolean): UTF8String; virtual; // '...is carrying:'
      function GetPresenceStatement(Perspective: TAvatar; Mode: TGetPresenceStatementMode): UTF8String; virtual; // various
      function GetDescriptionWriting(Perspective: TAvatar): UTF8String; virtual; // 'there is no...'

      // Misc (these should be organised at some point)
      function GetNavigationInstructions(Direction: TCardinalDirection; Child: TThing; Perspective: TAvatar; var Message: TMessage): TNavigationInstruction; override;
      procedure FindMatchingThings(Perspective: TAvatar; Options: TFindMatchingThingsOptions; PositionFilter: TThingPositionFilter; PropertyFilter: TThingFeatures; List: TThingList); override;
      function FindThingTraverser(Thing: TThing; Perspective: TAvatar; Options: TFindThingOptions): Boolean; override;
      function IsExplicitlyReferencedThing(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; out Count: Cardinal; out GrammaticalNumber: TGrammaticalNumber): Boolean; virtual; abstract; // compares Tokens to FName, essentially
      procedure EnumerateExplicitlyReferencedThings(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; FromOutside, FromFarAway: Boolean; Directions: TCardinalDirectionSet; Reporter: TThingReporter); override;
      procedure Moved(OldParent: TAtom; Care: TPlacementStyle; Perspective: TAvatar); virtual;
      procedure Shake(Perspective: TAvatar); virtual;
      procedure Press(Perspective: TAvatar); virtual;
      function GetFeatures(): TThingFeatures; virtual;
      function CanDig(Target: TThing; Perspective: TAvatar; var Message: TMessage): Boolean; virtual; // can this be used as a digging tool to dig the given target?
      function Dig(Spade: TThing; Perspective: TAvatar; var Message: TMessage): Boolean; virtual; // can this be dug by the given spade? if so, do it
      procedure Dug(Target: TThing; Perspective: TAvatar; var Message: TMessage); virtual; // this was used as a digging tool to dig the given target
      function IsOpen(): Boolean; virtual;
      function CanSeeIn(): Boolean; virtual; // must return true if IsOpen() is true
      function CanSeeOut(): Boolean; virtual; // must return true if IsOpen() is true
      function Open(Perspective: TAvatar; var Message: TMessage): Boolean; virtual; // can this be opened? if so, do it
      function Close(Perspective: TAvatar; var Message: TMessage): Boolean; virtual; // can this be closed? if so, do it
      // XXX eventually we should add opening and closing tools, just like we have digging tools
      {$IFDEF DEBUG} function Debug(Perspective: TAvatar): UTF8String; override; {$ENDIF}
      property Parent: TAtom read FParent;
      property Position: TThingPosition read FPosition write FPosition;
   end;

   { Thing that can move of its own volition }
   TAvatar = class(TThing)
    protected
      function IsImplicitlyReferenceable(Perspective: TAvatar; PropertyFilter: TThingFeatures): Boolean; override;
      class function CreateFromProperties(Properties: TTextStreamProperties): TAtom; override;
    public
      procedure DoLook(); virtual; abstract;
      procedure AvatarMessage(Message: TMessage); virtual; abstract;
      procedure AnnounceAppearance(); virtual; abstract;
      procedure AnnounceDisappearance(); virtual; abstract;
      procedure AnnounceDeparture(Destination: TAtom; Direction: TCardinalDirection); virtual; abstract;
      procedure AnnounceDeparture(Destination: TAtom); virtual; abstract;
      procedure AnnounceArrival(Source: TAtom); virtual; abstract;
      procedure AnnounceArrival(Source: TAtom; Direction: TCardinalDirection); virtual; abstract;
      procedure AutoDisambiguated(Message: UTF8String); virtual; abstract;
      function HasConnectedPlayer(): Boolean; virtual; abstract;
      function IsReadyForRemoval(): Boolean; virtual; abstract;
      procedure RemoveFromWorld(); virtual;
      function Locate(Thing: TThing; Options: TFindThingOptions = [foFindAnywhere]): TSubjectiveInformation; // foFromOutside will be added automatically if necessary
   end;

   {$IFDEF DEBUG} // used by AssertDirectionHasDestination()
   TDummyAvatar = class(TAvatar) // @RegisterStorableClass IFDEF DEBUG
    public
      function IsPlural(Perspective: TAvatar): Boolean; override;
      function GetName(Perspective: TAvatar): UTF8String; override;
      function GetDescriptionSelf(Perspective: TAvatar): UTF8String; override;
      function CanSurfaceHold(const Manifest: TThingSizeManifest; const ManifestCount: Integer): Boolean; override;
      function GetIntrinsicMass(): TThingMass; override;
      function GetIntrinsicSize(): TThingSize; override;
      function IsExplicitlyReferencedThing(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; out Count: Cardinal; out GrammaticalNumber: TGrammaticalNumber): Boolean; override;
      procedure DoLook(); override;
      procedure AvatarMessage(Message: TMessage); override;
      procedure AnnounceAppearance(); override;
      procedure AnnounceDisappearance(); override;
      procedure AnnounceDeparture(Destination: TAtom; Direction: TCardinalDirection); override;
      procedure AnnounceDeparture(Destination: TAtom); override;
      procedure AnnounceArrival(Source: TAtom); override;
      procedure AnnounceArrival(Source: TAtom; Direction: TCardinalDirection); override;
      procedure AutoDisambiguated(Message: UTF8String); override;
      function HasConnectedPlayer(): Boolean; override;
      function IsReadyForRemoval(): Boolean; override;
   end;
   {$ENDIF}

   TLocation = class(TAtom)
    protected
     type
      TDirectionalLandmark = record
        Direction: TCardinalDirection; // XXX we should make this a set
        Options: TLandmarkOptions;
        Atom: TAtom; // may or may not be a TThing in this room; often a remote TLocation or TThing
      end;
     var
      FLandmarks: array of TDirectionalLandmark;
      class function HandleLandmarkProperties(Properties: TTextStreamProperties; var Values: TStreamedLandmarks): Boolean;
      function CanFindInDirection(Direction: TCardinalDirection; Atom: TAtom): Boolean;
      function FindThingDirectionalTraverser(Thing: TThing; Perspective: TAvatar; Distance: Cardinal; Direction: TCardinalDirection; Options: TFindThingOptions; var SubjectiveInformation: TSubjectiveInformation): Boolean; virtual;
      {$IFDEF DEBUG} procedure AssertDirectionHasDestination(Direction: TCardinalDirection; Atom: TAtom); {$ENDIF}
    public
      constructor Create();
      destructor Destroy(); override;
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      class function HandleUniqueLocationProperty(Properties: TTextStreamProperties; PossibleName: UTF8String; var Value: TLocation): Boolean;

      function HasLandmark(Direction: TCardinalDirection): Boolean;
      procedure AddLandmark(Direction: TCardinalDirection; Atom: TAtom; Options: TLandmarkOptions);
      procedure AddSurroundings(Atom: TAtom; const Directions: TCardinalDirectionSet = cdCompassDirection);
      function GetAtomForDirectionalNavigation(Direction: TCardinalDirection): TAtom; virtual; // returns first landmark in given direction that is loPermissibleNavigationTarget
      function IsPlural(Perspective: TAvatar): Boolean; override;
      function GetLookTowardsDirection(Perspective: TAvatar; Direction: TCardinalDirection): UTF8String; override;
      function GetLookTowardsDirectionDefault(Perspective: TAvatar; Direction: TCardinalDirection): UTF8String; virtual;
      function GetDescriptionSelf(Perspective: TAvatar): UTF8String; override;
      function GetDescriptionLandmarks(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Directions: TCardinalDirectionSet = cdAllDirections; Context: TAtom = nil): UTF8String; virtual; // this describes the landmarks; used by GetDescriptionHere and GetDescriptionRemoteHorizon; see also note [context]
      function GetDescriptionHere(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Directions: TCardinalDirectionSet = cdAllDirections; Context: TAtom = nil): UTF8String; override; // see note [context]
      function GetDescriptionRemoteHorizon(Perspective: TAvatar; Context: TThing; Depth: Cardinal): UTF8String; override; // includes our major landmarks in descendant GetLooks // see note [context]
      function GetDescriptionRemoteBrief(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Direction: TCardinalDirection): UTF8String; override;
      function GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection; LeadingPhrase: UTF8String; Options: TLeadingPhraseOptions): UTF8String; override;
      function GetNavigationInstructions(Direction: TCardinalDirection; Child: TThing; Perspective: TAvatar; var Message: TMessage): TNavigationInstruction; override;
      procedure FailNavigation(Direction: TCardinalDirection; Perspective: TAvatar; out Message: TMessage); { also called when trying to dig in and push something in this direction }
      function GetEntrance(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: TMessage; NotificationList: TAtomList): TAtom; override;
      function CanSurfaceHold(const Manifest: TThingSizeManifest; const ManifestCount: Integer): Boolean; override;
      procedure EnumerateExplicitlyReferencedThings(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; FromOutside, FromFarAway: Boolean; Directions: TCardinalDirectionSet; Reporter: TThingReporter); override;
      procedure FindMatchingThings(Perspective: TAvatar; Options: TFindMatchingThingsOptions; PositionFilter: TThingPositionFilter; PropertyFilter: TThingFeatures; List: TThingList); override;
      function FindThing(Thing: TThing; Perspective: TAvatar; Options: TFindThingOptions; out SubjectiveInformation: TSubjectiveInformation): Boolean; override;
      {$IFDEF DEBUG} function Debug(Perspective: TAvatar): UTF8String; override; {$ENDIF}
   end;

function MakeAtomFromStream(AClass: TClass; Stream: TTextStream): TObject;
function GetRegisteredAtomClass(AClassName: UTF8String): TClass;

procedure ForceTravel(Traveller: TThing; Destination: TAtom; Direction: TCardinalDirection; Perspective: TAvatar);
procedure ForceTravel(Traveller: TThing; Destination: TAtom; Position: TThingPosition; Perspective: TAvatar); // only tpOn and tpIn allowed

{ Note [travel]:
    Navigation works as follows:
       avEnter, avClimbOn, and avUseTransportation actions invoke the Position-based ForceTravel() above directly.
       avGo actions first invoke the player's parent's GetNavigationInstructions() method to find out what to do
         GetNavigationInstructions() then typicially defers up until you reach the location.
         The TLocation.GetNavigationInstructions() looks in the appropriate direction.
         The THole.GetNavigationInstructions() turns 'up' and 'out' into a location
       Then, if that returns something, it calls ForceTravel().
       ForceTravel() calls...
       - GetSurface() (for avClimbOn and avUseTransportationSubject, via tpOn), or
       - GetEntrance() (for avEnter, via tpIn, and avGo, via the TCardinalDirection version of ForceTravel).
         - TThing.GetEntrance() looks for the child that is a tpOpening and defers to it, else defers to GetInside()
         - TLocation.GetEntrance() calls GetSurface().
         - TThresholdLocation.GetEntrance() fast-forwards you to the other side.
}

{ Note [context]:
    Several of the description methods have a Context argument.
    This represents the (child) object from which we are getting a
    description. For example, if you are next to a pedestal and you
    look around, and the pedestal is a key factor in the description
    of the surrounding area, then it may be included in the room's
    description as a key point. However, if you are on the pedestal,
    then the pedestal will talk about itself and it's important that
    the pedestal not be mentioned again by the room when the room
    gives its "horizon" description.
    The Context can be nil if the horizon description was requested
    without having previously described anything. }

{ Note [proxy]:
    Some of the APIs have "Proxy" in their name and, by default, just
    defer to the same APIs without "Proxy" in their name. These are
    methods whose non-proxy versions walk down the tree. The proxy
    versions are used when crossing from one tree to another (e.g.
    when dealing with landmarks), in particular when crossing from a
    location to another, or from within a proxy method of a location
    to a specific object within that location (e.g. room -> landmark
    threshold -> doorway). This allows you to further fork at that
    point. The regular methods should not go back up the tree, since
    that risks an infinite loop (or at least a lot of redundant work). }

procedure ConnectLocations(SourceLocation: TLocation; Direction: TCardinalDirection; Destination: TLocation; Options: TLandmarkOptions = [loPermissibleNavigationTarget]); // Calls AddLandmark in both directions. Verifies that a symmetric connection has been made.

procedure QueueForDisposal(Atom: TAtom);
procedure EmptyDisposalQueue();

implementation

uses
   sysutils, broadcast, exceptions, typinfo {$IFOPT C+}, typedump {$ENDIF};

procedure TSubjectiveInformation.Reset();
begin
   Directions := [];
   Reachable := [];
   RequiredAbilitiesToNavigate := [];
end;


procedure ForceTravel(Traveller: TThing; Destination: TAtom; Direction: TCardinalDirection; Perspective: TAvatar);
var
   SpecificDestination, Source: TAtom;
   SourceAncestor, DestinationAncestor: TAtom;
   Message: TMessage;
   Position: TThingPosition;
   NotificationList: TAtomList;
   NotificationTarget: TAtom;
   DisambiguationOpening: TThing;
begin
   Assert(Assigned(Destination), 'ForceTravel requires a non-nil destination.');
   Assert(Assigned(Traveller), 'ForceTravel requires a non-nil traveller.');
   Source := Traveller.Parent;
   SourceAncestor := Source;
   while (SourceAncestor is TThing) do
      SourceAncestor := (SourceAncestor as TThing).Parent;
   DestinationAncestor := Destination;
   while (DestinationAncestor is TThing) do
      DestinationAncestor := (DestinationAncestor as TThing).Parent;
   if (SourceAncestor <> DestinationAncestor) then
      Source := SourceAncestor;
   Position := tpOn;
   DisambiguationOpening := nil;
   Message := TMessage.Create();
   NotificationList := TAtomList.Create();
   try
      SpecificDestination := Destination.GetEntrance(Traveller, Direction, Perspective, Position, DisambiguationOpening, Message, NotificationList);
      if (Assigned(SpecificDestination)) then
      begin
         Assert(Message.AsKind = mkSuccess, 'Selected destination yet simultaneously failed.');
         Assert(Message.AsText = '', 'Succeeded but with an explicit error message.');
         if (Assigned(DisambiguationOpening) and (DisambiguationOpening <> SpecificDestination)) then
            Perspective.AutoDisambiguated('through ' + DisambiguationOpening.GetDefiniteName(Perspective));
         if (Perspective = Traveller) then
            Perspective.AnnounceDeparture(Destination, Direction);
         for NotificationTarget in NotificationList do
            NotificationTarget.HandlePassedThrough(Traveller, Source, SpecificDestination, Position, Perspective);
         if ((Traveller.Parent = SpecificDestination) and (Traveller.Position = Position)) then
         begin
            if (Perspective = Traveller) then
            begin
               Message := TMessage.Create(mkNoOp, '_ _ back where _ started, _ _.', 
                                          [Capitalise(Traveller.GetDefiniteName(Perspective)),
                                           IsAre(Perspective.IsPlural(Perspective)),
                                           Traveller.GetSubjectPronoun(Perspective),
                                           ThingPositionToString(Position),
                                           SpecificDestination.GetDefiniteName(Perspective)]);
               Perspective.AvatarMessage(Message);
            end;
         end
         else
         begin
            SpecificDestination.Put(Traveller, Position, psCarefully, Perspective);
            if (Perspective = Traveller) then
            begin
               Perspective.AnnounceArrival(Source.GetRepresentative(), ReverseCardinalDirection(Direction));
               Perspective.DoLook();
            end;
         end;
      end
      else
      begin
         Assert(Message.AsKind <> mkSuccess, 'Failed to get a specific destination yet still succeeded.');
         Assert(Message.AsText <> '', 'Failed without an explicit message.');
         Message.PrefaceFailureTopic('_ cannot go _.',
                                    [Capitalise(Traveller.GetDefiniteName(Perspective)),
                                     CardinalDirectionToString(Direction)]);
         Perspective.AvatarMessage(Message);
      end;
   finally
      NotificationList.Free();
   end;
end;

procedure ForceTravel(Traveller: TThing; Destination: TAtom; Position: TThingPosition; Perspective: TAvatar);
var
   SpecificDestination, Source: TAtom;
   Message: TMessage;
   Success: Boolean;
   Ancestor: TAtom;
   NotificationList: TAtomList;
   NotificationTarget: TAtom;
   DisambiguationOpening: TThing;
   Direction: TCardinalDirection;
begin
   Assert(Assigned(Destination));
   Assert(Assigned(Traveller));
   Assert(Position in [tpOn, tpIn]);
   Source := Traveller.Parent;
   Ancestor := Destination;
   while ((Ancestor is TThing) and (Ancestor <> Traveller)) do
      Ancestor := (Ancestor as TThing).Parent;
   if (Ancestor = Traveller) then
   begin
      Perspective.AvatarMessage(TMessage.Create(mkCannotMoveBecauseLocation, 'That would prove rather challenging given where _ _ relative to _.',
                                                [Destination.GetDefiniteName(Perspective),
                                                 IsAre(Destination.IsPlural(Perspective)),
                                                 Traveller.GetReflexivePronoun(Perspective)]));
   end
   else
   if ((Position = tpOn) or (Destination is TLocation)) then
   begin
      SpecificDestination := Destination.GetSurface();
      if (Assigned(SpecificDestination)) then
         Destination := SpecificDestination;
      Assert(Assigned(Destination));
      Assert(Destination is TThing, 'if you want to be "on" a TLocation, give it a surface available from GetSurface()');
      Message := TMessage.Create();
      Success := Destination.CanPut(Traveller, tpOn, psCarefully, Perspective, Message);
      if (Success) then
      begin
         Assert(Message.AsKind = mkSuccess);
         Assert(Message.AsText = '');
         if ((Traveller.Parent = Destination) and (Traveller.Position = tpOn)) then
         begin
            if (Perspective = Traveller) then
            begin
               Message := TMessage.Create(mkNoOp, '_ _ back where _ started, _ _.', 
                                          [Capitalise(Traveller.GetDefiniteName(Perspective)),
                                           IsAre(Perspective.IsPlural(Perspective)),
                                           Traveller.GetSubjectPronoun(Perspective),
                                           ThingPositionToString(tpOn),
                                           Destination.GetDefiniteName(Perspective)]);
               Perspective.AvatarMessage(Message);
            end;
         end
         else
         begin
            Destination.Put(Traveller, tpOn, psCarefully, Perspective);
            if (Perspective = Traveller) then
            begin
               // XXX announcements, like AnnounceArrival() and co
               Perspective.DoLook();
            end;
         end;
      end
      else
      begin
         Assert(Message.AsKind <> mkSuccess);
         Assert(Message.AsText <> '');
         Message.PrefaceFailureTopic('_ cannot get onto _.',
                                     [Capitalise(Traveller.GetDefiniteName(Perspective)),
                                      Destination.GetDefiniteName(Perspective)]);
         Perspective.AvatarMessage(Message);
      end;
   end
   else
   if (Position = tpIn) then
   begin
      Assert(Destination is TThing);
      DisambiguationOpening := nil;
      Message := TMessage.Create();
      NotificationList := TAtomList.Create();
      try
         Ancestor := Source;
         while ((Ancestor is TThing) and (Ancestor <> Destination)) do
            Ancestor := (Ancestor as TThing).Parent;
         if (Ancestor = Destination) then
            Direction := cdOut
         else
            Direction := cdIn;
         SpecificDestination := Destination.GetEntrance(Traveller, Direction, Perspective, Position, DisambiguationOpening, Message, NotificationList);
         if (Assigned(SpecificDestination)) then
         begin
            Assert(Message.AsKind = mkSuccess);
            Assert(Message.AsText = '');
            if (Assigned(DisambiguationOpening) and (DisambiguationOpening <> SpecificDestination)) then
               Perspective.AutoDisambiguated('through ' + DisambiguationOpening.GetDefiniteName(Perspective));
            if (Perspective = Traveller) then
               Perspective.AnnounceDeparture(Destination);
            for NotificationTarget in NotificationList do
               NotificationTarget.HandlePassedThrough(Traveller, Source, SpecificDestination, Position, Perspective);
            if ((Traveller.Parent = SpecificDestination) and (Traveller.Position = Position)) then
            begin
               if (Perspective = Traveller) then
               begin
                  Message := TMessage.Create(mkNoOp, '_ _ back where _ started, _ _.', 
                                             [Capitalise(Traveller.GetDefiniteName(Perspective)),
                                              IsAre(Perspective.IsPlural(Perspective)),
                                              Traveller.GetSubjectPronoun(Perspective),
                                              ThingPositionToString(Position),
                                              SpecificDestination.GetDefiniteName(Perspective)]);
                  Perspective.AvatarMessage(Message);
               end;
            end
            else
            begin
               SpecificDestination.Put(Traveller, Position, psCarefully, Perspective);
               if (Perspective = Traveller) then
               begin
                  Perspective.AnnounceArrival(Source.GetRepresentative());
                  Perspective.DoLook();
               end;
            end;
         end
         else
         begin
            Assert(Message.AsKind <> mkSuccess);
            Assert(Message.AsText <> '');
            Message.PrefaceFailureTopic('_ cannot enter _.',
                                        [Capitalise(Traveller.GetDefiniteName(Perspective)),
                                         Destination.GetDefiniteName(Perspective)]);
            Perspective.AvatarMessage(Message);
         end;
      finally
         NotificationList.Free();
      end;
   end
   else
      Assert(False, 'unexpected position for navigation: ' + IntToStr(Cardinal(Position)));
end;

{$INCLUDE atomlisthelper.inc} // generated by regen.pl

var
   DisposalQueue: TAtomList;

procedure InitDisposalQueue();
begin
   if (not Assigned(DisposalQueue)) then
      DisposalQueue := TAtomList.Create([slOwner]);
end;

procedure QueueForDisposal(Atom: TAtom);
begin
   Assert(Assigned(DisposalQueue));
   DisposalQueue.AppendItem(Atom);
end;

procedure EmptyDisposalQueue();
begin
   Assert(Assigned(DisposalQueue));
   DisposalQueue.FreeItems();
end;


procedure TStreamedChildren.AddChild(Child: TThing; Position: TThingPosition);
begin
   Assert(Length(FChildren) = Length(FPositions));
   SetLength(FChildren, Length(FChildren) + 1);
   SetLength(FPositions, Length(FPositions) + 1);
   FChildren[High(FChildren)] := Child;
   FPositions[High(FPositions)] := Position;
   Assert(Length(FChildren) = Length(FPositions));
end;

procedure TStreamedChildren.Apply(Parent: TAtom);
var
   Index: Cardinal;
begin
   Assert(Length(FChildren) = Length(FPositions));
   if (Length(FChildren) > 0) then
      for Index := Low(FChildren) to High(FChildren) do
         Parent.Add(FChildren[Index], FPositions[Index]);
   SetLength(FChildren, 0);
   SetLength(FPositions, 0);
end;


procedure TStreamedLandmarks.AddLandmark(Direction: TCardinalDirection; Atom: TAtom; Options: TLandmarkOptions);
begin
   Assert(Length(FLandmarks) = Length(FDirections));
   Assert(Length(FLandmarks) = Length(FOptions));
   SetLength(FDirections, Length(FDirections) + 1);
   SetLength(FLandmarks, Length(FLandmarks) + 1);
   SetLength(FOptions, Length(FOptions) + 1);
   FDirections[High(FDirections)] := Direction;
   FLandmarks[High(FLandmarks)] := Atom;
   FOptions[High(FOptions)] := Options;
   Assert(Length(FLandmarks) = Length(FDirections));
   Assert(Length(FLandmarks) = Length(FOptions));
end;

procedure TStreamedLandmarks.Apply(Parent: TLocation);
var
   Index: Cardinal;
begin
   Assert(Length(FLandmarks) = Length(FDirections));
   Assert(Length(FLandmarks) = Length(FOptions));
   if (Length(FLandmarks) > 0) then
      for Index := Low(FLandmarks) to High(FLandmarks) do // $R-
         Parent.AddLandmark(FDirections[Index], FLandmarks[Index], FOptions[Index]);
   SetLength(FDirections, 0);
   SetLength(FLandmarks, 0);
   SetLength(FOptions, 0);
end;


constructor TAtom.Create();
begin
   {$IFDEF DEBUG}
    FIntegritySelf := PtrUInt(Self);
   {$ENDIF}
   inherited;
   FChildren := TThingList.Create([slOwner]);
end;

destructor TAtom.Destroy();
begin
   {$IFDEF DEBUG}
   if (FIntegritySelf > 0) then
   begin
      Assert(FIntegritySelf < High(PtrUInt), 'Tried to free an already-freed TAtom');
      Assert(FIntegritySelf = PtrUInt(Self), 'Evidence of heap corruption detected');
      FIntegritySelf := High(PtrUInt);
   end;
   {$ENDIF}
   FChildren.Free();
   inherited;
end;

constructor TAtom.Read(Stream: TReadStream);
begin
   {$IFDEF DEBUG}
    FIntegritySelf := PtrUInt(Self);
   {$ENDIF}
   inherited;
   FChildren := Stream.ReadObject() as TThingList;
end;

procedure TAtom.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteObject(FChildren);
end;

class function TAtom.HandleChildProperties(Properties: TTextStreamProperties; var Values: TStreamedChildren): Boolean;
var
   Position: TThingPosition;
   Child: TThing;
   Stream: TTextStream;
begin
   if (Properties.Name = pnChild) then
   begin
      Stream := Properties.Accept();
      Position := Stream.specialize GetEnum<TThingPosition>();
      Stream.ExpectPunctuation(',');
      Child := Stream.specialize GetObject<TThing>();
      Values.AddChild(Child, Position);
      Properties.Advance();
      Result := False;
   end
   else
      Result := True;
end;

class procedure TAtom.DescribeProperties(Describer: TPropertyDescriber);
begin
end;

function MakeAtomFromStream(AClass: TClass; Stream: TTextStream): TObject;
var
   Atom: TAtom;
   Properties: TTextStreamProperties;
begin
   Assert(Assigned(AClass));
   if (not AClass.InheritsFrom(TAtom)) then
      Stream.Fail(AClass.ClassName + ' is not a TAtom');
   Properties := TTextStreamProperties.Create(Stream);
   try
      Atom := TAtomClass(AClass).CreateFromProperties(Properties);
      if (not Properties.Done) then
         Stream.Fail('Could not parse property "' + Properties.Name + '"');
   finally
      Properties.Free();
   end;
   Result := Atom;
end;

{$IFDEF DEBUG}
function GetRegisteredAtomClass(AClassName: UTF8String): TClass;
var
   Candidate: StorableClass;
begin
   Result := nil;
   Candidate := GetRegisteredClass(AClassName);
   if ((Candidate <> nil) and Candidate.InheritsFrom(TAtom)) then
      Result := TAtomClass(Candidate);
end;
{$ENDIF}

{$IFOPT C+}
procedure TAtom.AssertChildPositionOk(Thing: TThing; Position: TThingPosition);
var
   TempPosition: TThingPosition;
   Child: TThing;
begin
   TempPosition := tpIn;
   Assert((Position <> tpIn) or (GetInside(TempPosition) <> nil), 'Tried to put something inside something without an inside (use CanPut!)');
   TempPosition := tpIn;
   Assert((Position <> tpIn) or (GetInside(TempPosition) = Self), 'Tried to put something inside something but the inside is something else (use GetInside!)');
   if (Position in tpOpening) then
      for Child in FChildren do
         Assert(not (Child.FPosition in tpOpening), 'Can''t have two things that are the tpOpening of another thing (see note in grammarian.pas)');
end;
{$ENDIF}

function TAtom.GetChildren(const PositionFilter: TThingPositionFilter): TThingList;
begin
   Result := TThingList.Create([slDropDuplicates]);
   EnumerateChildren(Result, PositionFilter);
end;

procedure TAtom.EnumerateChildren(List: TThingList; const PositionFilter: TThingPositionFilter);
var
   Child: TThing;
begin
   for Child in FChildren do
      if (Child.FPosition in PositionFilter) then
         List.AppendItem(Child);
end;

procedure TAtom.Add(Thing: TThing; Position: TThingPosition);
{$IFOPT C+}
var
   ParentSearch: TAtom;
{$ENDIF}
begin
   {$IFOPT C+} AssertChildPositionOk(Thing, Position); {$ENDIF}
   if (Assigned(Thing.FParent)) then
      Thing.FParent.Remove(Thing);
   Assert(not Assigned(Thing.FParent));
   FChildren.AppendItem(Thing);
   Thing.FParent := Self;
   Thing.FPosition := Position;
   {$IFOPT C+}
   // check for cycles
   ParentSearch := Thing;
   Assert(ParentSearch is TThing);
   repeat
      ParentSearch := (ParentSearch as TThing).Parent;
      Assert(ParentSearch <> Thing);
   until (not (ParentSearch is TThing));
   {$ENDIF}
end;

procedure TAtom.Add(Thing: TThingList.TEnumerator; Position: TThingPosition);
var
   OldParent: TAtom;
   ActualThing: TThing;
   {$IFOPT C+}
   ParentSearch: TAtom;
   {$ENDIF}
begin
   Assert(Thing.FList <> FChildren);
   ActualThing := Thing.Current;
   {$IFOPT C+} AssertChildPositionOk(ActualThing, Position); {$ENDIF}
   Assert(Assigned(ActualThing));
   OldParent := ActualThing.FParent;
   FChildren.AdoptItem(Thing);
   ActualThing.FParent := Self;
   ActualThing.FPosition := Position;
   if (Assigned(OldParent)) then
      OldParent.Removed(ActualThing);
   {$IFOPT C+}
   // check for cycles
   ParentSearch := ActualThing;
   Assert(ParentSearch is TThing);
   repeat
      ParentSearch := (ParentSearch as TThing).Parent;
      Assert(ParentSearch <> ActualThing);
   until (not (ParentSearch is TThing));
   {$ENDIF}
end;

procedure TAtom.Remove(Thing: TThing);
begin
   FChildren.RemoveItem(Thing);
   Thing.FParent := nil;
   Removed(Thing);
end;

procedure TAtom.Remove(Thing: TThingList.TEnumerator);
var
   OldThing: TThing;
begin
   Assert(Thing.FList = FChildren);
   OldThing := Thing.Current;
   Thing.Remove();
   OldThing.FParent := nil;
   Removed(OldThing);
end;

procedure TAtom.Removed(Thing: TThing);
begin
end;

function TAtom.CanPut(Thing: TThing; ThingPosition: TThingPosition; Care: TPlacementStyle; Perspective: TAvatar; var Message: TMessage): Boolean;
begin
   Assert(Message.IsValid);
   if (ThingPosition = tpOn) then
   begin
      Result := CanSurfaceHold(Thing.GetIntrinsicSize(), 1);
      if (not Result) then
         Message := TMessage.Create(mkTooBig, 'There is not enough room on _ for _.',
                                              [GetDefiniteName(Perspective),
                                               Thing.GetDefiniteName(Perspective)]);
   end
   else
   if (ThingPosition = tpIn) then
   begin
      Result := CanInsideHold(Thing.GetOutsideSizeManifest(), 1);
      if (not Result) then
      begin
         if ((not Assigned(GetInside(ThingPosition))) and (Self is TThing)) then
            Message := TMessage.Create(mkNoOpening, '_ _ not appear to have an opening.',
                                                    [Capitalise(GetDefiniteName(Perspective)),
                                                     TernaryConditional('does', 'do', IsPlural(Perspective))])
         else
            Message := TMessage.Create(mkTooBig, 'There is not enough room in _ for _.',
                                                 [GetDefiniteName(Perspective),
                                                  Thing.GetDefiniteName(Perspective)]);;
      end;
   end
   else
   begin
      Assert(False, 'Unexpected position ' + IntToStr(Cardinal(ThingPosition)));
      Result := False;
   end;
end;

procedure TAtom.Put(Thing: TThing; Position: TThingPosition; Care: TPlacementStyle; Perspective: TAvatar);
var
   OldParent: TAtom;
begin
   OldParent := Thing.FParent;
   Assert((OldParent <> Self) or (Position <> Thing.FPosition)); // added this late, so callers might still need updating
   Add(Thing, Position);
   Thing.Moved(OldParent, Care, Perspective);
   HandleAdd(Thing, Perspective);
end;

function TAtom.GetMassManifest(): TThingMassManifest;
var
   Child: TThing;
begin
   Zero(Result);
   for Child in FChildren do
      Result := Result + Child.GetMassManifest();
end;

function TAtom.GetOutsideSizeManifest(): TThingSizeManifest;
var
   Child: TThing;
begin
   Zero(Result);
   for Child in FChildren do
      if (Child.Position in tpOutside) then
         Result := Result + Child.GetOutsideSizeManifest();
end;

function TAtom.GetInsideSizeManifest(): TThingSizeManifest;
var
   Child: TThing;
begin
   Zero(Result);
   for Child in FChildren do
      if (Child.Position in tpContained) then
         Result := Result + Child.GetOutsideSizeManifest();
end;

function TAtom.GetSurfaceSizeManifest(): TThingSizeManifest;
var
   Child: TThing;
begin
   Zero(Result);
   for Child in FChildren do
      if (Child.Position in tpSurface) then
         Result := Result + Child.GetIntrinsicSize();
end;

function TAtom.GetRepresentative(): TAtom;
begin
   Result := Self;
end;

function TAtom.GetSurface(): TThing;
begin
   Result := nil;
end;

function TAtom.GetInside(var PositionOverride: TThingPosition): TThing;
var
   Child: TThing;
begin
   Assert(PositionOverride = tpIn, 'GetInside() must be called with an argument preinitialised to tpIn');
   Result := nil;
   for Child in FChildren do
   begin
      if (Child.Position in tpOpening) then
      begin
         Assert(not Assigned(Result));
         Result := Child.GetInside(PositionOverride);
         {$IFOPT C-} Exit; {$ENDIF} // no need to go through the list if not checking for assertions anyway
      end;
   end;
end;

function TAtom.CanInsideHold(const Manifest: TThingSizeManifest; const ManifestCount: Integer): Boolean;
var
   Inside: TThing;
   Position: TThingPosition;
begin
   Position := tpIn;
   Inside := GetInside(Position);
   if (Assigned(Inside)) then
   begin
      Assert(Inside <> Self, 'If you make GetInside() return the element proper, then you must override CanInsideHold() also.');
      if (Position = tpIn) then
         Result := Inside.CanInsideHold(Manifest, ManifestCount)
      else
      if (Position = tpOn) then
         Result := Inside.CanSurfaceHold(Manifest, ManifestCount)
      else
         raise Exception.Create('Unexpected or unknown overriding inside thing position ' + IntToStr(Cardinal(Position)));
   end
   else
      Result := False;
end;

function TAtom.GetDefaultDestination(var Position: TThingPosition): TAtom;
begin
   Assert(Position in [tpAt, tpOn, tpIn]); // at means we want the default
   Position := tpOn;
   Result := GetSurface();
   if (not Assigned(Result)) then
   begin
      Result := Self;
   end;
end;

procedure TAtom.HandlePassedThrough(Traveller: TThing; AFrom, ATo: TAtom; AToPosition: TThingPosition; Perspective: TAvatar);
begin
end;

procedure TAtom.HandleAdd(Thing: TThing; Perspective: TAvatar);
begin
end;

{ this is used when notifying players of something, e.g. shouting }
{ it should propagate to all of the things that should get the message }
procedure TAtom.GetNearbyThingsByClass(List: TThingList; FromOutside: Boolean; Filter: TThingClass);
var
   Child: TThing;
begin
   for Child in FChildren do
      if (FromOutside <> (Child.Position in tpContained)) then { assumes that we are closed (TThing.GetNearbyThingsByClass solves that) }
         Child.GetNearbyThingsByClass(List, True, Filter);
end;

function TAtom.GetSurroundingsRoot(out FromOutside: Boolean): TAtom;
begin
   Result := Self;
   FromOutside := True;
end;

procedure TAtom.FindMatchingThings(Perspective: TAvatar; Options: TFindMatchingThingsOptions; PositionFilter: TThingPositionFilter; PropertyFilter: TThingFeatures; List: TThingList);
var
   Child: TThing;
begin
   for Child in FChildren do
      if (((fomIncludePerspectiveChildren in Options) or (Child <> Perspective)) and (IsChildTraversable(Child, Perspective, fomFromOutside in Options))) then
         Child.FindMatchingThings(Perspective, Options + [fomFromOutside], PositionFilter, PropertyFilter, List);
end;

procedure TAtom.ProxiedFindMatchingThings(Perspective: TAvatar; Options: TFindMatchingThingsOptions; PositionFilter: TThingPositionFilter; PropertyFilter: TThingFeatures; List: TThingList);
begin
   FindMatchingThings(Perspective, Options, PositionFilter, PropertyFilter, List);
end;

function TAtom.FindThing(Thing: TThing; Perspective: TAvatar; Options: TFindThingOptions; out SubjectiveInformation: TSubjectiveInformation): Boolean;
begin
   // The semantics of FindThing are a bit vague.
   // It's used by CanReach() to determine if a thing can be manipulated.
   // It's used by Locate() to determine, basically, what direction something is in (using Options foFindAnywhere).
   // In theory the Perspective argument can be used to determine if that particular Perspective can see that particular item
   // e.g. a ghost only being visible to other ghosts
   SubjectiveInformation.Reset();
   Result := FindThingTraverser(Thing, Perspective, Options);
   if (Result) then
      Include(SubjectiveInformation.Reachable, rpReachable);
end;

function TAtom.FindThingTraverser(Thing: TThing; Perspective: TAvatar; Options: TFindThingOptions): Boolean;
var
   Child: TThing;
begin
   for Child in FChildren do
      if (IsChildTraversable(Child, Perspective, foFromOutside in Options)) then
         if (Child.FindThingTraverser(Thing, Perspective, Options + [foFromOutside])) then
         begin
            Result := True;
            exit;
         end;
   Result := False;
end;

function TAtom.ProxiedFindThingTraverser(Thing: TThing; Perspective: TAvatar; Options: TFindThingOptions): Boolean;
begin
   Result := FindThingTraverser(Thing, Perspective, Options);
end;

procedure TAtom.EnumerateExplicitlyReferencedThings(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; FromOutside, FromFarAway: Boolean; Directions: TCardinalDirectionSet; Reporter: TThingReporter);
var
   Child: TThing;
begin
   for Child in FChildren do
      if (IsChildTraversable(Child, Perspective, FromOutside)) then
         Child.EnumerateExplicitlyReferencedThings(Tokens, Start, Perspective, True, FromFarAway, Directions, Reporter);
end;

procedure TAtom.ProxiedEnumerateExplicitlyReferencedThings(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; FromOutside, FromFarAway: Boolean; Directions: TCardinalDirectionSet; Reporter: TThingReporter);
begin
   EnumerateExplicitlyReferencedThings(Tokens, Start, Perspective, FromOutside, FromFarAway, Directions, Reporter);
end;

function TAtom.GetLongName(Perspective: TAvatar): UTF8String;
begin
   Result := GetName(Perspective);
end;

function TAtom.GetDefiniteName(Perspective: TAvatar): UTF8String;
begin
   Result := 'the ' + GetName(Perspective);
end;

function TAtom.GetLongDefiniteName(Perspective: TAvatar): UTF8String;
begin
   Result := 'the ' + GetLongName(Perspective);
end;

function TAtom.GetSubjectPronoun(Perspective: TAvatar): UTF8String;
begin
   Result := 'it';
end;

function TAtom.GetObjectPronoun(Perspective: TAvatar): UTF8String;
begin
   Result := 'it';
end;

function TAtom.GetReflexivePronoun(Perspective: TAvatar): UTF8String;
begin
   Result := 'itself';
end;

function TAtom.GetPossessivePronoun(Perspective: TAvatar): UTF8String;
begin
   Result := 'its';
end;

function TAtom.GetPossessiveAdjective(Perspective: TAvatar): UTF8String;
begin
   Result := 'its';
end;

function TAtom.GetTitle(Perspective: TAvatar): UTF8String;
begin
   Result := GetName(Perspective) + WithSpaceIfNotEmpty(ParentheticallyIfNotEmpty(GetContext(Perspective)));
end;

function TAtom.GetContext(Perspective: TAvatar): UTF8String;
var
   Current, Representative: TAtom;
   PertinentPosition: TThingPosition;
   Fragment: UTF8String;
begin
   Result := '';
   Current := Self;
   while (Current is TThing) do
   begin
      PertinentPosition := (Current as TThing).Position;
      Current := (Current as TThing).FParent;
      Assert(Assigned(Current));
      Representative := Current.GetRepresentative();
      Assert(Assigned(Representative));
      if (Representative <> Current) then
      begin
         Current := Representative;
         PertinentPosition := tpAt;
      end;
      Fragment := Current.GetContextFragment(Perspective, PertinentPosition, Self);
      if (Length(Fragment) > 0) then
      begin
         if (Length(Result) > 0) then
            Result := Result + ', ';
         Result := Result + Fragment;
      end;
   end;
end;

function TAtom.GetContextFragment(Perspective: TAvatar; PertinentPosition: TThingPosition; Context: TAtom = nil): UTF8String;
begin
   Result := ThingPositionToString(PertinentPosition) + ' ' + GetDefiniteName(Perspective);
end;

function TAtom.GetPresenceStatementFragment(Perspective: TAvatar; PertinentPosition: TThingPosition): UTF8String;
begin
   Result := ThingPositionToString(PertinentPosition) + ' ' + GetLongDefiniteName(Perspective);
end;

function TAtom.GetLook(Perspective: TAvatar): UTF8String;
begin
   Result := Capitalise(GetTitle(Perspective)) +
             WithNewlineIfNotEmpty(GetBasicDescription(Perspective, psThereIsAThingHere, cdAllDirections)) +
             WithNewlineIfNotEmpty(GetHorizonDescription(Perspective)) +
             WithNewlineIfNotEmpty(GetDescriptionOn(Perspective, [optDeepOn])) +
             WithNewlineIfNotEmpty(GetDescriptionChildren(Perspective, [optOmitPerspective]));
end;

function TAtom.GetBasicDescription(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Directions: TCardinalDirectionSet = cdAllDirections; Context: TAtom = nil): UTF8String;
begin
   Result := WithSpaces([GetDescriptionSelf(Perspective),
                         GetDescriptionState(Perspective),
                         GetDescriptionHere(Perspective, Mode, Directions, Context)]);
end;

function TAtom.GetHorizonDescription(Perspective: TAvatar): UTF8String;
begin
   Result := '';
end;

function TAtom.GetDescriptionRemoteHorizon(Perspective: TAvatar; Context: TThing; Depth: Cardinal): UTF8String;
begin
   if (Depth = 0) then // child
      Result := GetBasicDescription(Perspective, psThereIsAThingHere, cdAllDirections, Context)
   else // deeper descendants
      Result := '';
end;

function TAtom.GetDescriptionState(Perspective: TAvatar): UTF8String; { e.g. 'The bottle is open.' }
begin
   Result := '';
end;

function TAtom.GetDescriptionOn(Perspective: TAvatar; Options: TGetDescriptionOnOptions; Prefix: UTF8String): UTF8String;

   procedure ProcessBatch(Children: TThingList);
   var
      Mode: TGetPresenceStatementMode;
      Child: TThing;
   begin
      for Child in Children do
      begin
         if ((Child.Position = tpOn) and ((optPrecise in Options) or (Child <> Perspective))) then
         begin
            if (Length(Result) > 0) then
               Result := Result + #10;
            if (optPrecise in Options) then
               Mode := psOnThatThingIsAThing
            else
               Mode := psThereIsAThingHere;
            Result := Result + Prefix + Child.GetPresenceStatement(Perspective, Mode) + WithSpaceIfNotEmpty(Child.GetDescriptionState(Perspective));
            if (optDeepOn in Options) then
            begin
               if (Length(Prefix) = 0) then
                  Result := Result + WithNewlineIfNotEmpty(Child.GetDescriptionOn(Perspective, Options + [optPrecise], Prefix))
               else
                  Result := Result + WithNewlineIfNotEmpty(Child.GetDescriptionOn(Perspective, Options + [optPrecise], Prefix + '  '));
            end
         end;
      end;
   end;

var
   Surface: TThing;
begin
   Result := '';
   ProcessBatch(FChildren);
   Surface := GetSurface();
   if (Assigned(Surface) and (Surface <> Self)) then
      ProcessBatch(Surface.FChildren);
end;

function TAtom.GetDescriptionChildren(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions; Prefix: UTF8String = ''): UTF8String;
begin
   Result := '';
end;

function TAtom.IsChildTraversable(Child: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean;
begin
   Assert(Child.FParent = Self);
   Result := True;
end;

{$IFDEF DEBUG}
function TAtom.Debug(Perspective: TAvatar): UTF8String;
var
   Child: TThing;
   ChildResult: UTF8String;
begin
   Result := GetName(Perspective) + #10 +
             'Long Name: ' + GetLongDefiniteName(Perspective) + #10 +
             'Long Name (without perspective): ' + GetLongDefiniteName(nil) + #10 +
             'Class: ' + ClassName + #10 +
             'Children: ';
   ChildResult := '';
   for Child in FChildren do
   begin
      if (ChildResult <> '') then
         ChildResult := ChildResult + '; ';
      ChildResult := ChildResult + Child.GetLongName(Perspective) + ' (' + ThingPositionToString(Child.Position) + ')';
   end;
   if (ChildResult = '') then
      ChildResult := 'none';
   Result := Result + ChildResult;
end;
{$ENDIF}

procedure TAtom.WalkChildren(Callback: TThingCallback);
var
   Child: TThing;
begin
   for Child in FChildren do
      Callback(Child);
end;


constructor TThing.Read(Stream: TReadStream);
begin
   inherited;
   Stream.ReadReference(@Pointer(FParent));
   FPosition := TThingPosition(Stream.ReadCardinal());
end;

procedure TThing.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteReference(FParent);
   Stream.WriteCardinal(Cardinal(FPosition));
end;

class function TThing.HandleUniqueThingProperty(Properties: TTextStreamProperties; PossibleName: UTF8String; var Value: TThing; Superclass: TThingClass): Boolean;
begin
   if (Properties.Name = PossibleName) then
   begin
      Properties.EnsureNotSeen(PossibleName);
      Value := Properties.Accept().specialize GetObject<TThing>();
      Assert(Assigned(Value));
      if ((Value.ClassType <> Superclass) and (not Value.InheritsFrom(Superclass))) then
         Properties.Fail('Value of "' + PossibleName + '" must be a subclass of "' + Superclass.ClassName + '"');
      Properties.Advance();
      Result := False;
   end
   else
      Result := True;
end;

procedure TThing.GetNearbyThingsByClass(List: TThingList; FromOutside: Boolean; Filter: TThingClass);
var
   Child: TThing;
begin
   if (Self is Filter) then
      List.AppendItem(Self);
   if (FromOutside and CanSeeOut()) then
   begin
      for Child in FChildren do
         if (Child.Position in tpContained) then
            Child.GetNearbyThingsByClass(List, True, Filter);
   end
   else
      inherited;
end;

function TThing.GetSurroundingsRoot(out FromOutside: Boolean): TAtom;
begin
   Assert(Assigned(FParent));
   if (FPosition in tpContained) then
   begin
      Result := FParent;
      FromOutside := False;
   end
   else
      Result := FParent.GetSurroundingsRoot(FromOutside);
end;

function TThing.GetMassManifest(): TThingMassManifest;
begin
   Result := inherited GetMassManifest() + GetIntrinsicMass();
end;

function TThing.GetOutsideSizeManifest(): TThingSizeManifest;
begin
   Result := inherited GetOutsideSizeManifest() + GetIntrinsicSize();
end;

function TThing.GetSurface(): TThing;
begin
   Result := Self;
end;

function TThing.CanSurfaceHold(const Manifest: TThingSizeManifest; const ManifestCount: Integer): Boolean;
var
   Surface: TThing;
begin
   Surface := GetSurface();
   Assert(Assigned(Surface));
   if (Surface <> Self) then
      Result := Surface.CanSurfaceHold(Manifest, ManifestCount)
   else
      Result := (GetSurfaceSizeManifest() + Manifest) <= (GetIntrinsicSize());
end;

procedure TThing.HandleAdd(Thing: TThing; Perspective: TAvatar);
var
   Nothing: TThingSizeManifest;
   SelectedSlideChild, Candidate, Target: TThing;
begin
   Zero(Nothing);
   Target := FParent.GetSurface();
   if (Assigned(Target) and (Target <> Self)) then // otherwise no point trying, we have nowhere to slide the stuff to
   begin
      while (not CanSurfaceHold(Nothing, 0)) do
      begin
         SelectedSlideChild := nil;
         for Candidate in FChildren do
         begin
            if (Candidate.Position = tpOn) then
            begin
               if (Assigned(SelectedSlideChild)) then
               begin
                  if (((not (SelectedSlideChild is TAvatar)) and (SelectedSlideChild.GetSurfaceSizeManifest() < Candidate.GetSurfaceSizeManifest())) or (Candidate is TAvatar)) then
                  begin
                     continue;
                  end;
               end;
               SelectedSlideChild := Candidate;
            end;
         end;
         if (Assigned(SelectedSlideChild)) then
         begin
            DoBroadcast([Self], nil, [C(M(@SelectedSlideChild.GetDefiniteName)), SP, MP(SelectedSlideChild, M('slides'), M('slide')), SP, M('off'), SP, M(@GetDefiniteName), M('.')]);
            Target.Put(SelectedSlideChild, tpOn, psRoughly, Perspective);
         end
         else
         begin
            break;
         end;
      end;
   end;
end;

function TThing.GetDefaultDestination(var Position: TThingPosition): TAtom;
begin
   Assert(Position in [tpAt, tpOn, tpIn], 'GetDefaultDestination was called with ' + ThingPositionToString(Position)); // at means we want the default
   case (Position) of
      tpAt: begin
               Result := nil;
               if (IsOpen()) then
               begin
                  Position := tpIn;
                  Result := GetInside(Position);
               end;
               if (not Assigned(Result)) then
               begin
                  Position := tpOn;
                  Result := GetSurface();
               end;
            end;
      tpOn: Result := GetSurface();
      tpIn: Result := GetInside(Position);
      else Result := nil; // unreachable
   end;
   if (not Assigned(Result)) then
   begin
      Result := Self;
   end;
end;

function TThing.GetEntrance(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: TMessage; NotificationList: TAtomList): TAtom;
var
   Child: TThing;
   RawMessage: UTF8String;
begin
   Assert(Message.IsValid);
   // first, look for an opening we can use as the entrance, and defer to it if we have one
   Assert(Assigned(FChildren));
   for Child in FChildren do
   begin
      if (Child.Position in tpOpening) then
      begin
         Assert(IsChildTraversable(Child, Perspective, True));
         DisambiguationOpening := Child;
         Result := Child.GetEntrance(Traveller, Direction, Perspective, PositionOverride, DisambiguationOpening, Message, NotificationList);
         exit;
      end;
   end;
   // no opening, so instead try to directly put the thing inside our insides
   PositionOverride := tpIn;
   Result := GetInside(PositionOverride);
   if (Assigned(Result)) then
   begin
      if (Result = Self) then
      begin
         if (not CanPut(Traveller, PositionOverride, psCarefully, Perspective, Message)) then
            Result := nil;
      end
      else
      begin
         NotificationList.AppendItem(Self);
      end;
   end
   else
   if (Perspective.GetIntrinsicSize() > GetOutsideSizeManifest()) then
   begin
      // no entrance, no inside, but the thing is too small anyway
      // "You are bigger than the apple."
      Message := TMessage.Create(mkTooBig, '_ _ bigger than _.',
                                           [Capitalise(Perspective.GetDefiniteName(Perspective)),
                                            IsAre(Perspective.IsPlural(Perspective)),
                                            GetDefiniteName(Perspective)]);
   end
   else
   begin
      // no entrance, no inside
      // try to give a message along the lines of "the bottle is closed", but failing that, the default below
      RawMessage := GetDescriptionState(Perspective);
      if (RawMessage <> '') then
         Message := TMessage.Create(mkNoOpening, RawMessage)
      else
         Message := TMessage.Create(mkNoOpening, '_ _ no discernible entrance.',
                                                 [Capitalise(GetDefiniteName(Perspective)),
                                                  TernaryConditional('has', 'have', IsPlural(Perspective))]);
   end;
end;

function TThing.GetTransportationDestination(Perspective: TAvatar): TTransportationInstruction;
begin
   Result.TravelType := ttNone;
end;

function TThing.GetIndefiniteName(Perspective: TAvatar): UTF8String;
begin
   Result := GetName(Perspective);
   if (not IsPlural(Perspective)) then
      Result := IndefiniteArticle(Result) + ' ' + Result;
end;

function TThing.GetDefiniteName(Perspective: TAvatar): UTF8String;
var
   Context: TAtom;
begin
   Result := inherited;
   Assert(Assigned(FParent));
   Context := FParent.GetRepresentative();
   if (Context is TThing) then
      case FPosition of
         tpAmbiguousPartOfImplicit: Result := Result + ' of ' + Context.GetDefiniteName(Perspective);
      else
         ;
      end;
end;

function TThing.GetLongDefiniteName(Perspective: TAvatar): UTF8String;
var
   Context: TAtom;
begin
   Result := inherited;
   Assert(Assigned(FParent));
   Context := FParent.GetRepresentative();
   if (Context is TThing) then
      case FPosition of
       tpPartOfImplicit, tpAmbiguousPartOfImplicit: Result := Result + ' of ' + Context.GetDefiniteName(Perspective);
       tpOnImplicit: Result := Result + ' on ' + Context.GetDefiniteName(Perspective);
       tpAtImplicit: Result := Result + ' at ' + Context.GetDefiniteName(Perspective);
       tpAroundImplicit: Result := Result + ' near ' + Context.GetDefiniteName(Perspective);
      else
         ;
      end;
end;

function TThing.IsPlural(Perspective: TAvatar): Boolean;
begin
   Result := False;
end;

function TThing.GetTitle(Perspective: TAvatar): UTF8String;
begin
   Assert(Assigned(FParent));
   if (Perspective.Parent = Self) then
      Result := Capitalise(ThingPositionToString(Perspective.Position)) + ' ' + GetDefiniteName(Perspective) + WithSpaceIfNotEmpty(ParentheticallyIfNotEmpty(GetContext(Perspective)))
   else
      Result := inherited;
end;

function TThing.GetHorizonDescription(Perspective: TAvatar): UTF8String;
var
   RemoteHorizon: UTF8String;
begin
   Assert(Assigned(Perspective));
   if ((Perspective.Parent = Self) and (Perspective.Position in tpContained) and not CanSeeOut()) then
   begin
      Result := ''; // nothing to see, perspective is inside a closed container, us
   end
   else
   begin
      RemoteHorizon := FParent.GetRepresentative().GetDescriptionRemoteHorizon(Perspective, Self, 0);
      if (FPosition in tpPertinent) then
      begin
         Result := GetPresenceStatement(Perspective, psTheThingIsOnThatThing) + WithNewlineIfMultiline(RemoteHorizon);
      end
      else
      begin
         Result := RemoteHorizon;
      end;
   end;
end;

function TThing.GetDescriptionRemoteHorizon(Perspective: TAvatar; Context: TThing; Depth: Cardinal): UTF8String;
begin
   Result := inherited;
   if (CanSeeOut() or (Assigned(Context) and ((Context.FParent <> Self) or (not (Context.FPosition in tpContained))))) then
   begin
      // Context is able to see out of us, recurse.
      Result := Result + WithSpaceIfNotEmpty(FParent.GetRepresentative().GetDescriptionRemoteHorizon(Perspective, Self, Depth + 1)); // $R- (we assume tree is never going to be four billion things deep since we'd run out of memory long before integers in that case)
   end;
end;

function TThing.GetExamine(Perspective: TAvatar): UTF8String;
var
   Writing: UTF8String;
begin
   if (tfExaminingReads in GetFeatures()) then
      Writing := GetDescriptionWriting(Perspective)
   else
      Writing := '';
   Result := GetBasicDescription(Perspective, psThereIsAThingHere, cdAllDirections) +
             WithNewlineIfNotEmpty(Writing) +
             WithNewlineIfNotEmpty(GetDescriptionOn(Perspective, [optDeepOn, optPrecise])) +
             WithNewlineIfNotEmpty(GetDescriptionChildren(Perspective, [optDeepChildren, optThorough]));
end;

function TThing.GetLookUnder(Perspective: TAvatar): UTF8String;
begin
   Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + IsAre(IsPlural(Perspective)) + ' ' + ThingPositionToString(FPosition) + ' ' + FParent.GetDefiniteName(Perspective) + '.';
   // Consider actually looking cdDown from here, somehow. (Would be particularly useful for e.g. TScenery used as a ceiling.)
end;

function TThing.GetLookTowardsDirection(Perspective: TAvatar; Direction: TCardinalDirection): UTF8String;
// XXX refactor things like GetNavigationInstructions

   function DeferToParent(): UTF8String;
   begin
      Assert(Assigned(FParent));
      Result := FParent.GetLookTowardsDirection(Perspective, Direction)
   end;

begin
   Assert(Direction <> cdIn);
   if (Perspective.Parent = Self) then
   begin
      if (not (Direction in cdPhysicalDirections)) then
      begin
         Assert(Direction = cdOut);
         if ((Perspective.Position in tpContained) and (not CanSeeOut())) then
         begin
            Assert(not IsOpen(), 'if something is open, CanSeeOut() and CanSeeIn() should be true');
            Result := GetDescriptionClosed(Perspective)
         end
         else
         begin
            Result := FParent.GetRepresentative().GetDescriptionRemoteDetailed(Perspective, Direction, 'Looking ' + CardinalDirectionToString(Direction), []);
         end;
      end
      else
      if (Perspective.Position in tpDeferNavigationToParent) then
      begin
         Result := DeferToParent();
      end
      else
      begin
         Result := Capitalise(Perspective.GetDefiniteName(Perspective)) + ' ' + IsAre(Perspective.IsPlural(Perspective)) + ' ' + ThingPositionToString(Perspective.Position) + ' ' + GetDefiniteName(Perspective) + '.';
      end;
   end
   else
      Result := DeferToParent();
end;

function TThing.GetLookIn(Perspective: TAvatar): UTF8String;
var
   PositionOverride: TThingPosition;
   Inside: TThing;
   Representative: TAtom;
   Contents, Lead: UTF8String;
   Options: TLeadingPhraseOptions;
   {$IFOPT C+} Child: TThing; {$ENDIF}
begin
   if (CanSeeIn() or ((Perspective.Parent = Self) and (Perspective.Position in tpContained))) then
   begin
      {$IFOPT C+}
         for Child in FChildren do
            if (Child.Position in tpOpening) then
               Assert(IsChildTraversable(Child, Perspective, True));
      {$ENDIF}
      PositionOverride := tpIn;
      Inside := GetInside(PositionOverride);
      if (Assigned(Inside)) then
      begin
         Representative := Inside.GetRepresentative();
         Lead := 'Looking in ' + GetDefiniteName(Perspective);
         Options := [];
         if (Representative = Self) then
            Include(Options, lpNamesTarget);
         Result := Representative.GetDescriptionRemoteDetailed(Perspective, cdIn, Lead, Options);
      end
      else
         Result := '';
      Contents := GetDescriptionIn(Perspective, [optDeepChildren, optThorough, optFar]);
      if (Contents = '') then
         Contents := GetDescriptionEmpty(Perspective);
      if (Result = '') then
         Result := Contents
      else
         Result := Result + #10 + Contents;
   end
   else
   begin
      Result := GetDescriptionNoInside(Perspective);
   end;
end;

function TThing.GetLookAt(Perspective: TAvatar): UTF8String;
begin
   Result := GetBasicDescription(Perspective, psThereIsAThingHere, cdAllDirections) +
             WithNewlineIfNotEmpty(GetDescriptionOn(Perspective, [optDeepOn, optPrecise])) +
             WithNewlineIfNotEmpty(GetDescriptionChildren(Perspective, [optDeepChildren]));
end;

function TThing.GetDescriptionEmpty(Perspective: TAvatar): UTF8String;
begin
   Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + IsAre(IsPlural(Perspective)) + ' empty.';
end;

function TThing.GetDescriptionNoInside(Perspective: TAvatar): UTF8String;
var
   PositionOverride: TThingPosition;
   Inside: TThing;
begin
   Assert(not IsOpen());
   PositionOverride := tpIn;
   Inside := GetInside(PositionOverride);
   if (not Assigned(Inside)) then
      Result := 'It is not clear how to get inside ' + GetDefiniteName(Perspective) + '.'
   else
      Result := GetDescriptionClosed(Perspective);
end;

function TThing.GetDescriptionClosed(Perspective: TAvatar): UTF8String;
begin
   Assert(not IsOpen());
   Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + IsAre(IsPlural(Perspective)) + ' closed.';
end;

function TThing.GetInventory(Perspective: TAvatar): UTF8String;
begin
   Result := GetDescriptionCarried(Perspective, True);
   if (Result = '') then
      Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + IsAre(IsPlural(Perspective)) + ' not carrying anything.';
end;

function TThing.GetDescriptionHere(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Directions: TCardinalDirectionSet = cdAllDirections; Context: TAtom = nil): UTF8String;
var
   Child: TThing;
   FromOutside: Boolean;
begin
   FromOutside := ((Perspective.Parent = Self) and (Perspective.Position in tpContained));
   Result := '';
   for Child in FChildren do
   begin
      { we exclude context so that, e.g., we don't say "there's a pedestal here!" when you're on it }
      if ((Child <> Context) and (Child.Position in tpAutoDescribe) and IsChildTraversable(Child, Perspective, FromOutside)) then
      begin
         if (Length(Result) > 0) then
            Result := Result + ' ';
         Result := Result + Child.GetPresenceStatement(Perspective, Mode) + WithSpaceIfNotEmpty(Child.GetDescriptionState(Perspective));
      end;
   end;
end;

function TThing.GetDescriptionDirectional(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Direction: TCardinalDirection): UTF8String;
begin
   Result := Capitalise(CardinalDirectionToDirectionString(Direction)) + ' ' + IsAre(IsPlural(Perspective)) + ' ' + GetIndefiniteName(Perspective) + '.' + WithSpaceIfNotEmpty(GetDescriptionState(Perspective));
end;

function TThing.GetDescriptionChildren(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions; Prefix: UTF8String): UTF8String;
var
   Additional: UTF8String;
begin
   if (CanSeeIn() or ((Perspective.Parent = Self) and (Perspective.Position in tpContained))) then
      Result := GetDescriptionIn(Perspective, Options, Prefix)
   else
      Result := '';
   Additional := GetDescriptionCarried(Perspective, optDeepChildren in Options, Prefix);
   if ((Length(Result) > 0) and (Length(Additional) > 0)) then
      Result := Result + #10;
   Result := Result + Additional;
end;

function TThing.GetDescriptionRemoteBrief(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Direction: TCardinalDirection): UTF8String;
var
   Child: TThing;
   S: UTF8String;
begin
   Result := GetDescriptionDirectional(Perspective, Mode, Direction);
   for Child in FChildren do
   begin
      if ((Child.Position in tpAutoDescribe) and (IsChildTraversable(Child, Perspective, True))) then
         S := Child.GetDescriptionRemoteBrief(Perspective, Mode, Direction);
      if (Length(S) > 0) then
      begin
         if (Length(Result) > 0) then
            Result := Result + ' ';
         Result := Result + S;
      end;
   end;
end;

function TThing.GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection; LeadingPhrase: UTF8String; Options: TLeadingPhraseOptions): UTF8String;
begin
   if ((lpNamesTarget in Options) and not (lpMandatory in Options)) then
      Result := ''
   else
      Result := LeadingPhrase + ', you see ' + GetIndefiniteName(Perspective) + '. ';
   Result := Result + GetBasicDescription(Perspective, psThereIsAThingThere, cdAllDirections - [ReverseCardinalDirection(Direction)]);
end;

function TThing.GetDescriptionIn(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions; Prefix: UTF8String = ''): UTF8String;

   procedure ProcessBatch(Children: TThingList; ExpectedPositionFilter: TThingPositionFilter);
   var
      Child: TThing;
   begin
      for Child in Children do
      begin
         if (((Child <> Perspective) or (not (optOmitPerspective in Options))) and (Child.Position in ExpectedPositionFilter)) then
         begin
            if (Length(Result) > 0) then
               Result := Result + #10;
            Result := Result + Prefix + '  ' + Capitalise(Child.GetIndefiniteName(Perspective)) + '.';
            if (optDeepChildren in Options) then
               Result := Result + WithNewlineIfNotEmpty(Child.GetDescriptionIn(Perspective, Options - [optFar, optThorough], Prefix + '  '));
         end;
      end;
   end;

var
   Inside, Surface: TThing;
   ExpectedPosition: TThingPosition;
   ExpectedPositionFilter: TThingPositionFilter;
begin
   Result := '';
   if (optThorough in Options) then
      ExpectedPositionFilter := [tpIn, tpEmbedded]
   else
      ExpectedPositionFilter := [tpIn];
   ProcessBatch(FChildren, ExpectedPositionFilter);
   Surface := GetSurface();
   if (Assigned(Surface) and (Surface <> Self)) then
      ProcessBatch(Surface.FChildren, ExpectedPositionFilter);
   if (optFar in Options) then
   begin
      ExpectedPosition := tpIn;
      Inside := GetInside(ExpectedPosition);
      if (Assigned(Inside) and (((Inside <> Self) and (Inside <> Surface)) or (not (ExpectedPosition in ExpectedPositionFilter)))) then
         ProcessBatch(Inside.FChildren, [ExpectedPosition]);
   end;
   if (Length(Result) > 0) then
      Result := Prefix + GetDescriptionInTitle(Perspective, Options) + #10 + Result;
end;

function TThing.GetDescriptionInTitle(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions): UTF8String;
begin
   Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + TernaryConditional('contains', 'contain', IsPlural(Perspective)) + ':';
end;

function TThing.GetDescriptionCarried(Perspective: TAvatar; DeepCarried: Boolean; Prefix: UTF8String = ''): UTF8String;

   procedure ProcessBatch(Children: TThingList);
   var
      Child: TThing;
   begin
      for Child in Children do
      begin
         if (Child.Position = tpCarried) then
         begin
            if (Length(Result) > 0) then
               Result := Result + #10;
            Result := Result + Prefix + '  ' + Capitalise(Child.GetIndefiniteName(Perspective)) + '.';
            if (DeepCarried) then
               Result := Result + WithNewlineIfNotEmpty(Child.GetDescriptionOn(Perspective, [optDeepOn, optPrecise], Prefix + '  ')) +
                                  WithNewlineIfNotEmpty(Child.GetDescriptionChildren(Perspective, [optDeepChildren], Prefix + '  '));
         end;
      end;
   end;

var
   Surface: TThing;
begin
   Result := '';
   ProcessBatch(FChildren);
   Surface := GetSurface();
   if (Assigned(Surface) and (Surface <> Self)) then
      ProcessBatch(Surface.FChildren);
   if (Length(Result) > 0) then
      Result := Prefix + GetDescriptionCarriedTitle(Perspective, DeepCarried) + #10 + Result;
end;

function TThing.GetDescriptionCarriedTitle(Perspective: TAvatar; DeepCarried: Boolean): UTF8String;
begin
   Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + TernaryConditional('is carrying', 'are carrying', IsPlural(Perspective)) + ':';
end;

function TThing.GetPresenceStatement(Perspective: TAvatar; Mode: TGetPresenceStatementMode): UTF8String;
begin
   if (Mode = psThereIsAThingHere) then
      Result := 'There ' + IsAre(IsPlural(Perspective)) + ' ' + GetIndefiniteName(Perspective) + ' here.'
   else
   if (Mode = psThereIsAThingThere) then
      Result := 'There ' + IsAre(IsPlural(Perspective)) + ' ' + GetIndefiniteName(Perspective) + ' there.'
   else
   if (Mode = psOnThatThingIsAThing) then
      Result := Capitalise(ThingPositionToString(FPosition)) + ' ' + FParent.GetDefiniteName(Perspective) + ' ' + IsAre(IsPlural(Perspective)) + ' ' + GetIndefiniteName(Perspective) + '.'
   else
   if (Mode = psTheThingIsOnThatThing) then
   begin
      Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + IsAre(IsPlural(Perspective)) + ' ' + FParent.GetPresenceStatementFragment(Perspective, FPosition) + '.';
   end
   else
   if (Mode = psOnThatSpecialThing) then
   begin
      if (FParent is TThing) then
         Result := ThingPositionToString(FPosition) + ' ' + FParent.GetLongDefiniteName(Perspective)
      else
         Result := '';
   end
   else
   begin
      Assert(False, 'unknown mode');
      Result := '<error>';
   end;
end;

function TThing.GetDescriptionWriting(Perspective: TAvatar): UTF8String;
begin
   Result := 'There is no discernible writing on ' + GetDefiniteName(Perspective) + '.';
end;

function TThing.CanReach(Subject: TAtom; Perspective: TAvatar; var Message: TMessage): Boolean;

   {$IFOPT C+}
   function GetRootFor(Atom: TAtom): TAtom; // for assertions only
   begin
      while (Atom is TThing) do
         Atom := (Atom as TThing).Parent;
      Result := Atom;
   end;
   {$ENDIF}

var
   SelfRoot: TAtom;
   FromOutside: Boolean;
   FindOptions: TFindThingOptions;
   SubjectiveInformation: TSubjectiveInformation;
   Direction: TCardinalDirection;
   DirectionMessage: UTF8String;
begin
   Assert(Assigned(Subject));
   Assert(Message.IsValid);
   Assert(Assigned(FParent));
   SelfRoot := GetSurroundingsRoot(FromOutside);
   Assert(Assigned(SelfRoot));
   if (Subject is TLocation) then
   begin
      Result := SelfRoot = Subject;
      if (not Result) then
      begin
         Message := TMessage.Create(mkNotReachable, '_ _ _ _ anymore.',
                                    [Capitalise(GetDefiniteName(Perspective)),
                                     TernaryConditional('isn''t', 'aren''t', IsPlural(Perspective)),
                                     'at', // XXX
                                           // "at" might not work, e.g. "You aren't in Kansas anymore."
                                           // needs "in" not "at", as does "The Garden"; but "The
                                           // Beach" or "Whole Foods" wants "at"... maybe TLocations
                                           // need a ThingPosition analogue with tpIn and tpAt!
                                     Subject.GetDefiniteName(Perspective)]);
      end
      else
         Assert(FromOutside);
   end
   else
   if (Subject is TThing) then
   begin
      // (similar to Locate() logic)
      FindOptions := [];
      if (FromOutside) then
         Include(FindOptions, foFromOutside);
      Result := SelfRoot.FindThing(Subject as TThing, Perspective, FindOptions, SubjectiveInformation);
      if (not Result) then
      begin
         Message := TMessage.Create(mkNotReachable, '_ can''t see _ anymore.',
                                    [Capitalise(GetDefiniteName(Perspective)),
                                     Subject.GetDefiniteName(Perspective)]);
      end
      else
      if (not (rpReachable in SubjectiveInformation.Reachable)) then
      begin
         Assert(GetRootFor(Self) <> GetRootFor(Subject));
         Result := False;
         DirectionMessage := '';
         for Direction := Low(SubjectiveInformation.Directions) to High(SubjectiveInformation.Directions) do
         begin
            if (Direction in SubjectiveInformation.Directions) then
            begin
               if (DirectionMessage <> '') then
               begin
                  // it's in more than one direction, give up trying to explain where it is
                  DirectionMessage := '';
                  Break;
               end;
               DirectionMessage := ' (' + CardinalDirectionToDirectionString(Direction) + ')';
            end;
         end;
         Message := TMessage.Create(mkNotReachable, '_ _ too far away_.',
                                                    [Capitalise(Subject.GetDefiniteName(Perspective)),
                                                     IsAre(Subject.IsPlural(Perspective)),
                                                     DirectionMessage]);
         // SelfRoot := GetRootFor(Self);
         // SubjectRoot := GetRootFor(Subject);
         // '... You are in _ but _ is in _.',
         // [..., SelfRoot.GetDefiniteName(Perspective), Subject.GetDefiniteName(Perspective), SubjectRoot.GetDefiniteName(Perspective)]
      end;
   end
   else
      raise Exception.Create('CanReach() does not know how to handle objects of class ' + Subject.ClassName());
end;

function TThing.HasAbilityToTravelTo(Destination: TAtom; RequiredAbilities: TNavigationAbilities; Perspective: TAvatar; var Message: TMessage): Boolean;
begin
   Assert(Message.IsValid);
   Result := False;
   Message := TMessage.Create(mkBogus, '_ can''t travel by _.',
                             [Capitalise(GetIndefiniteName(Perspective)),
                              GetReflexivePronoun(Perspective)]);
end;

function TThing.CanPut(Thing: TThing; ThingPosition: TThingPosition; Care: TPlacementStyle; Perspective: TAvatar; var Message: TMessage): Boolean;
begin
   Assert(Message.IsValid);
   if ((ThingPosition = tpIn) and (not IsOpen())) then
   begin
      Result := False;
      Message := TMessage.Create(mkClosed, GetDescriptionNoInside(Perspective));
   end
   else
   begin
      Result := inherited;
   end;
end;

function TThing.CanMove(Perspective: TAvatar; var Message: TMessage): Boolean;
begin
   Assert(Message.IsValid);
   Result := True;
end;

function TThing.CanTake(Perspective: TAvatar; var Message: TMessage): Boolean;
begin
   Assert(Message.IsValid);
   Result := CanMove(Perspective, Message);
end;

function TThing.CanShake(Perspective: TAvatar; var Message: TMessage): Boolean;
begin
   Assert(Message.IsValid);
   Result := CanTake(Perspective, Message);
end;

function TThing.GetNavigationInstructions(Direction: TCardinalDirection; Child: TThing; Perspective: TAvatar; var Message: TMessage): TNavigationInstruction;
var
   UsefulReferent, CandidateThing: TThing;
   Candidate: TAtom;
   WeOpen: Boolean;
   PositionOverride: TThingPosition;
begin
   // We're moving Perspective in Direction, from Child.Position relative to us
   // Perspective is the traveller, Child is the ancester-or-self of Perspective that's a child of us.
   WeOpen := IsOpen();
   if ((Child.Position in tpContained) and (not WeOpen)) then
   begin
      // can't go anywhere if we're closed inside
      Result.TravelType := ttNone;
      Message := TMessage.Create(mkClosed, '_ cannot travel _ while _ _. _',
                                 [Capitalise(Perspective.GetDefiniteName(Perspective)),
                                  CardinalDirectionToString(Direction),
                                  ThingPositionToString(Child.Position),
                                  GetDefiniteName(Perspective),
                                  GetDescriptionClosed(Perspective)]);
   end
   else
   if ((Child.Position in tpContained) and (Direction in [cdOut])) then
   begin
      // trying to exit a container
      Result.TravelType := ttByPosition;
      Result.RequiredAbilities := [naWalk];
      Result.PositionTarget := FParent.GetSurface(); // XXX what if it's nil?
      Result.Position := tpOn;
   end
   else
   if ((Child.Position in tpContained) and (Direction in [cdIn])) then
   begin
      // trying to enter a container from inside
      PositionOverride := tpIn;
      Candidate := GetInside(PositionOverride);
      Assert(Assigned(Candidate));
      Result.TravelType := ttByPosition;
      Result.RequiredAbilities := [naWalk];
      Result.PositionTarget := Candidate;
      Result.Position := PositionOverride;
   end
   else
   if ((Child.Position in tpArguablyOn) and (Direction in [cdOut, cdDown])) then
   begin
      // trying to climb down
      // XXX this branch is a bit ugly, would be nice to have a cleaner way of deferring up the tree
      PositionOverride := tpAt;
      Candidate := GetDefaultDestination(PositionOverride);
      Assert(Assigned(Candidate));
      if ((Candidate <> Self) or (Perspective.Parent = Candidate) or (PositionOverride in tpContained)) then
      begin
         // if our default destination is either inside us or on some
         // descendant, or if this wouldn't actually move the
         // traveller, then defer to our parent instead.
         Result := FParent.GetNavigationInstructions(Direction, Self, Perspective, Message);
      end
      else
      begin
         Result.TravelType := ttByPosition;
         Result.RequiredAbilities := [naWalk];
         Result.PositionTarget := Candidate;
         Result.Position := PositionOverride;
      end;
   end
   else
   if (Direction in [cdIn]) then
   begin
      // trying to enter a container
      PositionOverride := tpIn;
      Candidate := GetInside(PositionOverride);
      if (Assigned(Candidate) and WeOpen) then
      begin
         Result.TravelType := ttByPosition;
         Result.RequiredAbilities := [naWalk];
         Result.PositionTarget := Candidate;
         Result.Position := PositionOverride;
      end
      else
      begin
         Message := TMessage.Create(mkClosed, GetDescriptionNoInside(Perspective));
      end;
   end
   else
   if (Child.Position in tpDeferNavigationToParent) then
   begin
      // defer to parent
      Result := FParent.GetNavigationInstructions(Direction, Self, Perspective, Message);
   end
   else
   begin
      // we cannot figure out how to travel in a direction from here
      CandidateThing := Perspective;
      repeat
         Assert(CandidateThing.Parent is TThing); // because otherwise, how did we, a TThing, end up an ancestor??
         UsefulReferent := CandidateThing;
         CandidateThing := CandidateThing.Parent as TThing;
      until (CandidateThing = Self) or (not (CandidateThing.Position in tpTransitivePositions));
      Result.TravelType := ttNone;
      Message := TMessage.Create(mkCannotMoveBecauseLocation, '_ cannot go _; _ _ _ _.',
                                 [Capitalise(Perspective.GetDefiniteName(Perspective)),
                                  CardinalDirectionToString(Direction),
                                  Perspective.GetSubjectPronoun(Perspective),
                                  TernaryConditional('is', 'are', Perspective.IsPlural(Perspective)),
                                  ThingPositionToString(UsefulReferent.Position),
                                  UsefulReferent.Parent.GetDefiniteName(Perspective)]);
   end;
end;

function TThing.IsChildTraversable(Child: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean;
begin
   Assert(Child.FParent = Self);
   Result := ((not (Child.Position in tpContained)) or (not FromOutside) or (CanSeeIn()));
end;

function TThing.IsImplicitlyReferenceable(Perspective: TAvatar; PropertyFilter: TThingFeatures): Boolean;
begin
   Result := PropertyFilter <= GetFeatures();
end;

function TThing.IsExplicitlyReferenceable(Perspective: TAvatar): Boolean;
begin
   Result := True;
end;

procedure TThing.FindMatchingThings(Perspective: TAvatar; Options: TFindMatchingThingsOptions; PositionFilter: TThingPositionFilter; PropertyFilter: TThingFeatures; List: TThingList);
begin
   Assert(Assigned(FParent));
   if ((FPosition in PositionFilter) and ((fomIncludeNonImplicits in Options) or IsImplicitlyReferenceable(Perspective, PropertyFilter))) then
      List.AppendItem(Self);
   inherited;
end;

function TThing.FindThingTraverser(Thing: TThing; Perspective: TAvatar; Options: TFindThingOptions): Boolean;
begin
   if (Thing = Self) then
      Result := True
   else
      Result := inherited;
end;

procedure TThing.EnumerateExplicitlyReferencedThings(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; FromOutside, FromFarAway: Boolean; Directions: TCardinalDirectionSet; Reporter: TThingReporter);
var
   Count: Cardinal;
   GrammaticalNumber: TGrammaticalNumber;
begin
   if (IsExplicitlyReferencedThing(Tokens, Start, Perspective, Count, GrammaticalNumber)) then
      Reporter(Self, Count, GrammaticalNumber);
   inherited;
end;

procedure TThing.Moved(OldParent: TAtom; Care: TPlacementStyle; Perspective: TAvatar);
begin
   if (Self <> Perspective) then
   begin
      // TODO: we should have special versions of this for picking up, dropping, placing, etc.
      // (currently ThingPositionToDirectionString does something weird for tpCarried, where it assumes the "it" pronoun for us)
      DoBroadcast([OldParent, FParent], Perspective, [
         C(M(@Perspective.GetDefiniteName)), MP(Perspective, M(' moves '), M(' move ')), M(@GetDefiniteName), SP(),
         M(ThingPositionToDirectionString(Position)), SP(), M(@FParent.GetDefiniteName), M('.')
      ]);
   end;
end;

procedure TThing.Shake(Perspective: TAvatar);
begin
end;

procedure TThing.Press(Perspective: TAvatar);
begin
   Perspective.AvatarMessage(TMessage.Create(mkNoOp, 'Nothing happens.'));
end;

function TThing.GetFeatures(): TThingFeatures;
begin
   Result := [];
end;

function TThing.CanDig(Target: TThing; Perspective: TAvatar; var Message: TMessage): Boolean;
begin
   Assert(Message.IsValid);
   Message := TMessage.Create(mkInappropriateTool, '_ _ not make a good digging tool.',
                                                   [Capitalise(GetDefiniteName(Perspective)),
                                                    TernaryConditional('does', 'do', IsPlural(Perspective))]);
   Result := False;
end;

function TThing.Dig(Spade: TThing; Perspective: TAvatar; var Message: TMessage): Boolean;
begin
   Assert(Message.IsValid);
   Message := TMessage.Create(mkBogus, '_ cannot dig _.',
                                       [Capitalise(Perspective.GetDefiniteName(Perspective)),
                                        GetDefiniteName(Perspective)]);
   Result := False;
end;

procedure TThing.Dug(Target: TThing; Perspective: TAvatar; var Message: TMessage);
begin
   Assert(Message.IsValid);
end;

function TThing.IsOpen(): Boolean;
begin
   Result := False;
end;

function TThing.CanSeeIn(): Boolean;
begin
   Result := IsOpen();
end;

function TThing.CanSeeOut(): Boolean;
begin
   Result := IsOpen();
end;

function TThing.Open(Perspective: TAvatar; var Message: TMessage): Boolean;
begin
   Assert(Message.IsValid);
   if (IsOpen()) then
      Message := TMessage.Create(mkRedundant, '_ is already open.', [Capitalise(GetDefiniteName(Perspective))])
   else
      Message := TMessage.Create(mkBogus, 'How to open _ is not readily apparent.', [GetDefiniteName(Perspective)]);
   Result := False;
end;

function TThing.Close(Perspective: TAvatar; var Message: TMessage): Boolean;
begin
   Assert(Message.IsValid);
   if (not IsOpen()) then
      Message := TMessage.Create(mkRedundant, '_ is not open.', [Capitalise(GetDefiniteName(Perspective))])
   else
      Message := TMessage.Create(mkBogus, 'How to close _ is not readily apparent.', [GetDefiniteName(Perspective)]);
   Result := False;
end;

{$IFDEF DEBUG}
function TThing.Debug(Perspective: TAvatar): UTF8String;
begin
   Result := inherited;
   Result := Result + #10 +
             // XXX should give GetFeatures() set
             'IsPlural: ' + TernaryConditional('singular', 'plural', IsPlural(Perspective)) + #10 +
             'Position: ' + ThingPositionToString(FPosition) + ' ' + FParent.GetName(Perspective) + #10 +
             'IsOpen: ' + TernaryConditional('closed', 'open', IsOpen()) + #10 +
             'CanSeeIn: ' + TernaryConditional('opaque', 'transparent', CanSeeIn()) + #10 +
             'CanSeeOut: ' + TernaryConditional('opaque', 'transparent', CanSeeOut()) + #10 +
             'GetIntrinsicSize(): ' + UTF8String(GetIntrinsicSize()) + #10 +
             'GetSurfaceSizeManifest(): ' + UTF8String(GetSurfaceSizeManifest());
end;
{$ENDIF}


class function TAvatar.CreateFromProperties(Properties: TTextStreamProperties): TAtom;
begin
   Properties.Fail('Cannot create avatars');
   Assert(False);
   Result := nil; // cannot be reached
end;

function TAvatar.IsImplicitlyReferenceable(Perspective: TAvatar; PropertyFilter: TThingFeatures): Boolean;
begin
   if (Perspective = Self) then
      Result := False
   else
      Result := inherited;
end;

procedure TAvatar.RemoveFromWorld();
begin
   FParent.Remove(Self);
end;

function TAvatar.Locate(Thing: TThing; Options: TFindThingOptions = [foFindAnywhere]): TSubjectiveInformation;
var
   Root: TAtom;
   FromOutside: Boolean;
begin
   Root := GetSurroundingsRoot(FromOutside);
   if (FromOutside) then
      Include(Options, foFromOutside);
   if (not Root.FindThing(Thing, Self, Options, Result)) then
   begin
      // This can happen when you do "debug location" from inside something that can't see something that wants to find the perspective, e.g. from the hole in a room with a door.
      Assert(Result.Directions = []);
      Assert(Result.Reachable = []);
   end;
end;

{$IFDEF DEBUG}
function TDummyAvatar.IsPlural(Perspective: TAvatar): Boolean;
begin
   Result := False;
end;

function TDummyAvatar.GetName(Perspective: TAvatar): UTF8String;
begin
   Result := '';
end;

function TDummyAvatar.GetDescriptionSelf(Perspective: TAvatar): UTF8String;
begin
   Result := '';
end;

function TDummyAvatar.CanSurfaceHold(const Manifest: TThingSizeManifest; const ManifestCount: Integer): Boolean;
begin
   Result := ManifestCount = 0;
end;

function TDummyAvatar.GetIntrinsicMass(): TThingMass;
begin
   Result := tmLight;
end;

function TDummyAvatar.GetIntrinsicSize(): TThingSize;
begin
   Result := tsSmall;
end;

function TDummyAvatar.IsExplicitlyReferencedThing(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; out Count: Cardinal; out GrammaticalNumber: TGrammaticalNumber): Boolean;
begin
   Result := False;
end;

procedure TDummyAvatar.DoLook();
begin
end;

procedure TDummyAvatar.AvatarMessage(Message: TMessage);
begin
end;

procedure TDummyAvatar.AnnounceAppearance();
begin
end;

procedure TDummyAvatar.AnnounceDisappearance();
begin
end;

procedure TDummyAvatar.AnnounceDeparture(Destination: TAtom; Direction: TCardinalDirection);
begin
end;

procedure TDummyAvatar.AnnounceDeparture(Destination: TAtom);
begin
end;

procedure TDummyAvatar.AnnounceArrival(Source: TAtom);
begin
end;

procedure TDummyAvatar.AnnounceArrival(Source: TAtom; Direction: TCardinalDirection);
begin
end;

procedure TDummyAvatar.AutoDisambiguated(Message: UTF8String);
begin
end;

function TDummyAvatar.HasConnectedPlayer(): Boolean;
begin
   Result := False;
end;

function TDummyAvatar.IsReadyForRemoval(): Boolean;
begin
   Result := True;
end;
{$ENDIF}

constructor TLocation.Create();
begin
   inherited;
end;

destructor TLocation.Destroy();
begin
   inherited;
end;

constructor TLocation.Read(Stream: TReadStream);
var
   Index: Cardinal;
begin
   inherited;
   SetLength(FLandmarks, Stream.ReadCardinal());
   if (Length(FLandmarks) > 0) then
      for Index := 0 to Length(FLandmarks) - 1 do
      begin
         FLandmarks[Index].Direction := TCardinalDirection(Stream.ReadCardinal());
         FLandmarks[Index].Options := TLandmarkOptions(Stream.ReadCardinal());
         Stream.ReadReference(@Pointer(FLandmarks[Index].Atom));
      end;
end;

procedure TLocation.Write(Stream: TWriteStream);
var
   Landmark: TDirectionalLandmark;
begin
   inherited;
   Stream.WriteCardinal(Length(FLandmarks));
   for Landmark in FLandmarks do
   begin
      Stream.WriteCardinal(Cardinal(Landmark.Direction));
      Stream.WriteCardinal(Cardinal(Landmark.Options));
      Stream.WriteReference(Landmark.Atom);
   end;
end;

class function TLocation.HandleLandmarkProperties(Properties: TTextStreamProperties; var Values: TStreamedLandmarks): Boolean;
var
   Direction: TCardinalDirection;
   Destination: TAtom;
   Options: TLandmarkOptions;
   Stream: TTextStream;
begin
   if (Properties.Name = pnLandmark) then
   begin
      Stream := Properties.Accept();
      Direction := Stream.specialize GetEnum<TCardinalDirection>();
      Stream.ExpectPunctuation(',');
      Destination := Stream.specialize GetObject<TAtom>();
      Stream.ExpectPunctuation(',');
      Options := Stream.specialize GetSet<TLandmarkOptions>();
      Values.AddLandmark(Direction, Destination, Options);
      Properties.Advance();
      Result := False;
   end
   else
      Result := True;
end;

class function TLocation.HandleUniqueLocationProperty(Properties: TTextStreamProperties; PossibleName: UTF8String; var Value: TLocation): Boolean;
begin
   if (Properties.Name = PossibleName) then
   begin
      Properties.EnsureNotSeen(PossibleName);
      Value := Properties.Accept().specialize GetObject<TLocation>();
      Properties.Advance();
      Result := False;
   end
   else
      Result := True;
end;

procedure TLocation.AddLandmark(Direction: TCardinalDirection; Atom: TAtom; Options: TLandmarkOptions);
begin
   Assert(Assigned(Atom));
   Assert((Atom is TLocation) or (Atom is TThing));
   Assert((Atom is TLocation) or ((Atom is TThing) and Assigned((Atom as TThing).Parent)), 'Cannot add landmark that does not have parent.');
   SetLength(FLandmarks, Length(FLandmarks)+1);
   FLandmarks[High(FLandmarks)].Direction := Direction;
   FLandmarks[High(FLandmarks)].Options := Options;
   FLandmarks[High(FLandmarks)].Atom := Atom;
end;

procedure TLocation.AddSurroundings(Atom: TAtom; const Directions: TCardinalDirectionSet = cdCompassDirection);
var
   Direction: TCardinalDirection;
begin
   for Direction in Directions do
      AddLandmark(Direction, Atom, []);
end;

function TLocation.HasLandmark(Direction: TCardinalDirection): Boolean;
var
   Landmark: TDirectionalLandmark;
begin
   for Landmark in FLandmarks do
   begin
      if (Landmark.Direction = Direction) then
      begin
         Result := True;
         Exit();
      end;
   end; 
   Result := False;
end;

function TLocation.CanFindInDirection(Direction: TCardinalDirection; Atom: TAtom): Boolean;
var
   Landmark: TDirectionalLandmark;
begin
   Assert(Assigned(Atom));
   for Landmark in FLandmarks do
   begin
      if ((Landmark.Direction = Direction) and (Landmark.Atom = Atom)) then
      begin
         Result := True;
         Exit;
      end;
   end;
   Result := False;
end;

{$IFDEF DEBUG}
procedure TLocation.AssertDirectionHasDestination(Direction: TCardinalDirection; Atom: TAtom);
var
   PositionOverride: TThingPosition;
   DisambiguationOpening: TThing;
   Message: TMessage;
   Landmark: TAtom;
   ActualDestination, DesiredDestination: TAtom;
   NotificationList: TAtomList;
   Traveller: TAvatar;
begin
   Assert(Assigned(Atom));
   NotificationList := TAtomList.Create();
   try
      Traveller := TDummyAvatar.Create();
      try
         PositionOverride := tpOn;
         DisambiguationOpening := nil;
         Message := TMessage.Create();
         Landmark := GetAtomForDirectionalNavigation(Direction);
         Assert(Assigned(Landmark));
         ActualDestination := Landmark.GetEntrance(Traveller, Direction, Traveller, PositionOverride, DisambiguationOpening, Message, NotificationList);
         Assert(Message.AsKind = mkSuccess);
         PositionOverride := tpOn;
         DisambiguationOpening := nil;
         Message := TMessage.Create();
         DesiredDestination := Atom.GetEntrance(Traveller, Direction, Traveller, PositionOverride, DisambiguationOpening, Message, NotificationList);
         Assert(Message.AsKind = mkSuccess);
         Assert(Assigned(ActualDestination));
         Assert(Assigned(DesiredDestination));
         Assert(ActualDestination = DesiredDestination);
      finally
         Traveller.Free();
      end;
   finally
      NotificationList.Free();
   end;
end;
{$ENDIF}


function TLocation.IsPlural(Perspective: TAvatar): Boolean;
begin
   Result := False; // good default, but override for things like "The Everglades"
   // (needed for things like "_The Everglades_ are out of reach." in response to "take everglades" from a nearby room)
end;

function TLocation.GetLookTowardsDirection(Perspective: TAvatar; Direction: TCardinalDirection): UTF8String;
var
   Landmark: TDirectionalLandmark;
   Index, Count: Cardinal;
begin
   Count := 0;
   for Landmark in FLandmarks do
   begin
      if (Landmark.Direction = Direction) then
      begin
         Inc(Count);
      end;
   end;      
   Index := 0;
   if (Count > 0) then
   begin
      for Landmark in FLandmarks do
      begin
         if (Landmark.Direction = Direction) then
         begin
            if (Index = 0) then
            begin       
               Result := Landmark.Atom.GetDescriptionRemoteDetailed(Perspective, Direction, 'Looking ' + CardinalDirectionToString(Direction), []);
            end
            else
            begin
               Result := Result + ' Beyond that, you can see ';
               if (Index > 1) then
               begin
                  if (Index < Count) then
                     Result := Result + ', '
                  else
                     if (Index > 2) then
                        Result := Result + ', and '
                     else
                        Result := Result + ' and ';
               end;
               Result := Result + Landmark.Atom.GetIndefiniteName(Perspective);
            end;
            Inc(Index);
         end;
      end;
      if (Count > 1) then
         Result := Result + '.';
   end
   else
      Result := GetLookTowardsDirectionDefault(Perspective, Direction);
end;

function TLocation.GetLookTowardsDirectionDefault(Perspective: TAvatar; Direction: TCardinalDirection): UTF8String;
begin
   Assert(Assigned(Perspective));
   Result := Capitalise(Perspective.GetDefiniteName(Perspective)) + ' ' + TernaryConditional('sees', 'see', Perspective.IsPlural(Perspective)) + ' nothing noteworthy when looking ' + CardinalDirectionToString(Direction) + '.';
end;

function TLocation.GetDescriptionSelf(Perspective: TAvatar): UTF8String;
begin
   Result := '';
end;

function TLocation.GetDescriptionLandmarks(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Directions: TCardinalDirectionSet = cdAllDirections; Context: TAtom = nil): UTF8String;
var
   S: UTF8String;
   Landmark: TDirectionalLandmark;
begin
   Assert(Mode in [psThereIsAThingThere, // look north
                   psThereIsAThingHere]); // look
   Result := '';
   for Landmark in FLandmarks do
   begin
      if ((Landmark.Atom <> Context) and (loAutoDescribe in Landmark.Options) and (Landmark.Direction in Directions)) then
      begin
         S := Landmark.Atom.GetDescriptionRemoteBrief(Perspective, Mode, Landmark.Direction);
         if (Length(S) > 0) then
         begin
            if (Length(Result) > 0) then
               Result := Result + ' ';
            Result := Result + S;
         end;
      end;
   end;
end;

function TLocation.GetDescriptionHere(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Directions: TCardinalDirectionSet = cdAllDirections; Context: TAtom = nil): UTF8String;

   procedure ProcessBatch(Children: TThingList);
   var
      Child: TThing;
   begin
      for Child in Children do
      begin
         if ((Child.Position in tpAutoDescribe) and
             (Child <> Perspective) and
             (Child <> Context)) then
         begin
            if (Length(Result) > 0) then
               Result := Result + ' ';
            Result := Result + Child.GetPresenceStatement(Perspective, Mode);
            Result := Result + WithSpaceIfNotEmpty(Child.GetDescriptionState(Perspective));
         end;
      end;
   end;

var
   Surface: TThing;
begin
   Result := GetDescriptionLandmarks(Perspective, Mode, Directions, Context);
   ProcessBatch(FChildren);
   Surface := GetSurface();
   if (Assigned(Surface)) then
      ProcessBatch(Surface.FChildren);
end;

function TLocation.GetDescriptionRemoteHorizon(Perspective: TAvatar; Context: TThing; Depth: Cardinal): UTF8String;
begin
   Result := GetDescriptionLandmarks(Perspective, psThereIsAThingThere, cdAllDirections, Context);
end;

function TLocation.GetDescriptionRemoteBrief(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Direction: TCardinalDirection): UTF8String;
begin
   Assert(Mode in [psThereIsAThingThere, // look north (called indirectly from TLocation.GetDescriptionRemoteDetailed() below)
                   psThereIsAThingHere]); // if we're an autodescribed thing to the north of where the player is
   Result := Capitalise(CardinalDirectionToDirectionString(Direction)) + ' ' + IsAre(IsPlural(Perspective)) + ' ' + GetDefiniteName(Perspective) + '.';
end;

function TLocation.GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection; LeadingPhrase: UTF8String; Options: TLeadingPhraseOptions): UTF8String;
begin
   Result := LeadingPhrase + ', you see:' + #10 +
             Capitalise(GetName(Perspective)) +
             WithNewlineIfNotEmpty(GetBasicDescription(Perspective, psThereIsAThingThere, cdAllDirections - [ReverseCardinalDirection(Direction)]));
end;

function TLocation.GetAtomForDirectionalNavigation(Direction: TCardinalDirection): TAtom;
var
   Landmark: TDirectionalLandmark;
begin
   for Landmark in FLandmarks do
      if ((Landmark.Direction = Direction) and (loPermissibleNavigationTarget in Landmark.Options)) then
      begin
         Result := Landmark.Atom;
         Exit;
      end;
   Result := nil;
end;

function TLocation.GetNavigationInstructions(Direction: TCardinalDirection; Child: TThing; Perspective: TAvatar; var Message: TMessage): TNavigationInstruction;
var
   Destination: TAtom;
begin
   Assert(Message.IsValid);
   Result.TravelType := ttNone;
   Destination := GetAtomForDirectionalNavigation(Direction);
   if (Assigned(Destination)) then
   begin
      // XXX We should figure out what the actual required abilities are, not just say [naWalk] for all directions!
      Result.TravelType := ttByDirection;
      Result.RequiredAbilities := [naWalk];
      Result.DirectionTarget := Destination;
      Result.Direction := Direction;
   end
   else
      FailNavigation(Direction, Perspective, Message);
end;

procedure TLocation.FailNavigation(Direction: TCardinalDirection; Perspective: TAvatar; out Message: TMessage);
begin
   Message := TMessage.Create(mkCannotMoveBecauseCustom, '_ can''t go _ from here.',
                              [Capitalise(Perspective.GetDefiniteName(Perspective)),
                               CardinalDirectionToString(Direction)]);
end;

function TLocation.GetEntrance(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: TMessage; NotificationList: TAtomList): TAtom;
begin
   Assert(Message.IsValid);
   PositionOverride := tpOn;
   Result := GetSurface();
   if (not Assigned(Result)) then
   begin
      PositionOverride := tpIn;
      Result := GetInside(PositionOverride);
      if (not Assigned(Result)) then
      begin
         PositionOverride := tpAt; // ...this might not work very well, we'll have to see.
         Result := Self;
      end;
   end;
end;

function TLocation.CanSurfaceHold(const Manifest: TThingSizeManifest; const ManifestCount: Integer): Boolean;
var
   Surface: TThing;
begin
   Surface := GetSurface();
   if (Assigned(Surface)) then
      Result := Surface.CanSurfaceHold(Manifest, ManifestCount)
   else
      Result := False;
end;

procedure TLocation.FindMatchingThings(Perspective: TAvatar; Options: TFindMatchingThingsOptions; PositionFilter: TThingPositionFilter; PropertyFilter: TThingFeatures; List: TThingList);
var
   Landmark: TDirectionalLandmark;
begin
   // We don't really care if Options includes fomFromOutside, because everything is always inside us and anyone outside us who can see us can see inside us by definition.
   inherited;
   for Landmark in FLandmarks do
      if (loThreshold in Landmark.Options) then
      begin
         Landmark.Atom.ProxiedFindMatchingThings(Perspective, Options + [fomFromOutside], PositionFilter, PropertyFilter, List);
      end;
end;

function TLocation.FindThing(Thing: TThing; Perspective: TAvatar; Options: TFindThingOptions; out SubjectiveInformation: TSubjectiveInformation): Boolean;
var
   Direction: TCardinalDirection;
   Landmark: TDirectionalLandmark;
begin
   // We don't really care if Options includes foFromOutside, because everything is always inside us and anyone outside us who can see us can see inside us by definition.
   Result := inherited;
   for Landmark in FLandmarks do
      if ((loThreshold in Landmark.Options) or ((loAutoDescribe in Landmark.Options) and (foFindAnywhere in Options))) then
      begin
         if (Landmark.Atom.ProxiedFindThingTraverser(Thing, Perspective, Options)) then
         begin
            Result := True;
            if ((foWithPreciseDirections in Options) or not (loConsiderDirectionUnimportantWhenFindingChildren in Landmark.Options)) then
               Include(SubjectiveInformation.Directions, Landmark.Direction);
            Include(SubjectiveInformation.Reachable, rpReachable);
            if (not (loPermissibleNavigationTarget in Landmark.Options)) then
               Include(SubjectiveInformation.RequiredAbilitiesToNavigate, naDebugTeleport)
            else
               Include(SubjectiveInformation.RequiredAbilitiesToNavigate, naWalk); // XXX we're just assuming that everything is walkable
            // long term we should add ability to set this to naFly, naJump, naWalk, or combinations thereof
            Exit;
         end;
      end;
   for Direction := Low(TCardinalDirection) to High(TCardinalDirection) do
   begin
      if (FindThingDirectionalTraverser(Thing, Perspective, 0, Direction, Options, SubjectiveInformation)) then
      begin
         Result := True;
         Exit;
      end;
   end;
end;

function TLocation.FindThingDirectionalTraverser(Thing: TThing; Perspective: TAvatar; Distance: Cardinal; Direction: TCardinalDirection; Options: TFindThingOptions; var SubjectiveInformation: TSubjectiveInformation): Boolean;

   procedure Internal(CurrentDirection: TCardinalDirection; MustBeVisibleFromFarAway, Reversed: Boolean);
   var
      Landmark: TDirectionalLandmark;
      CandidateAtom: TAtom;
      CandidateOptions: TLandmarkOptions;
   begin
      for Landmark in FLandmarks do
         if (Landmark.Direction = CurrentDirection) then
         begin
            CandidateOptions := Landmark.Options;
            if (((not MustBeVisibleFromFarAway) or (loVisibleFromFarAway in CandidateOptions)) and
                ((not Reversed) or (not (loNotVisibleFromBehind in CandidateOptions)))) then
            begin
               CandidateAtom := Landmark.Atom;
               if (CandidateAtom is TThing) then
               begin
                  if (CandidateAtom.FindThingTraverser(Thing, Perspective, Options)) then
                  begin
                     Result := True;
                     if ((Thing = CandidateAtom) or (foWithPreciseDirections in Options) or (not (loConsiderDirectionUnimportantWhenFindingChildren in CandidateOptions))) then
                        Include(SubjectiveInformation.Directions, Direction);
                  end;
               end
               else
               if (CandidateAtom is TLocation) then
               begin
                  if ((CandidateAtom as TLocation).FindThingDirectionalTraverser(Thing, Perspective, Distance+1, CurrentDirection, Options, SubjectiveInformation)) then // $R-
                     Result := True;
               end
               else
                  Assert(False, 'Not sure how to handle directional atom of class ' + CandidateAtom.ClassName);
               if (Result) then
               begin
                  Include(SubjectiveInformation.Reachable, rpNotReachable);
                  Exit;
               end;
            end;
         end;
   end;

begin
   Assert(Distance < High(Distance));
   Result := False;
   if (Distance > 0) then
   begin
      Internal(cdReverse[Direction], Distance > 1, True);
      if (Result) then
         Exit;
   end;
   Internal(Direction, Distance > 0, False);
end;

procedure TLocation.EnumerateExplicitlyReferencedThings(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; FromOutside, FromFarAway: Boolean; Directions: TCardinalDirectionSet; Reporter: TThingReporter);

   procedure Internal(CurrentDirection: TCardinalDirection; Reversed: Boolean);
   var
      Landmark: TDirectionalLandmark;
      CandidateAtom, Ancestor: TAtom;
      CandidateOptions: TLandmarkOptions;
      ShouldInclude: Boolean;
      ContinueDirections: TCardinalDirectionSet;
   begin
      if (Reversed) then
         ContinueDirections := []
      else
         ContinueDirections := [ CurrentDirection ];
      for Landmark in FLandmarks do
         if (Landmark.Direction = CurrentDirection) then
         begin
            CandidateOptions := Landmark.Options;
            if (((not FromFarAway) or (loVisibleFromFarAway in CandidateOptions)) and
                ((not Reversed) or (not (loNotVisibleFromBehind in CandidateOptions)))) then
            begin
               CandidateAtom := Landmark.Atom;
               Assert(Assigned(CandidateAtom));
               if ((CandidateAtom is TThing) and not FromFarAway) then
               begin
                  // The inherited version of EnumerateExplicitlyReferencedThings already lists things that are in this location,
                  // so we skip those things here.
                  Ancestor := (CandidateAtom as TThing).Parent;
                  while ((Assigned(Ancestor)) and (Ancestor is TThing)) do
                     Ancestor := (Ancestor as TThing).Parent;
                  ShouldInclude := Ancestor <> Self;
               end
               else
               begin
                  ShouldInclude := True;
               end;
               if (ShouldInclude) then
                  CandidateAtom.EnumerateExplicitlyReferencedThings(Tokens, Start, Perspective, True, True, ContinueDirections, Reporter);
            end;
         end;
   end;

var
   Direction: TCardinalDirection;
begin
   if (not FromFarAway) then
      inherited;
   for Direction in Directions do
   begin
      Internal(Direction, False);
      if (FromFarAway) then
      begin
         // This handles landmarks that are between the source and us, but that are
         // on the "incoming" side. For example, B in the following situation
         // assuming that x is looking east towards the other location. A would be
         // seen from the first room but B can only be seen if we're looking
         // "backwards" from the second location even though it should be
         // considered as visible from x. (A and B are landmarks.)
         //
         //   +---+    +---+
         //   | x A -- B   |
         //   +---+    +---+
         Internal(cdReverse[Direction], True);
      end;
   end;
end;

{$IFDEF DEBUG}
function TLocation.Debug(Perspective: TAvatar): UTF8String;

   function PresenceModeToString(const PresenceMode: TGetPresenceStatementMode): UTF8String;
   begin
      case (PresenceMode) of
         psThereIsAThingHere { look }: Result := 'psThereIsAThingHere';
         psThereIsAThingThere { look north }: Result := 'psThereIsAThingThere';
         psOnThatThingIsAThing { nested look }: Result := 'psOnThatThingIsAThing';
         psTheThingIsOnThatThing { find }: Result := 'psTheThingIsOnThatThing';
         psOnThatSpecialThing { find (something far away) -- only if parent is TThing, not TLocation }: Result := 'psOnThatSpecialThing';
      end;
   end;

var
   Direction: TCardinalDirection;
   PresenceMode: TGetPresenceStatementMode;
   Child: TThing;
   Landmark: TDirectionalLandmark;
begin
   Result :=
      'GetLook:' + WithNewlineIfMultiline(GetLook(Perspective)) + #10 +
      'GetBasicDescription:'#10;
   for PresenceMode in [psThereIsAThingThere, psThereIsAThingHere] do // GetBasicDescription only supports those two (and asserts otherwise)
      Result := Result +  '  ' + PresenceModeToString(PresenceMode) + ':' + WithNewlineIfMultiline(GetBasicDescription(Perspective, PresenceMode)) + #10;
   Result := Result +
      'GetHorizonDescription:' + WithNewlineIfMultiline(GetHorizonDescription(Perspective)) + #10 +
      'GetDescriptionRemoteHorizon:' + WithNewlineIfMultiline(GetDescriptionRemoteHorizon(Perspective, Perspective, 0)) + #10 +
      'GetDescriptionSelf:' + WithNewlineIfMultiline(GetDescriptionSelf(Perspective)) + #10 +
      'GetDescriptionState:' + WithNewlineIfMultiline(GetDescriptionState(Perspective)) + #10 +
      'GetDescriptionHere:'#10;
   for PresenceMode in [psThereIsAThingThere, psThereIsAThingHere] do // GetDescriptionHere can defer to GetBasicDescription which only supports those two
      Result := Result + '  ' + PresenceModeToString(PresenceMode) + ':' + WithNewlineIfMultiline(GetDescriptionHere(Perspective, PresenceMode)) + #10;
   Result := Result + 'GetDescriptionOn:' + #10 +
      '  []:' + WithNewlineIfMultiline(GetDescriptionOn(Perspective, [])) + #10 +
      '  [optDeepOn]:' + WithNewlineIfMultiline(GetDescriptionOn(Perspective, [optDeepOn])) + #10 +
      '  [optPrecise]:' + WithNewlineIfMultiline(GetDescriptionOn(Perspective, [optPrecise])) + #10 +
      '  [optDeepOn, optPrecise]:' + WithNewlineIfMultiline(GetDescriptionOn(Perspective, [optDeepOn, optPrecise])) + #10 +
      'GetDescriptionChildren (all options enabled):' + WithNewlineIfMultiline(GetDescriptionChildren(Perspective, [optDeepChildren, optFar, optThorough, optOmitPerspective])) + #10 +
      'GetLookTowardsDirection:'#10;
   for Direction in TCardinalDirection do
      Result := Result + '  ' + CardinalDirectionToString(Direction) + ':' + WithNewlineIfMultiline(GetLookTowardsDirection(Perspective, Direction)) + #10;
   Result := Result + 'GetDescriptionRemoteBrief:'#10;
   for Direction in TCardinalDirection do
      for PresenceMode in [psThereIsAThingThere, psThereIsAThingHere] do // GetDescriptionRemoteBrief can defer to GetBasicDescription which only supports those two
         Result := Result + '  ' + CardinalDirectionToString(Direction) + ' ' + PresenceModeToString(PresenceMode) + ':' + WithNewlineIfMultiline(GetDescriptionRemoteBrief(Perspective, PresenceMode, Direction)) + #10;
   Result := Result + 'GetDescriptionRemoteBrief:'#10;
   for Direction in TCardinalDirection do
      Result := Result + '  ' + CardinalDirectionToString(Direction) + ':' + WithNewlineIfMultiline(GetDescriptionRemoteDetailed(Perspective, Direction, 'Debugging', [])) + #10;
   Child := GetSurface();
   if (Assigned(Child)) then
      Result := Result + 'GetSurface: ' + Child.GetLongDefiniteName(Perspective) + #10;
   if (FChildren.Length = 0) then
   begin
      Result := Result + 'No children.' + #10;
   end
   else
   begin
      Result := Result + 'Children:' + #10;
      for Child in FChildren do
         Result := Result + ' - ' + Child.GetDefiniteName(nil) + #10;
   end;
   if (Length(FLandmarks) = 0) then
   begin
      Result := Result + 'No landmarks.';
   end
   else
   begin
      Result := Result + 'Landmarks:';
      for Landmark in FLandmarks do
         Result := Result + #10 + ' - ' + CardinalDirectionToString(Landmark.Direction) + ': ' + Landmark.Atom.GetDefiniteName(nil) + ' ' + specialize SetToString<TLandmarkOptions>(Landmark.Options);
   end;
end;
{$ENDIF}

procedure ConnectLocations(SourceLocation: TLocation; Direction: TCardinalDirection; Destination: TLocation; Options: TLandmarkOptions = [loPermissibleNavigationTarget]);

   procedure ConnectLocationsOneWay(A: TLocation; Direction: TCardinalDirection; B: TLocation);
   begin
      A.AddLandmark(Direction, B, Options);
      {$IFDEF DEBUG}
      A.AssertDirectionHasDestination(Direction, B);
      {$ENDIF}
   end;

begin
   ConnectLocationsOneWay(SourceLocation, Direction, Destination);
   ConnectLocationsOneWay(Destination, cdReverse[Direction], SourceLocation);
end;


initialization
   DisposalQueue := nil;
{$INCLUDE registrations/physics.inc}
   InitDisposalQueue();
finalization
   if (Assigned(DisposalQueue)) then
      DisposalQueue.Free();
end.
