{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
unit thingseeker;

interface

uses
   world, grammarian;

type
   TAllImpliedScope = set of (aisSurroundings, aisSelf);
   TClauseKind = (ckStart,
                  ckComma, ckCommaAnd, ckAnd{,
                  ckThatIs, ckThatIsNot, ckThatAre, ckThatAreNot,
                  ckBut,
                  ckFrom, ckIn});
   TClauseKinds = set of TClauseKind;

   TThingCollector = class
    protected
      FTokenCount: Cardinal;
      FMultiplesSuggested, FDisambiguate: Boolean;
      FThingList: PThingItem;
      {$IFOPT C+} FCurrentlyCollecting, FCollected: Boolean; {$ENDIF}
      FCurrentPreferredGrammaticalNumber: TGrammaticalNumber;
      FCurrentBestLength: Cardinal;
      FCurrentBestGrammaticalNumber: TGrammaticalNumber;
      FCurrentBestThingList: PThingItem;
      procedure ReferencedCallback(Thing: TThing; Count: Cardinal; GrammaticalNumber: TGrammaticalNumber);
    public
      constructor Create();
      destructor Destroy(); override;
      function Collect(Perspective: TAvatar; Tokens, OriginalTokens: TTokens; Start: Cardinal; PreferredGrammaticalNumber: TGrammaticalNumber; Scope: TAllImpliedScope; Ends: TClauseKinds): Boolean;
      function GetTokenCount(): Cardinal;
      function GetMultiplesSuggested(): Boolean;
      function GetDisambiguate(): Boolean;
      function GetThingList(): PThingItem; { must be called exactly once after Collect() returns true }
   end;

function ThingListToString(Things: PThingItem; Perspective: TAvatar; Conjunction: AnsiString): AnsiString;

implementation

//{$DEFINE DEBUG_SEEKER}

uses
   sysutils;

constructor TThingCollector.Create();
begin
   inherited;
end;

destructor TThingCollector.Destroy();
begin
   {$IFOPT C+} Assert(not FCurrentlyCollecting); {$ENDIF}
   {$IFOPT C+} Assert(not FCollected); {$ENDIF}
   Assert(FTokenCount = 0);
   Assert(not Assigned(FThingList));
   Assert(not Assigned(FCurrentBestThingList));
   inherited;
end;

procedure TThingCollector.ReferencedCallback(Thing: TThing; Count: Cardinal; GrammaticalNumber: TGrammaticalNumber);
var
   ThingItem: PThingItem;
begin
{$IFDEF DEBUG_SEEKER} Writeln(' - ReferencedCallback(', Thing.GetDefiniteName(nil), ', ', Count, ', ', GrammaticalNumberToString(GrammaticalNumber), ')'); {$ENDIF}
   if (Count < FCurrentBestLength) then
      Exit;
   if (Count > FCurrentBestLength) then
   begin
{$IFDEF DEBUG_SEEKER} Writeln('     new record length!'); {$ENDIF}
      FreeThingList(FCurrentBestThingList);
      FCurrentBestThingList := nil;
      FCurrentBestLength := Count;
      FCurrentBestGrammaticalNumber := [];
   end;
{$IFDEF DEBUG_SEEKER} Writeln('     FCurrentBestGrammaticalNumber=', GrammaticalNumberToString(FCurrentBestGrammaticalNumber)); {$ENDIF}
{$IFDEF DEBUG_SEEKER} Writeln('     GrammaticalNumber=', GrammaticalNumberToString(GrammaticalNumber)); {$ENDIF}
   if (FCurrentBestGrammaticalNumber <> GrammaticalNumber) then
   begin
{$IFDEF DEBUG_SEEKER} Writeln('     examining grammatical number'); {$ENDIF}
      if (FCurrentBestGrammaticalNumber = []) then
      begin
{$IFDEF DEBUG_SEEKER} Writeln('     assuming new one is better'); {$ENDIF}
         FCurrentBestGrammaticalNumber := GrammaticalNumber;
         Assert(not Assigned(FCurrentBestThingList));
      end
      else
      if (GrammaticalNumber >< FCurrentPreferredGrammaticalNumber = []) then
      begin
{$IFDEF DEBUG_SEEKER} Writeln('     new one is useless, we have better already'); {$ENDIF}
         { Either we're looking for plural and this is singular, or vice versa. Either way, not a match. }
         Exit;
      end
      else
      if (GrammaticalNumber <> gnAmbiguous) then
      begin
{$IFDEF DEBUG_SEEKER} Writeln('     new record grammatical number!'); {$ENDIF}
         Assert(FCurrentBestGrammaticalNumber >< gnAmbiguous = FCurrentPreferredGrammaticalNumber);
         Assert(GrammaticalNumber = FCurrentPreferredGrammaticalNumber);
         FreeThingList(FCurrentBestThingList);
         FCurrentBestThingList := nil;
         FCurrentBestLength := Count;
         FCurrentBestGrammaticalNumber := FCurrentPreferredGrammaticalNumber;
      end;
   end;
   New(ThingItem);
   ThingItem^.Value := Thing;
   ThingItem^.Next := FCurrentBestThingList;
   FCurrentBestThingList := ThingItem;
{$IFDEF DEBUG_SEEKER} Writeln('     current grammatical number: ', GrammaticalNumberToString(FCurrentBestGrammaticalNumber)); {$ENDIF}
end;

type
   TThingSelectionMechanism = (tsmPickAll, tsmPickOne, tsmPickNumber, tsmPickSome, tsmPickOnlyNumber);
   TClauseMode = (cmSpecific, cmAll, cmAllInSingularContext, cmSpecificNoToken); { cmAllInSingularContext is for cases like "look at all" - "can't look at multiple things" }

const
   cmAlls = [cmAll, cmAllInSingularContext];

type
   TAbstractClause = class
     private
      FMode: TClauseMode;
      FNumber: Cardinal;
      FSelectionMechanism: TThingSelectionMechanism;
      FMultiplesSuggested: Boolean;
      FThings: PThingItem;
      FInputFragment: AnsiString;
      FNext: TAbstractClause;
      FPrevious: TAbstractClause;
      function GetInputFragment(Perspective: TAvatar; SiblingCount: Cardinal): AnsiString;
     public
      constructor Create(Number: Cardinal; SelectionMechanism: TThingSelectionMechanism; MultiplesSuggested: Boolean; Things: PThingItem; InputFragment: AnsiString); virtual;
      constructor CreateAll(Things: PThingItem); virtual;
      constructor CreateNoMatchAll(); virtual;
      constructor CreateNoTokens(); virtual;
      destructor Destroy(); override;
      procedure CheckContext(); virtual; abstract;
      function Select(): Boolean; { returns true if the result should be explicitly disambiguated }
      procedure Bank(var ChainEnd: PPThingItem; var MultiplesSuggested: Boolean); virtual;
      procedure Add(Next: TAbstractClause);
   end;
   TAbstractClauseClass = class of TAbstractClause;

type
   EMatcherException = class
      FClause: TAbstractClause;
      constructor Create(Clause: TAbstractClause);
      function Message(Perspective: TAvatar; Input: AnsiString): AnsiString; virtual; abstract;
   end;
   ESelectNotEnough = class(EMatcherException)
      FCount: Cardinal;
      constructor Create(Clause: TAbstractClause; Count: Cardinal);
      function Message(Perspective: TAvatar; Input: AnsiString): AnsiString; override;
   end;
   ESelectTooMany = class(EMatcherException)
      FWanted, FGot: Cardinal;
      constructor Create(Clause: TAbstractClause; Wanted, Got: Cardinal);
      function Message(Perspective: TAvatar; Input: AnsiString): AnsiString; override;
   end;

constructor EMatcherException.Create(Clause: TAbstractClause);
begin
   inherited Create();
   FClause := Clause;
end;

constructor ESelectNotEnough.Create(Clause: TAbstractClause; Count: Cardinal);
begin
   inherited Create(Clause);
   FCount := Count;
end;

function ESelectNotEnough.Message(Perspective: TAvatar; Input: AnsiString): AnsiString;
begin
   Result := 'About "' + Input + '"... I can only find ' + NumberToEnglish(FCount) + ': ' + ThingListToString(FClause.FThings, Perspective, 'and') + '.';
end;

constructor ESelectTooMany.Create(Clause: TAbstractClause; Wanted, Got: Cardinal);
begin
   inherited Create(Clause);
   FWanted := Wanted;
   FGot := Got;
end;

function ESelectTooMany.Message(Perspective: TAvatar; Input: AnsiString): AnsiString;
begin
   if (FWanted = 1) then
      Result := 'Which "' + Input + '" do you mean, ' + ThingListToString(FClause.FThings, Perspective, 'or') + '?'
   else
      Result := 'About "' + Input + '"... I count ' + NumberToEnglish(FGot) + ', not ' + NumberToEnglish(FWanted) + '.';
end;

constructor TAbstractClause.Create(Number: Cardinal; SelectionMechanism: TThingSelectionMechanism; MultiplesSuggested: Boolean; Things: PThingItem; InputFragment: AnsiString);
begin
   inherited Create();
   FMode := cmSpecific;
   FNumber := Number;
   FSelectionMechanism := SelectionMechanism;
   FMultiplesSuggested := MultiplesSuggested;
   FThings := Things;
   FInputFragment := InputFragment;
{$IFDEF DEBUG_SEEKER} Writeln(' - TAbstractClause.Create(', Number, ', ..., ', MultiplesSuggested, ', ', ThingListToString(Things, nil, 'and'), ', ', InputFragment, ')'); {$ENDIF}
end;

constructor TAbstractClause.CreateAll(Things: PThingItem);
begin
   inherited Create();
   if (Assigned(Things)) then
      FMode := cmAll
   else
      FMode := cmAllInSingularContext;
   FNumber := 0;
   FSelectionMechanism := tsmPickAll;
   FMultiplesSuggested := True;
   FThings := Things;
{$IFDEF DEBUG_SEEKER} Writeln(' - TAbstractClause.CreateAll(', ThingListToString(Things, nil, 'and'), ')'); {$ENDIF}
end;

constructor TAbstractClause.CreateNoMatchAll();
begin
   inherited Create();
   FMode := cmAll;
   FNumber := 0;
   FSelectionMechanism := tsmPickAll;
   FMultiplesSuggested := True;
   FThings := nil;
{$IFDEF DEBUG_SEEKER} Writeln(' - TAbstractClause.CreateNoMatchAll()'); {$ENDIF}
end;

constructor TAbstractClause.CreateNoTokens();
begin
   inherited Create();
   FMode := cmSpecificNoToken;
{$IFDEF DEBUG_SEEKER} Writeln(' - TAbstractClause.CreateNoTokens()'); {$ENDIF}
end;

destructor TAbstractClause.Destroy();
begin
   FreeThingList(FThings);
   FNext.Free();
   inherited;
end;

function TAbstractClause.GetInputFragment(Perspective: TAvatar; SiblingCount: Cardinal): AnsiString;
begin
   if ((Assigned(FThings)) and (not Assigned(FThings^.Next))) then
      Result := FThings^.Value.GetLongDefiniteName(Perspective)
   else
   if (Length(FInputFragment) > 0) then
      Result := FInputFragment
   else
   if (FMode in cmAlls) then
      Result := 'everything'
   else
      raise EAssertionFailed.Create('tried to describe nothingness in TAbstractClause.GetInputFragment()');
   if (SiblingCount > 0) then
   begin
      Assert(Assigned(FNext));
      Result := Result + ' ' + FNext.GetInputFragment(Perspective, SiblingCount-1);
   end;
end;

function TAbstractClause.Select(): Boolean;
var
   Count: Cardinal;
   Handle: PThingItem;

   procedure SelectN(N: Cardinal);
   begin
      Assert(N <= Count);
      Handle := FThings;
      while (N > 1) do
      begin
         Assert(Assigned(Handle));
         Handle := Handle^.Next;
      end;
      Assert(Assigned(Handle));
      if (Assigned(Handle^.Next)) then
      begin
         FreeThingList(Handle^.Next);
         Handle^.Next := nil;
      end;
   end;

begin
   Count := 0;
   Handle := FThings;
   while (Assigned(Handle)) do
   begin
      Handle := Handle^.Next;
      Inc(Count);
   end;
   if (Count < FNumber) then
   begin
      Assert((Count > 0) or (FSelectionMechanism = tsmPickAll));
      if (Count > 0) then
         raise ESelectNotEnough.Create(Self, Count);
   end;
   case FSelectionMechanism of
    tsmPickAll: Result := False;
    tsmPickOne:
       begin
          SelectN(1);
          Result := Count > 1;
       end;
    tsmPickNumber:
       begin
          SelectN(FNumber);
          Result := False;
       end;
    tsmPickSome:
       begin
          Assert(FNumber >= 2);
          if (Count <= 2) then
          begin
             SelectN(Count);
             Result := False;
          end
          else
          begin
             if (Count > 7) then
                SelectN(7)
             else
                SelectN(Count div 2);
             Result := True;
          end;
       end;
    tsmPickOnlyNumber:       
       begin
          if (Count <> FNumber) then
          begin
             Assert(Count > FNumber);
             raise ESelectTooMany.Create(Self, FNumber, Count);
          end;
          Result := False;
       end;
    else
       raise EAssertionFailed.Create('unknown thing selection mechanism');
   end;
end;

procedure TAbstractClause.Bank(var ChainEnd: PPThingItem; var MultiplesSuggested: Boolean);
begin
   Assert(Assigned(ChainEnd));
   Assert(not Assigned(ChainEnd^));
   MultiplesSuggested := MultiplesSuggested or FMultiplesSuggested or (Assigned(FThings) and Assigned(FThings^.Next));
   ChainEnd^ := FThings;
   if (Assigned(FThings)) then
   begin
      while (Assigned(ChainEnd^)) do
         ChainEnd := @(ChainEnd^^.Next);
      FThings := nil;
   end;
end;

procedure TAbstractClause.Add(Next: TAbstractClause);
begin
   Assert(Assigned(Next));
   Assert(not Assigned(FNext));
   Assert(not Assigned(Next.FPrevious));
   FNext := Next;
   Next.FPrevious := Self;
end;


type
   TStartClause = class(TAbstractClause)
      procedure CheckContext(); override;
   end;

procedure TStartClause.CheckContext();
begin
   Assert(not Assigned(FPrevious));
end;


type
   TAbstractJoiningClause = class(TAbstractClause)
      procedure CheckContext(); override;
      procedure Bank(var ChainEnd: PPThingItem; var MultiplesSuggested: Boolean); override;
   end;
   TCommaClause = class(TAbstractJoiningClause) end;
   TAndClause = class(TAbstractJoiningClause) end;

procedure TAbstractJoiningClause.CheckContext();
begin
   Assert(Assigned(FPrevious));
end;

procedure TAbstractJoiningClause.Bank(var ChainEnd: PPThingItem; var MultiplesSuggested: Boolean);
begin
   MultiplesSuggested := True;
   inherited;
end;

const
   ClauseKindToClass: array[TClauseKind] of TAbstractClauseClass = (
      TStartClause,
      TCommaClause,
      TAndClause,
      TAndClause{,
      TThatIsClause,
      TThatIsNotClause,
      TThatAreClause,
      TThatAreNotClause,
      TButClause,
      TFromClause,
      TInClause}
   );

function TThingCollector.Collect(Perspective: TAvatar; Tokens, OriginalTokens: TTokens; Start: Cardinal; PreferredGrammaticalNumber: TGrammaticalNumber; Scope: TAllImpliedScope; Ends: TClauseKinds): Boolean;

   procedure Deduplicate(var Things: PThingItem);
   var
      CurrentThing, CompareThing, PreviousThing: PThingItem;
   begin
      { if this ends up being a bottleneck, it can probably be sped up using a hash table instead of rewalking the list each time }
      if (Assigned(Things)) then
      begin
         PreviousThing := Things;
         CurrentThing := Things^.Next;
         while (Assigned(CurrentThing)) do
         begin
            CompareThing := Things;
            while (CompareThing <> CurrentThing) do
            begin
               if (CompareThing^.Value = CurrentThing^.Value) then
               begin
                  PreviousThing^.Next := CurrentThing^.Next;
                  Dispose(CurrentThing);
                  CurrentThing := PreviousThing;
                  Break;
               end;
               CompareThing := CompareThing^.Next;
            end;
            PreviousThing := CurrentThing;
            CurrentThing := CurrentThing^.Next;
         end;
      end;
   end;

   procedure Collapse(FirstClause: TAbstractClause; out Things: PThingItem; out MultiplesSuggested: Boolean; out Disambiguate: Boolean);
   var
      CurrentClause: TAbstractClause;
      ChainEnd: PPThingItem;
   begin
      Assert(FirstClause is TStartClause);
      CurrentClause := FirstClause;
      Things := nil;
      MultiplesSuggested := False;
      ChainEnd := @Things;
      Disambiguate := False;
      { this is going to get far more complicated when we have restricter support }
      try
{$IFDEF DEBUG_SEEKER} Writeln(' + multiples now ', MultiplesSuggested); {$ENDIF}
         repeat
{$IFDEF DEBUG_SEEKER} Writeln(' + collapsing "', CurrentClause.FInputFragment, '" (', CurrentClause.GetInputFragment(Perspective, 0), ')'); {$ENDIF}
            CurrentClause.CheckContext();
            try
               Disambiguate := Disambiguate or CurrentClause.Select();
            except
               on E: EMatcherException do
               begin
                  Fail(E.Message(Perspective, CurrentClause.FInputFragment));
               end;
            end;
            CurrentClause.Bank(ChainEnd, MultiplesSuggested);
{$IFDEF DEBUG_SEEKER} Writeln(' + multiples now ', MultiplesSuggested); {$ENDIF}
            CurrentClause := FirstClause.FNext;
         until not Assigned(CurrentClause);
      except
         FreeThingList(Things);
         raise;
      end;
      Deduplicate(Things);
   end;

var
   CurrentToken: Cardinal;

   function CollectArticleAndThings(ClauseKind: TClauseKind; ClauseLength: Cardinal; out Clause: TAbstractClause): Boolean;

      function CollectExplicitThings(var GrammaticalNumber: TGrammaticalNumber): Boolean;
      var
         FromOutside: Boolean;
      begin
{$IFDEF DEBUG_SEEKER} Writeln(' - CollectExplicitThings(', GrammaticalNumberToString(GrammaticalNumber), ')'); {$ENDIF}
         Assert(not Assigned(FCurrentBestThingList));
         Assert(CurrentToken < Length(Tokens));
         FCurrentPreferredGrammaticalNumber := GrammaticalNumber;
         FCurrentBestLength := 0;
         FCurrentBestGrammaticalNumber := [];
         FCurrentBestThingList := nil;
         try
            Perspective.GetSurroundingsRoot(FromOutside).AddExplicitlyReferencedThings(Tokens, CurrentToken, Perspective, FromOutside, @ReferencedCallback);
         except
            FreeThingList(FCurrentBestThingList);
            raise;
         end;
         Result := Assigned(FCurrentBestThingList);
         if (Result) then
            Inc(CurrentToken, FCurrentBestLength);
{$IFDEF DEBUG_SEEKER} Writeln(' - CollectExplicitThings returned ', Result, ': ', ThingListToString(FCurrentBestThingList, Perspective, 'and'), ' (', GrammaticalNumberToString(FCurrentBestGrammaticalNumber), ')'); {$ENDIF}
      end;

      function CollectImplicitThings(): Boolean;
      var
         FromOutside: Boolean;
      begin
         Assert(not Assigned(FCurrentBestThingList));
         if (aisSurroundings in Scope) then
         begin
            FCurrentBestThingList := nil;
            Perspective.GetSurroundingsRoot(FromOutside).AddImplicitlyReferencedThings(Perspective, FromOutside, aisSelf in Scope, tpExplicit, [], FCurrentBestThingList);
            Result := Assigned(FCurrentBestThingList);
         end
         else
         if (aisSelf in Scope) then
         begin
            FCurrentBestThingList := nil;
            Perspective.                                 AddImplicitlyReferencedThings(Perspective, True,        True,             tpExplicit, [], FCurrentBestThingList);
            Result := Assigned(FCurrentBestThingList);
         end
         else
         begin
            FCurrentBestThingList := nil;
            Result := True;
         end;
      end;

   var
      Start: Cardinal;

      function GetMatchedTokens(): AnsiString;
      begin
         Result := Serialise(OriginalTokens, Start, CurrentToken - Start);
      end;

   var
      GrammaticalNumber: TGrammaticalNumber;
      ClauseClass: TAbstractClauseClass;
   begin
      Assert(not Assigned(FCurrentBestThingList));
      ClauseClass := ClauseKindToClass[ClauseKind];
      Assert(Assigned(ClauseClass));
      Clause := nil;
      Inc(CurrentToken, ClauseLength);
      if (CurrentToken < Length(Tokens)) then
      begin
         Start := CurrentToken;

{
   object lists:
    - "all <plural*>"             - there must be one or more, pick them all, allow exceptions
    - "all the <plural*>"         - there must be one or more, pick them all, allow exceptions; had definite article
    - "all of the <plural*>"      - there must be one or more, pick them all, allow exceptions; had definite article
    - "all"                       - pick everything referencable in scope, allow exceptions
    - "everything"                - pick everything referencable in scope, allow exceptions
    - "every <singular*>"         - there must be one or more, pick them all, allow exceptions
    - "any <plural>"              - there must be zero or more, pick them all, allow exceptions
    - "any of the <plural*>"      - there must be one or more, pick one "randomly", allow exceptions; had definite article
    - "any <singular>"            - there must be one or more, pick one "randomly", allow exceptions

    - "a <singular*>"             - there must be one or more, pick one "randomly"
    - "an <singular*>"            - there must be one or more, pick one "randomly"
    - "one of the <plural*>"      - there must be one or more, pick one "randomly"; had definite article
    - "one <singular*>"           - there must be one or more, pick one "randomly"
    - "<number> <plural*>"        - there must be <number> or more, pick <number> of them "randomly"
    - "some <plural>"             - there must be two or more, pick a non-integer fraction of them "randomly"    \__
    - "some <singular>"           - there must be two or more, pick one "randomly"                               /  |
    - "the one <singular*>"       - there must be one, pick number; had definite article                            |
    - "the <number> <plural*>"    - there must be <number>, pick number; had definite article                       |
    - "the <plural>"              - there must be one or more, pick them all; had definite article               \__|
    - "the <singular>"            - there must be one, pick number; had definite article                         /  |
    - "<plural>"                  - there must be one or more, pick them all                                     \__|
    - "<singular>"                - there must be one, pick number                                               /  `----> prefer based on verb
    + "one" / "ones"
}

         if (Tokens[CurrentToken] = 'all') then
         begin
            Inc(CurrentToken);
            if (CollectImplicitThings()) then
            begin
               Clause := ClauseClass.CreateAll(FCurrentBestThingList);
               FCurrentBestThingList := nil;
            end
            else
            begin
               Clause := ClauseClass.CreateNoMatchAll();
            end;
         end
         else
         begin
            GrammaticalNumber := PreferredGrammaticalNumber;
            if (CollectExplicitThings(GrammaticalNumber)) then
            begin
               Assert(CurrentToken > Start);
               if (gnPlural in FCurrentBestGrammaticalNumber) then { take apples }
               begin
                  Clause := ClauseClass.Create(1, tsmPickAll, True, FCurrentBestThingList, GetMatchedTokens())
               end
               else { take apple }
               begin
                  Assert(gnSingular in FCurrentBestGrammaticalNumber);
                  Clause := ClauseClass.Create(1, tsmPickOnlyNumber, False, FCurrentBestThingList, GetMatchedTokens());
               end;
               FCurrentBestThingList := nil;
            end;
         end;
      end
      else
      if (ClauseLength > 0) then
      begin
         Clause := ClauseClass.CreateNoTokens();
         { so that the collapser can generate "from what?"-like messages }
      end;
      Result := Assigned(Clause);
      if (not Result) then
         Dec(CurrentToken, ClauseLength);
      FCurrentBestThingList := nil;
{$IFDEF DEBUG_SEEKER} Writeln(' = CollectArticleAndThings() returned ', Result); {$ENDIF}
   end;

   function TryClause(ClauseKind: TClauseKind; CandidateTokens: array of AnsiString; out NextClauseKind: TClauseKind; out NextClauseLength: Cardinal): Boolean;
   var
      Index: Cardinal;
   begin
      Assert(ClauseKind <> ckStart);
      Assert(Length(CandidateTokens) > 0);
      if (not (ClauseKind in Ends)) then
      begin
         if (CurrentToken + Length(CandidateTokens) <= Length(Tokens)) then
         begin
            for Index := 0 to Length(CandidateTokens) - 1 do
            begin
               if (Tokens[CurrentToken + Index] <> CandidateTokens[Index]) then
               begin
                  Result := False;
                  Exit;
               end;
            end;
         end;
         NextClauseKind := ClauseKind;
         NextClauseLength := Length(CandidateTokens);
         Result := True;
      end
      else
      begin
         Result := False;
      end;
   end;

var
   NextClauseKind: TClauseKind;
   NextClauseLength: Cardinal;
   FirstClause, CurrentClause, NextClause: TAbstractClause;
begin
{$IFDEF DEBUG_SEEKER} Writeln('collecting for: "' + Serialise(OriginalTokens, Start, 1) + '" of "' + Serialise(OriginalTokens, 0, Length(OriginalTokens))); {$ENDIF}
   {$IFOPT C+}
   Assert(not FCurrentlyCollecting);
   Assert(not FCollected);
   FCurrentlyCollecting := True;
   Result := False;
   try
   {$ENDIF}
      CurrentToken := Start;
      if (CollectArticleAndThings(ckStart, 0, FirstClause)) then
      begin
         try
            Assert(Assigned(FirstClause));
            CurrentClause := FirstClause;
            while ((CurrentToken < Length(Tokens)) and
                   ({TryClause(ckFrom, ['from'], NextClauseKind, NextClauseLength) or
                    TryClause(ckIn, ['in'], NextClauseKind, NextClauseLength) or }
                    TryClause(ckCommaAnd, [',', 'and'], NextClauseKind, NextClauseLength) or
                    TryClause(ckComma, [','], NextClauseKind, NextClauseLength) or
                    TryClause(ckAnd, ['and'], NextClauseKind, NextClauseLength)) and
                   (CollectArticleAndThings(NextClauseKind, NextClauseLength, NextClause))) do
            begin
               Assert(Assigned(NextClause));
               CurrentClause.Add(NextClause);
               CurrentClause := NextClause;
            end;
            Assert(FTokenCount = 0);
            Assert(not FMultiplesSuggested);
            Assert(not FDisambiguate);
            Assert(not Assigned(FThingList));
            Assert(Assigned(FirstClause));
            try
               Collapse(FirstClause, FThingList, FMultiplesSuggested, FDisambiguate);
               FTokenCount := CurrentToken - Start;
{$IFDEF DEBUG_SEEKER}
if (FTokenCount > 0) then
 Writeln('  collapsed "' + Serialise(OriginalTokens, Start, FTokenCount) + '" to "' + ThingListToString(FThingList, Perspective, 'and') + '"; multiples=', FMultiplesSuggested)
else
 Writeln('  collapsed nothing to "' + ThingListToString(FThingList, Perspective, 'and') + '"; multiples=', FMultiplesSuggested);
{$ENDIF}
            except
               FTokenCount := 0;
               FMultiplesSuggested := False;
               FDisambiguate := False;
               FreeThingList(FThingList);
               raise;
            end;
            Result := True;
         finally
            FirstClause.Free();
         end;
      end
      else
      begin
         Result := False;
      end;
   {$IFOPT C+}
   finally
      FCurrentlyCollecting := False;
      FCollected := Result;
      if (not FCollected) then
      begin
         Assert(FTokenCount = 0);
         Assert(not FMultiplesSuggested);
         Assert(not FDisambiguate);
         Assert(not Assigned(FThingList));
      end;
   end;
   {$ENDIF}
end;

function TThingCollector.GetTokenCount(): Cardinal;
begin
   {$IFOPT C+} Assert(not FCurrentlyCollecting); {$ENDIF}
   {$IFOPT C+} Assert(FCollected); {$ENDIF}
   Result := FTokenCount;
end;

function TThingCollector.GetMultiplesSuggested(): Boolean;
begin
   {$IFOPT C+} Assert(not FCurrentlyCollecting); {$ENDIF}
   {$IFOPT C+} Assert(FCollected); {$ENDIF}
   Result := FMultiplesSuggested;
end;

function TThingCollector.GetDisambiguate(): Boolean;
begin
   {$IFOPT C+} Assert(not FCurrentlyCollecting); {$ENDIF}
   {$IFOPT C+} Assert(FCollected); {$ENDIF}
   Result := FDisambiguate;
end;

function TThingCollector.GetThingList(): PThingItem;
begin
   {$IFOPT C+} Assert(not FCurrentlyCollecting); {$ENDIF}
   {$IFOPT C+} Assert(FCollected); {$ENDIF}
   Result := FThingList;
   FTokenCount := 0;
   FMultiplesSuggested := False;
   FDisambiguate := False;
   FThingList := nil;   
   {$IFOPT C+} FCollected := False; {$ENDIF}
end;

function ThingListToString(Things: PThingItem; Perspective: TAvatar; Conjunction: AnsiString): AnsiString;
begin
   Result := '';
   while (Assigned(Things)) do
   begin
      if (Length(Result) > 0) then
      begin
         if (Assigned(Things^.Next)) then
            Result := Result + ', '
         else
            Result := Result + ', ' + Conjunction + ' ';
      end;
      Result := Result + Things^.Value.GetLongDefiniteName(Perspective);
      Things := Things^.Next;
   end;
end;

end.
