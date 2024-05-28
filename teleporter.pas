{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit teleporter;

interface

uses
   things, physics, storable;

type
   TTeleporter = class(TDescribedPhysicalThing) //@RegisterStorableClass
    protected
      FDestination: TThing;
    public
      constructor Create(Destination: TThing);
      procedure Press(Perspective: TAvatar); override;
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
   end;
   TObjectTeleporter = class(TDescribedPhysicalThing) //@RegisterStorableClass
    protected
      FDestination: TThing;
    public
      constructor Create(Destination: TThing);
      procedure Press(Perspective: TAvatar); override;
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
   end;

implementation

uses grammarian, thingdim, messages;

constructor TTeleporter.Read(Stream: TReadStream);
begin
   inherited;
   Stream.ReadReference(@Pointer(FDestination));
end;

procedure TTeleporter.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteReference(FDestination);
end;

constructor TTeleporter.Create(Destination: TThing);
begin
   inherited Create('teleporter', 'teleporter/teleporters', 'Push to teleport', tmLight, tsSmall);
   FDestination := Destination;
end;
procedure TTeleporter.Press(Perspective: TAvatar);
begin
   Perspective.AvatarMessage(TMessage.Create(mkSuccess, 'Poof! You get teleported.'));
   Perspective.AnnounceDisappearance();
   FDestination.Add(Perspective, tpOn);
   Perspective.AnnounceAppearance();
   Perspective.DoLook();
end;
constructor TObjectTeleporter.Read(Stream: TReadStream);
begin
   inherited;
   Stream.ReadReference(@Pointer(FDestination));
end;

procedure TObjectTeleporter.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteReference(FDestination);
end;

constructor TObjectTeleporter.Create(Destination: TThing);
begin
   inherited Create('object teleporter', 'object? teleporter/teleporters', 'Put it on the object and push it', tmLight, tsSmall);
   FDestination := Destination;
end;
procedure TObjectTeleporter.Press(Perspective: TAvatar);
begin
   if(not (FParent is TThing)) then
      Perspective.AvatarMessage(TMessage.Create(mkSuccess, 'You can only teleport things, not ' + FParent.GetDefiniteName(Perspective)))
   else
   begin
      Perspective.AvatarMessage(TMessage.Create(mkSuccess, 'Poof! ' + FParent.GetDefiniteName(Perspective) + ' gets teleported.'));
      FDestination.Parent.Add(FParent as TThing, FDestination.Position);
   end
end;

initialization
{$INCLUDE registrations/teleporter.inc}
end.