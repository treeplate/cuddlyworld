{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit testmechanics8;

interface

//{$DEFINE PLAY_IN_TEST_STAIRLAND}

procedure TestMechanics8();

implementation

uses
   sysutils, storable, matcher, physics, player, locations, things, threshold, stairs, thingdim, grammarian, world, testmechanics;

procedure TestMechanics8();

   function InitTest: TWorld;
   var
      World: TTestWorld;
      Tunnel, TunnelEnd, Bedroom, Cave: TLocation;
      Thing, Bed, Pillow, Stars, Ceiling: TThing;
      DoorFrame: TDoorWay;
   begin
      World := TTestWorld.Create();
      
      Tunnel := TGroundLocation.Create('Tunnel Trail', 'the tunnel trail', 'a tunnel trail', 'The tunnel has many turns.', CreateEarthSurface());
      Tunnel.Add(TLocationRepresentative.Create('many tunnel turns', 'many? tunnel? (turn/turns twist/twists)@', 'The tunnel twists in many directions, leading to a cave when going generally westward, and leading to the end of the tunnel in a northern direction.'), tpPartOfImplicit);

      TunnelEnd := TGroundLocation.Create('Tunnel End', 'the tunnel end', 'a tunnel end', 'The tunnel end room has white walls.', CreateEarthSurface());
      TunnelEnd.Add(TStructure.Create('north wall', '(white (north northern)@)* wall/walls', 'The northern wall of the tunnel end room is white.', 'There does not seem to be any way to attach things to the wall.'), tpPartOfImplicit);
      TunnelEnd.Add(TStructure.Create('east wall', '(white (east eastern)@)* wall/walls', 'The eastern wall of the tunnel end room is white.', 'There does not seem to be any way to attach things to the wall.'), tpPartOfImplicit);
      TunnelEnd.Add(TStructure.Create('west wall', '(white (west western)@)* wall/walls', 'The western wall of the tunnel end room is white.', 'There does not seem to be any way to attach things to the wall.'), tpPartOfImplicit);

      Bedroom := TGroundLocation.Create('Bedroom', 'the bedroom', 'a bedroom', 'The bedroom is a large room. On the ceiling are some stars.', CreateStoneSurface());
      Bed := TDescribedPhysicalThing.Create('bed', 'bed/beds', 'The bed is a medium-sized bed.', tmPonderous, tsMassive);
      Pillow := TDescribedPhysicalThing.Create('pillow', '((car? pillow/pillows) car/cars)@', 'The pillow has drawings of cars on it.', tmLight, tsSmall);
      Stars := TFeature.Create('stars', 'pretty? ceiling? star/stars', 'The ceiling has stars on it.');
      Ceiling := TStructure.Create('ceiling', 'pretty? starry? ceiling/ceilings', 'The ceiling has some pretty stars on it.', 'Putting things on a ceiling seems like an exercise in futility.');
      Bedroom.Add(Ceiling, tpPartOfImplicit);
      Ceiling.Add(Stars, tpPartOfImplicit);

      Bedroom.GetSurface().Add(Bed, tpOn);
      Bed.GetSurface().Add(Pillow, tpOn);
      Bedroom.GetSurface().Add(TDescribedPhysicalThing.Create('eat block', '((blue eat word block/blocks)& word/words)@', 'This block is blue and says "eat".', tmLight, tsSmall), tpOn);
      Bedroom.GetSurface().Add(TDescribedPhysicalThing.Create('it block', '((yellow it word block/blocks)& word/words)@', 'This block is yellow and says "it".', tmLight, tsSmall), tpOn);
      Bedroom.GetSurface().Add(TDescribedPhysicalThing.Create('is block', '((orange is word block/blocks)& word/words)@', 'This block is orange and says "is".', tmLight, tsSmall), tpOn);
      Bedroom.GetSurface().Add(TDescribedPhysicalThing.Create('hat block', '((red hat word block/blocks)& word/words)@', 'This block is red and says "hat".', tmLight, tsSmall), tpOn);
      Bedroom.GetSurface().Add(TDescribedPhysicalThing.Create('make block', '((green make word block/blocks)& word/words)@', 'This block is green and says "make".', tmLight, tsSmall), tpOn);

      Cave := TGroundLocation.Create('Cave', 'the cave', 'a cave', 'The cave is round and dark.', CreateEarthSurface());
      Ceiling := TStructure.Create('ceiling', '(round dark)* (ceiling/ceilings roof/rooves roof/roofs)@', 'The ceiling is dark and round, just like the rest of the cave.', 'Putting things on a ceiling seems like an exercise in futility.');
      Cave.Add(Ceiling, tpPartOfImplicit);
      Cave.AddLandmark(cdUp, Ceiling, [loPermissibleNavigationTarget]);
      Thing := TBag.Create('brown sack', '(elongated brown (sack/sacks bag/bags)@)&', 'The sack is brown.', tsBig);
      Cave.GetSurface().Add(Thing, tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('clove of garlic', '((clove/cloves of garlic) (garlic clove/cloves)&)@', 'There''s nothing special about the clove of garlic.', tmLight, tsSmall), tpIn);
      Thing.Add(TDescribedPhysicalThing.Create('lunch', 'lunch/lunches', 'There''s nothing special about the lunch.', tmLight, tsSmall), tpIn);
      Thing.Add(TDescribedPhysicalThing.Create('wooden spoon', '((wooden wood)@ (spoon/spoons utensil/utensils (silverware set/sets)&)@)&', 'The spoon is made of wood.', tmLight, tsSmall), tpIn);

      Thing := TDescribedPhysicalThing.Create('kitchen table', '(non-descript kitchen table/tables)&', 'The kitchen table is non-descript.', tmPonderous, tsMassive);
      Cave.GetSurface().Add(Thing, tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('plastic knife', '(plastic (knife/knives utensil/utensils (silverware set/sets)&)@)&', 'The knife is made of plastic.', tmLight, tsSmall), tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('plastic fork', '(plastic (fork/forks utensil/utensils (silverware set/sets)&)@)&', 'The fork is made of plastic.', tmLight, tsSmall), tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('plastic spoon', '(plastic (spoon/spoons utensil/utensils (silverware set/sets)&)@)&', 'The spoon is made of plastic.', tmLight, tsSmall), tpOn);

      Thing := TDescribedPhysicalThing.Create('desk', '(non-descript (desk/desks table/tables)@)&', 'The desk is non-descript.', tmPonderous, tsMassive);
      Cave.GetSurface().Add(Thing, tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('stainless steel knife', '(stainless steel (knife/knives utensil/utensils (silverware set/sets)&)@)&', 'The knife is made of stainless steel.', tmLight, tsSmall), tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('stainless steel fork', '(stainless steel (fork/forks utensil/utensils (silverware set/sets)&)@)&', 'The fork is made of stainless steel.', tmLight, tsSmall), tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('stainless steel spoon', '(stainless steel (spoon/spoons utensil/utensils (silverware set/sets)&)@)&', 'The spoon is made of stainless steel.', tmLight, tsSmall), tpOn);

      Thing := TDescribedPhysicalThing.Create('dining room table', '(non-descript dining room table/tables)&', 'The dining room table is non-descript.', tmPonderous, tsMassive);
      Cave.GetSurface().Add(Thing, tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('silver knife', '(silver (knife/knives utensil/utensils (silverware set/sets)&)@)&', 'The knife is made of silver.', tmLight, tsSmall), tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('silver fork', '(silver (fork/forks utensil/utensils (silverware set/sets)&)@)&', 'The fork is made of silver.', tmLight, tsSmall), tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('silver spoon', '(silver (spoon/spoons utensil/utensils (silverware set/sets)&)@)&', 'The spoon is made of silver.', tmLight, tsSmall), tpOn);
      Cave.GetSurface().Add(TSpade.Create(), tpOn);

      DoorFrame := TDoorWay.Create('door frame', '(door frame/frames)', 'The door frame is a frame around where a door would go.', cdNorth,
                                   TDoor.Create('door', 'flat? door/doors',
                                                TDoorSide.Create('side', '(flat front)* door? side/sides', 'the front side of the door is flat.'),
                                                TDoorSide.Create('side', '(flat back)* door? side/sides', 'the back side of the door is flat.')));
      DoorFrame.Door.Description := 'The door is flat.';
      World.AddLocation(ConnectThreshold(Tunnel, TunnelEnd, DoorFrame));

      World.AddLocation(ConnectStairs(Cave, Bedroom));

      ConnectLocations(Tunnel, cdWest, Cave, [loPermissibleNavigationTarget, loAutoDescribe]);

      World.AddLocation(Tunnel);
      World.AddLocation(TunnelEnd);
      World.AddLocation(Bedroom);
      World.AddLocation(Cave);
      World.FStartLocation := Cave.GetSurface();

      Result := World;
   end;

   procedure RunTest(TestWorld: TWorld; TestPlayer: TPlayer; Proxy: TTestProxy);

      procedure RunCommand(Command: UTF8String; Responses: array of UTF8String);
      var
         Response: UTF8String;
      begin
         for Response in Responses do
            Proxy.ExpectString(Response);
         TestWorld.Perform(Command, TestPlayer);
         Proxy.ExpectDone();
      end;

   begin
      RunCommand('look then east',
                 [ 'Cave',
                   'The cave is round and dark. Stairs lead up to the bedroom. To the east is the tunnel trail.',
                   'There is a brown sack here.',
                   'There is a kitchen table here.',
                   'On the kitchen table is a plastic knife.',
                   'On the kitchen table is a plastic fork.',
                   'On the kitchen table is a plastic spoon.',
                   'There is a desk here.',
                   'On the desk is a stainless steel knife.',
                   'On the desk is a stainless steel fork.',
                   'On the desk is a stainless steel spoon.',
                   'There is a dining room table here.',
                   'On the dining room table is a silver knife.',
                   'On the dining room table is a silver fork.',
                   'On the dining room table is a silver spoon.',
                   'There is a spade here.',
                   '',
                   'Tunnel Trail',
                   'The tunnel has many turns. To the south is a door. To the west is the cave.']);
      RunCommand('put a bag on the turns', ['(the embroidered bag of holding labeled Tester)', 'Placed.']);
      RunCommand('find bag', ['The bag of holding is on the ground.']);
      RunCommand('put bag on side', ['(first taking the bag of holding)', 'Taken.', 'There does not seem to be any way to attach a bag of holding to the side of the door.']);
      RunCommand('put bag on door', ['The door is closed.']);
      RunCommand('west; push a table up; up',
                 [ 'Cave',
                   'The cave is round and dark. Stairs lead up to the bedroom. To the east is the tunnel trail.',
                   'There is a brown sack here.',
                   'There is a kitchen table here.',
                   'On the kitchen table is a plastic knife.',
                   'On the kitchen table is a plastic fork.',
                   'On the kitchen table is a plastic spoon.',
                   'There is a desk here.',
                   'On the desk is a stainless steel knife.',
                   'On the desk is a stainless steel fork.',
                   'On the desk is a stainless steel spoon.',
                   'There is a dining room table here.',
                   'On the dining room table is a silver knife.',
                   'On the dining room table is a silver fork.',
                   'On the dining room table is a silver spoon.',
                   'There is a spade here.',
                   '',
                   '(the non-descript kitchen table)',
                   '(through the opening for the stairwell)',
                   'The kitchen table is too big to fit in the opening for the stairwell.',
                   '',
                   '(through the opening for the stairwell)',
                   'Bedroom',
                   'The bedroom is a large room. On the ceiling are some stars. Stairs lead down to the cave.',
                   'There is a bed here.',
                   'On the bed is a pillow.',
                   'There is an eat block here.',
                   'There is an it block here.',
                   'There is an is block here.',
                   'There is a hat block here.',
                   'There is a make block here.' ]);
      RunCommand('put bag on ceiling', ['Putting things on a ceiling seems like an exercise in futility.']);
      RunCommand('put bag on stars', ['Putting things on a ceiling seems like an exercise in futility.']);
      RunCommand('drop rim', ['(first taking the rim of the bag of holding)', 'The rim of the bag of holding is part of the bag of holding.']);
      RunCommand('push bed down', ['(through the opening for the stairwell)', 'The bed is too big to fit in the opening for the stairwell.']);
      RunCommand('drop bag then enter bag then enter bag',
                 [ 'Dropped.',
                   '',
                   'In the bag of holding (at the bedroom)',
                   'The bag has the name "Tester" embroidered around its rim.',
                   'The bag of holding is on the ground. Stairs lead down to the cave.',
                   '',
                   'You are back where you started, in the bag of holding.'
                 ]);
   end;

begin
   Writeln('MECHANICS VIII (2024 cuddlycamp regression testing)');
   RunMechanicsHarness(@InitTest, @RunTest, False);
end;

end.