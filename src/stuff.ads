--  Ada has a real module system, they are called packages and are split into
--  two component parts, the specification (this file) and a body, the other.
package Stuff is
   --  pragma Preelaborate;

   --  Packages can be nested within the same file or externally.
   --  Nested packages are accessed via dot notation, e.g. Stuff.Things.My.
   package Things is
      My : constant Integer := 100;
   end Things;

   --  If there are sub-programs declared within the specification, the body
   --  of the sub-program must be declared within the package bod.
   procedure Do_Something;  --  If a subprogram takes no parameters, it is not
                            --  shown.

   --  Sometimes we want to hide how a type is defined from the outside world
   --  so that nobody can mess with it directly. The full type must be defined
   --  within the private section below.
   type Blobs is private;
private
   type Blobs is new Integer range -25 .. 25;
end Stuff;