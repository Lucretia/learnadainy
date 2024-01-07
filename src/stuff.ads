

--  Ada has a real module system. Modules are called packages and are split into
--  two component parts, the specification (this file) and a body, the other.
--  It is important to introduce packages early, as you will be using them from
--  the start.
package Stuff is
   --  We could add the following line in order to tell the compiler that this
   --  package does not have to run any code before the "main" procedure starts.
   --  pragma Preelaborate;

   --  Packages can be nested within the same file or externally.
   --  Nested packages are accessed via dot notation, e.g. Stuff.Things.My.
   package Things is
      My : constant Integer := 100;
   end Things;

   --  If there are sub-programs declared within the specification, the body
   --  of the sub-program must be declared within the package body.
   procedure Do_Something;  --  If a subprogram takes no parameters, empty
                            --  parentheses are not required, unlike other
                            --  languages.

   --  We can also make generic sub-programs.
   generic
      type Element is (<>);
   procedure Swap (Left, Right : in out Element);

   --  Sometimes we want to hide how a type is defined from the outside world
   --  so that nobody can mess with it directly. The full type must be defined
   --  within the private section below.
   type Blobs is private;
private
   type Blobs is new Integer range -25 .. 25;
end Stuff;
