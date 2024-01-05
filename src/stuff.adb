

package body Stuff is
   --  Sub-program body.
   procedure Do_Something is
      --  We can nest sub-programs too.
      --  Parameters are defined with the direction of travel, in, in out, out.
      --  If the direction of travel is not specified, they are in by default.
      function Times_4 (Value : in Integer) return Integer is
      begin
         return Value * 4;
      end Times_4;

      I : Integer := 4;
   begin
      I := Times_4 (I);
   end Do_Something;


   --  Generic procedure body.
   procedure Swap (Left, Right : in out Element) is
      Temp : Element := Left;
   begin
      Left  := Right;
      Right := Temp;
   end Swap;
begin
   --  If we need to initialise something within the package, we can do it
   --  here.
   Do_Something;
end Stuff;
