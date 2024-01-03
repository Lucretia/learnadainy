package body Stuff is
   --  Sub-program body.
   procedure Do_Something is
      --  We can nest sub-programs too.
      --  Parameters are defined with the direction of travel, in, in out, out.
      function Times_4 (Value : in Integer) return Integer is
      begin
         return Value * 4;
      end Times_4;

      I : Integer := 4;
   begin
      I := Times_4 (I);
   end Do_Something;
begin
   --  If we need to initialise something within the package, we can do it
   --  here.
   Do_Something;
end Stuff;