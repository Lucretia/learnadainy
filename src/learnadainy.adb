procedure Learnadainy is
   --  This is a comment.

   type Degrees is range 0 .. 360;   --  This is a type.

   type Hues is (Red, Green, Blue, Purple, Yellow);  --  So, is this.
   subtype Primaries is Hues range Red .. Blue;

   Blue_Hue   :          Primaries := Blue;  --  A variable.
   Red_Hue    : constant Primaries := Red;   --  A constant.
   Yellow_Hue : constant Hues      := Yellow;
   Colour_1   : constant Hues      := Red_Hue;
   Colour_2   : constant Primaries := Yellow_Hue;   --  Uncomment to see it fail.
begin
   null;
end Learnadainy;
