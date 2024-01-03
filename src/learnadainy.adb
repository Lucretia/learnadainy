procedure LearnAdaInY is
   --  The most important feature in Ada is the type, objects have types and an
   --  object of one type cannot be assigned to an object of another type.

   type Degrees is range 0 .. 360;   --  This is a type.

   type Hues is (Red, Green, Blue, Purple, Yellow);  --  So, is this.

   --  You can restrict a type's range using a subtype, this makes them
   --  compatible with each other, i.e. the subtype can be assigned to an object
   --  of the type, as can be seen below.
   subtype Primaries is Hues range Red .. Blue;

   --  You can define variables or constants like this:

   --  10 is the universal integer. These universal numerics can be used with
   --  any type which matches the base type.
   Angle : Degrees := 10;
   Value : Integer := 20;
   --  New_Angle : Degrees := Value;   -- Incompatible types.
   --  New_Value : Integer := Angle;

   Blue_Hue   :          Primaries := Blue;  --  A variable.
   Red_Hue    : constant Primaries := Red;   --  A constant.
   Yellow_Hue : constant Hues      := Yellow;
   Colour_1   : constant Hues      := Red_Hue;
   --  Colour_2   : constant Primaries := Yellow_Hue;   --  Comment to compile.
begin
   null;
end LearnAdaInY;
