with Ada.Unchecked_Conversion;
with Ada.Text_IO;

procedure LearnAdaInY is
   --  Indentation is 3 spaces.

   --  The most important feature in Ada is the type, objects have types and an
   --  object of one type cannot be assigned to an object of another type.

   --  You can and should define your own types for the domain you are
   --  modelling. But you can use the standard types to start with and then
   --  replace them later with your own types, this could be called a form of
   --  gradual typing.
   --
   --  But would only really be a good starting point for binding to other
   --  languages, like C. Ada is the only language with a standardised way to
   --  bind with [C](https://ada-lang.io/docs/arm/AA-B/AA-B.3).

   type Degrees is range 0 .. 360;   --  This is a type.

   type Hues is (Red, Green, Blue, Purple, Yellow);  --  So, is this.

   --  You can restrict a type's range using a subtype, this makes them
   --  compatible with each other, i.e. the subtype can be assigned to an
   --  object of the type, as can be seen below.
   subtype Primaries is Hues range Red .. Blue;  --  This is a range.

   --  You can define variables or constants like this:

   --  10 is the universal integer. These universal numerics can be used with
   --  any type which matches the base type.
   Angle : Degrees := 10;
   Value : Integer := 20;
   --  New_Angle : Degrees := Value;   -- Incompatible types won't compile.
   --  New_Value : Integer := Angle;

   Blue_Hue   :          Primaries := Blue;  --  A variable.
   Red_Hue    : constant Primaries := Red;   --  A constant.
   Yellow_Hue : constant Hues      := Yellow;
   Colour_1   : constant Hues      := Red_Hue;
   --  Colour_2   : constant Primaries := Yellow_Hue; --  uncomment to compile.

   --  You can force conversions, but the you are warned by the name of the
   --  package that you are doing something unsafe.
   function Degrees_To_Int is new Ada.Unchecked_Conversion
     (Source => Degrees,   --  Line continuations are indented by 2 spaces.
      Target => Integer);

   New_Value_2 : Integer := Degrees_To_Int (Angle);   --  Note, space before (.

   --  Ada has a style guide and GNAT will force you to adhere to it, so that
   --  all Ada source looks consistent.

   --  TODO: Add fixed and floating point types.

   --  You can have normal Latin 1 based strings by default.
   Str  : constant String    := "This is a constant string";
   --  When initialising from a string literal, the compiler knows the bounds,
   --  so we don't have to define them.

   --  Strings are arrays. Note how parentheses are used to access elements of
   --  an array? This is a mathematical notation. It was used because at the
   --  time square brackets were not available on all keyboards at the time
   --  Ada was created and because an array can be seen as a function from a
   --  mathematical perspective, so it made converting between arrays and
   --  functions easier.
   Char : constant Character := Str (Str'First);

   --  Ada 2022 includes the use of [] for array initialisation when using
   --  the containers, which were added in Ada 2012.

   --  Arrays are usually always defined as a type.
   --  They can be any dimension.
   type My_Array_1 is array (1 .. 4, 3 .. 7, -20 .. 20) of Integer;

   --  Yes, unlike other languages, you can index arrays with other discrete
   --  types / ranges.

   type Axes is (X, Y, Z);

   --  You can define the array's range using the 'Range attribute.
   type Vector is array (Axes'Range) of Float;

   V1 : constant Vector := (0.0, 0.0, 1.0);

   --  We can rename objects (aliases) to make readability a bit better.
   package IO renames Ada.Text_IO;
begin
   --  We can output enumerations as names.
   IO.Put_Line ("Blue_Hue   = " &  --  & is the string concatenation operator.
                Blue'Image);       --  ' accesses attributes on objects.
                  --  The Image attribute converts a value to a string.
                  --  Ada 2022 has extended Image to custom types too.
                  --  Access this with -gnat2022 compiler flag.
   IO.Put_Line ("Yellow_Hue = " &
                --  We can use the type's attribute.
                Primaries'Image (Yellow_Hue));

   --  We can define local variables within a declare block, this can be made
   --  more readable by giving it a label.
   Enum_IO : declare
      package Hue_IO is new IO.Enumeration_IO (Hues);

      --  Using a package makes everything inside that package visible within
      --  this block, it is good practice to only do this locally and not on
      --  a whole package within the context clause.
      use Hue_IO;
   begin
      --  We can print out the enumeration values too.
      Put (Purple); --  Note we don't have to prefix the Put procedure with
                    --  Hue_IO.
      IO.New_Line;  --  We still need to prefix with IO here.
      Put (Red_Hue);
      IO.New_Line;
   end Enum_IO;

   --  Loops have a consistent form.
   --  <form> can be while or for or missing as below.
   --
   --  Infinite - Uncomment to see it loop forever.
   --  loop
   --     null;
   --  end loop;  -- Useful to state machines.

   declare  --  We don't have to have a label.
      Counter : Positive := Positive'First;  --  This is 1.
   begin
      while Counter < 10 loop
         IO.Put_Line ("Counter = " & Counter'Image);

         Counter := Counter + 1;  --  There is no explicit inc/decrement.

         --  Ada 2022 introduced @ for LHS, so the above would be written as
         --  Counter := @ + 1;  --  Try it, -gnat2022.
      end loop;
   end;

   declare
      package Hue_IO is new IO.Enumeration_IO (Hues);

      --  We can have multiple packages on one line, but I tend to use one
      --  package per line for readability.
      use IO, Hue_IO;
   begin
      Put ("Hues : ");  -- Note, no prefix.

      --  Because we are using the 'Range attribute, the compiler knows it is
      --  safe and can omit run-time checks here.
      for Hue in Hues'Range loop
         Put (Hue);

         --  Types and objects know about their bounds, their First .. Last
         --  values. These can be specified as range types.
         if Hue /= Hues'Last then
            Put (", ");
         end if;
      end loop;

      IO.New_Line;
   end;

   --  All objects know their bounds, including strings.
   declare
      C : Character := Str (50);  --  Warning caused and exception raised at
                                  --  runtime.
      --  The exception raised above can only be handled by an outer scope,
      --  see [wikipbook](https://en.wikibooks.org/wiki/Ada_Programming/Exceptions#Exception_handlers).
   begin
      null;  --  We will never get to this point because of the above.
   end;
exception
   when Constraint_Error =>
      IO.Put_Line ("Caught the exception");
end LearnAdaInY;
