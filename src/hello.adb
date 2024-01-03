--  Ada compiler's accept compilation units which can be library packages,
--  tasks, sub-programs, generics, etc.
with Ada.Text_IO;  --  Get access to a library package.

procedure Hello is
begin
   Ada.Text_IO.Put_Line ("Hello, world");

   Ada.Text_IO.Put ("Hello again, world");
   Ada.Text_IO.New_Line;
end Hello;
