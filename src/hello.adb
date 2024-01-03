--  Ada compiler's accept compilation units which can be library packages,
--  tasks, sub-programs, generics, etc.

--  This is where "context clauses" go, these can be pragmas or ```with```
--  statements.
with Ada.Text_IO;  --  Get access to a library package.

procedure Hello is
begin
   Ada.Text_IO.Put_Line ("Hello, world");

   Ada.Text_IO.Put ("Hello again, world");
   Ada.Text_IO.New_Line;
end Hello;
