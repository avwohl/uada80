-- Ada.Tags.Generic_Dispatching_Constructor body for Z80
-- Generic constructor for dispatching operations implementation

function Ada.Tags.Generic_Dispatching_Constructor
  (The_Tag : Tag;
   Params  : not null access Parameters) return T'Class
is
begin
   -- This function requires compiler support to locate the
   -- correct constructor in the dispatch table based on The_Tag.
   -- On Z80, this is a stub that raises Tag_Error.
   raise Tag_Error with "Dispatching constructor not supported";
   -- The following is never executed but satisfies the return
   return Constructor (Params);
end Ada.Tags.Generic_Dispatching_Constructor;
