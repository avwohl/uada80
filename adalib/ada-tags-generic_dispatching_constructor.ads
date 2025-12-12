-- Ada.Tags.Generic_Dispatching_Constructor for Z80
-- Generic constructor for dispatching operations

with Ada.Tags;

generic
   type T (<>) is abstract tagged limited private;
   type Parameters (<>) is limited private;
   with function Constructor (Params : not null access Parameters) return T is abstract;
function Ada.Tags.Generic_Dispatching_Constructor
  (The_Tag : Tag;
   Params  : not null access Parameters) return T'Class;
pragma Preelaborate (Ada.Tags.Generic_Dispatching_Constructor);
