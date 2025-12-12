-- Ada.Iterator_Interfaces for Z80
-- Standard iterator interface types
--
-- Defines the interfaces used by for..of loops

package Ada.Iterator_Interfaces is
   pragma Pure;

   -- Forward_Iterator interface
   -- Used for forward iteration (First, Next)
   type Forward_Iterator is limited interface;

   function First (Object : Forward_Iterator) return Cursor is abstract;
   function Next
     (Object   : Forward_Iterator;
      Position : Cursor) return Cursor is abstract;

   -- Reversible_Iterator interface
   -- Extends Forward_Iterator with reverse iteration (Last, Previous)
   type Reversible_Iterator is limited interface and Forward_Iterator;

   function Last (Object : Reversible_Iterator) return Cursor is abstract;
   function Previous
     (Object   : Reversible_Iterator;
      Position : Cursor) return Cursor is abstract;

   -- Note: Cursor is a formal type parameter in the generic
   -- This is the non-generic version for Ada.Iterator_Interfaces

   -- Generic versions for specific cursor types
   generic
      type Cursor is private;
      with function Has_Element (Position : Cursor) return Boolean;
   package Forward_Iteration is
      type Iterator is limited interface;

      function First (Object : Iterator) return Cursor is abstract;
      function Next (Object : Iterator; Position : Cursor) return Cursor
        is abstract;
   end Forward_Iteration;

   generic
      type Cursor is private;
      with function Has_Element (Position : Cursor) return Boolean;
   package Reversible_Iteration is
      type Iterator is limited interface;

      function First (Object : Iterator) return Cursor is abstract;
      function Next (Object : Iterator; Position : Cursor) return Cursor
        is abstract;
      function Last (Object : Iterator) return Cursor is abstract;
      function Previous (Object : Iterator; Position : Cursor) return Cursor
        is abstract;
   end Reversible_Iteration;

private
   -- Abstract cursor type for non-generic interface
   type Cursor is null record;

end Ada.Iterator_Interfaces;
