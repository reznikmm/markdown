--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

package body Markdown.Blocks is

   ------------------
   -- Append_Child --
   ------------------

   not overriding procedure Append_Child
     (Self  : in out Container_Block;
      Child : not null Block_Access) is
   begin
      if Self.Last_Child = null then
         Child.Next := Child;
      else
         Child.Next := Self.Last_Child;
         Self.Last_Child.Next := Child;
      end if;

      Self.Last_Child := Child;
   end Append_Child;

   --------------------
   -- Visit_Children --
   --------------------

   procedure Visit_Children
     (Self    : Container_Block'Class;
      Visitor : in out Markdown.Visitors.Visitor'Class)
   is
   begin
      if Self.Last_Child = null then
         return;
      end if;

      declare
         Item : not null Block_Access := Self.Last_Child.Next;
      begin
         loop
            Item.Visit (Visitor);

            exit when Item = Self.Last_Child;

            Item := Item.Next;
         end loop;
      end;
   end Visit_Children;

end Markdown.Blocks;
