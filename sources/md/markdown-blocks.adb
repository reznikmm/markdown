--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Markdown.List_Items;
with Markdown.Lists;
with Markdown.Visitors;

package body Markdown.Blocks is

   type List_Access is access all Markdown.Lists.List'Class;

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
         Child.Next := Self.Last_Child.Next;
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

   ---------------------
   -- Wrap_List_Items --
   ---------------------

   procedure Wrap_List_Items (Self : in out Container_Block'Class) is
      type Visitor is new Markdown.Visitors.Visitor with record
         Is_List_Item : Boolean := False;
         Is_Ordered   : Boolean := False;
         Marker       : League.Strings.Universal_String;
      end record;

      overriding procedure List_Item
        (Self  : in out Visitor;
         Value : in out Markdown.List_Items.List_Item);

      overriding procedure List_Item
        (Self  : in out Visitor;
         Value : in out Markdown.List_Items.List_Item) is
      begin
         Self.Is_List_Item := True;
         Self.Is_Ordered := Value.Is_Ordered;
         Self.Marker := Value.Marker;
      end List_Item;

   begin
      if Self.Last_Child = null then
         return;
      end if;

      declare
         List : List_Access;
         Last : constant not null Block_Access := Self.Last_Child;
         Item : not null Block_Access := Last.Next;
         Next : not null Block_Access := Item.Next;
      begin
         Self.Last_Child := null;

         loop
            declare
               Checker : Visitor;
            begin
               Item.Visit (Checker);

               if Checker.Is_List_Item then
                  if List = null or else not List.Match (Checker.Marker) then
                     List := new Markdown.Lists.List;
                     Self.Append_Child (Block_Access (List));
                     List.Append_Child (Item);
                  else
                     List.Append_Child (Item);
                  end if;
               else
                  List := null;
                  Self.Append_Child (Item);
               end if;

               exit when Item = Last;

               Item := Next;
               Next := Item.Next;
            end;
         end loop;
      end;
   end Wrap_List_Items;

end Markdown.Blocks;
