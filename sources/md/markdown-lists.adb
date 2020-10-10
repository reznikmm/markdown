--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Markdown.Visitors;
with Markdown.List_Items;

package body Markdown.Lists is

   overriding procedure Append_Child
     (Self  : in out List;
      Child : not null Markdown.Blocks.Block_Access)
   is
      type Visitor is new Markdown.Visitors.Visitor with null record;

      overriding procedure List_Item
        (Ignore : in out Visitor;
         Value  : in out Markdown.List_Items.List_Item);

      overriding procedure List_Item
        (Ignore : in out Visitor;
         Value  : in out Markdown.List_Items.List_Item)
      is
         Marker : constant League.Strings.Universal_String := Value.Marker;
      begin
         Self.Is_Ordered := Value.Is_Ordered;

         if Self.Marker.Is_Empty and then Self.Is_Ordered then
            Self.Start := Natural'Wide_Wide_Value
              (Marker.Head_To (Marker.Length - 1).To_Wide_Wide_String);
         end if;

         Self.Is_Loose := Self.Is_Loose
           or Value.Has_Blank_Line
           or Self.Ends_Blank;

         Self.Ends_Blank := Value.Ends_With_Blank_Line;
         Self.Marker := Marker.Tail_From (Marker.Length);
      end List_Item;

      List_Item_Detector : Visitor;
   begin
      Child.Visit (List_Item_Detector);
      Markdown.Blocks.Container_Block (Self).Append_Child (Child);
   end Append_Child;

   ------------
   -- Create --
   ------------

   overriding function Create
     (Line : not null access Markdown.Blocks.Text_Line) return List
   is
      pragma Unreferenced (Line);
   begin
      return raise Program_Error;
   end Create;

   --------------
   -- Is_Loose --
   --------------

   function Is_Loose (Self : List'Class) return Boolean is
   begin
      return Self.Is_Loose;
   end Is_Loose;

   ----------------
   -- Is_Ordered --
   ----------------

   function Is_Ordered (Self : List'Class) return Boolean is
   begin
      return Self.Is_Ordered;
   end Is_Ordered;

   -----------
   -- Match --
   -----------

   function Match
     (Self   : List'Class;
      Marker : League.Strings.Universal_String) return Boolean
   is
      use type League.Strings.Universal_String;

      Tail : constant League.Strings.Universal_String :=
        Marker.Tail_From (Marker.Length);
   begin
      return Tail = Self.Marker;
   end Match;

   -----------
   -- Start --
   -----------

   function Start (Self : List'Class) return Natural is
   begin
      return Self.Start;
   end Start;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : in out List;
      Visitor : in out Markdown.Visitors.Visitor'Class) is
   begin
      Visitor.List (Self);
   end Visit;

end Markdown.Lists;
