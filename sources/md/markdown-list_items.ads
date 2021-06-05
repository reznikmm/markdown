--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Tags;

with League.Strings;

with Markdown.Blocks;
limited with Markdown.Visitors;

package Markdown.List_Items is

   type List_Item is new Markdown.Blocks.Container_Block with private;

   function Has_Blank_Line (Self : List_Item'Class) return Boolean;
   function Ends_With_Blank_Line (Self : List_Item'Class) return Boolean;

   function Is_Ordered (Self : List_Item'Class) return Boolean;
   --  Return True if list item has an ordered list marker.

   function Marker (Self : List_Item'Class)
     return League.Strings.Universal_String;
   --  Return the list marker.

   procedure Filter
     (Line : Markdown.Blocks.Text_Line;
      Tag  : in out Ada.Tags.Tag;
      CIP  : out Can_Interrupt_Paragraph);

private

   type List_Item is new Markdown.Blocks.Container_Block with record
      Starts_With_Blank_Line : Boolean := False;
      --  Item starts with an empty line
      Ends_With_Blank_Line   : Boolean := False;
      --  The last known line of the item was empty
      Has_Blank_Line         : Boolean := False;
      --  The item has at least one empty line (excluding line with a marker)
      First_Line             : Boolean := False;
      --  This is true only for the line with a marker
      Marker_Width           : Positive;
      Marker                 : League.Strings.Universal_String;
   end record;

   overriding function Create
     (Line   : not null access Markdown.Blocks.Text_Line) return List_Item;

   overriding procedure Visit
     (Self    : in out List_Item;
      Visitor : in out Markdown.Visitors.Visitor'Class);

   overriding procedure Consume_Continuation_Markers
     (Self  : in out List_Item;
      Line  : in out Markdown.Blocks.Text_Line;
      Match : out Boolean);

end Markdown.List_Items;
