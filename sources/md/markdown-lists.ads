--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with League.Strings;

with Markdown.Blocks;
limited with Markdown.Visitors;

package Markdown.Lists is

   type List is new Markdown.Blocks.Container_Block with private;

   function Is_Loose (Self : List'Class) return Boolean;
   --  A list is loose if any of its constituent list items are separated by
   --  blank lines, or if any of its constituent list items directly contain
   --  two block-level elements with a blank line between them.

   function Is_Ordered (Self : List'Class) return Boolean;
   --  Return True if list item has an ordered list marker.

   function Start (Self : List'Class) return Natural
     with Pre => Self.Is_Ordered;
   --  Start number of the first list item in case of ordered list

   function Match
     (Self   : List'Class;
      Marker : League.Strings.Universal_String) return Boolean;

private

   type List is new Markdown.Blocks.Container_Block with record
      Is_Loose   : Boolean := False;
      Ends_Blank : Boolean := False;
      Is_Ordered : Boolean := False;
      Start      : Natural := 1;
      Marker     : League.Strings.Universal_String;
   end record;

   overriding function Create
     (Line   : not null access Markdown.Blocks.Text_Line) return List;

   overriding procedure Append_Child
     (Self  : in out List;
      Child : not null Markdown.Blocks.Block_Access);

   overriding procedure Visit
     (Self    : in out List;
      Visitor : in out Markdown.Visitors.Visitor'Class);

   overriding procedure Consume_Continuation_Markers
     (Self  : in out List;
      Line  : in out Markdown.Blocks.Text_Line;
      Match : out Boolean) is null;

end Markdown.Lists;
