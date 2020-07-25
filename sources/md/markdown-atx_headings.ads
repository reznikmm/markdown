--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Tags;

with League.Strings;

with Markdown.Blocks;
limited with Markdown.Visitors;

package Markdown.ATX_Headings is

   type ATX_Heading is new Markdown.Blocks.Block with private;

   overriding function Create
     (Line   : not null access Markdown.Blocks.Text_Line) return ATX_Heading;

   overriding procedure Visit
     (Self    : ATX_Heading;
      Visitor : in out Markdown.Visitors.Visitor'Class);

   procedure Filter
     (Line : Markdown.Blocks.Text_Line;
      Tag  : in out Ada.Tags.Tag;
      CIP  : out Can_Interrupt_Paragraph);

   subtype Heading_Level is Positive range 1 .. 6;

   function Title (Self : ATX_Heading'Class)
     return League.Strings.Universal_String;

   function Level (Self : ATX_Heading'Class) return Heading_Level;

private

   type ATX_Heading is new Markdown.Blocks.Block with record
      Title : League.Strings.Universal_String;
      Level : Heading_Level := 1;
   end record;

end Markdown.ATX_Headings;
