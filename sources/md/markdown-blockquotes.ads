--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Tags;

with Markdown.Blocks;
limited with Markdown.Visitors;

package Markdown.Blockquotes is

   type Blockquote is new Markdown.Blocks.Container_Block with private;

   overriding function Create
     (Line   : not null access Markdown.Blocks.Text_Line) return Blockquote;

   overriding procedure Visit
     (Self    : in out Blockquote;
      Visitor : in out Markdown.Visitors.Visitor'Class);

   overriding procedure Consume_Continuation_Markers
     (Self  : Blockquote;
      Line  : in out Markdown.Blocks.Text_Line;
      Match : out Boolean);

   procedure Filter
     (Line : Markdown.Blocks.Text_Line;
      Tag  : in out Ada.Tags.Tag;
      CIP  : out Can_Interrupt_Paragraph);

private
   type Blockquote is new Markdown.Blocks.Container_Block with null record;

end Markdown.Blockquotes;
