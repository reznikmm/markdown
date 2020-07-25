--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Tags;

with League.String_Vectors;

with Markdown.Blocks;
limited with Markdown.Visitors;

package Markdown.Paragraphs is

   type Paragraph is new Markdown.Blocks.Block with private;

   overriding function Create
     (Line   : not null access Markdown.Blocks.Text_Line) return Paragraph;

   overriding procedure Append_Line
     (Self : in out Paragraph;
      Line : Markdown.Blocks.Text_Line;
      CIP  : Can_Interrupt_Paragraph;
      Ok   : in out Boolean);

   overriding procedure Visit
     (Self    : Paragraph;
      Visitor : in out Markdown.Visitors.Visitor'Class);

   procedure Filter
     (Line : Markdown.Blocks.Text_Line;
      Tag  : in out Ada.Tags.Tag;
      CIP  : out Can_Interrupt_Paragraph);

   function Lines (Self : Paragraph'Class)
     return League.String_Vectors.Universal_String_Vector;

private

   type Paragraph is new Markdown.Blocks.Block with record
      Lines : League.String_Vectors.Universal_String_Vector;
   end record;

end Markdown.Paragraphs;
