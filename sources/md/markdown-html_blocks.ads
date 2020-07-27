--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Tags;

with League.String_Vectors;

with Markdown.Blocks;
limited with Markdown.Visitors;

package Markdown.HTML_Blocks is

   type HTML_Block is new Markdown.Blocks.Block with private;

   function Lines (Self : HTML_Block'Class)
     return League.String_Vectors.Universal_String_Vector;

   overriding function Create
     (Line : not null access Markdown.Blocks.Text_Line)
        return HTML_Block;

   overriding procedure Append_Line
     (Self : in out HTML_Block;
      Line : Markdown.Blocks.Text_Line;
      CIP  : Can_Interrupt_Paragraph;
      Ok   : in out Boolean);

   overriding procedure Visit
     (Self    : HTML_Block;
      Visitor : in out Markdown.Visitors.Visitor'Class);

   procedure Filter
     (Line : Markdown.Blocks.Text_Line;
      Tag  : in out Ada.Tags.Tag;
      CIP  : out Can_Interrupt_Paragraph);

private

   type HTML_Block_Kind is range 1 .. 7;

   type HTML_Block is new Markdown.Blocks.Block with record
      Closed : Boolean := False;
      Kind   : HTML_Block_Kind;
      Lines  : League.String_Vectors.Universal_String_Vector;
   end record;

end Markdown.HTML_Blocks;
