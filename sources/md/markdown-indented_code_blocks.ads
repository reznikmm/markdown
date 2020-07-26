--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Tags;

with League.String_Vectors;

with Markdown.Blocks;
limited with Markdown.Visitors;

package Markdown.Indented_Code_Blocks is

   type Indented_Code_Block is new Markdown.Blocks.Block with private;

   function Lines (Self : Indented_Code_Block'Class)
     return League.String_Vectors.Universal_String_Vector;

   overriding function Create
     (Line : not null access Markdown.Blocks.Text_Line)
        return Indented_Code_Block;

   overriding procedure Append_Line
     (Self : in out Indented_Code_Block;
      Line : Markdown.Blocks.Text_Line;
      CIP  : Can_Interrupt_Paragraph;
      Ok   : in out Boolean);

   overriding procedure Visit
     (Self    : Indented_Code_Block;
      Visitor : in out Markdown.Visitors.Visitor'Class);

   procedure Filter
     (Line : Markdown.Blocks.Text_Line;
      Tag  : in out Ada.Tags.Tag;
      CIP  : out Can_Interrupt_Paragraph);

private

   type Indented_Code_Block is new Markdown.Blocks.Block with record
      Lines        : League.String_Vectors.Universal_String_Vector;
   end record;

end Markdown.Indented_Code_Blocks;
