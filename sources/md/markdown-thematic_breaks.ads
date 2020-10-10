--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Tags;

with Markdown.Blocks;
limited with Markdown.Visitors;

package Markdown.Thematic_Breaks is

   type Thematic_Break is new Markdown.Blocks.Block with private;

   overriding function Create
     (Line : not null access Markdown.Blocks.Text_Line) return Thematic_Break;

   overriding procedure Visit
     (Self    : in out Thematic_Break;
      Visitor : in out Markdown.Visitors.Visitor'Class);

   procedure Filter
     (Line : Markdown.Blocks.Text_Line;
      Tag  : in out Ada.Tags.Tag;
      CIP  : out Can_Interrupt_Paragraph);

private

   type Thematic_Break is new Markdown.Blocks.Block with record
      null;
   end record;

end Markdown.Thematic_Breaks;
