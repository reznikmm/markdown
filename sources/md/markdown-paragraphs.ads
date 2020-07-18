--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Tags;

with League.Strings;
with League.String_Vectors;

with Markdown.Blocks;
limited with Markdown.Visitors;

package Markdown.Paragraphs is

   type Paragraph is new Markdown.Blocks.Block with private;

   overriding function Create
     (Line   : not null access Markdown.Blocks.Text_Line) return Paragraph;

   overriding procedure Consume_Continuation_Markers
     (Self   : Paragraph;
      Line   : League.Strings.Universal_String;
      From   : in out Positive;
      Column : in out Positive;
      Match  : out Continuation_Kind);

   overriding procedure Append_Child
     (Self  : in out Paragraph;
      Child : not null Markdown.Blocks.Block_Access);

   overriding procedure Append_Line
     (Self   : in out Paragraph;
      Line   : League.Strings.Universal_String;
      From   : Positive;
      Column : Positive);

   overriding procedure Visit
     (Self    : Paragraph;
      Visitor : in out Markdown.Visitors.Visitor'Class);

   procedure Filter
     (Line   : League.Strings.Universal_String;
      From   : in out Positive;
      Column : in out Positive;
      Tag    : in out Ada.Tags.Tag);

   function Lines (Self : Paragraph'Class)
     return League.String_Vectors.Universal_String_Vector;

private

   type Paragraph is new Markdown.Blocks.Block with record
      Lines : League.String_Vectors.Universal_String_Vector;
   end record;

end Markdown.Paragraphs;
