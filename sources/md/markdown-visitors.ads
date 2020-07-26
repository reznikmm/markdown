--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Markdown.ATX_Headings;
with Markdown.Blockquotes;
with Markdown.Indented_Code_Blocks;
with Markdown.Paragraphs;
with Markdown.Thematic_Breaks;

package Markdown.Visitors is

   type Visitor is limited interface;

   not overriding procedure ATX_Heading
     (Self  : in out Visitor;
      Value : Markdown.ATX_Headings.ATX_Heading) is null;

   not overriding procedure Blockquote
     (Self  : in out Visitor;
      Value : Markdown.Blockquotes.Blockquote) is null;

   not overriding procedure Indented_Code_Block
     (Self  : in out Visitor;
      Value : Markdown.Indented_Code_Blocks.Indented_Code_Block) is null;

   not overriding procedure Paragraph
     (Self  : in out Visitor;
      Value : Markdown.Paragraphs.Paragraph) is null;

   not overriding procedure Thematic_Break
     (Self  : in out Visitor;
      Value : Markdown.Thematic_Breaks.Thematic_Break) is null;

end Markdown.Visitors;
