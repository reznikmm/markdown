--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Markdown.ATX_Headings;
with Markdown.Blockquotes;
with Markdown.Fenced_Code_Blocks;
with Markdown.HTML_Blocks;
with Markdown.Indented_Code_Blocks;
with Markdown.Link_Reference_Definitions;
with Markdown.List_Items;
with Markdown.Lists;
with Markdown.Paragraphs;
with Markdown.Thematic_Breaks;

package Markdown.Visitors is

   type Visitor is limited interface;

   not overriding procedure ATX_Heading
     (Self  : in out Visitor;
      Value : Markdown.ATX_Headings.ATX_Heading) is null;

   not overriding procedure Blockquote
     (Self  : in out Visitor;
      Value : in out Markdown.Blockquotes.Blockquote) is null;

   not overriding procedure Fenced_Code_Block
     (Self  : in out Visitor;
      Value : Markdown.Fenced_Code_Blocks.Fenced_Code_Block) is null;

   not overriding procedure HTML_Block
     (Self  : in out Visitor;
      Value : Markdown.HTML_Blocks.HTML_Block) is null;

   not overriding procedure Indented_Code_Block
     (Self  : in out Visitor;
      Value : Markdown.Indented_Code_Blocks.Indented_Code_Block) is null;

   not overriding procedure Link_Reference_Definition
     (Self  : in out Visitor;
      Value : Markdown.Link_Reference_Definitions.Link_Reference_Definition) is
        null;

   not overriding procedure List_Item
     (Self  : in out Visitor;
      Value : in out Markdown.List_Items.List_Item) is null;

   not overriding procedure List
     (Self  : in out Visitor;
      Value : Markdown.Lists.List) is null;

   not overriding procedure Paragraph
     (Self  : in out Visitor;
      Value : Markdown.Paragraphs.Paragraph) is null;

   not overriding procedure Thematic_Break
     (Self  : in out Visitor;
      Value : Markdown.Thematic_Breaks.Thematic_Break) is null;

end Markdown.Visitors;
