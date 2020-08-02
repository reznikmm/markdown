--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Wide_Wide_Text_IO;

with League.Strings;
with League.String_Vectors;

with Markdown.ATX_Headings;
with Markdown.Blockquotes;
with Markdown.Fenced_Code_Blocks;
with Markdown.HTML_Blocks;
with Markdown.Indented_Code_Blocks;
with Markdown.Inline_Parsers;
with Markdown.Link_Reference_Definitions;
with Markdown.Paragraphs;
with Markdown.Parsers;
with Markdown.Thematic_Breaks;
with Markdown.Visitors;

procedure MD_Driver is
   package Visitors is
      type Visitor is new Markdown.Visitors.Visitor with null record;

      overriding procedure ATX_Heading
        (Self  : in out Visitor;
         Block : Markdown.ATX_Headings.ATX_Heading);

      overriding procedure Blockquote
        (Self  : in out Visitor;
         Block : Markdown.Blockquotes.Blockquote);

      overriding procedure Fenced_Code_Block
        (Self  : in out Visitor;
         Block : Markdown.Fenced_Code_Blocks.Fenced_Code_Block);

      overriding procedure HTML_Block
        (Self  : in out Visitor;
         Block : Markdown.HTML_Blocks.HTML_Block);

      overriding procedure Indented_Code_Block
        (Self  : in out Visitor;
         Block : Markdown.Indented_Code_Blocks.Indented_Code_Block);

      overriding procedure Paragraph
        (Self  : in out Visitor;
         Block : Markdown.Paragraphs.Paragraph);

      overriding procedure Thematic_Break
        (Self  : in out Visitor;
         Value : Markdown.Thematic_Breaks.Thematic_Break);

   end Visitors;

   package body Visitors is

      procedure Write_Annotated_Text
        (Text  : Markdown.Inline_Parsers.Annotated_Text);

      overriding procedure ATX_Heading
        (Self  : in out Visitor;
         Block : Markdown.ATX_Headings.ATX_Heading)
      is
         pragma Unreferenced (Self);
         Image : Wide_Wide_String := Block.Level'Wide_Wide_Image;
         Lines : League.String_Vectors.Universal_String_Vector;
      begin
         Lines.Append (Block.Title);
         Image (1) := 'h';
         Ada.Wide_Wide_Text_IO.Put ("<");
         Ada.Wide_Wide_Text_IO.Put (Image);
         Ada.Wide_Wide_Text_IO.Put (">");

         Write_Annotated_Text (Markdown.Inline_Parsers.Parse (Lines));

         Ada.Wide_Wide_Text_IO.Put ("</");
         Ada.Wide_Wide_Text_IO.Put (Image);
         Ada.Wide_Wide_Text_IO.Put_Line (">");
      end ATX_Heading;

      overriding procedure Blockquote
        (Self  : in out Visitor;
         Block : Markdown.Blockquotes.Blockquote) is
      begin
         Ada.Wide_Wide_Text_IO.Put_Line ("<blockquote>");
         Block.Visit_Children (Self);
         Ada.Wide_Wide_Text_IO.Put_Line ("</blockquote>");
      end Blockquote;

      overriding procedure Fenced_Code_Block
        (Self  : in out Visitor;
         Block : Markdown.Fenced_Code_Blocks.Fenced_Code_Block)
      is
         pragma Unreferenced (Self);
         Words : constant League.String_Vectors.Universal_String_Vector :=
           Block.Info_String.Split (' ', League.Strings.Skip_Empty);

         Lines : constant League.String_Vectors.Universal_String_Vector :=
           Block.Lines;
      begin
         Ada.Wide_Wide_Text_IO.Put ("<pre><code");

         if not Block.Info_String.Is_Empty then
            Ada.Wide_Wide_Text_IO.Put (" class=""language-");
            Ada.Wide_Wide_Text_IO.Put (Words (1).To_Wide_Wide_String);
            Ada.Wide_Wide_Text_IO.Put ('"');
         end if;
         Ada.Wide_Wide_Text_IO.Put (">");

         for J in 1 .. Lines.Length loop
            Ada.Wide_Wide_Text_IO.Put_Line (Lines (J).To_Wide_Wide_String);
         end loop;

         Ada.Wide_Wide_Text_IO.Put_Line ("</code></pre>");
      end Fenced_Code_Block;

      overriding procedure HTML_Block
        (Self  : in out Visitor;
         Block : Markdown.HTML_Blocks.HTML_Block)
      is
         pragma Unreferenced (Self);
         Lines : constant League.String_Vectors.Universal_String_Vector :=
           Block.Lines;
      begin
         for J in 1 .. Lines.Length loop
            Ada.Wide_Wide_Text_IO.Put_Line (Lines (J).To_Wide_Wide_String);
         end loop;
      end HTML_Block;

      overriding procedure Indented_Code_Block
        (Self  : in out Visitor;
         Block : Markdown.Indented_Code_Blocks.Indented_Code_Block)
      is
         pragma Unreferenced (Self);
         Lines : constant League.String_Vectors.Universal_String_Vector :=
           Block.Lines;
      begin
         Ada.Wide_Wide_Text_IO.Put ("<pre><code>");

         for J in 1 .. Lines.Length loop
            Ada.Wide_Wide_Text_IO.Put_Line (Lines (J).To_Wide_Wide_String);
         end loop;

         Ada.Wide_Wide_Text_IO.Put_Line ("</code></pre>");
      end Indented_Code_Block;

      overriding procedure Paragraph
        (Self  : in out Visitor;
         Block : Markdown.Paragraphs.Paragraph)
      is
         pragma Unreferenced (Self);

         Lines : constant League.String_Vectors.Universal_String_Vector :=
           Block.Lines;
         Text  : constant Markdown.Inline_Parsers.Annotated_Text :=
           Markdown.Inline_Parsers.Parse (Lines);

         Image : Wide_Wide_String := Block.Setext_Heading'Wide_Wide_Image;
      begin
         if Block.Setext_Heading = 0 then
            Ada.Wide_Wide_Text_IO.Put ("<p>");
            Write_Annotated_Text (Text);
            Ada.Wide_Wide_Text_IO.Put_Line ("</p>");
         else
            Image (1) := 'h';
            Ada.Wide_Wide_Text_IO.Put ("<");
            Ada.Wide_Wide_Text_IO.Put (Image);
            Ada.Wide_Wide_Text_IO.Put (">");

            Write_Annotated_Text (Text);

            Ada.Wide_Wide_Text_IO.Put ("</");
            Ada.Wide_Wide_Text_IO.Put (Image);
            Ada.Wide_Wide_Text_IO.Put_Line (">");
         end if;
      end Paragraph;

      overriding procedure Thematic_Break
        (Self  : in out Visitor;
         Value : Markdown.Thematic_Breaks.Thematic_Break)
      is
         pragma Unreferenced (Self, Value);
      begin
         Ada.Wide_Wide_Text_IO.Put_Line ("<hr />");
      end Thematic_Break;

      procedure Write_Annotated_Text
        (Text  : Markdown.Inline_Parsers.Annotated_Text)
      is
         procedure Write
           (From  : in out Positive;
            Next  : in out Positive;
            Limit : Natural);

         procedure Write
           (From  : in out Positive;
            Next  : in out Positive;
            Limit : Natural) is
         begin
            while From <= Text.Count and then
              Text.Annotation (From).From <= Limit
            loop
               declare
                  Item : constant Markdown.Inline_Parsers.Annotation :=
                    Text.Annotation (From);
               begin
                  Ada.Wide_Wide_Text_IO.Put
                    (Text.Plain_Text.Slice
                       (Next, Item.From - 1).To_Wide_Wide_String);
                  Next := Item.From;
                  From := From + 1;

                  case Item.Kind is
                  when Markdown.Inline_Parsers.Soft_Line_Break =>
                     Next := Next + 1;
                     Ada.Wide_Wide_Text_IO.New_Line;
                  when Markdown.Inline_Parsers.Emphasis =>
                     Ada.Wide_Wide_Text_IO.Put ("<em>");
                     Write (From, Next, Item.To);
                     Ada.Wide_Wide_Text_IO.Put ("</em>");
                  when Markdown.Inline_Parsers.Strong =>
                     Ada.Wide_Wide_Text_IO.Put ("<strong>");
                     Write (From, Next, Item.To);
                     Ada.Wide_Wide_Text_IO.Put ("</strong>");
                  end case;

                  exit when Next >= Limit;

               end;
            end loop;

            if Next <= Limit then
               Ada.Wide_Wide_Text_IO.Put
                 (Text.Plain_Text.Slice
                    (Next, Limit).To_Wide_Wide_String);
               Next := Limit + 1;
            end if;
         end Write;

         Next  : Positive := 1;  --  Position in Text,Plain_Text
         From  : Positive := Text.Annotation'First;
      begin
         Write (From, Next, Text.Plain_Text.Length);
      end Write_Annotated_Text;

   end Visitors;

   Parser  : Markdown.Parsers.Parser;
   Visitor : Visitors.Visitor;
begin
   Parser.Register (Markdown.ATX_Headings.Filter'Access);
   Parser.Register (Markdown.Blockquotes.Filter'Access);
   Parser.Register (Markdown.Thematic_Breaks.Filter'Access);
   Parser.Register (Markdown.Indented_Code_Blocks.Filter'Access);
   Parser.Register (Markdown.Fenced_Code_Blocks.Filter'Access);
   Parser.Register (Markdown.HTML_Blocks.Filter'Access);
   Parser.Register (Markdown.Link_Reference_Definitions.Filter'Access);
   Parser.Register (Markdown.Paragraphs.Filter'Access);

   while not Ada.Wide_Wide_Text_IO.End_Of_File loop
      declare
         Line  : constant League.Strings.Universal_String :=
           League.Strings.To_Universal_String
             (Ada.Wide_Wide_Text_IO.Get_Line);
      begin
         Parser.Append_Line (Line);
      end;
   end loop;

   Parser.Append_Line (League.Strings.Empty_Universal_String);

   Parser.Visit (Visitor);
end MD_Driver;
