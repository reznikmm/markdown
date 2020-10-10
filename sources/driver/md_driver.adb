--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Wide_Wide_Text_IO;

with League.Strings;
with League.String_Vectors;
with XML.SAX.Attributes;
--  with XML.SAX.HTML5_Writers;
--  with XML.SAX.Pretty_Writers;
with Custom_Writers;
with XML.SAX.Output_Destinations.Strings;

with Markdown.ATX_Headings;
with Markdown.Blockquotes;
with Markdown.Fenced_Code_Blocks;
with Markdown.HTML_Blocks;
with Markdown.Indented_Code_Blocks;
with Markdown.Inline_Parsers;
with Markdown.Link_Reference_Definitions;
with Markdown.List_Items;
with Markdown.Paragraphs;
with Markdown.Parsers;
with Markdown.Thematic_Breaks;
with Markdown.Visitors;
with Markdown.Lists;

procedure MD_Driver is

   LF : constant Wide_Wide_Character := Ada.Characters.Wide_Wide_Latin_1.LF;

   New_Line : constant Wide_Wide_String := (1 => LF);

   function Trim_Doctype (Text : League.Strings.Universal_String)
     return League.Strings.Universal_String;

   package Visitors is
      type Visitor is limited new Markdown.Visitors.Visitor with record
         Parser    : Markdown.Parsers.Parser;
         Is_Tight  : Boolean := False;
         New_Line  : League.Strings.Universal_String;
         Namespace : League.Strings.Universal_String;
         Writer    : aliased Custom_Writers.Writer;
         Output    : aliased XML.SAX.Output_Destinations.Strings
           .String_Output_Destination;
      end record;

      overriding procedure ATX_Heading
        (Self  : in out Visitor;
         Block : Markdown.ATX_Headings.ATX_Heading);

      overriding procedure Blockquote
        (Self  : in out Visitor;
         Block : in out Markdown.Blockquotes.Blockquote);

      overriding procedure Fenced_Code_Block
        (Self  : in out Visitor;
         Block : Markdown.Fenced_Code_Blocks.Fenced_Code_Block);

      overriding procedure HTML_Block
        (Self  : in out Visitor;
         Block : Markdown.HTML_Blocks.HTML_Block);

      overriding procedure Indented_Code_Block
        (Self  : in out Visitor;
         Block : Markdown.Indented_Code_Blocks.Indented_Code_Block);

      overriding procedure List
        (Self  : in out Visitor;
         Block : Markdown.Lists.List);

      overriding procedure List_Item
        (Self  : in out Visitor;
         Block : in out Markdown.List_Items.List_Item);

      overriding procedure Paragraph
        (Self  : in out Visitor;
         Block : Markdown.Paragraphs.Paragraph);

      overriding procedure Thematic_Break
        (Self  : in out Visitor;
         Value : Markdown.Thematic_Breaks.Thematic_Break);

   end Visitors;

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   package body Visitors is

      procedure Write_Annotated_Text
        (Self  : in out Visitor'Class;
         Text  : Markdown.Inline_Parsers.Annotated_Text);

      overriding procedure ATX_Heading
        (Self  : in out Visitor;
         Block : Markdown.ATX_Headings.ATX_Heading)
      is
         Image : Wide_Wide_String := Block.Level'Wide_Wide_Image;
         Lines : League.String_Vectors.Universal_String_Vector;

         Empty : XML.SAX.Attributes.SAX_Attributes renames
           XML.SAX.Attributes.Empty_SAX_Attributes;

      begin
         Lines.Append (Block.Title);
         Image (1) := 'h';
         Self.Writer.Start_Element
           (Namespace_URI => Self.Namespace,
            Local_Name    => +Image,
            Attributes    => Empty);

         Self.Write_Annotated_Text (Self.Parser.Parse_Inlines (Lines));

         Self.Writer.End_Element
           (Namespace_URI => Self.Namespace,
            Local_Name    => +Image);
      end ATX_Heading;

      overriding procedure Blockquote
        (Self  : in out Visitor;
         Block : in out Markdown.Blockquotes.Blockquote)
      is
         Empty : XML.SAX.Attributes.SAX_Attributes renames
           XML.SAX.Attributes.Empty_SAX_Attributes;
      begin
         Self.Writer.Start_Element
           (Namespace_URI => Self.Namespace,
            Local_Name    => +"blockquote",
            Attributes    => Empty);
         Block.Visit_Children (Self);
         Self.Writer.End_Element
           (Namespace_URI => Self.Namespace,
            Local_Name    => +"blockquote");
      end Blockquote;

      overriding procedure Fenced_Code_Block
        (Self  : in out Visitor;
         Block : Markdown.Fenced_Code_Blocks.Fenced_Code_Block)
      is
         use type League.Strings.Universal_String;

         Words : constant League.String_Vectors.Universal_String_Vector :=
           Block.Info_String.Split (' ', League.Strings.Skip_Empty);

         Lines : constant League.String_Vectors.Universal_String_Vector :=
           Block.Lines;

         Attr : XML.SAX.Attributes.SAX_Attributes;
      begin
         Self.Writer.Start_Element
           (Namespace_URI => Self.Namespace,
            Local_Name    => +"pre",
            Attributes    => Attr);

         if not Block.Info_String.Is_Empty then
            Attr.Set_Value
              (Namespace_URI => Self.Namespace,
               Local_Name    => +"class",
               Value         => "language-" & Words (1));
         end if;

         Self.Writer.Start_Element
           (Namespace_URI => Self.Namespace,
            Local_Name    => +"code",
            Attributes    => Attr);

         for J in 1 .. Lines.Length loop
            Self.Writer.Characters (Lines (J));
            Self.Writer.Characters (Self.New_Line);
         end loop;

         Self.Writer.End_Element
           (Namespace_URI => Self.Namespace,
            Local_Name    => +"code");
         Self.Writer.End_Element
           (Namespace_URI => Self.Namespace,
            Local_Name    => +"pre");
      end Fenced_Code_Block;

      overriding procedure HTML_Block
        (Self  : in out Visitor;
         Block : Markdown.HTML_Blocks.HTML_Block)
      is
         Lines : constant League.String_Vectors.Universal_String_Vector :=
           Block.Lines;
      begin
         for J in 1 .. Lines.Length loop
            if J > 1 then
               Self.Writer.Characters (Self.New_Line);
            end if;

            Self.Writer.Unescaped_Characters (Lines (J));
         end loop;
      end HTML_Block;

      overriding procedure Indented_Code_Block
        (Self  : in out Visitor;
         Block : Markdown.Indented_Code_Blocks.Indented_Code_Block)
      is
         Lines : constant League.String_Vectors.Universal_String_Vector :=
           Block.Lines;

         Empty : XML.SAX.Attributes.SAX_Attributes renames
           XML.SAX.Attributes.Empty_SAX_Attributes;

      begin
         Self.Writer.Start_Element
           (Namespace_URI => Self.Namespace,
            Local_Name    => +"pre",
            Attributes    => Empty);

         Self.Writer.Start_Element
           (Namespace_URI => Self.Namespace,
            Local_Name    => +"code",
            Attributes    => Empty);

         for J in 1 .. Lines.Length loop
            Self.Writer.Characters (Lines (J));
            Self.Writer.Characters (Self.New_Line);
         end loop;

         Self.Writer.End_Element
           (Namespace_URI => Self.Namespace,
            Local_Name    => +"code");
         Self.Writer.End_Element
           (Namespace_URI => Self.Namespace,
            Local_Name    => +"pre");
      end Indented_Code_Block;

      overriding procedure List
        (Self  : in out Visitor;
         Block : Markdown.Lists.List)
      is
         Empty : XML.SAX.Attributes.SAX_Attributes renames
           XML.SAX.Attributes.Empty_SAX_Attributes;

         Is_Tight : constant Boolean := Self.Is_Tight;

         Name : constant League.Strings.Universal_String :=
           (if Block.Is_Ordered then +"ol" else +"ul");
      begin
         Self.Is_Tight := not Block.Is_Loose;
         Self.Writer.Start_Element
           (Namespace_URI => Self.Namespace,
            Local_Name    => Name,
            Attributes    => Empty);
         Block.Visit_Children (Self);
         Self.Writer.End_Element
           (Namespace_URI => Self.Namespace,
            Local_Name    => Name);
         Self.Is_Tight := Is_Tight;
      end List;

      overriding procedure List_Item
        (Self  : in out Visitor;
         Block : in out Markdown.List_Items.List_Item)
      is
         Empty : XML.SAX.Attributes.SAX_Attributes renames
           XML.SAX.Attributes.Empty_SAX_Attributes;

      begin
         Self.Writer.Start_Element
           (Namespace_URI => Self.Namespace,
            Local_Name    => +"li",
            Attributes    => Empty);
         Block.Visit_Children (Self);
         Self.Writer.End_Element
           (Namespace_URI => Self.Namespace,
            Local_Name    => +"li");
      end List_Item;

      overriding procedure Paragraph
        (Self  : in out Visitor;
         Block : Markdown.Paragraphs.Paragraph)
      is

         Lines : constant League.String_Vectors.Universal_String_Vector :=
           Block.Lines;
         Text  : constant Markdown.Inline_Parsers.Annotated_Text :=
           Self.Parser.Parse_Inlines (Lines);

         Image : Wide_Wide_String := Block.Setext_Heading'Wide_Wide_Image;
         Empty : XML.SAX.Attributes.SAX_Attributes renames
           XML.SAX.Attributes.Empty_SAX_Attributes;

      begin
         if Self.Is_Tight then

            Self.Write_Annotated_Text (Text);

         elsif Block.Setext_Heading = 0 then
            Self.Writer.Start_Element
              (Namespace_URI => Self.Namespace,
               Local_Name    => +"p",
               Attributes    => Empty);

            Self.Write_Annotated_Text (Text);

            Self.Writer.End_Element
              (Namespace_URI => Self.Namespace,
               Local_Name    => +"p");
         else
            Image (1) := 'h';
            Self.Writer.Start_Element
              (Namespace_URI => Self.Namespace,
               Local_Name    => +Image,
               Attributes    => Empty);

            Self.Write_Annotated_Text (Text);

            Self.Writer.End_Element
              (Namespace_URI => Self.Namespace,
               Local_Name    => +Image);
         end if;
      end Paragraph;

      overriding procedure Thematic_Break
        (Self  : in out Visitor;
         Value : Markdown.Thematic_Breaks.Thematic_Break)
      is
         pragma Unreferenced (Value);
         Empty : XML.SAX.Attributes.SAX_Attributes renames
           XML.SAX.Attributes.Empty_SAX_Attributes;

      begin
         Self.Writer.Start_Element
           (Namespace_URI => Self.Namespace,
            Local_Name    => +"hr",
            Attributes    => Empty);

         Self.Writer.End_Element
           (Namespace_URI => Self.Namespace,
            Local_Name    => +"hr");
      end Thematic_Break;

      procedure Write_Annotated_Text
        (Self  : in out Visitor'Class;
         Text  : Markdown.Inline_Parsers.Annotated_Text)
      is
         procedure Write
           (From  : in out Positive;
            Next  : in out Positive;
            Limit : Natural);

         Empty : XML.SAX.Attributes.SAX_Attributes renames
           XML.SAX.Attributes.Empty_SAX_Attributes;

         procedure Write
           (From  : in out Positive;
            Next  : in out Positive;
            Limit : Natural) is
         begin
            while From <= Text.Annotation.Last_Index and then
              Text.Annotation (From).To <= Limit
            loop
               declare
                  Item : constant Markdown.Inline_Parsers.Annotation :=
                    Text.Annotation (From);
               begin
                  if Next <= Item.From - 1 then
                     Self.Writer.Characters
                       (Text.Plain_Text.Slice
                          (Next, Item.From - 1).To_Wide_Wide_String);
                     Next := Item.From;
                  end if;

                  From := From + 1;

                  case Item.Kind is
                     when Markdown.Inline_Parsers.Soft_Line_Break =>
                        Next := Next + 1;
                        Self.Writer.Characters (Self.New_Line);
                     when Markdown.Inline_Parsers.Emphasis =>
                        Self.Writer.Start_Element
                          (Namespace_URI => Self.Namespace,
                           Local_Name    => +"em",
                           Attributes    => Empty);
                        Write (From, Next, Item.To);
                        Self.Writer.End_Element
                          (Namespace_URI => Self.Namespace,
                           Local_Name    => +"em");
                     when Markdown.Inline_Parsers.Strong =>
                        Self.Writer.Start_Element
                          (Namespace_URI => Self.Namespace,
                           Local_Name    => +"strong",
                           Attributes    => Empty);
                        Write (From, Next, Item.To);
                        Self.Writer.End_Element
                          (Namespace_URI => Self.Namespace,
                           Local_Name    => +"strong");
                     when Markdown.Inline_Parsers.Code_Span =>
                        Self.Writer.Start_Element
                          (Namespace_URI => Self.Namespace,
                           Local_Name    => +"code",
                           Attributes    => Empty);
                        Write (From, Next, Item.To);
                        Self.Writer.End_Element
                          (Namespace_URI => Self.Namespace,
                           Local_Name    => +"code");
                     when Markdown.Inline_Parsers.Link =>
                        declare
                           Attr  : XML.SAX.Attributes.SAX_Attributes;
                           Title : constant League.Strings.Universal_String :=
                             Item.Title.Join (' ');
                        begin
                           Attr.Set_Value
                             (Self.Namespace, +"href", Item.Destination);
                           if not Title.Is_Empty then
                              Attr.Set_Value
                                (Self.Namespace, +"title", Title);
                           end if;
                           Self.Writer.Start_Element
                             (Namespace_URI => Self.Namespace,
                              Local_Name    => +"a",
                              Attributes    => Attr);
                           Write (From, Next, Item.To);
                           Self.Writer.End_Element
                             (Namespace_URI => Self.Namespace,
                              Local_Name    => +"a");
                        end;
                     when Markdown.Inline_Parsers.Open_HTML_Tag =>
                        declare
                           Attr : XML.SAX.Attributes.SAX_Attributes;
                        begin
                           for J of Item.Attr loop
                              if J.Value.Is_Empty then
                                 Attr.Set_Value
                                   (Self.Namespace, J.Name, J.Name);
                              else
                                 Attr.Set_Value
                                   (Self.Namespace, J.Name, J.Value.Join (LF));
                              end if;
                           end loop;

                           Self.Writer.Start_Element
                             (Namespace_URI => Self.Namespace,
                              Local_Name    => Item.Tag,
                              Attributes    => Attr);

                           if Item.Is_Empty then
                              Self.Writer.End_Element
                                (Namespace_URI => Self.Namespace,
                                 Local_Name    => Item.Tag);
                           end if;
                        end;
                     when Markdown.Inline_Parsers.Close_HTML_Tag =>
                        Self.Writer.End_Element
                          (Namespace_URI => Self.Namespace,
                           Local_Name    => Item.Tag);
                     when Markdown.Inline_Parsers.HTML_Comment =>
                        Self.Writer.Comment (Item.HTML_Comment.Join (LF));
                     when Markdown.Inline_Parsers
                        .HTML_Processing_Instruction =>
                           Self.Writer.Processing_Instruction
                             (Item.HTML_PI.Join (LF));
                     when Markdown.Inline_Parsers.HTML_Declaration =>
                        Self.Writer.Comment (Item.HTML_Decl.Join (LF));
                     when Markdown.Inline_Parsers.HTML_CDATA =>
                        Self.Writer.Start_CDATA;
                        Self.Writer.Characters (Item.HTML_CDATA.Join (LF));
                        Self.Writer.End_CDATA;
                  end case;
               end;
            end loop;

            if Next <= Limit then
               Self.Writer.Characters
                 (Text.Plain_Text.Slice
                    (Next, Limit).To_Wide_Wide_String);
               Next := Limit + 1;
            end if;
         end Write;

         Next  : Positive := 1;  --  Position in Text,Plain_Text
         From  : Positive := Text.Annotation.First_Index;
      begin
         Write (From, Next, Text.Plain_Text.Length);
      end Write_Annotated_Text;

   end Visitors;

   ------------------
   -- Trim_Doctype --
   ------------------

   function Trim_Doctype (Text : League.Strings.Universal_String)
     return League.Strings.Universal_String
   is
      Pos : constant Positive := Text.Index (">");
      Result : League.Strings.Universal_String :=
        Text.Slice (Pos + 1, Text.Length - 8);
   begin
      if Result.Ends_With (New_Line) then
         Result := Result.Head_To (Result.Length - 1);
      end if;

      return Result;
   end Trim_Doctype;

   Visitor : Visitors.Visitor;
   Parser  : Markdown.Parsers.Parser renames Visitor.Parser;
   Empty   : XML.SAX.Attributes.SAX_Attributes;
   Result  : League.Strings.Universal_String;
begin
   Parser.Register (Markdown.ATX_Headings.Filter'Access);
   Parser.Register (Markdown.Blockquotes.Filter'Access);
   Parser.Register (Markdown.Thematic_Breaks.Filter'Access);
   Parser.Register (Markdown.Indented_Code_Blocks.Filter'Access);
   Parser.Register (Markdown.Fenced_Code_Blocks.Filter'Access);
   Parser.Register (Markdown.HTML_Blocks.Filter'Access);
   Parser.Register (Markdown.Link_Reference_Definitions.Filter'Access);
   Parser.Register (Markdown.List_Items.Filter'Access);
   Parser.Register (Markdown.Paragraphs.Filter'Access);

   Visitor.Writer.Set_Output_Destination (Visitor.Output'Unchecked_Access);
   Visitor.New_Line := +New_Line;
   Visitor.Namespace := +"http://www.w3.org/1999/xhtml";

   while not Ada.Wide_Wide_Text_IO.End_Of_File loop
      declare
         Line  : constant League.Strings.Universal_String :=
           League.Strings.To_Universal_String
             (Ada.Wide_Wide_Text_IO.Get_Line);
      begin
         Parser.Append_Line (Line);
      end;
   end loop;

   Parser.Stop;

   Visitor.Writer.Start_Document;
   Visitor.Writer.Start_Prefix_Mapping
     (Prefix        => +"",
      Namespace_URI => Visitor.Namespace);
   Visitor.Writer.Start_Element
     (Namespace_URI => Visitor.Namespace,
      Local_Name    => +"html",
      Attributes    => Empty);
   Parser.Visit (Visitor);
   Visitor.Writer.End_Element
     (Namespace_URI => Visitor.Namespace,
      Local_Name    => +"html");
   Visitor.Writer.End_Document;

   Result := Trim_Doctype (Visitor.Output.Get_Text);

   if not Result.Is_Empty then
      Ada.Wide_Wide_Text_IO.Put_Line (Result.To_Wide_Wide_String);
   end if;
end MD_Driver;
