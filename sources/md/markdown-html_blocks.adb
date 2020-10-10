--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with League.Regexps;
with League.Strings;

with Markdown.Visitors;

package body Markdown.HTML_Blocks is
   function "+" (Text : Wide_Wide_String)
     return League.Regexps.Regexp_Pattern
       is (League.Regexps.Compile (League.Strings.To_Universal_String (Text)));

   Open_Pattern :
     constant array (HTML_Block_Kind) of League.Regexps.Regexp_Pattern :=
       (1 => +"^\ {0,3}\<(script|pre|style)([\ \t\n\v\f\r\>]|$)",
        2 => +"^\ {0,3}\<\!\-\-",
        3 => +"^\ {0,3}\<\?",
        4 => +"^\ {0,3}\<\![A-Z]",
        5 => +"^\ {0,3}\<\!\[CDATA\[",
        6 => +("^\ {0,3}\<\/?(" &
          "address|" &
          "article|" &
          "aside|" &
          "base|" &
          "basefont|" &
          "blockquote|" &
          "body|" &
          "caption|" &
          "center|" &
          "col|" &
          "colgroup|" &
          "dd|" &
          "details|" &
          "dialog|" &
          "dir|" &
          "div|" &
          "dl|" &
          "dt|" &
          "fieldset|" &
          "figcaption|" &
          "figure|" &
          "footer|" &
          "form|" &
          "frame|" &
          "frameset|" &
          "h1|" &
          "h2|" &
          "h3|" &
          "h4|" &
          "h5|" &
          "h6|" &
          "head|" &
          "header|" &
          "hr|" &
          "html|" &
          "iframe|" &
          "legend|" &
          "li|" &
          "link|" &
          "main|" &
          "menu|" &
          "menuitem|" &
          "nav|" &
          "noframes|" &
          "ol|" &
          "optgroup|" &
          "option|" &
          "p|" &
          "param|" &
          "section|" &
          "source|" &
          "summary|" &
          "table|" &
          "tbody|" &
          "td|" &
          "tfoot|" &
          "th|" &
          "thead|" &
          "title|" &
          "tr|" &
          "track|" &
          "ul)([\ \t\n\v\f\r]|\/?\>|$)"),
        7 => +("^\ {0,3}(\<" &
          "[a-zA-Z][a-zA-Z0-9\-]*" &  --  tag name
            "([\ \t\n\v\f\r]+[a-zA-Z_\:][a-zA-Z0-9_\.\:\-]*" &  --  attr name
            "([\ \t\n\v\f\r]*\=[\ \t\n\v\f\r]*" &
            "([^\ \t\n\v\f\r\""\'\=\<\>\`]+|\'[^\']*\'|\""[^\""]*\"")" &  -- va
            ")?" &
            ")*" &
            "[\ \t\n\v\f\r]*\/?\>" &
            "|\<\/" &
            "[a-zA-Z][a-zA-Z0-9\-]*" &  --  tag name
            "[\ \t\n\v\f\r]*\>" &
            ")([\ \t\n\v\f\r]|$)"
         ));

   Close_Pattern : constant array (HTML_Block_Kind range 1 .. 5) of
     League.Regexps.Regexp_Pattern :=
       (1 => +"\<\/script\>|\<\/pre\>|\<\/style\>",
        2 => +"\-\-\>",
        3 => +"\?\>",
        4 => +"\>",
        5 => +"\]\]\>");

   Blank_Pattern : constant League.Regexps.Regexp_Pattern :=
     League.Regexps.Compile
       (League.Strings.To_Universal_String
          ("^[\ \t]*$"));

   -----------------
   -- Append_Line --
   -----------------

   overriding procedure Append_Line
     (Self : in out HTML_Block;
      Line : Markdown.Blocks.Text_Line;
      CIP  : Can_Interrupt_Paragraph;
      Ok   : in out Boolean)
   is
      pragma Unreferenced (CIP);
   begin
      if Self.Closed then
         Ok := False;

         return;
      elsif Self.Kind in Close_Pattern'Range then
         Self.Lines.Append (Line.Line);
         Self.Closed := Close_Pattern (Self.Kind).Find_Match
           (Line.Line).Is_Matched;
      elsif Blank_Pattern.Find_Match (Line.Line).Is_Matched then
         Self.Closed := True;
      else
         Self.Lines.Append (Line.Line);
      end if;

      Ok := True;
   end Append_Line;

   ------------
   -- Create --
   ------------

   overriding function Create
     (Line : not null access Markdown.Blocks.Text_Line) return HTML_Block is
   begin
      return Result : HTML_Block do
         for J in Open_Pattern'Range loop
            declare
               Match : constant League.Regexps.Regexp_Match :=
                 Open_Pattern (J).Find_Match (Line.Line);
            begin
               if Match.Is_Matched then
                  Result.Lines.Append (Line.Line);
                  Result.Kind := J;
                  Result.Closed := J in Close_Pattern'Range and then
                    Close_Pattern (J).Find_Match (Line.Line).Is_Matched;
                  Line.Line.Clear;
                  exit;
               end if;
            end;
         end loop;
      end return;
   end Create;

   ------------
   -- Filter --
   ------------

   procedure Filter
     (Line : Markdown.Blocks.Text_Line;
      Tag  : in out Ada.Tags.Tag;
      CIP  : out Can_Interrupt_Paragraph)
   is
   begin
      for J in Open_Pattern'Range loop
         declare
            Pattern : League.Regexps.Regexp_Pattern renames Open_Pattern (J);
            Match : constant League.Regexps.Regexp_Match :=
              Pattern.Find_Match (Line.Line);
         begin
            if Match.Is_Matched then
               Tag := HTML_Block'Tag;
               --  All types of HTML blocks except type 7 may interrupt a
               --  paragraph.
               CIP := J /= 7;
               return;
            end if;
         end;
      end loop;
   end Filter;

   -----------
   -- Lines --
   -----------

   function Lines (Self : HTML_Block'Class)
     return League.String_Vectors.Universal_String_Vector
   is
   begin
      return Self.Lines;
   end Lines;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : in out HTML_Block;
      Visitor : in out Markdown.Visitors.Visitor'Class) is
   begin
      Visitor.HTML_Block (Self);
   end Visit;

end Markdown.HTML_Blocks;
