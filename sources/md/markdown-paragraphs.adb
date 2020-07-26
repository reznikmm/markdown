--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Markdown.Visitors;

with League.Regexps;
with League.Strings;

package body Markdown.Paragraphs is

   Pattern : constant League.Regexps.Regexp_Pattern :=
     League.Regexps.Compile
       (League.Strings.To_Universal_String
          ("^\ {0,3}(\-{2,}|\=+)\ *$"));

   -----------------
   -- Append_Line --
   -----------------

   overriding procedure Append_Line
     (Self : in out Paragraph;
      Line : Markdown.Blocks.Text_Line;
      CIP  : Can_Interrupt_Paragraph;
      Ok   : in out Boolean)
   is
      Match : constant League.Regexps.Regexp_Match :=
        Pattern.Find_Match (Line.Line);
   begin
      Ok := not Line.Line.Is_Empty and Self.Setext_Level = 0;

      if Ok then
         if Match.Is_Matched then
            if Line.Line.Element (Match.First_Index (1)).To_Wide_Wide_Character
              = '-'
            then
               Self.Setext_Level := 2;
            else
               Self.Setext_Level := 1;
            end if;
         elsif CIP then
            Ok := False;
         else
            Self.Lines.Append (Line.Line);
         end if;
      end if;
   end Append_Line;

   ------------
   -- Create --
   ------------

   overriding function Create
     (Line : not null access Markdown.Blocks.Text_Line)
      return Paragraph is
   begin
      pragma Assert (not Line.Line.Is_Empty);

      return Result : Paragraph do
         Result.Lines.Append (Line.Line);
         Line.Line.Clear;
      end return;
   end Create;

   ------------
   -- Filter --
   ------------

   procedure Filter
     (Line : Markdown.Blocks.Text_Line;
      Tag  : in out Ada.Tags.Tag;
      CIP  : out Can_Interrupt_Paragraph) is
   begin
      if not Line.Line.Is_Empty then
         Tag := Paragraph'Tag;
         CIP := False;
      end if;
   end Filter;

   -----------
   -- Lines --
   -----------

   function Lines (Self : Paragraph'Class)
     return League.String_Vectors.Universal_String_Vector is
   begin
      return Self.Lines;
   end Lines;

   --------------------
   -- Setext_Heading --
   --------------------

   function Setext_Heading (Self : Paragraph'Class) return Heading_Level is
   begin
      return Self.Setext_Level;
   end Setext_Heading;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : Paragraph;
      Visitor : in out Markdown.Visitors.Visitor'Class) is
   begin
      Visitor.Paragraph (Self);
   end Visit;

end Markdown.Paragraphs;
