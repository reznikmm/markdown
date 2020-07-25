--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Markdown.Visitors;

package body Markdown.Paragraphs is

   -----------------
   -- Append_Line --
   -----------------

   overriding procedure Append_Line
     (Self : in out Paragraph;
      Line : Markdown.Blocks.Text_Line;
      CIP  : Can_Interrupt_Paragraph;
      Ok   : in out Boolean) is
   begin
      Ok := not Line.Line.Is_Empty and not CIP;

      if Ok then
         Self.Lines.Append (Line.Line);
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
