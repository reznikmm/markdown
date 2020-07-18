--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Markdown.Visitors;

package body Markdown.Paragraphs is

   ------------------
   -- Append_Child --
   ------------------

   overriding procedure Append_Child
     (Self  : in out Paragraph;
      Child : not null Markdown.Blocks.Block_Access) is
   begin
      raise Program_Error;
   end Append_Child;

   -----------------
   -- Append_Line --
   -----------------

   overriding procedure Append_Line
     (Self   : in out Paragraph;
      Line   : League.Strings.Universal_String;
      From   : Positive;
      Column : Positive)
   is
      pragma Unreferenced (Column);
   begin
      Self.Lines.Append (Line.Tail_From (From));
   end Append_Line;

   ----------------------------------
   -- Consume_Continuation_Markers --
   ----------------------------------

   overriding procedure Consume_Continuation_Markers
     (Self   : Paragraph;
      Line   : League.Strings.Universal_String;
      From   : in out Positive;
      Column : in out Positive;
      Match  : out Continuation_Kind)
   is
      pragma Unreferenced (Self, Column);
   begin
      if From <= Line.Length then
         Match := Consumed;
      else
         Match := No_Match;
      end if;
   end Consume_Continuation_Markers;

   ------------
   -- Create --
   ------------

   overriding function Create
     (Line : not null access Markdown.Blocks.Text_Line)
      return Paragraph is
      pragma Unreferenced (Line);
   begin
      return Result : Paragraph;
   end Create;

   ------------
   -- Filter --
   ------------

   procedure Filter
     (Line   : League.Strings.Universal_String;
      From   : in out Positive;
      Column : in out Positive;
      Tag    : in out Ada.Tags.Tag) is
      pragma Unreferenced (Column);
   begin
      if From <= Line.Length then
         From := From + 0;  --  To avoid a compiler warning
         Tag := Paragraph'Tag;
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
