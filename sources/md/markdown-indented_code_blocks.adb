--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with League.Regexps;
with League.Strings;

with Markdown.Visitors;

package body Markdown.Indented_Code_Blocks is

   Blank_Pattern : constant League.Regexps.Regexp_Pattern :=
     League.Regexps.Compile
       (League.Strings.To_Universal_String
          ("^\ {0,3}$"));


   -----------------
   -- Append_Line --
   -----------------

   overriding procedure Append_Line
     (Self : in out Indented_Code_Block;
      Line : Markdown.Blocks.Text_Line;
      CIP  : Can_Interrupt_Paragraph;
      Ok   : in out Boolean)
   is
      pragma Unreferenced (CIP);
      Match : constant League.Regexps.Regexp_Match :=
        Blank_Pattern.Find_Match (Line.Line);
   begin
      if Line.Line.Starts_With ("    ") then
         Ok := True;
         Self.Lines.Append (Line.Line.Tail_From (5));
      elsif Match.Is_Matched then
         Ok := True;
         Self.Lines.Append (League.Strings.Empty_Universal_String);
      else
         Ok := False;
      end if;
   end Append_Line;

   ------------
   -- Create --
   ------------

   overriding function Create
     (Line : not null access Markdown.Blocks.Text_Line)
      return Indented_Code_Block
   is
   begin
      pragma Assert (Line.Line.Starts_With ("    "));

      return Result : Indented_Code_Block do
         Result.Lines.Append (Line.Line.Tail_From (5));
         Line.Line.Clear;
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
      if Line.Line.Starts_With ("    ") then
         Tag := Indented_Code_Block'Tag;
         CIP := False;
      end if;
   end Filter;

   -----------
   -- Lines --
   -----------

   function Lines (Self : Indented_Code_Block'Class)
     return League.String_Vectors.Universal_String_Vector
   is
      First : Natural;
      Last : Natural;
   begin
      for J in 1 .. Self.Lines.Length loop
         First := J;

         exit when not Self.Lines (J).Is_Empty;
      end loop;

      for J in reverse 1 .. Self.Lines.Length loop
         Last := J;

         exit when not Self.Lines (J).Is_Empty;
      end loop;

      return Self.Lines.Slice (First, Last);
   end Lines;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : in out Indented_Code_Block;
      Visitor : in out Markdown.Visitors.Visitor'Class)
   is
   begin
      Visitor.Indented_Code_Block (Self);
   end Visit;

end Markdown.Indented_Code_Blocks;
