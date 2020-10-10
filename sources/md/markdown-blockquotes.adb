--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with League.Regexps;
with League.Strings;

with Markdown.Visitors;

package body Markdown.Blockquotes is

   Prefix : constant League.Regexps.Regexp_Pattern :=
     League.Regexps.Compile
       (League.Strings.To_Universal_String
          ("^\ {0,3}\>\ ?"));

   ----------------------------------
   -- Consume_Continuation_Markers --
   ----------------------------------

   overriding procedure Consume_Continuation_Markers
     (Self  : in out Blockquote;
      Line  : in out Markdown.Blocks.Text_Line;
      Match : out Boolean)
   is
      pragma Unreferenced (Self);
      Matched : constant League.Regexps.Regexp_Match :=
        Prefix.Find_Match (Line.Line);
   begin
      Match := Matched.Is_Matched;

      if Match then
         Line.Line := Line.Line.Tail_From (Matched.Last_Index + 1);
         Line.Column := Line.Column + Matched.Last_Index;
      end if;
   end Consume_Continuation_Markers;

   ------------
   -- Create --
   ------------

   overriding function Create
     (Line : not null access Markdown.Blocks.Text_Line) return Blockquote
   is
      Matched : constant League.Regexps.Regexp_Match :=
        Prefix.Find_Match (Line.Line);
   begin
      pragma Assert (Matched.Is_Matched);
      Line.Line := Line.Line.Tail_From (Matched.Last_Index + 1);
      Line.Column := Line.Column + Matched.Last_Index;
      return Result : Blockquote;
   end Create;

   ------------
   -- Filter --
   ------------

   procedure Filter
     (Line : Markdown.Blocks.Text_Line;
      Tag  : in out Ada.Tags.Tag;
      CIP  : out Can_Interrupt_Paragraph)
   is
      Matched : constant League.Regexps.Regexp_Match :=
        Prefix.Find_Match (Line.Line);
   begin
      if Matched.Is_Matched then
         Tag := Blockquote'Tag;
         CIP := True;
      end if;
   end Filter;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : in out Blockquote;
      Visitor : in out Markdown.Visitors.Visitor'Class) is
   begin
      Visitor.Blockquote (Self);
   end Visit;

end Markdown.Blockquotes;
