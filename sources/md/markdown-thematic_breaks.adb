--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with League.Regexps;
with League.Strings;

with Markdown.Visitors;

package body Markdown.Thematic_Breaks is

   Pattern : constant League.Regexps.Regexp_Pattern :=
     League.Regexps.Compile
       (League.Strings.To_Universal_String
          ("^\ {0,3}((\-\ *){3,}|(\*\ *){3,}|(_\ *){3,})$"));


   ------------
   -- Create --
   ------------

   overriding function Create
     (Line : not null access Markdown.Blocks.Text_Line) return Thematic_Break
   is
      Match : constant League.Regexps.Regexp_Match :=
        Pattern.Find_Match (Line.Line);
   begin
      pragma Assert (Match.Is_Matched);
      Line.Line.Clear;

      return Result : Thematic_Break;
   end Create;

   ------------
   -- Filter --
   ------------

   procedure Filter
     (Line : Markdown.Blocks.Text_Line;
      Tag  : in out Ada.Tags.Tag;
      CIP  : out Can_Interrupt_Paragraph)
   is
      Match : constant League.Regexps.Regexp_Match :=
        Pattern.Find_Match (Line.Line);
   begin
      if Match.Is_Matched then
         Tag := Thematic_Break'Tag;
         CIP := True;
      end if;
   end Filter;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : in out Thematic_Break;
      Visitor : in out Markdown.Visitors.Visitor'Class) is
   begin
      Visitor.Thematic_Break (Self);
   end Visit;

end Markdown.Thematic_Breaks;
