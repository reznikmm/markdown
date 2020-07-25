--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with League.Regexps;

with Markdown.Visitors;

package body Markdown.ATX_Headings is

   Prefix : constant League.Regexps.Regexp_Pattern :=
     League.Regexps.Compile
       (League.Strings.To_Universal_String
          ("^\ {0,3}(\#{1,6})(\ |$)"));

   Suffix : constant League.Regexps.Regexp_Pattern :=
     League.Regexps.Compile
       (League.Strings.To_Universal_String
          ("\ +\#*\ *$"));

   ------------
   -- Create --
   ------------

   overriding function Create
     (Line : not null access Markdown.Blocks.Text_Line) return ATX_Heading
   is
      Prefix_Match : constant League.Regexps.Regexp_Match :=
        Prefix.Find_Match (Line.Line);
      Suffix_Match : constant League.Regexps.Regexp_Match :=
        Suffix.Find_Match (Line.Line);
   begin
      pragma Assert (Prefix_Match.Is_Matched);

      return Result : ATX_Heading do
         Result.Level :=
           1 + Prefix_Match.Last_Index (1) - Prefix_Match.First_Index (1);

         if Suffix_Match.Is_Matched and then
           Suffix_Match.Last_Index >= Suffix_Match.First_Index
         then
            Result.Title := Line.Line.Slice
              (Prefix_Match.Last_Index + 1, Suffix_Match.First_Index - 1);
         else
            Result.Title := Line.Line.Tail_From (Prefix_Match.Last_Index + 1);
         end if;

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
      Prefix_Match : constant League.Regexps.Regexp_Match :=
        Prefix.Find_Match (Line.Line);
   begin
      if Prefix_Match.Is_Matched then
         Tag := ATX_Heading'Tag;
         CIP := True;
      end if;
   end Filter;

   -----------
   -- Level --
   -----------

   function Level (Self : ATX_Heading'Class) return Heading_Level is
   begin
      return Self.Level;
   end Level;

   -----------
   -- Title --
   -----------

   function Title
     (Self : ATX_Heading'Class)
     return League.Strings.Universal_String is
   begin
      return Self.Title;
   end Title;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : ATX_Heading;
      Visitor : in out Markdown.Visitors.Visitor'Class) is
   begin
      Visitor.ATX_Heading (Self);
   end Visit;

end Markdown.ATX_Headings;
