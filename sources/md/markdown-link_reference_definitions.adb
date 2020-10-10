--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with League.Regexps;

with Markdown.Common_Patterns;
with Markdown.Visitors;

package body Markdown.Link_Reference_Definitions is

   function "+" (Text : Wide_Wide_String)
     return League.Regexps.Regexp_Pattern
       is (League.Regexps.Compile (League.Strings.To_Universal_String (Text)));

   Blank_Pattern : League.Regexps.Regexp_Pattern renames
     Markdown.Common_Patterns.Blank_Pattern;

   Link_Label : Wide_Wide_String renames Markdown.Common_Patterns.Link_Label;
   --  Groups: 2

   Link_Title : Wide_Wide_String renames Markdown.Common_Patterns.Link_Title;
   --  Groups: 4

   Label_Pattern : constant League.Regexps.Regexp_Pattern :=
     +("^\ {0,3}(" & Link_Label & ")\:" &
       --  Grps:1    2,3
       "[\ \t\n\v\f\r\>]*"
      );

   Space_Pattern : constant League.Regexps.Regexp_Pattern :=
     +("^[\ \t\n\v\f\r\>]*");

   Title_Pattern : constant League.Regexps.Regexp_Pattern :=
     +("^[\ \t\n\v\f\r\>]*(" & Link_Title & ")?[\ \t\n\v\f\r\>]*$");
   --  Groups:            1    2,3,4,5

   Title_Close_Group : constant array (Positive range 1 .. 3) of Positive :=
     (2, 3, 5);  --  Close title group numbers

   -----------------
   -- Append_Line --
   -----------------

   overriding procedure Append_Line
     (Self : in out Link_Reference_Definition;
      Line : Markdown.Blocks.Text_Line;
      CIP  : Can_Interrupt_Paragraph;
      Ok   : in out Boolean)
   is
      pragma Unreferenced (CIP);
      Last      : Natural;
      Text      : League.Strings.Universal_String := Line.Line;
      Match     : League.Regexps.Regexp_Match;
   begin
      if Self.Has_Title then
         Ok := False;
      elsif not Self.Has_URL then
         Match := Space_Pattern.Find_Match (Text);
         pragma Assert (Match.Is_Matched);

         Text := Text.Tail_From (Match.Last_Index + 1);  --  drop spaces

         Markdown.Common_Patterns.Parse_Link_Destination
           (Line => Text,
            Last => Last,
            URL  => Self.URL);

         Self.Has_URL := Last > 0;

         if Last = 0 then
            Ok := False;
         elsif Last = Text.Length then
            Ok := True;
         else
            Text := Text.Tail_From (Last + 1);  --  drop link dest
            Match := Title_Pattern.Find_Match (Text);

            Ok := Match.Is_Matched;

            if Ok then
               Self.Has_Title :=
                 (for some J of Title_Close_Group =>
                    Match.Last_Index (J) >= Match.First_Index (J));

               Text := Match.Capture (1);

               if Self.Has_Title then
                  Self.Title.Append (Text.Slice (2, Text.Length - 1));
               elsif not Text.Is_Empty then
                  Self.Start := Text (1).To_Wide_Wide_Character;
                  Self.Title.Append (Text.Tail_From (2));
               end if;
            else
               null;  --  FIXME: Turn the block into a paragraph?
            end if;
         end if;
      elsif Self.Title.Is_Empty then
         Match := Title_Pattern.Find_Match (Text);
         Ok := Match.Is_Matched;

         if Ok then
            Self.Has_Title :=
              (for some J of Title_Close_Group =>
                 Match.Last_Index (J) >= Match.First_Index (J));

            Text := Match.Capture (1);

            if Self.Has_Title then
               Self.Title.Append (Text.Slice (2, Text.Length - 1));
            elsif not Text.Is_Empty then
               Self.Start := Text (1).To_Wide_Wide_Character;
               Self.Title.Append (Text.Tail_From (2));
            end if;
         else
            null;  --  FIXME: Turn the block into a paragraph?
         end if;
      elsif Blank_Pattern.Find_Match (Text).Is_Matched then
         Ok := False;
         null;  --  FIXME: Turn the block into a paragraph?
      else
         declare
            Escape : Boolean := False;
            To     : Natural := 0;
            Stop   : constant Wide_Wide_Character :=
              (if Self.Start = '(' then ')' else Self.Start);
         begin
            for J in 1 .. Text.Length loop
               if Escape then
                  Escape := False;
               elsif Text (J).To_Wide_Wide_Character = '\' then
                  Escape := True;
               elsif Text (J).To_Wide_Wide_Character = Stop then
                  Self.Has_Title := True;
                  To := J;
                  exit;
               end if;
            end loop;

            Ok := not Escape and then To in 0 | Text.Length;

            if Self.Has_Title and To > 1 then
               Self.Title.Append (Text.Tail_From (To - 1));
            elsif Ok then
               Self.Title.Append (Text);
            else
               null;  --  FIXME: Turn the block into a paragraph?
            end if;
         end;
      end if;

   end Append_Line;

   ------------
   -- Create --
   ------------

   overriding function Create
     (Line : not null access Markdown.Blocks.Text_Line)
     return Link_Reference_Definition
   is
      Last   : Natural;
      Text   : League.Strings.Universal_String := Line.Line;

      Match : League.Regexps.Regexp_Match :=
        Label_Pattern.Find_Match (Text);

   begin
      pragma Assert (Match.Is_Matched);

      return Self : Link_Reference_Definition do

         Self.Label := Match.Capture (1);

         if Match.Last_Index < Text.Length then
            Text := Text.Tail_From (Match.Last_Index + 1);  --  drop label

            Markdown.Common_Patterns.Parse_Link_Destination
              (Line => Text,
               Last => Last,
               URL  => Self.URL);

            Self.Has_URL := Last > 0;
            pragma Assert (Last > 0);

            if Last < Text.Length then
               Text := Text.Tail_From (Last + 1);  --  drop link dest
               Match := Title_Pattern.Find_Match (Text);

               Self.Has_Title :=
                 (for some J of Title_Close_Group =>
                    Match.Last_Index (J) >= Match.First_Index (J));

               Text := Match.Capture (1);

               if Self.Has_Title then
                  Self.Title.Append (Text.Slice (2, Text.Length - 1));
               elsif not Text.Is_Empty then
                  Self.Start := Text (1).To_Wide_Wide_Character;
                  Self.Title.Append (Text.Tail_From (2));
               end if;
            end if;
         end if;

         Line.Line.Clear;
      end return;
   end Create;

   -----------------
   -- Destination --
   -----------------

   function Destination
     (Self : Link_Reference_Definition'Class)
       return League.Strings.Universal_String is
   begin
      return Self.URL;
   end Destination;

   ------------
   -- Filter --
   ------------

   procedure Filter
     (Line : Markdown.Blocks.Text_Line;
      Tag  : in out Ada.Tags.Tag;
      CIP  :    out Can_Interrupt_Paragraph)
   is
      Ignore : League.Strings.Universal_String;
      Last   : Natural;
      Text   : League.Strings.Universal_String := Line.Line;
      Match  : constant League.Regexps.Regexp_Match :=
        Label_Pattern.Find_Match (Line.Line);
   begin
      CIP := False;

      if Match.Is_Matched then
         if Match.Last_Index < Text.Length then
            Text := Text.Tail_From (Match.Last_Index + 1);  --  drop label
            Markdown.Common_Patterns.Parse_Link_Destination
              (Line => Text,
               Last => Last,
               URL  => Ignore);

            if Last = 0 then
               return;  --  Wrong link destination
            elsif Last < Text.Length then
               Text := Text.Tail_From (Last + 1);  --  drop link dest

               if not Title_Pattern.Find_Match (Text).Is_Matched then
                  return;  --  Wrong link title
               end if;
            end if;
         end if;

         Tag := Link_Reference_Definition'Tag;
      end if;
   end Filter;

   -----------
   -- Label --
   -----------

   function Label
     (Self : Link_Reference_Definition'Class)
       return League.Strings.Universal_String is
   begin
      return Self.Label;
   end Label;

   -----------
   -- Title --
   -----------

   function Title
     (Self : Link_Reference_Definition'Class)
       return League.String_Vectors.Universal_String_Vector is
   begin
      return Self.Title;
   end Title;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : in out Link_Reference_Definition;
      Visitor : in out Markdown.Visitors.Visitor'Class) is
   begin
      Visitor.Link_Reference_Definition (Self);
   end Visit;

end Markdown.Link_Reference_Definitions;
