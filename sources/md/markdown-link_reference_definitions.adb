--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with League.Regexps;

with Markdown.Visitors;

package body Markdown.Link_Reference_Definitions is

   function "+" (Text : Wide_Wide_String)
     return League.Regexps.Regexp_Pattern
       is (League.Regexps.Compile (League.Strings.To_Universal_String (Text)));

   Blank_Pattern : constant League.Regexps.Regexp_Pattern :=
     League.Regexps.Compile
       (League.Strings.To_Universal_String
          ("^[\ \t]*$"));

   Link_Label : constant Wide_Wide_String :=
       "\[[\ \t\n\v\f\r]*((\\\[|[^\]\ \t\n\v\f\r])[\ \t\n\v\f\r\>]*)+\]";
   --    [ space        *((\  [| [^]space       ]) space          *)+ ]
   --  Groups:           12

   Link_Destination  : constant Wide_Wide_String :=
     "\<[^\<\>]*\>" &
     "|([^\<\ \\]|\\.)([^\ \\]|\\.)*";
   --  1              2      <--  Groups

   Link_Title : constant Wide_Wide_String :=
     "\""[^\""]*(\"")?" &              --  Group: 1
     "|\'[^\']*(\')?" &                --  Group: 2
     "|\(([^\(\)]|\\[\(\)])*(\))?";    --  Group: 3

   Label_Pattern : constant League.Regexps.Regexp_Pattern :=
     +("^\ {0,3}(" & Link_Label & ")\:" &
       --  Grps:1    2,3
       "[\ \t\n\v\f\r\>]*(" & Link_Destination & "(" &
       --  Groups:       4    5,6                 7
       "[\ \t\n\v\f\r\>]+(" & Link_Title &
       --  Groups:       8    9,10,11
       "))?)?[\ \t\n\v\f\r\>]*$"
      );

   Destination_Pattern : constant League.Regexps.Regexp_Pattern :=
     +("^[\ \t\n\v\f\r\>]*(" & Link_Destination & "(" &
       --  Groups:       1    2,3                 4
       "[\ \t\n\v\f\r\>]+(" & Link_Title &
       --  Groups:       5    6,7,8
       "))?)[\ \t\n\v\f\r\>]*$");

   Title_Pattern : constant League.Regexps.Regexp_Pattern :=
     +("^[\ \t\n\v\f\r\>]*(" & Link_Title & ")");
   --  Groups:            1    2,3,4

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
      Match     : League.Regexps.Regexp_Match;
      Has_Title : Boolean;
   begin
      if Self.URL.Is_Empty then
         Match := Destination_Pattern.Find_Match (Line.Line);
         Ok := Match.Is_Matched;

         if Ok then
            Has_Title := Match.Last_Index (4) >= Match.First_Index (4);

            if Has_Title then
               Self.URL := Line.Line.Slice
                 (Match.First_Index (1), Match.Last_Index (4) - 1);

               for J in 6 .. 8 loop
                  if Match.Last_Index (J) >= Match.First_Index (J) then
                     Self.Has_Title := True;
                     exit;
                  end if;
               end loop;

               Self.Start :=
                 Line.Line (Match.First_Index (5)).To_Wide_Wide_Character;

               if Self.Has_Title then
                  Self.Title.Append
                    (Line.Line.Slice
                       (Match.First_Index (5) + 1, Match.Last_Index (5) - 1));
               else
                  Self.Title.Append
                    (Line.Line.Slice
                       (Match.First_Index (5) + 1, Match.Last_Index (5)));
               end if;
            else
               Self.URL := Line.Line.Slice
                 (Match.First_Index (1), Match.Last_Index (1));
            end if;
         else
            null;  --  FIXME: Turn the block into a paragraph?
         end if;
      elsif Self.Title.Is_Empty then
         Match := Title_Pattern.Find_Match (Line.Line);
         Ok := Match.Is_Matched;

         if Ok then
            for J in 2 .. 4 loop
               if Match.Last_Index (J) >= Match.First_Index (J) then
                  Self.Has_Title := True;
                  exit;
               end if;
            end loop;

            Self.Start :=
              Line.Line (Match.First_Index (1)).To_Wide_Wide_Character;

            if Self.Has_Title then
               Self.Title.Append
                 (Line.Line.Slice
                    (Match.First_Index (1) + 1, Match.Last_Index (1) - 1));
            else
               Self.Title.Append
                 (Line.Line.Slice
                    (Match.First_Index (1) + 1, Match.Last_Index (1)));
            end if;
         else
            null;  --  FIXME: Turn the block into a paragraph?
         end if;
      elsif Blank_Pattern.Find_Match (Line.Line).Is_Matched then
         Ok := False;
         null;  --  FIXME: Turn the block into a paragraph?
      else
         declare
            Escape : Boolean := False;
            To     : Natural := 0;
            Stop   : constant Wide_Wide_Character :=
              (if Self.Start = '(' then ')' else Self.Start);
         begin
            for J in 1 .. Line.Line.Length loop
               if Escape then
                  Escape := False;
               elsif Line.Line (J).To_Wide_Wide_Character = '\' then
                  Escape := True;
               elsif Line.Line (J).To_Wide_Wide_Character = Stop then
                  To := J;
                  exit;
               end if;
            end loop;

            Ok := not Escape and then Blank_Pattern.Find_Match
              (Line.Line.Tail_From (To + 1)).Is_Matched;

            if Ok then
               Self.Has_Title := True;
               Self.Title.Append (Line.Line.Tail_From (To - 1));
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
      Match : constant League.Regexps.Regexp_Match :=
        Label_Pattern.Find_Match (Line.Line);

      Has_Destination : constant Boolean :=
        Match.Last_Index (4) >= Match.First_Index (4);
      Has_Title : constant Boolean := Has_Destination and then
        Match.Last_Index (8) >= Match.First_Index (8);
   begin
      pragma Assert (Match.Is_Matched);

      return Self : Link_Reference_Definition do

         Self.Label := Line.Line.Slice
           (Match.First_Index (1), Match.Last_Index (1));

         if Has_Destination then
            if Has_Title then
               Self.URL := Line.Line.Slice
                 (Match.First_Index (4), Match.First_Index (7) - 1);

               for J in 9 .. 11 loop
                  if Match.Last_Index (J) >= Match.First_Index (J) then
                     Self.Has_Title := True;
                     exit;
                  end if;
               end loop;

               if Self.Has_Title then
                  Self.Start :=
                    Line.Line (Match.First_Index (8)).To_Wide_Wide_Character;

                  Self.Title.Append
                    (Line.Line.Slice
                       (Match.First_Index (8) + 1, Match.Last_Index (8) - 1));
               else
                  Self.Title.Append
                    (Line.Line.Slice
                       (Match.First_Index (8) + 1, Match.Last_Index (8)));
               end if;
            else
               Self.URL := Line.Line.Slice
                 (Match.First_Index (4), Match.Last_Index (4));
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
      Match : constant League.Regexps.Regexp_Match :=
        Label_Pattern.Find_Match (Line.Line);
   begin
      if Match.Is_Matched then
         Tag := Link_Reference_Definition'Tag;
         CIP := False;
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
     (Self    : Link_Reference_Definition;
      Visitor : in out Markdown.Visitors.Visitor'Class) is
   begin
      Visitor.Link_Reference_Definition (Self);
   end Visit;

end Markdown.Link_Reference_Definitions;
