--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Characters.Wide_Wide_Latin_1;

with League.Regexps;

package body Markdown.Inline_Parsers.Raw_HTML is

   function "+" (Text : Wide_Wide_String)
     return League.Regexps.Regexp_Pattern
       is (League.Regexps.Compile (League.Strings.To_Universal_String (Text)));

   Whitespace : constant Wide_Wide_String := "[\ \t\n\v\f\r]";

   Tag_Name : constant Wide_Wide_String :=
     "([a-zA-Z][a-zA-Z0-9\-]*)";
   --  1

   Attribute_Name : constant Wide_Wide_String :=
     "[a-zA-Z_\:][a-zA-Z0-9_\.\:\-]*";

   Unquoted_Attribute_Value : constant Wide_Wide_String :=
     "[^\ \t\n\v\f\r\""\'\=\<\>\`]+";

   Single_Quoted_Attribute_Value : constant Wide_Wide_String :=
     "\'[^\']*\'";

   Double_Quoted_Attribute_Value : constant Wide_Wide_String :=
     "\""[^\""]*\""";

   Attribute_Value : constant Wide_Wide_String :=
     Unquoted_Attribute_Value & "|" &
     Single_Quoted_Attribute_Value & "|" &
     Double_Quoted_Attribute_Value;

   Attribute_Value_Specification : constant Wide_Wide_String :=
     Whitespace & "*\=" &
     Whitespace & "*(" & Attribute_Value & ")";
   --               1

   Attribute : constant Wide_Wide_String :=
     Whitespace &
     "+(" & Attribute_Name & ")(" & Attribute_Value_Specification & ")?";
   --  1                       2    3

   Open_Tag  : constant Wide_Wide_String :=
     "\<" & Tag_Name & "(" & Attribute & ")*" & Whitespace & "*" & "\/?\>";
   --       1           2    345

   Closing_Tag : constant Wide_Wide_String :=
     "\<\/" & Tag_Name & Whitespace & "*\>";
   --         1

   Comment : constant Wide_Wide_String :=
     "\<\!\-\-(\-?[^\>\-](\-?[^\-])*)\-\-\>";
   --         1          2

   HTML_PI : constant Wide_Wide_String :=
     "\<\?((\?[^\>]|[^\?])*\??)\?\>";
   --     12

   Declaration                   : constant Wide_Wide_String :=
     "\<\!([A-Z]+" & Whitespace & "+[^\>]+)\>";
   --     1

   NO_CDATA : constant Wide_Wide_String :=
     "(\]((\][^\>]|[^\]])*\]?)|[^\]])*\]?";
   --  1 23
   CDATA : constant Wide_Wide_String :=
     "\<\!\[CDATA\[(" & NO_CDATA & ")\]\]\>";
   --              1    234

   HTML_Tag : constant Wide_Wide_String :=
     Open_Tag    & "|" &    --  12345
     Closing_Tag & "|" &  --  6
     Comment     & "|" &  --  7 8
     HTML_PI     & "|" & --  9 10
     Declaration & "|" & --  11
     CDATA;  --  12, 13, 14, 15

   HTML_Tag_Pattern : constant League.Regexps.Regexp_Pattern := +HTML_Tag;
   Attribute_Pattern  : constant League.Regexps.Regexp_Pattern :=
     +("^" & Attribute);

   ----------
   -- Find --
   ----------

   procedure Find
     (Text   : Plain_Texts.Plain_Text;
      Cursor : Position;
      State  : in out Optional_Inline_State)
   is
      LF : constant Wide_Wide_Character := Ada.Characters.Wide_Wide_Latin_1.LF;

      function Index_To_Position (Index : Positive) return Position;
      procedure Set_State
        (From, To : Positive;
         Value : Annotation);

      function Index_To_Position (Index : Positive) return Position is
         Left   : Natural := Index - 1;
         Result : Position := (Cursor.Line, 1);
      begin
         loop
            declare
               Line : constant League.Strings.Universal_String :=
                 Text.Line (Result.Line);
            begin
               if Line.Length > Left then
                  Text.Step (Left, Result);
                  exit;
               else
                  Text.Step (Line.Length, Result);
                  Left := Left - Line.Length - 1;
               end if;
            end;
         end loop;

         return Result;
      end Index_To_Position;

      procedure Set_State
        (From, To : Positive;
         Value    : Annotation) is
      begin
         State :=
           (Is_Set => True,
            Span   =>
              (From => Index_To_Position (From),
               To   => Index_To_Position (To)),
            Value  =>
              (Plain_Text => League.Strings.Empty_Universal_String,
               Annotation => Annotation_Vectors.To_Vector (Value, 1)));
      end Set_State;

      Lines : constant League.Strings.Universal_String :=
        Text.Join (Cursor, LF);

      Match : constant League.Regexps.Regexp_Match :=
        HTML_Tag_Pattern.Find_Match (Lines);

   begin
      if not Match.Is_Matched then
         State := (Is_Set => False);
      elsif Match.Last_Index (1) >= Match.First_Index (1) then
         declare
            Piece  : League.Strings.Universal_String :=
              Lines.Tail_From (Match.Last_Index (1) + 1);
            Result : Annotation :=
              (Kind     => Open_HTML_Tag,
               From     => 1,
               To       => 0,
               Tag      => Match.Capture (1),
               Is_Empty => Match.Capture.Ends_With ("/>"),
               Attr     => <>);
         begin
            while not Piece.Is_Empty loop
               declare
                  AM : constant League.Regexps.Regexp_Match :=
                    Attribute_Pattern.Find_Match (Piece);
                  Attr : League.Strings.Universal_String;
                  X : HTML_Attribute;
               begin
                  exit when not AM.Is_Matched;

                  X.Name := AM.Capture (1);

                  if AM.First_Index (2) < AM.Last_Index (2) then
                     Attr := AM.Capture (3);

                     if Attr (1).To_Wide_Wide_Character in ''' | '"' then
                        Attr := Attr.Slice (2, Attr.Length - 1);
                     end if;

                     X.Value := Attr.Split (LF);
                  end if;

                  Result.Attr.Append (X);
                  Piece := Piece.Tail_From (AM.Last_Index + 1);
               end;
            end loop;

            Set_State (Match.First_Index, Match.Last_Index, Result);
         end;
      elsif Match.Last_Index (6) >= Match.First_Index (6) then
         declare
            Result : constant Annotation :=
              (Kind     => Close_HTML_Tag,
               From     => 1,
               To       => 0,
               Tag      => Match.Capture (6));
         begin
            Set_State (Match.First_Index, Match.Last_Index, Result);
         end;
      elsif Match.Last_Index (7) >= Match.First_Index (7) then
         declare
            Result : constant Annotation :=
              (Kind         => HTML_Comment,
               From         => 1,
               To           => 0,
               HTML_Comment => Match.Capture (7).Split (LF));
         begin
            Set_State (Match.First_Index, Match.Last_Index, Result);
         end;
      elsif Match.Capture.Ends_With ("?>") then
         declare
            Result : constant Annotation :=
              (Kind    => HTML_Processing_Instruction,
               From    => 1,
               To      => 0,
               HTML_PI => Match.Capture (9).Split (LF));
         begin
            Set_State (Match.First_Index, Match.Last_Index, Result);
         end;
      elsif Match.Last_Index (11) >= Match.First_Index (11) then
         declare
            Result : constant Annotation :=
              (Kind      => HTML_Declaration,
               From      => 1,
               To        => 0,
               HTML_Decl => Match.Capture (11).Split (LF));
         begin
            Set_State (Match.First_Index, Match.Last_Index, Result);
         end;
      else
         declare
            Result : constant Annotation :=
              (Kind       => HTML_CDATA,
               From       => 1,
               To         => 0,
               HTML_CDATA => Match.Capture (12).Split (LF));
         begin
            Set_State (Match.First_Index, Match.Last_Index, Result);
         end;
      end if;
   end Find;

end Markdown.Inline_Parsers.Raw_HTML;
