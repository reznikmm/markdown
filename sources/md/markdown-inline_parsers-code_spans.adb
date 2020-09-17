--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with League.Regexps;

package body Markdown.Inline_Parsers.Code_Spans is

   function "+" (Text : Wide_Wide_String)
     return League.Regexps.Regexp_Pattern
       is (League.Regexps.Compile (League.Strings.To_Universal_String (Text)));

   Code_Span_Start : constant League.Regexps.Regexp_Pattern :=
     +("(\\[\`\\])*(\`+)|\\\`");
   --   1          2
   Code_Span_End : constant League.Regexps.Regexp_Pattern := +("\`+");

   procedure Parse_Code_Span
     (Text   : Plain_Texts.Plain_Text;
      Marker : League.Strings.Universal_String;
      Result : out League.Strings.Universal_String;
      Found  : out Boolean;
      Cursor : in out Position);

   ----------
   -- Find --
   ----------

   procedure Find
     (Text   : Plain_Texts.Plain_Text;
      Cursor : Position;
      State  : in out Optional_Inline_State)
   is
      Line   : League.Strings.Universal_String := Text.Line (Cursor);
      Offset : Natural := Cursor.Column - 1;  --  Dropped characters count
      Index  : Positive := Cursor.Line;
   begin
      loop
         declare
            Match : constant League.Regexps.Regexp_Match :=
              Code_Span_Start.Find_Match (Line);
         begin
            if not Match.Is_Matched then

               exit when Index = Text.Last.Line;

               Index := Index + 1;
               Offset := 0;
               Line := Text.Line (Index);

            elsif Match.Last_Index (2) < Match.First_Index (2) then

               Line := Line.Tail_From (Match.Last_Index + 1);
               Offset := Offset + Match.Last_Index;

            else
               declare
                  Found  : Boolean := False;
                  Marker : constant League.Strings.Universal_String :=
                    Match.Capture (2);
                  Result : Optional_Inline_State (True);
               begin
                  Result.Span.From := (Index, Match.First_Index (2) + Offset);
                  Result.Span.To := Result.Span.From;
                  Text.Step (Marker.Length, Result.Span.To);

                  exit when Result.Span.To > Text.Last;

                  Parse_Code_Span
                    (Text,
                     Marker => Marker,
                     Result => Result.Value.Plain_Text,
                     Found  => Found,
                     Cursor => Result.Span.To);

                  if Found then
                     Result.Value.Annotation.Append
                       ((Kind => Code_Span,
                         From => 1,
                         To   => Result.Value.Plain_Text.Length));

                     State := Result;

                     return;
                  else
                     Line := Line.Tail_From (Match.Last_Index + 1);
                     Offset := Offset + Match.Last_Index;
                  end if;
               end;
            end if;
         end;
      end loop;

      State := (Is_Set => False);
   end Find;

   ---------------------
   -- Parse_Code_Span --
   ---------------------

   procedure Parse_Code_Span
     (Text   : Plain_Texts.Plain_Text;
      Marker : League.Strings.Universal_String;
      Result : out League.Strings.Universal_String;
      Found  : out Boolean;
      Cursor : in out Position)
   is
      use type League.Strings.Universal_String;

      Line   : League.Strings.Universal_String := Text.Line (Cursor);
      Offset : Natural := Cursor.Column - 1;  --  Dropped characters count
      Index  : Positive := Cursor.Line;
   begin
      if Cursor.Column = 1 then
         Result.Append (" ");
      end if;

      loop
         declare
            Match : constant League.Regexps.Regexp_Match :=
              Code_Span_End.Find_Match (Line);
         begin
            if Match.Is_Matched then
               if Match.Capture = Marker then
                  Found := True;
                  Cursor := (Index, Match.Last_Index + Offset);

                  Result.Append
                    (Line.Head_To (Match.First_Index - 1));

                  if Result.Starts_With (" ")
                    and then Result.Ends_With (" ")
                    and then Result.Length /= Result.Count (' ')
                  then
                     Result := Result.Slice (2, Result.Length - 1);
                  end if;

                  return;
               else
                  Result.Append (Line.Head_To (Match.Last_Index));
                  Line := Line.Tail_From (Match.Last_Index + 1);
                  Offset := Offset + Match.Last_Index;
               end if;

            elsif Index < Text.Last.Line then

               Result.Append (Line);
               Result.Append (' ');
               Index := Index + 1;
               Offset := 0;
               Line := Text.Line (Index);

            else

               exit;

            end if;
         end;
      end loop;

      Found := False;
   end Parse_Code_Span;

end Markdown.Inline_Parsers.Code_Spans;
