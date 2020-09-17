--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with League.Regexps;

package body Markdown.Inline_Parsers.Autolinks is

   function "+" (Text : Wide_Wide_String)
     return League.Regexps.Regexp_Pattern
       is (League.Regexps.Compile (League.Strings.To_Universal_String (Text)));

   Absolute_URI : constant Wide_Wide_String :=
     "[a-zA-Z][a-zA-Z0-9\+\.\-]+\:[^\ \t\n\v\f\r\<\>]*";

   EMail : constant Wide_Wide_String :=
     "[a-zA-Z0-9\.\!\#\$\%\&\'\*\+\/\=\?\^_\`\{\|\}\~\-]+" &
     "\@[a-zA-Z0-9]" &
     "([a-zA-Z0-9\-]{0,39}[a-zA-Z0-9])?" &                     --  s/39/61/
     "(\.[a-zA-Z0-9]([a-zA-Z0-9\-]{0,39}[a-zA-Z0-9])?)*";      --  s/39/61/

   Autolink_Pattern : constant Wide_Wide_String :=
     "\<(" & Absolute_URI & "|(" & EMail & "))\>";
   --   1                     2

   Autolink : constant League.Regexps.Regexp_Pattern := +Autolink_Pattern;

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
              Autolink.Find_Match (Line);
         begin
            if not Match.Is_Matched then

               exit when Index = Text.Last.Line;

               Index := Index + 1;
               Offset := 0;
               Line := Text.Line (Index);
            else
               declare
                  Result : Optional_Inline_State (True);
                  URL    : League.Strings.Universal_String :=
                    Match.Capture (1);
               begin
                  if Match.First_Index (2) <= Match.Last_Index (2) then
                     URL.Prepend ("mailto:");
                  end if;

                  Result.Span.From := (Index, Match.First_Index + Offset);
                  Result.Span.To   := (Index, Match.Last_Index + Offset);
                  Result.Value.Plain_Text.Append (Match.Capture (1));
                  Result.Value.Annotation.Append
                    ((Kind        => Markdown.Inline_Parsers.Link,
                      From        => 1,
                      To          => Result.Value.Plain_Text.Length,
                      Title       => <>,
                      Destination => URL));

                  State := Result;
                  return;
               end;
            end if;
         end;
      end loop;

      State := (Is_Set => False);
   end Find;

end Markdown.Inline_Parsers.Autolinks;
