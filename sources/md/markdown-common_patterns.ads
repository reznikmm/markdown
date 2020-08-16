--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with League.Regexps;
with League.Strings;

package Markdown.Common_Patterns is

   Blank_Pattern : constant League.Regexps.Regexp_Pattern :=
     League.Regexps.Compile
       (League.Strings.To_Universal_String
          ("^[\ \t]*$"));

   Link_Label : constant Wide_Wide_String :=
       "\[[\ \t\n\v\f\r]*((\\\[|[^\]\ \t\n\v\f\r])[\ \t\n\v\f\r\>]*)+\]";
   --    [ space        *((\  [| [^]space       ]) space          *)+ ]
   --  Groups:           12

   Link_Title : constant Wide_Wide_String :=
     "\""[^\""]*(\"")?" &              --  Group: 1
     "|\'[^\']*(\')?" &                --  Group: 2
     "|\(([^\(\)]|\\[\(\)])*(\))?";    --  Group: 3, 4

   procedure Parse_Link_Destination
     (Line : League.Strings.Universal_String;
      Last : out Natural;
      URL  : out League.Strings.Universal_String);
   --  Parse Line ad link destination and result its length in Last if found,
   --  or zero otherwise. Set URL to link destination stripping <> if needed.

end Markdown.Common_Patterns;
