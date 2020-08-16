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

   Link_Destination  : constant Wide_Wide_String :=
     "\<[^\<\>]*\>" &
     "|([^\<\ \\]|\\.)([^\ \\]|\\.)*";
   --  1              2      <--  Groups

   Link_Title : constant Wide_Wide_String :=
     "\""[^\""]*(\"")?" &              --  Group: 1
     "|\'[^\']*(\')?" &                --  Group: 2
     "|\(([^\(\)]|\\[\(\)])*(\))?";    --  Group: 3, 4

end Markdown.Common_Patterns;
