--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with League.String_Vectors;

package body Markdown.Common_Patterns is

   Link_Destination : constant Wide_Wide_String := "\<([^\<\>\\]|\\.)*\>";

   Link_Destination_Pattern : constant League.Regexps.Regexp_Pattern :=
     League.Regexps.Compile
       (League.Strings.To_Universal_String (Link_Destination));

   ----------------------------
   -- Parse_Link_Destination --
   ----------------------------

   procedure Parse_Link_Destination
     (Line : League.Strings.Universal_String;
      Last : out Natural;
      URL  : out League.Strings.Universal_String)
   is
      function Unescape
        (Text : League.Strings.Universal_String)
          return League.Strings.Universal_String;

      --------------
      -- Unescape --
      --------------

      function Unescape
        (Text : League.Strings.Universal_String)
          return League.Strings.Universal_String
      is
         Masked : constant League.Strings.Universal_String :=
           League.Strings.To_Universal_String
             ("!""#$%&'()*+,-./:;<=>?@[]^_`{|}~");
         List   : constant League.String_Vectors.Universal_String_Vector :=
           Text.Split ('\');
         Result : League.Strings.Universal_String;
      begin
         if List.Length > 0 then
            Result := List (1);
         end if;

         for J in 2 .. List.Length loop
            declare
               Item : constant League.Strings.Universal_String := List (J);
            begin
               if Item.Is_Empty or else Masked.Index (Item (1)) = 0 then
                  Result.Append ("\");
               end if;

               Result.Append (Item);
            end;
         end loop;

         return Result;
      end Unescape;

      Is_Escape : Boolean := False;
      Stop      : Natural := 0;  --  index of first unmatched '('
      Count     : Natural := 0;  --  Count of unmatched '('
   begin
      if Line.Starts_With ("<") then
         declare
            Match : constant League.Regexps.Regexp_Match :=
              Link_Destination_Pattern.Find_Match (Line);
            Text : League.Strings.Universal_String;
         begin
            if Match.Is_Matched then
               Text := Match.Capture;
               URL := Unescape (Text.Slice (2, Text.Length - 1));
               Last := Match.Last_Index;
            else
               Last := 0;
            end if;

            return;
         end;
      end if;

      Last := Line.Length;

      for J in 1 .. Line.Length loop
         declare
            Char : constant Wide_Wide_Character :=
              Line (J).To_Wide_Wide_Character;
         begin
            if Is_Escape then
               Is_Escape := False;
            elsif Char = '\' then
               Is_Escape := True;
            elsif Char <= ' ' then
               Last := J - 1;
               exit;
            elsif Char = '(' then
               if Count = 0 then
                  Stop := J;
               end if;

               Count := Count + 1;
            elsif Char = ')' then
               if Count = 0 then
                  Last := J - 1;
                  exit;
               else
                  Count := Count - 1;
               end if;
            end if;
         end;
      end loop;

      if Count > 0 then
         Last := Stop - 1;
      elsif Is_Escape then
         Last := Last - 1;
      end if;

      if Last > 0 then
         URL := Unescape (Line.Head_To (Last));
      end if;
   end Parse_Link_Destination;

end Markdown.Common_Patterns;
