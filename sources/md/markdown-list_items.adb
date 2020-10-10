--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with League.Regexps;

with Markdown.Visitors;

package body Markdown.List_Items is

   Bullet_List_Marker : constant Wide_Wide_String :=
     "[\-\+\*]";

   Ordered_List_Marker : constant Wide_Wide_String :=
     "[0-9]{1,9}[\.\)]";

   Marker_Pattern : constant Wide_Wide_String :=
     "^\ {0,3}(" &                        --  1
     Bullet_List_Marker & "|" &
     Ordered_List_Marker & ")" &
     "(\ {1,4}[^\ \t\n\v\f\r]|\ {5,})";   --  2

   Prefix : constant League.Regexps.Regexp_Pattern :=
     League.Regexps.Compile
       (League.Strings.To_Universal_String
          (Marker_Pattern));

   ----------------------------------
   -- Consume_Continuation_Markers --
   ----------------------------------

   overriding procedure Consume_Continuation_Markers
     (Self  : List_Item;
      Line  : in out Markdown.Blocks.Text_Line;
      Match : out Boolean)
   is
      Space : constant Wide_Wide_String := (1 .. Self.Marker_Width => ' ');
   begin
      Match := Line.Line.Starts_With (Space);

      if Match then
         Line.Line := Line.Line.Tail_From (Self.Marker_Width + 1);
         Line.Column := Line.Column + Self.Marker_Width;
      elsif Line.Line.Is_Empty then
         Match := True;
      end if;
   end Consume_Continuation_Markers;

   ------------
   -- Create --
   ------------

   overriding function Create
     (Line : not null access Markdown.Blocks.Text_Line) return List_Item
   is
      Matched : constant League.Regexps.Regexp_Match :=
        Prefix.Find_Match (Line.Line);

      Suffix  : League.Strings.Universal_String;
   begin
      pragma Assert (Matched.Is_Matched);

      Suffix := Matched.Capture (2);

      return Result : List_Item do
         Result.Marker := Matched.Capture (1);

         if Suffix.Ends_With (" ") then
            Line.Line := Line.Line.Tail_From (Matched.Last_Index (1) + 1);
            Line.Column := Line.Column + Matched.Last_Index (1);
            Result.Marker_Width := Result.Marker.Length + 1;
         else
            Line.Line := Line.Line.Tail_From (Matched.Last_Index);
            Line.Column := Line.Column + Matched.Last_Index - 1;
            Result.Marker_Width := Result.Marker.Length + Suffix.Length - 1;
         end if;
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
      Marker  : League.Strings.Universal_String;
      Matched : constant League.Regexps.Regexp_Match :=
        Prefix.Find_Match (Line.Line);
   begin
      if Matched.Is_Matched then
         Marker := Matched.Capture (1);
         Tag := List_Item'Tag;

         if Marker.Ends_With (".") or else Marker.Ends_With (")") then
            CIP := Natural'Wide_Wide_Value
              (Marker.Head_To (Marker.Length - 1).To_Wide_Wide_String) = 1;
         else
            CIP := True;
         end if;
      end if;
   end Filter;

   ----------------
   -- Is_Ordered --
   ----------------

   function Is_Ordered (Self : List_Item'Class) return Boolean is
   begin
      return Self.Marker.Ends_With (".") or else
        Self.Marker.Ends_With (")");
   end Is_Ordered;

   ------------
   -- Marker --
   ------------

   function Marker
     (Self : List_Item'Class) return League.Strings.Universal_String is
   begin
      return Self.Marker;
   end Marker;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : in out List_Item;
      Visitor : in out Markdown.Visitors.Visitor'Class) is
   begin
      Visitor.List_Item (Self);
   end Visit;

end Markdown.List_Items;