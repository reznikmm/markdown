--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with League.Regexps;

with Markdown.Visitors;

package body Markdown.Fenced_Code_Blocks is

   Pattern : constant League.Regexps.Regexp_Pattern :=
     League.Regexps.Compile
       (League.Strings.To_Universal_String
          ("^\ {0,3}((\`{3,})\ *([^\`]*)|(\~{3,})\ *(.*))$"));
   --               12          3       4           5
   -----------------
   -- Append_Line --
   -----------------

   overriding procedure Append_Line
     (Self : in out Fenced_Code_Block;
      Line : Markdown.Blocks.Text_Line;
      CIP  : Can_Interrupt_Paragraph;
      Ok   : in out Boolean)
   is
      pragma Unreferenced (CIP);

      Match  : constant League.Regexps.Regexp_Match :=
        Pattern.Find_Match (Line.Line);
   begin
      if Self.Closed then
         Ok := False;

         return;
      elsif Match.Is_Matched
         --  Closing fences cannot have info string
        and then Match.Last_Index (3) < Match.First_Index (3)
        and then Match.Last_Index (5) < Match.First_Index (5)
      then
         if Self.Is_Tick_Fence then
            Self.Closed := Match.Last_Index (2) > 0 and then
              Match.Last_Index (2) - Match.First_Index (2) + 1 >=
                Self.Fence_Length;
         else
            Self.Closed := Match.Last_Index (2) = 0 and then
              Match.Last_Index (4) - Match.First_Index (4) + 1 >=
                Self.Fence_Length;
         end if;
      end if;

      if not Self.Closed then
         declare
            From : Positive := 1;
         begin
            for J in 1 .. Self.Fence_Indent loop
               if J <= Line.Line.Length and then
                 Line.Line (J).To_Wide_Wide_Character = ' '
               then
                  From := J + 1;
               else
                  exit;
               end if;
            end loop;

            if From > Line.Line.Length then
               Self.Blank.Append (League.Strings.Empty_Universal_String);
            else
               Self.Lines.Append (Self.Blank);
               Self.Blank.Clear;
               Self.Lines.Append (Line.Line.Tail_From (From));
            end if;
         end;
      elsif not Self.Blank.Is_Empty then
         Self.Lines.Append (Self.Blank);
         Self.Blank.Clear;
      end if;

      Ok := True;
   end Append_Line;

   ------------
   -- Create --
   ------------

   overriding function Create
     (Line : not null access Markdown.Blocks.Text_Line)
      return Fenced_Code_Block
   is
      Match : constant League.Regexps.Regexp_Match :=
        Pattern.Find_Match (Line.Line);
   begin
      pragma Assert (Match.Is_Matched);

      return Result : Fenced_Code_Block do
         Result.Fence_Indent := Match.First_Index (1) - 1;
         Result.Is_Tick_Fence := Match.Last_Index (2) > 0;

         if Result.Is_Tick_Fence then
            Result.Fence_Length :=
              Match.Last_Index (2) - Match.First_Index (2) + 1;
            Result.Info_String := Line.Line.Slice
              (Match.First_Index (3), Match.Last_Index (3));
         else
            Result.Fence_Length :=
              Match.Last_Index (4) - Match.First_Index (4) + 1;
            Result.Info_String := Line.Line.Slice
              (Match.First_Index (5), Match.Last_Index (5));
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
      Match : constant League.Regexps.Regexp_Match :=
        Pattern.Find_Match (Line.Line);
   begin
      if Match.Is_Matched then
         Tag := Fenced_Code_Block'Tag;
         CIP := True;
      end if;
   end Filter;

   -----------------
   -- Info_String --
   -----------------

   function Info_String (Self : Fenced_Code_Block'Class)
     return League.Strings.Universal_String is
   begin
      return Self.Info_String;
   end Info_String;

   -----------
   -- Lines --
   -----------

   function Lines (Self : Fenced_Code_Block'Class)
     return League.String_Vectors.Universal_String_Vector is
   begin
      return Self.Lines;
   end Lines;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : in out Fenced_Code_Block;
      Visitor : in out Markdown.Visitors.Visitor'Class) is
   begin
      Visitor.Fenced_Code_Block (Self);
   end Visit;

end Markdown.Fenced_Code_Blocks;
