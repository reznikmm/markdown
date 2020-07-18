--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------
with Ada.Tags.Generic_Dispatching_Constructor;

package body Markdown.Parsers is

   type New_Block_Access is access all Markdown.Blocks.Block'Class;

   procedure Create_Blocks
     (Self    : in out Parser'Class;
      Line    : League.Strings.Universal_String;
      From    : in out Positive;
      Column  : in out Positive;
      Created : out Block_Vectors.Vector);

   -----------------
   -- Append_Line --
   -----------------

   procedure Append_Line
     (Self : in out Parser'Class;
      Line : League.Strings.Universal_String)
   is
      procedure Close_Blocks (Open : in out Block_Vectors.Vector);

      ------------------
      -- Close_Blocks --
      ------------------

      procedure Close_Blocks (Open : in out Block_Vectors.Vector) is
      begin
         if Open.Is_Empty and not Self.Open.Is_Empty then
            Self.Blocks.Append (Self.Open.First_Element);
         end if;
--           for J in reverse Open.Last_Index + 1 .. Self.Open.Last_Index loop
--              --  Close block
--              null;
--           end loop;

         Self.Open.Move (Source => Open);
      end Close_Blocks;

      Open    : Block_Vectors.Vector;
      Created : Block_Vectors.Vector;
      From    : Positive := 1;
      Column  : Positive := 1;
      Match   : Markdown.Continuation_Kind := Markdown.No_Match;
   begin
      for Block of Self.Open loop
         Block.Consume_Continuation_Markers (Line, From, Column, Match);

         exit when Match /= Markdown.Match;

         Open.Append (Block);
      end loop;

      if Match /= Markdown.Consumed then
         Self.Create_Blocks (Line, From, Column, Created);
      end if;

      if not Created.Is_Empty then
         Close_Blocks (Open);

         if not Self.Open.Is_Empty then
            Self.Open.Last_Element.Append_Child (Created.First_Element);
         end if;

         Self.Open.Append (Created);
      end if;

      if From <= Line.Length then
         Self.Open.Last_Element.Append_Line (Line, From, Column);
      elsif Created.Is_Empty then
         Close_Blocks (Open);
      end if;
   end Append_Line;

   -------------------
   -- Create_Blocks --
   -------------------

   procedure Create_Blocks
     (Self    : in out Parser'Class;
      Line    : League.Strings.Universal_String;
      From    : in out Positive;
      Column  : in out Positive;
      Created : out Block_Vectors.Vector)
   is
      function Constructor is new Ada.Tags.Generic_Dispatching_Constructor
        (Markdown.Blocks.Block,
         Markdown.Blocks.Text_Line,
         Markdown.Blocks.Create);

      Again : Boolean := True;
   begin
      while Again and From <= Line.Length loop
         Again := False;

         for Filter of Self.Filters loop
            declare
               use type Ada.Tags.Tag;

               Tag    : Ada.Tags.Tag := Ada.Tags.No_Tag;
               Object : New_Block_Access;
               Block  : Markdown.Blocks.Block_Access;
               Text   : aliased Markdown.Blocks.Text_Line :=
                 (Line, From, Column);
            begin
               Filter (Line, From, Column, Tag);

               if Tag /= Ada.Tags.No_Tag then
                  Object := new Markdown.Blocks.Block'Class'
                    (Constructor (Tag, Text'Access));
                  Block := Markdown.Blocks.Block_Access (Object);

                  if not Created.Is_Empty then
                     Created.Last_Element.Append_Child (Block);
                  end if;

                  Created.Append (Block);

                  Again := Block.Is_Container;
                  exit;
               end if;
            end;
         end loop;
      end loop;
   end Create_Blocks;

   --------------
   -- Register --
   --------------

   procedure Register
     (Self   : in out Parser'Class;
      Filter : Block_Start_Filter) is
   begin
      Self.Filters.Append (Filter);
   end Register;

   -----------
   -- Visit --
   -----------

   procedure Visit
     (Self    : Parser'Class;
      Visitor : in out Markdown.Visitors.Visitor'Class) is
   begin
      for Block of Self.Blocks loop
         Block.Visit (Visitor);
      end loop;
   end Visit;

end Markdown.Parsers;
