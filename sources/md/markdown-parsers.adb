--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------
with Ada.Tags.Generic_Dispatching_Constructor;

package body Markdown.Parsers is

   type New_Block_Access is access all Markdown.Blocks.Block'Class;

   procedure Create_Block
     (Self       : in out Parser'Class;
      Line       : in out Blocks.Text_Line;
      Tag        : Ada.Tags.Tag;
      Containers : in out Container_Vectors.Vector;
      Leaf       : out Blocks.Block_Access);

   procedure Find_Block_Start
     (Self : Parser'Class;
      Line : Blocks.Text_Line;
      Tag  : out Ada.Tags.Tag;
      Int  : out Boolean);

   ----------------------
   -- Find_Block_Start --
   ----------------------

   procedure Find_Block_Start
     (Self : Parser'Class;
      Line : Blocks.Text_Line;
      Tag  : out Ada.Tags.Tag;
      Int  : out Boolean)
   is
      use type Ada.Tags.Tag;
   begin
      Tag := Ada.Tags.No_Tag;

      for Filter of Self.Filters loop
         Filter (Line, Tag, Int);

         exit when Tag /= Ada.Tags.No_Tag;
      end loop;
   end Find_Block_Start;

   -----------------
   -- Append_Line --
   -----------------

   procedure Append_Line
     (Self : in out Parser'Class;
      Text : League.Strings.Universal_String)
   is
      procedure Close_Blocks (Open : in out Container_Vectors.Vector);

      ------------------
      -- Close_Blocks --
      ------------------

      procedure Close_Blocks (Open : in out Container_Vectors.Vector) is
      begin
         if Open.Is_Empty and not Self.Open.Is_Empty then
            Self.Blocks.Append
              (Markdown.Blocks.Block_Access (Self.Open.First_Element));
         end if;
--           for J in reverse Open.Last_Index + 1 .. Self.Open.Last_Index loop
--              --  Close block
--              null;
--           end loop;

         Self.Open.Move (Source => Open);
      end Close_Blocks;

      use type Ada.Tags.Tag;

      Tag    : Ada.Tags.Tag;
      New_Containers : Container_Vectors.Vector;
      New_Leaf       : Blocks.Block_Access;
      Int_Para       : Boolean;
      Line    : Markdown.Blocks.Text_Line := (Text, 1);
      Open    : Container_Vectors.Vector;
      Match   : Boolean;
   begin
      for Block of Self.Open loop
         Block.Consume_Continuation_Markers (Line, Match);

         exit when not Match;

         Open.Append (Block);
      end loop;

      Self.Find_Block_Start (Line, Tag, Int_Para);

      if not Self.Open.Is_Empty and not Match then
         Self.Open_Leaf := null;
      elsif Self.Open_Leaf.Is_Assigned then
         Match := False;
         Self.Open_Leaf.Append_Line (Line, Int_Para, Match);

         if Match then
            Line.Line.Clear;
            Tag := Ada.Tags.No_Tag;
         else
            Self.Open_Leaf := null;
         end if;
      end if;

      if not Line.Line.Is_Empty then
         while Tag /= Ada.Tags.No_Tag and not New_Leaf.Is_Assigned loop
            Self.Create_Block (Line, Tag, New_Containers, New_Leaf);
            Self.Find_Block_Start (Line, Tag, Int_Para);
         end loop;
         pragma Assert (Line.Line.Is_Empty);
      end if;

      if New_Leaf.Is_Assigned then
         Self.Open_Leaf := New_Leaf;
         Close_Blocks (Open);

         if New_Containers.Is_Empty then
            if Self.Open.Is_Empty then
               Self.Blocks.Append (New_Leaf);
            else
               Self.Open.Last_Element.Append_Child (New_Leaf);
            end if;
         elsif not Self.Open.Is_Empty then
            Self.Open.Last_Element.Append_Child
              (Markdown.Blocks.Block_Access (New_Containers.First_Element));
         end if;

         Self.Open.Append (New_Containers);
      end if;

      if not New_Leaf.Is_Assigned then
         Close_Blocks (Open);
      end if;
   end Append_Line;

   ------------------
   -- Create_Block --
   ------------------

   procedure Create_Block
     (Self       : in out Parser'Class;
      Line       : in out Blocks.Text_Line;
      Tag        : Ada.Tags.Tag;
      Containers : in out Container_Vectors.Vector;
      Leaf       : out Blocks.Block_Access)
   is
      pragma Unreferenced (Self);
      function Constructor is new Ada.Tags.Generic_Dispatching_Constructor
        (Markdown.Blocks.Block,
         Markdown.Blocks.Text_Line,
         Markdown.Blocks.Create);

      Object : New_Block_Access;
      Block  : Markdown.Blocks.Block_Access;
      Text   : aliased Markdown.Blocks.Text_Line := Line;
   begin
      Object := new Markdown.Blocks.Block'Class'
        (Constructor (Tag, Text'Access));
      Block := Markdown.Blocks.Block_Access (Object);
      Line := Text;

      if not Containers.Is_Empty then
         Containers.Last_Element.Append_Child (Block);
      end if;

      if Block.Is_Container then
         Containers.Append (Markdown.Blocks.Container_Block_Access (Block));
      else
         Leaf := Block;
      end if;
   end Create_Block;

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
