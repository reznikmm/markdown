--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------
with Ada.Tags.Generic_Dispatching_Constructor;

with League.Regexps;

with Markdown.Link_Reference_Definitions;
with Markdown.List_Items;
with Markdown.Lists;

package body Markdown.Parsers is

   Blank_Pattern : constant League.Regexps.Regexp_Pattern :=
     League.Regexps.Compile
       (League.Strings.To_Universal_String
          ("^[\ \t]*$"));

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

   function Normalize_Link_Label
     (Self : Parser'Class;
      Label : League.Strings.Universal_String)
        return League.Strings.Universal_String;

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
            Self.Blocks.Append_Child
              (Markdown.Blocks.Block_Access (Self.Open.First_Element));
         end if;

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

         Self.Open_Leaf := New_Leaf;
         pragma Assert (Blank_Pattern.Find_Match (Line.Line).Is_Matched);
      end if;

      Close_Blocks (Open);

      if New_Leaf.Is_Assigned then
         if New_Containers.Is_Empty then
            if Self.Open.Is_Empty then
               Self.Blocks.Append_Child (New_Leaf);
            else
               Self.Open.Last_Element.Append_Child (New_Leaf);
            end if;
         end if;
      end if;

      if not Self.Open.Is_Empty and not New_Containers.Is_Empty then
         Self.Open.Last_Element.Append_Child
           (Markdown.Blocks.Block_Access (New_Containers.First_Element));
      end if;

      Self.Open.Append (New_Containers);
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

   --------------------------
   -- Normalize_Link_Label --
   --------------------------

   function Normalize_Link_Label
     (Self : Parser'Class;
      Label : League.Strings.Universal_String)
      return League.Strings.Universal_String
   is
      pragma Unreferenced (Self);
      List : constant League.String_Vectors.Universal_String_Vector :=
        Label.To_Casefold.Split (' ', League.Strings.Skip_Empty);
   begin
      return List.Join (" ");
   end Normalize_Link_Label;

   -------------------
   -- Parse_Inlines --
   -------------------

   function Parse_Inlines
     (Self : Parser'Class;
      Text : League.String_Vectors.Universal_String_Vector)
        return Markdown.Inline_Parsers.Annotated_Text is
   begin
      return Markdown.Inline_Parsers.Parse (Self, Text);
   end Parse_Inlines;

   --------------
   -- Register --
   --------------

   procedure Register
     (Self   : in out Parser'Class;
      Filter : Block_Start_Filter) is
   begin
      Self.Filters.Append (Filter);
   end Register;

   -------------
   -- Resolve --
   -------------

   overriding procedure Resolve
     (Self        : Parser;
      Label       : League.Strings.Universal_String;
      Found       : out Boolean;
      Destination : out League.Strings.Universal_String;
      Title       : out League.String_Vectors.Universal_String_Vector)
   is
      Cursor : constant Link_Maps.Cursor :=
        Self.Links.Find (Self.Normalize_Link_Label (Label));
   begin
      Found := Link_Maps.Has_Element (Cursor);

      if Found then
         Destination := Link_Maps.Element (Cursor).Destination;
         Title := Link_Maps.Element (Cursor).Title;
      end if;
   end Resolve;

   ----------
   -- Stop --
   ----------

   procedure Stop (Self : in out Parser'Class) is
      type Visitor is new Markdown.Visitors.Visitor with null record;

      overriding procedure Link_Reference_Definition
        (Ignore : in out Visitor;
         Value  : Link_Reference_Definitions.Link_Reference_Definition);

      overriding procedure Blockquote
        (Self  : in out Visitor;
         Value : in out Markdown.Blockquotes.Blockquote);

      overriding procedure List
        (Self  : in out Visitor;
         Value : Markdown.Lists.List);

      overriding procedure List_Item
        (Self  : in out Visitor;
         Value : in out Markdown.List_Items.List_Item);

      overriding procedure Blockquote
        (Self  : in out Visitor;
         Value : in out Markdown.Blockquotes.Blockquote) is
      begin
         Value.Wrap_List_Items;
         Value.Visit_Children (Self);
      end Blockquote;

      overriding procedure Link_Reference_Definition
        (Ignore : in out Visitor;
         Value  : Link_Reference_Definitions.Link_Reference_Definition)
      is
         Cursor  : Link_Maps.Cursor;
         Success : Boolean;
      begin
         Self.Links.Insert
           (Self.Normalize_Link_Label (Value.Label),
            (Value.Destination, Value.Title),
            Cursor, Success);
      end Link_Reference_Definition;

      overriding procedure List
        (Self  : in out Visitor;
         Value : Markdown.Lists.List) is
      begin
         Value.Visit_Children (Self);
      end List;

      overriding procedure List_Item
        (Self  : in out Visitor;
         Value : in out Markdown.List_Items.List_Item)
      is
      begin
         Value.Wrap_List_Items;
         Value.Visit_Children (Self);
      end List_Item;

      Updater : Visitor;
   begin
      if not Self.Open.Is_Empty then
         Self.Blocks.Append_Child
           (Markdown.Blocks.Block_Access (Self.Open.First_Element));
      end if;

      Self.Links.Clear;
      Self.Blocks.Wrap_List_Items;
      Self.Visit (Updater);
   end Stop;

   -----------
   -- Visit --
   -----------

   procedure Visit
     (Self    : Parser'Class;
      Visitor : in out Markdown.Visitors.Visitor'Class) is
   begin
      Self.Blocks.Visit_Children (Visitor);
   end Visit;

end Markdown.Parsers;
