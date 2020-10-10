--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Tags;

with League.Strings;
with League.Strings.Hash;
with League.String_Vectors;

with Markdown.Blockquotes;
with Markdown.Blocks;
with Markdown.Inline_Parsers;
with Markdown.Link_Registers;
with Markdown.Visitors;

package Markdown.Parsers is

   type Parser is limited new Markdown.Link_Registers.Link_Register with
     private;

   procedure Append_Line
     (Self : in out Parser'Class;
      Text : League.Strings.Universal_String);

   procedure Stop (Self : in out Parser'Class);

   function Parse_Inlines
     (Self : Parser'Class;
      Text : League.String_Vectors.Universal_String_Vector)
        return Markdown.Inline_Parsers.Annotated_Text;

   type Block_Start_Filter is access procedure
     (Line : Markdown.Blocks.Text_Line;
      Tag  : in out Ada.Tags.Tag;
      CIP  : out Can_Interrupt_Paragraph);

   procedure Register
     (Self   : in out Parser'Class;
      Filter : Block_Start_Filter);

   procedure Visit
     (Self    : Parser'Class;
      Visitor : in out Markdown.Visitors.Visitor'Class);

private

   package Block_Vectors is new Ada.Containers.Vectors
     (Positive, Markdown.Blocks.Block_Access, Markdown.Blocks."=");

   package Container_Vectors is new Ada.Containers.Vectors
     (Positive, Markdown.Blocks.Container_Block_Access, Markdown.Blocks."=");

   package Filter_Vectors is new Ada.Containers.Vectors
     (Positive, Block_Start_Filter, "=");

   type Link_Information is record
      Destination : League.Strings.Universal_String;
      Title       : League.String_Vectors.Universal_String_Vector;
   end record;

   package Link_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => League.Strings.Universal_String,
      Element_Type    => Link_Information,
      Hash            => League.Strings.Hash,
      Equivalent_Keys => League.Strings."=",
      "="             => "=");

   type Parser is limited new Markdown.Link_Registers.Link_Register with record
      Links     : Link_Maps.Map;
      Blocks    : Markdown.Blockquotes.Blockquote;  --  a dummy root block
      Open      : Container_Vectors.Vector;
      Open_Leaf : Markdown.Blocks.Block_Access;
      Filters   : Filter_Vectors.Vector;
   end record;

   overriding procedure Resolve
     (Self        : Parser;
      Label       : League.Strings.Universal_String;
      Found       : out Boolean;
      Destination : out League.Strings.Universal_String;
      Title       : out League.String_Vectors.Universal_String_Vector);

end Markdown.Parsers;
