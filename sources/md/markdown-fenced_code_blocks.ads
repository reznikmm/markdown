--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Tags;

with League.String_Vectors;
with League.Strings;

with Markdown.Blocks;
limited with Markdown.Visitors;


package Markdown.Fenced_Code_Blocks is
   type Fenced_Code_Block is new Markdown.Blocks.Block with private;

   function Lines (Self : Fenced_Code_Block'Class)
     return League.String_Vectors.Universal_String_Vector;

   function Info_String (Self : Fenced_Code_Block'Class)
     return League.Strings.Universal_String;

   overriding function Create
     (Line : not null access Markdown.Blocks.Text_Line)
        return Fenced_Code_Block;

   overriding procedure Append_Line
     (Self : in out Fenced_Code_Block;
      Line : Markdown.Blocks.Text_Line;
      CIP  : Can_Interrupt_Paragraph;
      Ok   : in out Boolean);

   overriding procedure Visit
     (Self    : in out Fenced_Code_Block;
      Visitor : in out Markdown.Visitors.Visitor'Class);

   procedure Filter
     (Line : Markdown.Blocks.Text_Line;
      Tag  : in out Ada.Tags.Tag;
      CIP  : out Can_Interrupt_Paragraph);

private

   type Fenced_Code_Block is new Markdown.Blocks.Block with record
      Is_Tick_Fence : Boolean;
      Fence_Length  : Positive;
      Fence_Indent  : Natural;
      Closed        : Boolean := False;
      Lines         : League.String_Vectors.Universal_String_Vector;
      Blank         : League.String_Vectors.Universal_String_Vector;
      Info_String   : League.Strings.Universal_String;
   end record;

end Markdown.Fenced_Code_Blocks;
