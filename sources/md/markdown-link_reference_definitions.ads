--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Tags;

with League.Strings;
with League.String_Vectors;

with Markdown.Blocks;
limited with Markdown.Visitors;

package Markdown.Link_Reference_Definitions is

   type Link_Reference_Definition is new Markdown.Blocks.Block with private;

   function Label (Self : Link_Reference_Definition'Class)
     return League.Strings.Universal_String;

   function Destination (Self : Link_Reference_Definition'Class)
     return League.Strings.Universal_String;

   function Title (Self : Link_Reference_Definition'Class)
     return League.String_Vectors.Universal_String_Vector;

   overriding function Create
     (Line : not null access Markdown.Blocks.Text_Line)
        return Link_Reference_Definition;

   overriding procedure Append_Line
     (Self : in out Link_Reference_Definition;
      Line : Markdown.Blocks.Text_Line;
      CIP  : Can_Interrupt_Paragraph;
      Ok   : in out Boolean);

   overriding procedure Visit
     (Self    : Link_Reference_Definition;
      Visitor : in out Markdown.Visitors.Visitor'Class);

   procedure Filter
     (Line : Markdown.Blocks.Text_Line;
      Tag  : in out Ada.Tags.Tag;
      CIP  : out Can_Interrupt_Paragraph);

private

   type Link_Reference_Definition is new Markdown.Blocks.Block with record
      Has_Title : Boolean := False;  --  Link has a complete title
      Has_URL   : Boolean := False;  --  Link has a link destination
      Start     : Wide_Wide_Character;
      Label     : League.Strings.Universal_String;
      URL       : League.Strings.Universal_String;
      Title     : League.String_Vectors.Universal_String_Vector;
   end record;

end Markdown.Link_Reference_Definitions;
