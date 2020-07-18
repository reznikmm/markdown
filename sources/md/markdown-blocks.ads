--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

--  with Ada.Containers.Vectors;

with League.Strings;
--  with League.String_Vectors;

limited with Markdown.Visitors;

package Markdown.Blocks is

   type Block is abstract tagged limited private;
   type Block_Access is access all Block'Class with Storage_Size => 0;

   type Text_Line is record
      Line   : League.Strings.Universal_String;
      From   : Positive;
      Column : Positive;
   end record;

   not overriding function Create
     (Line   : not null access Text_Line) return Block is abstract;

   not overriding function Is_Container (Self : Block) return Boolean is
     (False);

   not overriding procedure Consume_Continuation_Markers
     (Self   : Block;
      Line   : League.Strings.Universal_String;
      From   : in out Positive;
      Column : in out Positive;
      Match  : out Continuation_Kind) is abstract;

   not overriding procedure Append_Child
     (Self  : in out Block;
      Child : not null Block_Access) is abstract;

   not overriding procedure Append_Line
     (Self   : in out Block;
      Line   : League.Strings.Universal_String;
      From   : Positive;
      Column : Positive) is abstract;

   not overriding procedure Visit
     (Self    : Block;
      Visitor : in out Markdown.Visitors.Visitor'Class) is abstract;

private
   type Block is abstract tagged limited null record;

end Markdown.Blocks;
