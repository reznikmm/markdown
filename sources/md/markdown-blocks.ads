--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with League.Strings;

limited with Markdown.Visitors;

package Markdown.Blocks is

   type Block is abstract tagged limited private;
   type Block_Access is access all Block'Class with Storage_Size => 0;

   function Is_Assigned (Value : access Block'Class) return Boolean is
     (Value /= null);

   type Text_Line is record
      Line   : League.Strings.Universal_String;
      Column : Positive;
   end record;

   not overriding function Create
     (Line : not null access Text_Line) return Block is abstract;

   not overriding function Is_Container (Self : Block) return Boolean is
     (False);

   not overriding procedure Append_Line
     (Self : in out Block;
      Line : Text_Line;
      CIP  : Can_Interrupt_Paragraph;
      Ok   : in out Boolean) is null;

   not overriding procedure Visit
     (Self    : in out Block;
      Visitor : in out Markdown.Visitors.Visitor'Class) is abstract;

   type Container_Block is abstract new Block with private;

   type Container_Block_Access is access all Container_Block'Class
     with Storage_Size => 0;

   not overriding procedure Append_Child
     (Self  : in out Container_Block;
      Child : not null Block_Access);

   overriding function Is_Container (Self : Container_Block) return Boolean is
     (True);

   not overriding procedure Consume_Continuation_Markers
     (Self  : in out Container_Block;
      Line  : in out Text_Line;
      Match : out Boolean) is abstract;

   procedure Wrap_List_Items (Self : in out Container_Block'Class);
   --  Create List when neede and move List_Items inside.

   procedure Visit_Children
     (Self    : Container_Block'Class;
      Visitor : in out Markdown.Visitors.Visitor'Class);

private

   type Block is abstract tagged limited record
      Next : Block_Access;
   end record;

   type Container_Block is abstract new Block with record
      Last_Child : Block_Access;
   end record;

end Markdown.Blocks;
