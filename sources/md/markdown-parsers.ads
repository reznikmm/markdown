--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Containers.Vectors;
with Ada.Tags;

with League.Strings;

with Markdown.Blocks;
with Markdown.Visitors;

package Markdown.Parsers is

   type Parser is tagged limited private;

   procedure Append_Line
     (Self : in out Parser'Class;
      Line : League.Strings.Universal_String);

   type Block_Start_Filter is access procedure
     (Line   : League.Strings.Universal_String;
      From   : in out Positive;
      Column : in out Positive;
      Tag    : in out Ada.Tags.Tag);

   procedure Register
     (Self   : in out Parser'Class;
      Filter : Block_Start_Filter);

   procedure Visit
     (Self    : Parser'Class;
      Visitor : in out Markdown.Visitors.Visitor'Class);

private

   package Block_Vectors is new Ada.Containers.Vectors
     (Positive, Markdown.Blocks.Block_Access, Markdown.Blocks."=");

   package Filter_Vectors is new Ada.Containers.Vectors
     (Positive, Block_Start_Filter, "=");

   type Parser is tagged limited record
      Blocks  : Block_Vectors.Vector;
      Open    : Block_Vectors.Vector;
      Filters : Filter_Vectors.Vector;
   end record;
end Markdown.Parsers;
