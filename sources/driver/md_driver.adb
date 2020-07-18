--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Wide_Wide_Text_IO;

with League.Strings;
with League.String_Vectors;

with Markdown.Paragraphs;
with Markdown.Parsers;
with Markdown.Visitors;

procedure MD_Driver is
   package Visitors is
      type Visitor is new Markdown.Visitors.Visitor with null record;

      overriding procedure Paragraph
        (Self  : in out Visitor;
         Block : Markdown.Paragraphs.Paragraph);
   end Visitors;

   package body Visitors is

      overriding procedure Paragraph
        (Self  : in out Visitor;
         Block : Markdown.Paragraphs.Paragraph)
      is
         pragma Unreferenced (Self);
         Lines : constant League.String_Vectors.Universal_String_Vector :=
           Block.Lines;
      begin
         Ada.Wide_Wide_Text_IO.Put ("<p>");

         for J in 1 .. Lines.Length loop
            if J /= 1 then
               Ada.Wide_Wide_Text_IO.New_Line;
            end if;
            Ada.Wide_Wide_Text_IO.Put (Lines (J).To_Wide_Wide_String);
         end loop;
         Ada.Wide_Wide_Text_IO.Put_Line ("</p>");
      end Paragraph;
   end Visitors;

   Parser  : Markdown.Parsers.Parser;
   Visitor : Visitors.Visitor;
begin
   Parser.Register (Markdown.Paragraphs.Filter'Access);

   while not Ada.Wide_Wide_Text_IO.End_Of_File loop
      declare
         Line  : constant League.Strings.Universal_String :=
           League.Strings.To_Universal_String
             (Ada.Wide_Wide_Text_IO.Get_Line);
      begin
         Parser.Append_Line (Line);
      end;
   end loop;

   Parser.Append_Line (League.Strings.Empty_Universal_String);

   Parser.Visit (Visitor);
end MD_Driver;
