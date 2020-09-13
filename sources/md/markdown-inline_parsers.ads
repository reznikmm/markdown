--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Containers.Vectors;
with League.Strings;
with League.String_Vectors;

with Markdown.Link_Registers;

package Markdown.Inline_Parsers is

   type Annotation_Kind is
     (Soft_Line_Break, Emphasis, Strong, Link, Code_Span);

   type Annotation (Kind : Annotation_Kind := Annotation_Kind'First) is record
      From : Positive;
      To   : Natural;
      case Kind is
         when Emphasis | Strong | Link =>

            case Kind is
               when Link =>
                  Destination : League.Strings.Universal_String;
                  Title       : League.String_Vectors.Universal_String_Vector;
               when others =>
                  null;
            end case;

         when Soft_Line_Break | Code_Span =>
            null;
      end case;
   end record;

   package Annotation_Vectors is new
     Ada.Containers.Vectors (Positive, Annotation);

   type Annotated_Text is record
      Plain_Text : League.Strings.Universal_String;
      Annotation : Annotation_Vectors.Vector;
   end record;

   function Parse
     (Register : Markdown.Link_Registers.Link_Register'Class;
      Lines    : League.String_Vectors.Universal_String_Vector)
        return Annotated_Text;

end Markdown.Inline_Parsers;
