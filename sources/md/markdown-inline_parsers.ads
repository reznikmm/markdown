--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with League.Strings;
with League.String_Vectors;

package Markdown.Inline_Parsers is

   type Annotation_Kind is
     (Soft_Line_Break, Emphasis, Strong);

   type Annotation (Kind : Annotation_Kind := Annotation_Kind'First) is record
      From : Positive;
      case Kind is
         when Emphasis | Strong =>
            To : Positive;
         when Soft_Line_Break =>
            null;
      end case;
   end record;

   type Annotation_Array is array (Positive range <>) of Annotation;

   type Annotated_Text (Count : Natural) is record
      Plain_Text : League.Strings.Universal_String;
      Annotation : Annotation_Array (1 .. Count);
   end record;

   function Parse (Text : League.String_Vectors.Universal_String_Vector)
     return Annotated_Text;

end Markdown.Inline_Parsers;
