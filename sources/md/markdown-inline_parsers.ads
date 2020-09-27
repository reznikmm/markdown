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
     (Soft_Line_Break, Emphasis, Strong, Link, Code_Span,
      Open_HTML_Tag, Close_HTML_Tag, HTML_Comment,
      HTML_Processing_Instruction, HTML_Declaration, HTML_CDATA);

   type HTML_Attribute is record
      Name  : League.Strings.Universal_String;
      Value : League.String_Vectors.Universal_String_Vector;
      --  An empty vector means no value for the attribute
   end record;

   package Attr_Vectors is new Ada.Containers.Vectors
     (Positive, HTML_Attribute);

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
         when Open_HTML_Tag | Close_HTML_Tag =>
            Tag  : League.Strings.Universal_String;

            case Kind is
               when Open_HTML_Tag =>
                  Attr : Attr_Vectors.Vector;
                  Is_Empty : Boolean;
               when others =>
                  null;
            end case;
         when HTML_Comment =>
            HTML_Comment : League.String_Vectors.Universal_String_Vector;
         when HTML_Processing_Instruction =>
            HTML_PI : League.String_Vectors.Universal_String_Vector;
         when HTML_Declaration =>
            HTML_Decl : League.String_Vectors.Universal_String_Vector;
         when HTML_CDATA =>
            HTML_CDATA : League.String_Vectors.Universal_String_Vector;
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

private

   type Position is record
      Line   : Positive;
      Column : Natural;
   end record;

   function "+" (Cursor : Position; Value : Integer) return Position is
     ((Cursor.Line, Cursor.Column + Value));

   function "<" (Left, Right : Position) return Boolean is
     (Left.Line < Right.Line or
       (Left.Line = Right.Line and Left.Column < Right.Column));

   function "<=" (Left, Right : Position) return Boolean is
     (Left < Right or Left = Right);

   function ">" (Left, Right : Position) return Boolean is
     (Left.Line > Right.Line or
       (Left.Line = Right.Line and Left.Column > Right.Column));

   package Plain_Texts is
      type Plain_Text is tagged limited private;

      procedure Initialize
        (Self : in out Plain_Text'Class;
         Text : League.String_Vectors.Universal_String_Vector;
         From : Position := (1, 1);
         To   : Position := (Positive'Last, Positive'Last));

      procedure Initialize
        (Self : in out Plain_Text'Class;
         Text : Plain_Text'Class;
         From : Position;
         To   : Position := (Positive'Last, Positive'Last));

      function First (Self : Plain_Text'Class) return Position;
      function Last (Self : Plain_Text'Class) return Position;
      function Line
        (Self : Plain_Text'Class;
         From : Position) return League.Strings.Universal_String;
      function Line
        (Self  : Plain_Text'Class;
         Index : Positive) return League.Strings.Universal_String;
      function Lines (Self  : Plain_Text'Class) return Positive;
      pragma Unreferenced (Lines);

      procedure Step
        (Self   : Plain_Text'Class;
         Value  : Natural;
         Cursor : in out Position);

      function Join
        (Self : Plain_Text'Class;
         From : Position;
         Char : Wide_Wide_Character) return League.Strings.Universal_String;

   private
      type Plain_Text is tagged limited record
         Data : League.String_Vectors.Universal_String_Vector;
         From : Position;
         To   : Position;
      end record;
   end Plain_Texts;

   type Inline_Span is record
      From : Position;
      To   : Position;
   end record;

   type Optional_Inline_State (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Span  : Inline_Span;
            Value : Annotated_Text;
         when False =>
            null;
      end case;
   end record;

end Markdown.Inline_Parsers;
