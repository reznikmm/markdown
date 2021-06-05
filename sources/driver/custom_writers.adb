--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Strings.Unbounded;
with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Integer_Text_IO;

package body Custom_Writers is

   New_Line : constant Wide_Wide_String :=
     (1 => Ada.Characters.Wide_Wide_Latin_1.LF);

   Amp_Entity_Reference  : constant String
     := "&amp;";
--     Apos_Entity_Reference : constant String
--       := "&apos;";
   Quot_Entity_Reference : constant String
     := "&quot;";
   Gt_Entity_Reference   : constant String
     := "&gt;";
   Lt_Entity_Reference   : constant String
     := "&lt;";

   procedure Close_Tag (Self : in out Writer'Class);

   function Escape
    (Text       : League.Strings.Universal_String;
     Escape_All : Boolean := False)
       return League.Strings.Universal_String;


   ----------------
   -- Characters --
   ----------------

   overriding procedure Characters
     (Self    : in out Writer;
      Text    : League.Strings.Universal_String;
      Success : in out Boolean)
   is
      pragma Unreferenced (Success);
   begin
      Self.Close_Tag;

      if Self.CDATA then
         Self.Output.Put (Text);
      else
         Self.Output.Put (Escape (Text, False));
      end if;
   end Characters;

   ---------------
   -- Close_Tag --
   ---------------

   procedure Close_Tag (Self : in out Writer'Class) is
   begin
      if not Self.Tag.Is_Empty then
         Self.Output.Put (">");
         Self.Tag.Clear;
      end if;
   end Close_Tag;

   -------------
   -- Comment --
   -------------

   overriding procedure Comment
    (Self    : in out Writer;
     Text    : League.Strings.Universal_String;
     Success : in out Boolean)
   is
      pragma Unreferenced (Success);
   begin
      Self.Output.Put ("<!--");
      Self.Output.Put (Text);
      Self.Output.Put ("-->");
   end Comment;

   ---------------
   -- End_CDATA --
   ---------------

   overriding procedure End_CDATA
    (Self    : in out Writer;
     Success : in out Boolean)
   is
      pragma Unreferenced (Success);
   begin
      Self.CDATA := False;
      Self.Output.Put ("]]>");
   end End_CDATA;

   -----------------
   -- End_Element --
   -----------------

   overriding procedure End_Element
     (Self           : in out Writer;
      Namespace_URI  : League.Strings.Universal_String;
      Local_Name     : League.Strings.Universal_String;
      Qualified_Name : League.Strings.Universal_String;
      Success        : in out Boolean)
   is
      pragma Unreferenced (Namespace_URI, Qualified_Name, Success);
      use type League.Strings.Universal_String;
   begin
      if Self.Tag = Local_Name and then
        Self.Tag.To_Wide_Wide_String not in "code" | "html" | "a" | "li"
        and then (Self.Tag.Length = 1 or else
                   Self.Tag (2).To_Wide_Wide_Character not in '1' .. '9')
      then
         Self.Output.Put ("/>");
         Self.Tag.Clear;
      else
         Self.Close_Tag;
         Self.Output.Put ("</");
         Self.Output.Put (Local_Name);
         Self.Output.Put (">");
      end if;

      if Local_Name.Starts_With ("h") or else
        Local_Name.To_Wide_Wide_String = "p"
      then
         Self.Output.Put (New_Line);
      end if;
   end End_Element;

   ------------------
   -- Error_String --
   ------------------

   overriding function Error_String
     (Self : Writer) return League.Strings.Universal_String
   is
      pragma Unreferenced (Self);
   begin
      return League.Strings.Empty_Universal_String;
   end Error_String;

   function Escape
    (Text       : League.Strings.Universal_String;
     Escape_All : Boolean := False)
       return League.Strings.Universal_String
   is
      UTF_8 : constant String := Text.To_UTF_8_String;
      Result : Ada.Strings.Unbounded.Unbounded_String;

   begin
      for Code of UTF_8 loop
         case Code is
            when '&' =>
               Ada.Strings.Unbounded.Append (Result, Amp_Entity_Reference);

            when '"' =>
               if Escape_All then
                  Ada.Strings.Unbounded.Append (Result, "%22");
               else
                  Ada.Strings.Unbounded.Append (Result, Quot_Entity_Reference);
               end if;

            when '>' =>
               Ada.Strings.Unbounded.Append (Result, Gt_Entity_Reference);

            when '<' =>
               Ada.Strings.Unbounded.Append (Result, Lt_Entity_Reference);

            when 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' |
               '-' | '_' | '.' | '~' | '/' | '@' | '+' | ',' |
               '(' | ')' | '#' | '?' | '=' | ':' | '*'
               =>

               Ada.Strings.Unbounded.Append (Result, Code);

--              when '\' =>
--                 if Escape_All then
--                    Ada.Strings.Unbounded.Append (Result, "%5C");
--                 else
--                    Ada.Strings.Unbounded.Append (Result, Code);
--                 end if;

            when others =>

               if Escape_All or
                 Character'Pos (Code) in 16#1#  .. 16#8#
               | 16#B#  .. 16#C#
               | 16#E#  .. 16#1F#
               | 16#7F#
               then
                  declare
                     Image : String (1 .. 7);  --  -#16#xx#

                  begin
                     Ada.Integer_Text_IO.Put
                       (To   => Image,
                        Item => Character'Pos (Code),
                        Base => 16);
                     Ada.Strings.Unbounded.Append (Result, "%");
                     Ada.Strings.Unbounded.Append (Result, Image (5 .. 6));
                  end;
               else
                  Ada.Strings.Unbounded.Append (Result, Code);
               end if;
         end case;
      end loop;

      return League.Strings.From_UTF_8_String
        (Ada.Strings.Unbounded.To_String (Result));
   end Escape;

   overriding procedure Processing_Instruction
    (Self    : in out Writer;
     Target  : League.Strings.Universal_String;
     Data    : League.Strings.Universal_String;
     Success : in out Boolean)
   is
      pragma Unreferenced (Success);
   begin
      Self.Output.Put ("<?");
      Self.Output.Put (Target);
      if not Data.Is_Empty then
         Self.Output.Put (" ");
         Self.Output.Put (Data);
      end if;
      Self.Output.Put ("?>");
   end Processing_Instruction;

   ----------------------------
   -- Set_Output_Destination --
   ----------------------------

   procedure Set_Output_Destination
     (Self   : in out Writer'Class;
      Output : not null SAX_Output_Destination_Access) is
   begin
      Self.Output := Output;
   end Set_Output_Destination;

   -----------------
   -- Start_CDATA --
   -----------------

   overriding procedure Start_CDATA
    (Self    : in out Writer;
     Success : in out Boolean)
   is
      pragma Unreferenced (Success);
   begin
      Self.Output.Put ("<![CDATA[");
      Self.CDATA := True;
   end Start_CDATA;

   -------------------
   -- Start_Element --
   -------------------

   overriding procedure Start_Element
     (Self           : in out Writer;
      Namespace_URI  : League.Strings.Universal_String;
      Local_Name     : League.Strings.Universal_String;
      Qualified_Name : League.Strings.Universal_String;
      Attributes     : XML.SAX.Attributes.SAX_Attributes;
      Success        : in out Boolean) is
      pragma Unreferenced (Success, Namespace_URI, Qualified_Name);
   begin
      Self.Close_Tag;
      Self.Output.Put ("<");
      Self.Output.Put (Local_Name);

      if Local_Name.To_Wide_Wide_String = "hr" then
         Self.Output.Put (" ");
      end if;

      for J in 1 .. Attributes.Length loop
         Self.Output.Put (" ");
         Self.Output.Put (Attributes.Local_Name (J));
         Self.Output.Put ("=""");

         if Attributes.Local_Name (J).To_Wide_Wide_String = "href" then
            Self.Output.Put (Escape (Attributes.Value (J), True));
         elsif Attributes.Local_Name (J).To_Wide_Wide_String = "class" then
            Self.Output.Put (Attributes.Value (J));
         else
            Self.Output.Put (Escape (Attributes.Value (J), False));
         end if;

         Self.Output.Put ("""");
      end loop;

      Self.Tag := Local_Name;
   end Start_Element;

   --------------------------
   -- Unescaped_Characters --
   --------------------------

   not overriding procedure Unescaped_Characters
     (Self : in out Writer;
      Text : League.Strings.Universal_String) is
   begin
      Self.Close_Tag;
      Self.Output.Put (Text);
   end Unescaped_Characters;

end Custom_Writers;
