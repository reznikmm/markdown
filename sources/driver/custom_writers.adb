--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Characters.Wide_Wide_Latin_1;

package body Custom_Writers is

   New_Line : constant Wide_Wide_String :=
     (1 => Ada.Characters.Wide_Wide_Latin_1.LF);

   Amp_Entity_Reference  : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("&amp;");
--     Apos_Entity_Reference : constant League.Strings.Universal_String
--       := League.Strings.To_Universal_String ("&apos;");
   Quot_Entity_Reference : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("&quot;");
   Gt_Entity_Reference   : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("&gt;");
   Lt_Entity_Reference   : constant League.Strings.Universal_String
     := League.Strings.To_Universal_String ("&lt;");

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
      Self.Output.Put (Escape (Text, True));
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
        Self.Tag.To_Wide_Wide_String not in "code" | "html" and then
        (Self.Tag.Length = 1 or else
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
      use type League.Strings.Universal_String;
      Code : Wide_Wide_Character;

   begin
      return Result : League.Strings.Universal_String do
         for J in 1 .. Text.Length loop
            Code := Text.Element (J).To_Wide_Wide_Character;

            case Code is
               when '&' =>
                  Result.Append (Amp_Entity_Reference);

--                 when ''' =>
--                    if Escape_All then
--                       Result.Append (Apos_Entity_Reference);
--                    else
--                   Result.Append (Text.Element (J).To_Wide_Wide_Character);
--                    end if;

               when '"' =>
                  if Escape_All then
                     Result.Append (Quot_Entity_Reference);
                  else
                     Result.Append (Text.Element (J).To_Wide_Wide_Character);
                  end if;

               when '>' =>
                  if Escape_All then
                     Result.Append (Gt_Entity_Reference);
                  else
                     Result.Append (Text.Element (J).To_Wide_Wide_Character);
                  end if;

               when '<' =>
                  Result.Append (Lt_Entity_Reference);

               when others =>

                  if Wide_Wide_Character'Pos (Code) in 16#1#  .. 16#8#
                       | 16#B#  .. 16#C#
                       | 16#E#  .. 16#1F#
                       | 16#7F# .. 16#84#
                       | 16#86# .. 16#9F#
                  then
                     declare
                        Image : constant Wide_Wide_String :=
                          Integer'Wide_Wide_Image
                            (Wide_Wide_Character'Pos (Code));

                     begin
                        Result := Result
                          & "&#"
                          & Image (Image'First + 1 .. Image'Last)
                          & ";";
                     end;
                  else
                     Result.Append (Text.Element (J).To_Wide_Wide_Character);
                  end if;
            end case;
         end loop;
      end return;
   end Escape;

   ----------------------------
   -- Set_Output_Destination --
   ----------------------------

   procedure Set_Output_Destination
     (Self   : in out Writer'Class;
      Output : not null SAX_Output_Destination_Access) is
   begin
      Self.Output := Output;
   end Set_Output_Destination;

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
         Self.Output.Put (Attributes.Value (J));
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
