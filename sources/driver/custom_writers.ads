--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with League.Strings;

with XML.SAX.Attributes;
with XML.SAX.Output_Destinations;
with XML.SAX.Writers;

package Custom_Writers is

   type SAX_Output_Destination_Access is
     access all XML.SAX.Output_Destinations.SAX_Output_Destination'Class;

   type Writer is limited new XML.SAX.Writers.SAX_Writer with private;

   procedure Set_Output_Destination
     (Self   : in out Writer'Class;
      Output : not null SAX_Output_Destination_Access);

   not overriding procedure Unescaped_Characters
     (Self : in out Writer;
      Text : League.Strings.Universal_String);

private

   type Writer is limited new XML.SAX.Writers.SAX_Writer with record
      Output : SAX_Output_Destination_Access;
      Tag    : League.Strings.Universal_String;
   end record;

   overriding function Error_String
     (Self : Writer) return League.Strings.Universal_String;

   overriding procedure Characters
     (Self    : in out Writer;
      Text    : League.Strings.Universal_String;
      Success : in out Boolean);

   overriding procedure End_Element
     (Self           : in out Writer;
      Namespace_URI  : League.Strings.Universal_String;
      Local_Name     : League.Strings.Universal_String;
      Qualified_Name : League.Strings.Universal_String;
      Success        : in out Boolean);

   overriding procedure Start_Element
     (Self           : in out Writer;
      Namespace_URI  : League.Strings.Universal_String;
      Local_Name     : League.Strings.Universal_String;
      Qualified_Name : League.Strings.Universal_String;
      Attributes     : XML.SAX.Attributes.SAX_Attributes;
      Success        : in out Boolean);

end Custom_Writers;
