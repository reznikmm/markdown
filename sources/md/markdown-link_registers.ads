--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with League.Strings;
with League.String_Vectors;

package Markdown.Link_Registers is

   type Link_Register is limited interface;

   not overriding procedure Resolve
     (Self        : Link_Register;
      Label       : League.Strings.Universal_String;
      Found       : out Boolean;
      Destination : out League.Strings.Universal_String;
      Title       : out League.String_Vectors.Universal_String_Vector) is
        abstract;

end Markdown.Link_Registers;
