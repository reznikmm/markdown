--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

private
package Markdown.Inline_Parsers.Autolinks is

   procedure Find
     (Text   : Plain_Texts.Plain_Text;
      Cursor : Position;
      State  : in out Optional_Inline_State);

end Markdown.Inline_Parsers.Autolinks;
