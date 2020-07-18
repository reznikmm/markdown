--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Markdown.Paragraphs;

package Markdown.Visitors is

   type Visitor is limited interface;

   not overriding procedure Paragraph
     (Self  : in out Visitor;
      Value : Markdown.Paragraphs.Paragraph) is null;

end Markdown.Visitors;
