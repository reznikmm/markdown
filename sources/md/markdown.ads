--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

package Markdown is

   pragma Pure;

   type Continuation_Kind is (No_Match, Match, Consumed);
   --  @value No_Match   Line doesn't match current  block
   --  @value Match      Line prefix matches current  block
   --  @value Consumed   Whole line matches current  block

   subtype Matches is Continuation_Kind range Match .. Consumed;

end Markdown;
