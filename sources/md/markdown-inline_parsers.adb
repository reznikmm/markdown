--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Containers.Vectors;
with Ada.Containers.Generic_Anonymous_Array_Sort;

with League.Characters;

package body Markdown.Inline_Parsers is

   type Position is record
      Line   : Positive;
      Column : Positive;
   end record;

   function "+" (Cursor : Position; Value : Integer) return Position is
     ((Cursor.Line, Cursor.Column + Value));

   type Delimiter_Kind is ('*', '_');

   type Delimiter (Kind : Delimiter_Kind := '*') is record
      From : Position;
      Is_Deleted : Boolean := False;

      case Kind is
         when '*' | '_' =>
            Count : Positive;
            Is_Active : Boolean;
            Can_Open  : Boolean;
            Can_Close : Boolean;
      end case;
   end record;

   package Delimiter_Vectors is new Ada.Containers.Vectors
     (Positive, Delimiter);

   type Markup is record
      From   : Position;
      Length : Positive;
   end record;

   type Markup_Index is new Positive;

   package Markup_Vectors is new Ada.Containers.Vectors (Markup_Index, Markup);

   procedure Find_Markup
     (Text   : League.String_Vectors.Universal_String_Vector;
      Markup : out Markup_Vectors.Vector);

   type Scanner_State is record
      Is_White_Space : Boolean := True;
      Is_Punctuation : Boolean := False;
   end record;

   procedure Read_Delimiter
     (Text         : League.String_Vectors.Universal_String_Vector;
      Cursor       : in out Position;
      State        : in out Scanner_State;
      Item         : out Delimiter;
      Is_Delimiter : out Boolean);

   procedure Read_Character
     (Text   : League.String_Vectors.Universal_String_Vector;
      Cursor : in out Position;
      Result : in out League.Strings.Universal_String);

   function Get_State
     (Text   : League.String_Vectors.Universal_String_Vector;
      Cursor : Position) return Scanner_State;

   function To_Annotation
     (Item : Markup;
      Pos  : Positive) return Annotation;

   function To_Annotated_Text
     (Text   : League.String_Vectors.Universal_String_Vector;
      Markup : Markup_Vectors.Vector) return Annotated_Text;

   procedure Step
     (Text   : League.String_Vectors.Universal_String_Vector;
      Value  : Natural;
      Cursor : in out Position);

   function Count_Character
     (Line : League.Strings.Universal_String;
      From : Positive) return Positive;

   ---------------------
   -- Count_Character --
   ---------------------

   function Count_Character
     (Line : League.Strings.Universal_String;
      From : Positive) return Positive
   is
      use type League.Characters.Universal_Character;

      Char   : constant League.Characters.Universal_Character := Line (From);
      Result : Positive := 1;
   begin
      for J in From + 1 .. Line.Length loop
         exit when Line (J) /= Char;
         Result := Result + 1;
      end loop;

      return Result;
   end Count_Character;

   -----------------
   -- Find_Markup --
   -----------------

   procedure Find_Markup
     (Text   : League.String_Vectors.Universal_String_Vector;
      Markup : out Markup_Vectors.Vector)
   is
      State        : Scanner_State;
      Stack        : Delimiter_Vectors.Vector;
      Cursor       : Position := (1, 1);
      Item         : Delimiter;
      Is_Delimiter : Boolean;
   begin
      while Cursor.Line <= Text.Length loop
         Read_Delimiter (Text, Cursor, State, Item, Is_Delimiter);

         if Is_Delimiter then
            Stack.Append (Item);
         end if;
      end loop;

      declare
         Stack_Bottom : constant Positive := 1;
         J : Positive := Stack_Bottom;
         Openers_Bottom : array
           (Delimiter_Kind range '*' .. '_', Natural range 0 .. 2) of
             Positive := (others => (others => Stack_Bottom));
      begin
         while J <= Stack.Last_Index loop
            declare
               Closer : Delimiter renames Stack (J);
               Found  : Boolean := False;
               Count  : Positive;
            begin
               if not Closer.Is_Deleted and then
--                 Closer.Kind in Openers_Bottom'Range (1) and then
                 Closer.Can_Close
               then
                  for K in reverse
                    Openers_Bottom (Closer.Kind, Closer.Count mod 3) .. J - 1
                  loop
                     if not Stack (K).Is_Deleted and then
                       Stack (K).Kind = Closer.Kind and then
                       Stack (K).Can_Open and then
                       --  If one of the delimiters can both open and close
                       --  emphasis, then the sum of the lengths of the
                       --  delimiter runs containing the opening and closing
                       --  delimiters must not be a multiple of 3 unless both
                       --  lengths are multiples of 3.
                       (not ((Stack (K).Can_Open and Stack (K).Can_Close) or
                             (Closer.Can_Open and Closer.Can_Close))
                       or else (Stack (K).Count + Closer.Count) mod 3 /= 0
                       or else (Stack (K).Count mod 3 = 0
                                and Closer.Count mod 3 = 0))
                     then
                        declare
                           Opener : Delimiter renames Stack (K);
                        begin
                           Count := Positive'Min
                             (2, Positive'Min (Opener.Count, Closer.Count));

                           Markup.Append
                             ((Opener.From + (Opener.Count - Count), Count));

                           Markup.Append ((Closer.From, Count));

                           for M in K + 1 .. J - 1 loop
                              Stack (M).Is_Deleted := True;
                           end loop;

                           if Opener.Count = Count then
                              Opener.Is_Deleted := True;
                           else
                              Opener.Count := Opener.Count - Count;
                           end if;

                           if Closer.Count = Count then
                              Closer.Is_Deleted := True;
                              J := J + 1;
                           else
                              Closer.Count := Closer.Count - Count;
                              Closer.From := Closer.From  + Count;
                           end if;

                           Found := True;
                           exit;
                        end;
                     end if;
                  end loop;

                  if not Found then
                     Openers_Bottom (Closer.Kind, Closer.Count mod 3) :=
                       Positive'Max (J - 1, Stack_Bottom);

                     if not Closer.Can_Open then
                        Closer.Is_Deleted := True;
                     end if;

                     J := J + 1;
                  end if;
               else
                  J := J + 1;
               end if;
            end;
         end loop;
      end;
   end Find_Markup;

   ---------------
   -- Get_State --
   ---------------

   function Get_State
     (Text   : League.String_Vectors.Universal_String_Vector;
      Cursor : Position) return Scanner_State
   is
      Line : League.Strings.Universal_String;
      --  FIXME: use Zs and Pc, Pd, Pe, Pf, Pi, Po, or Ps
   begin
      if Cursor.Line > Text.Length
        or else Text (Cursor.Line).Is_Empty
      then
         return (Is_White_Space => True, Is_Punctuation => False);
      else
         Line := Text (Cursor.Line);
      end if;

      if Line (Cursor.Column).To_Wide_Wide_Character in ' ' then
         return (Is_White_Space => True, Is_Punctuation => False);
      elsif Line (Cursor.Column).To_Wide_Wide_Character in
        '!' | '"' | '#' | '$' | '%' | '&' | ''' | '(' | ')' | '*' | '+' |
        ',' | '-' | '.' | '/' | ':' | ';' | '<' | '=' | '>' | '?' | '@' |
        '[' | '\' | ']' | '^' | '_' | '`' | '{' | '|' | '}' | '~'
      then
         return (Is_White_Space => False, Is_Punctuation => True);
      else
         return (Is_White_Space => False, Is_Punctuation => False);
      end if;
   end Get_State;

   -----------
   -- Parse --
   -----------

   function Parse (Text : League.String_Vectors.Universal_String_Vector)
     return Annotated_Text
   is
      Markup : Markup_Vectors.Vector;

   begin
      Find_Markup (Text, Markup);

      return To_Annotated_Text (Text, Markup);

   end Parse;

   --------------------
   -- Read_Character --
   --------------------

   procedure Read_Character
     (Text   : League.String_Vectors.Universal_String_Vector;
      Cursor : in out Position;
      Result : in out League.Strings.Universal_String)
   is
      Line : League.Strings.Universal_String renames Text (Cursor.Line);
   begin
      if Line.Is_Empty then
         Step (Text, 1, Cursor);
         return;
      end if;

      case Line (Cursor.Column).To_Wide_Wide_Character is
         when '\' =>
            Step (Text, 1, Cursor);
            if Cursor.Line > Text.Length then
               Result.Append ('\');
            elsif Get_State (Text, Cursor).Is_Punctuation then
               Result.Append (Line (Cursor.Column));
               Step (Text, 1, Cursor);
            else
               Result.Append ('\');
            end if;
         when others =>
            Result.Append (Line (Cursor.Column));
            Step (Text, 1, Cursor);
      end case;
   end Read_Character;

   --------------------
   -- Read_Delimiter --
   --------------------

   procedure Read_Delimiter
     (Text         : League.String_Vectors.Universal_String_Vector;
      Cursor       : in out Position;
      State        : in out Scanner_State;
      Item         : out Delimiter;
      Is_Delimiter : out Boolean)
   is
      function Get_Follow_State (Cursor : Position) return Scanner_State;

      ----------------------
      -- Get_Follow_State --
      ----------------------

      function Get_Follow_State (Cursor : Position) return Scanner_State is
      begin
         if Cursor.Column = 1 then
            return (Is_White_Space => True, Is_Punctuation => False);
         else
            return Get_State (Text, Cursor);
         end if;
      end Get_Follow_State;

      Line   : League.Strings.Universal_String renames Text (Cursor.Line);
      Follow : Scanner_State;
   begin
      if Line.Is_Empty then
         State := Get_State (Text, Cursor);
         Step (Text, 1, Cursor);
         Is_Delimiter := False;
         return;
      end if;

      case Line (Cursor.Column).To_Wide_Wide_Character is
         when '*' =>
            declare
               Next   : Delimiter :=
                 (Kind   => '*',
                  From   => Cursor,
                  Count  => Count_Character (Line, Cursor.Column),
                  others => False);
            begin
               Step (Text, Next.Count, Cursor);
               Follow := Get_Follow_State (Cursor);

               --  Left flanking
               Next.Can_Open := not Follow.Is_White_Space and then
                 (not Follow.Is_Punctuation or else
                   (State.Is_White_Space or State.Is_Punctuation));

               --  Right flanking
               Next.Can_Close := not State.Is_White_Space and then
                 (not State.Is_Punctuation or else
                   (Follow.Is_White_Space or Follow.Is_Punctuation));

               State := Follow;
               Item := Next;
               Is_Delimiter := True;
            end;

         when '_' =>
            declare
               Left_Flanking : Boolean;
               Right_Flanking : Boolean;
               Next   : Delimiter :=
                 (Kind   => '_',
                  From   => Cursor,
                  Count  => Count_Character (Line, Cursor.Column),
                  others => False);
            begin
               Step (Text, Next.Count, Cursor);
               Follow := Get_Follow_State (Cursor);

               Left_Flanking := not Follow.Is_White_Space and then
                 (not Follow.Is_Punctuation or else
                   (State.Is_White_Space or State.Is_Punctuation));

               --  Right flanking
               Right_Flanking := not State.Is_White_Space and then
                 (not State.Is_Punctuation or else
                   (Follow.Is_White_Space or Follow.Is_Punctuation));

               Next.Can_Open := Left_Flanking and
                 (not Right_Flanking or else State.Is_Punctuation);

               Next.Can_Close := Right_Flanking and
                 (not Left_Flanking or else Follow.Is_Punctuation);

               State := Follow;
               Item := Next;
               Is_Delimiter := True;
            end;

         when '\' =>
            State := Get_State (Text, Cursor);
            Step (Text, 2, Cursor);
            Is_Delimiter := False;

         when others =>
            State := Get_State (Text, Cursor);
            Step (Text, 1, Cursor);
            Is_Delimiter := False;

      end case;

      if Cursor.Column = 1 then
         State := (Is_White_Space => True, Is_Punctuation => False);
      end if;
   end Read_Delimiter;

   ----------
   -- Step --
   ----------

   procedure Step
     (Text   : League.String_Vectors.Universal_String_Vector;
      Value  : Natural;
      Cursor : in out Position)
   is
      Line : League.Strings.Universal_String renames Text (Cursor.Line);
   begin
      if Cursor.Column + Value > Line.Length then
         Cursor := (Cursor.Line + 1, 1);
      else
         Cursor.Column := Cursor.Column + Value;
      end if;
   end Step;

   -----------------------
   -- To_Annotated_Text --
   -----------------------

   function To_Annotated_Text
     (Text   : League.String_Vectors.Universal_String_Vector;
      Markup : Markup_Vectors.Vector) return Annotated_Text
   is
      function Less (Left, Right : Positive) return Boolean;
      procedure Swap (Left, Right : Positive);

      Map : array
        (Positive range 1 .. Natural (Markup.Length)) of Markup_Index;

      Annotation_Map : array
        (Markup_Index range 1 .. Markup.Last_Index / 2) of Natural :=
        (others => 0);

      ----------
      -- Less --
      ----------

      function Less (Left, Right : Positive) return Boolean is
         L : Inline_Parsers.Markup renames Markup (Map (Left));
         R : Inline_Parsers.Markup renames Markup (Map (Right));
      begin
         return L.From.Line < R.From.Line
           or (L.From.Line = R.From.Line and L.From.Column < R.From.Column);
      end Less;

      ----------
      -- Swap --
      ----------

      procedure Swap (Left, Right : Positive) is
         Temp : constant Markup_Index := Map (Left);
      begin
         Map (Left) := Map (Right);
         Map (Right) := Temp;
      end Swap;

      procedure Sort is new Ada.Containers.Generic_Anonymous_Array_Sort
        (Index_Type => Positive,
         Less       => Less,
         Swap       => Swap);

      Plain_Text : League.Strings.Universal_String;
      Cursor     : Position := (1, 1);
      Index      : Positive := Map'First;
      Last       : Natural := 0;
      Annotation : Annotation_Array (1 .. Map'Last / 2 + Text.Length);

   begin
      for J in 1 .. Map'Last loop
         Map (J) := Markup_Index (J);
      end loop;

      Sort (1, Map'Last);

      while Cursor.Line <= Text.Length loop
         if Index in Map'Range and then
           Cursor = Markup (Map (Index)).From
         then
            declare
               Item : Inline_Parsers.Markup renames Markup (Map (Index));
            begin
               if Map (Index) mod 2 = 1 then  --  Open markup
                  Last := Last + 1;
                  Annotation (Last) := To_Annotation
                    (Item, Plain_Text.Length + 1);
                  Annotation_Map ((Map (Index) + 1) / 2) := Last;
               else  --  Close markup
                  Annotation
                    (Annotation_Map ((Map (Index)) / 2)).To :=
                      Plain_Text.Length;
               end if;

               Step (Text, Item.Length, Cursor);

               Index := Index + 1;
            end;
         else
            Read_Character (Text, Cursor, Plain_Text);
         end if;

         if Cursor.Line <= Text.Length and Cursor.Column = 1 then
            Plain_Text.Append (' ');
            Last := Last + 1;
            Annotation (Last) := (Soft_Line_Break, Plain_Text.Length);
         end if;
      end loop;

      return (Last, Plain_Text, Annotation (1 .. Last));
   end To_Annotated_Text;

   -------------------
   -- To_Annotation --
   -------------------

   function To_Annotation (Item : Markup;
      Pos  : Positive) return Annotation is
   begin
      if Item.Length = 1 then
         return (Emphasis, Pos, Pos);
      else
         return (Strong, Pos, Pos);
      end if;
   end To_Annotation;

end Markdown.Inline_Parsers;
