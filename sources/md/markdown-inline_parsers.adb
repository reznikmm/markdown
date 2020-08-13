--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Containers.Vectors;
with Ada.Containers.Generic_Anonymous_Array_Sort;
with Ada.Iterator_Interfaces;

with League.Characters;

package body Markdown.Inline_Parsers is

   type Position is record
      Line   : Positive;
      Column : Positive;
   end record;

   function "+" (Cursor : Position; Value : Integer) return Position is
     ((Cursor.Line, Cursor.Column + Value));

   type Delimiter_Kind is ('*', '_', '[', ']');

   subtype Emphasis_Kind is Delimiter_Kind range '*' .. '_';

   type Delimiter (Kind : Delimiter_Kind := '*') is record
      From : Position;
      Is_Deleted : Boolean := False;

      case Kind is
         when '*' | '_' =>
            Count : Positive;
            Is_Active : Boolean;
            Can_Open  : Boolean;
            Can_Close : Boolean;
         when '[' | ']' =>
            null;
      end case;
   end record;

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

   package Delimiter_Lists is
      type Delimiter_List is tagged private
        with
          Variable_Indexing => Reference,
          Default_Iterator  => Iterate,
          Iterator_Element  => Delimiter;

      type Delimiter_Index is new Positive;

      subtype Extended_Delimiter_Index is
        Delimiter_Index'Base range 0 .. Delimiter_Index'Last;

      function Has_Element
        (Cursor : Extended_Delimiter_Index) return Boolean is (Cursor > 0);

      package Delimiter_Iterator_Interfaces is new
        Ada.Iterator_Interfaces (Extended_Delimiter_Index, Has_Element);

      type Reference_Type
        (Element : not null access Delimiter) is
      null record
        with
          Implicit_Dereference => Element;

      function Reference
        (Self   : aliased in out Delimiter_List'Class;
         Cursor : Delimiter_Index) return Reference_Type
       with Inline;

      function Iterate (Self : aliased Delimiter_List'Class)
        return Delimiter_Iterator_Interfaces.Reversible_Iterator'Class;

      type Delimiter_Filter_Kind is
        (Any_Element,
         Emphasis_Close,
         Emphasis_Open);

      type Delimiter_Filter (Kind : Delimiter_Filter_Kind := Any_Element) is
         record
            case Kind is
               when Any_Element | Emphasis_Close =>
                  null;
               when Emphasis_Open =>
                  Emphasis : Emphasis_Kind;
                  Count    : Positive;
            end case;
         end record;

      function Each
        (Self   : aliased Delimiter_List'Class;
         Filter : Delimiter_Filter := (Kind => Any_Element);
         From   : Delimiter_Index := 1;
         To     : Extended_Delimiter_Index := Delimiter_Index'Last)
         return Delimiter_Iterator_Interfaces.Reversible_Iterator'Class;

      procedure Item_Not_Found
        (Self     : in out Delimiter_List'Class;
         Emphasis : Emphasis_Kind;
         Count    : Positive;
         Index    : Delimiter_Index);

      procedure Append
        (Self : in out Delimiter_List'Class;
         Item : Delimiter);

   private
      type X is array (Emphasis_Kind, Natural range 0 .. 2) of
        Extended_Delimiter_Index;

      package Delimiter_Vectors is new Ada.Containers.Vectors
        (Delimiter_Index, Delimiter);

      type Delimiter_List is tagged record
         Data : Delimiter_Vectors.Vector;
         Openers_Bottom : X := (others => (others => 0));
      end record;
   end Delimiter_Lists;

   package body Delimiter_Lists is

      type Iterator is limited new
        Delimiter_Iterator_Interfaces.Reversible_Iterator with
      record
         List   : not null access constant Delimiter_List;
         Filter : Delimiter_Filter;
         First  : Delimiter_Index;
         Last   : Extended_Delimiter_Index;
      end record;

      overriding function First
        (Self : Iterator) return Extended_Delimiter_Index;

      overriding function Last
        (Self : Iterator) return Extended_Delimiter_Index;

      overriding function Next
        (Self  : Iterator;
         Index : Extended_Delimiter_Index) return Extended_Delimiter_Index;

      overriding function Previous
        (Self  : Iterator;
         Index : Extended_Delimiter_Index) return Extended_Delimiter_Index;

      function Check
        (Item   : Delimiter;
         Filter : Delimiter_Filter) return Boolean;

      ------------
      -- Append --
      ------------

      procedure Append
        (Self : in out Delimiter_List'Class;
         Item : Delimiter) is
      begin
         Self.Data.Append (Item);

         if Item.Kind in Emphasis_Kind
           and then Item.Can_Open
           and then Self.Openers_Bottom
             (Item.Kind, Item.Count mod 3) = 0
         then
            Self.Openers_Bottom (Item.Kind, Item.Count mod 3) :=
              Self.Data.Last_Index;
         end if;
      end Append;

      -----------
      -- Check --
      -----------

      function Check
        (Item   : Delimiter;
         Filter : Delimiter_Filter) return Boolean is
      begin
         if not Item.Is_Deleted then
            case Filter.Kind is
               when Emphasis_Open =>
                  if Item.Kind = Filter.Emphasis
                    and then Item.Can_Open
                  then
                     return True;
                  end if;
               when Emphasis_Close =>
                  if Item.Kind in Emphasis_Kind
                    and then Item.Can_Close
                  then
                     return True;
                  end if;
               when Any_Element =>
                  return True;
            end case;
         end if;

         return False;
      end Check;

      ----------
      -- Each --
      ----------

      function Each
        (Self   : aliased Delimiter_List'Class;
         Filter : Delimiter_Filter := (Kind => Any_Element);
         From   : Delimiter_Index := 1;
         To     : Extended_Delimiter_Index := Delimiter_Index'Last)
         return Delimiter_Iterator_Interfaces.Reversible_Iterator'Class
      is
         Start : Delimiter_Index := From;
      begin
         if Filter.Kind = Emphasis_Open then
            Start := Delimiter_Index'Max
              (From,
               Self.Openers_Bottom (Filter.Emphasis, Filter.Count mod 3));
         end if;

         return Iterator'(Self'Access, Filter, Start,
                          Delimiter_Index'Min (To, Self.Data.Last_Index));
      end Each;

      -----------
      -- First --
      -----------

      overriding function First
        (Self : Iterator) return Extended_Delimiter_Index is
      begin
         for J in Self.First .. Self.Last loop
            if Check (Self.List.Data (J), Self.Filter) then
               return J;
            end if;
         end loop;

         return 0;
      end First;

      function Iterate (Self : aliased Delimiter_List'Class)
        return Delimiter_Iterator_Interfaces.Reversible_Iterator'Class
          is (Self.Each);

      --------------------
      -- Item_Not_Found --
      --------------------

      procedure Item_Not_Found
        (Self     : in out Delimiter_List'Class;
         Emphasis : Emphasis_Kind;
         Count    : Positive;
         Index    : Delimiter_Index) is
      begin
         Self.Openers_Bottom (Emphasis, Count mod 3) := Index;
      end Item_Not_Found;

      overriding function Last
        (Self : Iterator) return Extended_Delimiter_Index is
          (Self.Previous (Self.Last + 1));

      ----------
      -- Next --
      ----------

      overriding function Next
        (Self  : Iterator;
         Index : Extended_Delimiter_Index) return Extended_Delimiter_Index is
      begin
         if Index > 0 then
            for J in Index + 1 .. Self.Last loop
               if Check (Self.List.Data (J), Self.Filter) then
                  return J;
               end if;
            end loop;
         end if;

         return 0;
      end Next;

      --------------
      -- Previous --
      --------------

      overriding function Previous
        (Self  : Iterator;
         Index : Extended_Delimiter_Index) return Extended_Delimiter_Index is
      begin
         if Index > 0 then
            for J in reverse Self.First .. Index - 1 loop
               if Check (Self.List.Data (J), Self.Filter) then
                  return J;
               end if;
            end loop;
         end if;

         return 0;
      end Previous;

      function Reference
        (Self   : aliased in out Delimiter_List'Class;
         Cursor : Delimiter_Index) return Reference_Type is
      begin
         return (Element => Self.Data.Reference (Cursor).Element);
      end Reference;

   end Delimiter_Lists;

   procedure Process_Emphasis
     (DL     : in out Delimiter_Lists.Delimiter_List;
      Markup : out Markup_Vectors.Vector;
      Bottom : Delimiter_Lists.Delimiter_Index := 1);

   ----------------------
   -- Process_Emphasis --
   ----------------------

   procedure Process_Emphasis
     (DL     : in out Delimiter_Lists.Delimiter_List;
      Markup : out Markup_Vectors.Vector;
      Bottom : Delimiter_Lists.Delimiter_Index := 1)
   is
      use Delimiter_Lists;
   begin
      for J in DL.Each ((Kind => Emphasis_Close), Bottom) loop
         declare
            Closer : Delimiter renames DL (J);
            Found  : Boolean := False;
         begin
            Each_Open_Emphasis :
            for K in reverse DL.Each
              ((Emphasis_Open, Closer.Kind, Closer.Count), Bottom, J - 1)
            loop
               declare
                  Opener : Delimiter renames DL (K);
                  Count  : Positive range 1 .. 2;
               begin
                  while not Opener.Is_Deleted and then
                     --  If one of the delimiters can both open and close
                     --  emphasis, then the sum of the lengths of the
                     --  delimiter runs containing the opening and closing
                     --  delimiters must not be a multiple of 3 unless both
                     --  lengths are multiples of 3.
                    (not ((Opener.Can_Open and Opener.Can_Close) or
                          (Closer.Can_Open and Closer.Can_Close))
                     or else (Opener.Count + Closer.Count) mod 3 /= 0
                     or else (Opener.Count mod 3 = 0 and
                                 Closer.Count mod 3 = 0))
                  loop
                     Found := True;
                     Count := Positive'Min
                       (2, Positive'Min (Opener.Count, Closer.Count));

                     Markup.Append
                       ((Opener.From + (Opener.Count - Count), Count));

                     Markup.Append ((Closer.From, Count));

                     for M in K + 1 .. J - 1 loop
                        DL (M).Is_Deleted := True;
                     end loop;

                     if Opener.Count = Count then
                        Opener.Is_Deleted := True;
                     else
                        Opener.Count := Opener.Count - Count;
                     end if;

                     if Closer.Count = Count then
                        Closer.Is_Deleted := True;
                        exit Each_Open_Emphasis;
                     else
                        Closer.Count := Closer.Count - Count;
                        Closer.From := Closer.From  + Count;
                     end if;
                  end loop;
               end;
            end loop Each_Open_Emphasis;

            if not Found then
               DL.Item_Not_Found (Closer.Kind, Closer.Count, J);
            end if;
         end;
      end loop;
   end Process_Emphasis;

   -----------------
   -- Find_Markup --
   -----------------

   procedure Find_Markup
     (Text   : League.String_Vectors.Universal_String_Vector;
      Markup : out Markup_Vectors.Vector)
   is
      State        : Scanner_State;
      List         : Delimiter_Lists.Delimiter_List;
      Cursor       : Position := (1, 1);
      Item         : Delimiter;
      Is_Delimiter : Boolean;
   begin
      while Cursor.Line <= Text.Length loop
         Read_Delimiter (Text, Cursor, State, Item, Is_Delimiter);

         if Is_Delimiter then
            List.Append (Item);
         end if;
      end loop;

      Process_Emphasis (List, Markup);
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

   function Parse
     (Register : Markdown.Link_Registers.Link_Register'Class;
      Text     : League.String_Vectors.Universal_String_Vector)
        return Annotated_Text
   is
      pragma Unreferenced (Register);
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

         when '[' =>
            State := Get_State (Text, Cursor);
            Item := (Kind   => '[',
                     From       => Cursor,
                     Is_Deleted => False);
            Step (Text, 1, Cursor);
            Is_Delimiter := True;

         when ']' =>
            State := Get_State (Text, Cursor);
            Item := (Kind   => ']',
                     From       => Cursor,
                     Is_Deleted => False);
            Step (Text, 1, Cursor);
            Is_Delimiter := True;
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
