--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Containers.Generic_Anonymous_Array_Sort;
with Ada.Iterator_Interfaces;

with League.Characters;
with League.Regexps;

with Markdown.Common_Patterns;
with Markdown.Inline_Parsers.Autolinks;
with Markdown.Inline_Parsers.Code_Spans;

package body Markdown.Inline_Parsers is

   function "+" (Text : Wide_Wide_String)
     return League.Regexps.Regexp_Pattern
       is (League.Regexps.Compile (League.Strings.To_Universal_String (Text)));

   Link_Title : Wide_Wide_String renames Markdown.Common_Patterns.Link_Title;
   --  Groups: 4

   Link_Start_Pattern : constant League.Regexps.Regexp_Pattern :=
     +("^\([\ \t\n\v\f\r\>]*");

   Title_Pattern : constant League.Regexps.Regexp_Pattern :=
     +("^[\ \t\n\v\f\r\>]*(" & Link_Title & ")?([\ \t\n\v\f\r\>]*\))?");
   --  Groups:            1    2,3,4,5         6

   Title_Close_Group : constant array (Positive range 1 .. 3) of Positive :=
     (2, 3, 5);  --  Close title group numbers

   package body Plain_Texts is

      function First (Self : Plain_Text'Class) return Position is (Self.From);

      procedure Initialize
        (Self : in out Plain_Text'Class;
         Text : League.String_Vectors.Universal_String_Vector;
         From : Position := (1, 1);
         To   : Position := (Positive'Last, Positive'Last)) is
      begin
         Self.Data := Text;
         Self.From := From;

         if To.Line > Text.Length then
            Self.To := (Text.Length, Text (Text.Length).Length);
         else
            Self.To := To;
         end if;
      end Initialize;

      procedure Initialize
        (Self : in out Plain_Text'Class;
         Text : Plain_Text'Class;
         From : Position;
         To   : Position := (Positive'Last, Positive'Last)) is
      begin
         Self.Data := Text.Data;
         Self.From := From;

         if To > Text.To then
            Self.To := Text.To;
         elsif To.Column = 1 then
            Self.To := (To.Line - 1, Text.Data (To.Line - 1).Length);
         else
            Self.To := (To.Line, To.Column - 1);
         end if;
      end Initialize;

      function Last (Self : Plain_Text'Class) return Position is (Self.To);

      function Line
        (Self  : Plain_Text'Class;
         Index : Positive) return League.Strings.Universal_String
      is
         use type League.Strings.Universal_String;

         function Space (Count : Positive)
           return League.Strings.Universal_String;

         -----------
         -- Space --
         -----------

         function Space (Count : Positive)
           return League.Strings.Universal_String
         is
            Blank : constant Wide_Wide_String (1 .. 80) := (others => ' ');
         begin
            if Count <= Blank'Last then
               return League.Strings.To_Universal_String (Blank (1 .. Count));
            else
               return Blank & Space (Count - Blank'Last);
            end if;
         end Space;

         Result : League.Strings.Universal_String := Self.Data (Index);
      begin
         if Index = Self.From.Line and Self.From.Column > 1 then
            Result := Space (Self.From.Column - 1) &
              Result.Tail_From (Self.From.Column);
         end if;

         if Index = Self.To.Line and Self.To.Column < Result.Length then
            Result := Result.Head_To (Self.To.Column);
         end if;

         return Result;
      end Line;

      ----------
      -- Line --
      ----------

      function Line
        (Self : Plain_Text'Class;
         From : Position) return League.Strings.Universal_String is
      begin
         return Self.Data (From.Line).Tail_From (From.Column);
      end Line;

      function Lines (Self  : Plain_Text'Class) return Positive is
      begin
         return Self.To.Line - Self.From.Line + 1;
      end Lines;

      procedure Step
        (Self   : Plain_Text'Class;
         Value  : Natural;
         Cursor : in out Position)
      is
         Line : League.Strings.Universal_String renames
           Self.Data (Cursor.Line);
      begin
         if Cursor.Column + Value > Line.Length then
            Cursor := (Cursor.Line + 1, 1);
         else
            Cursor.Column := Cursor.Column + Value;
         end if;
      end Step;

   end Plain_Texts;

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
         when '[' =>
            null;
         when ']' =>
            To : Position;
      end case;
   end record;

   type Markup_Kind is (Emphasis, Link);

   type Markup (Kind : Markup_Kind := Emphasis) is record
      From   : Position;
      Length : Positive;
      case Kind is
         when Link =>
            URL : League.Strings.Universal_String;
            Title : League.String_Vectors.Universal_String_Vector;
         when Emphasis =>
            null;
      end case;
   end record;

   type Markup_Index is new Positive;

   package Markup_Vectors is new Ada.Containers.Vectors (Markup_Index, Markup);

   procedure Find_Markup
     (Register : Markdown.Link_Registers.Link_Register'Class;
      Text     : Plain_Texts.Plain_Text;
      Markup   : in out Markup_Vectors.Vector);

   type Scanner_State is record
      Is_White_Space : Boolean := True;
      Is_Punctuation : Boolean := False;
   end record;

   procedure Read_Delimiter
     (Text         : Plain_Texts.Plain_Text;
      Cursor       : in out Position;
      State        : in out Scanner_State;
      Item         : out Delimiter;
      Is_Delimiter : out Boolean);

   procedure Read_Character
     (Text   : Plain_Texts.Plain_Text;
      Cursor : in out Position;
      Result : in out League.Strings.Universal_String);

   function Get_State
     (Text   : Plain_Texts.Plain_Text;
      Cursor : Position) return Scanner_State;

   function To_Annotation
     (Item : Markup;
      Pos  : Positive) return Annotation;

   function To_Annotated_Text
     (Text   : Plain_Texts.Plain_Text;
      Markup : Markup_Vectors.Vector) return Annotated_Text;

   function Count_Character
     (Line : League.Strings.Universal_String;
      From : Positive) return Positive;

   type Inline_Kind is new Positive;
   --  Top priority inline kinds.

   type Inline_Parser_State is
     array (Inline_Kind range <>) of Optional_Inline_State;

   Known_Inline : constant array (Inline_Kind range 1 .. 2) of access
     procedure
       (Text   : Plain_Texts.Plain_Text;
        Cursor : Position;
        State  : in out Optional_Inline_State)
     :=
       (Markdown.Inline_Parsers.Code_Spans.Find'Access,
        Markdown.Inline_Parsers.Autolinks.Find'Access);

   procedure Parse_Inline
     (Text   : Plain_Texts.Plain_Text;
      State  : in out Inline_Parser_State;
      Result : out Optional_Inline_State);

   procedure Find_All_Inlines
     (Text   : Plain_Texts.Plain_Text;
      Cursor : Position;
      State  : out Inline_Parser_State);

   procedure Append
     (Self  : in out Annotated_Text;
      Value : Annotated_Text);

   procedure Append
     (Self  : in out Annotated_Text;
      Value : Annotated_Text)
   is
      Offset : constant Natural := Self.Plain_Text.Length;
   begin
      Self.Plain_Text.Append (Value.Plain_Text);

      for X of Value.Annotation loop
         declare
            Shifted : Annotation := X;
         begin
            Shifted.From := Shifted.From + Offset;
            Shifted.To := Shifted.To + Offset;
            Self.Annotation.Append (Shifted);
         end;
      end loop;
   end Append;

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

   ---------------------
   -- Delimiter_Lists --
   ---------------------

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
         Kind_Of,
         Emphasis_Close,
         Emphasis_Open);

      type Delimiter_Filter (Kind : Delimiter_Filter_Kind := Any_Element) is
         record
            case Kind is
               when Any_Element | Emphasis_Close =>
                  null;
               when Kind_Of =>
                  Given_Kind : Delimiter_Kind;
               when Emphasis_Open =>
                  Emphasis : Emphasis_Kind;
            end case;
         end record;

      function Each
        (Self   : aliased Delimiter_List'Class;
         Filter : Delimiter_Filter := (Kind => Any_Element);
         From   : Delimiter_Index := 1;
         To     : Extended_Delimiter_Index := Delimiter_Index'Last)
         return Delimiter_Iterator_Interfaces.Reversible_Iterator'Class;

      procedure Append
        (Self : in out Delimiter_List'Class;
         Item : Delimiter);

   private
      package Delimiter_Vectors is new Ada.Containers.Vectors
        (Delimiter_Index, Delimiter);

      type Delimiter_List is tagged record
         Data : Delimiter_Vectors.Vector;
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
               when Kind_Of =>
                  if Item.Kind = Filter.Given_Kind then
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
         return Delimiter_Iterator_Interfaces.Reversible_Iterator'Class is
      begin
         return Iterator'(Self'Access, Filter, From,
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
      Markup : in out Markup_Vectors.Vector;
      From   : Delimiter_Lists.Delimiter_Index := 1;
      To     : Delimiter_Lists.Delimiter_Index :=
        Delimiter_Lists.Delimiter_Index'Last);

   procedure Process_Links
     (Register : Markdown.Link_Registers.Link_Register'Class;
      Text     : Plain_Texts.Plain_Text;
      DL       : in out Delimiter_Lists.Delimiter_List;
      Markup   : in out Markup_Vectors.Vector;
      Bottom   : Delimiter_Lists.Delimiter_Index := 1);

   procedure Parse_Link_Ahead
     (Register : Markdown.Link_Registers.Link_Register'Class;
      Text     : Plain_Texts.Plain_Text;
      DL       : in out Delimiter_Lists.Delimiter_List;
      Open     : Delimiter_Lists.Delimiter_Index;
      Close    : Delimiter_Lists.Delimiter_Index;
      URL      : out League.Strings.Universal_String;
      Title    : out League.String_Vectors.Universal_String_Vector;
      Ok       : out Boolean);

   ----------------------
   -- Find_All_Inlines --
   ----------------------

   procedure Find_All_Inlines
     (Text   : Plain_Texts.Plain_Text;
      Cursor : Position;
      State  : out Inline_Parser_State) is
   begin
      for Kind in State'Range loop
         Known_Inline (Kind) (Text, Cursor, State (Kind));
      end loop;
   end Find_All_Inlines;

   ------------------
   -- Parse_Inline --
   ------------------

   procedure Parse_Inline
     (Text   : Plain_Texts.Plain_Text;
      State  : in out Inline_Parser_State;
      Result : out Optional_Inline_State) is
   begin
      Result := (Is_Set => False);

      for X of State loop
         if X.Is_Set then
            if Result.Is_Set then
               if X.Span.From < Result.Span.From then
                  Result := X;
               end if;
            else
               Result := X;
            end if;
         end if;
      end loop;

      if Result.Is_Set then
         declare
            Next : Position := Result.Span.To;
         begin
            Text.Step (1, Next);

            for Kind in State'Range loop
               if State (Kind).Is_Set
                 and then State (Kind).Span.From <= Result.Span.To
               then
                  if Next <= Text.Last then
                     Known_Inline (Kind) (Text, Next, State (Kind));
                  else
                     State (Kind) := (Is_Set => False);
                  end if;
               end if;
            end loop;
         end;
      end if;
   end Parse_Inline;

   ----------------------
   -- Parse_Link_Ahead --
   ----------------------

   procedure Parse_Link_Ahead
     (Register : Markdown.Link_Registers.Link_Register'Class;
      Text     : Plain_Texts.Plain_Text;
      DL       : in out Delimiter_Lists.Delimiter_List;
      Open     : Delimiter_Lists.Delimiter_Index;
      Close    : Delimiter_Lists.Delimiter_Index;
      URL      : out League.Strings.Universal_String;
      Title    : out League.String_Vectors.Universal_String_Vector;
      Ok       : out Boolean)
   is
      procedure To_Link_Label
        (Text  : Plain_Texts.Plain_Text;
         Open  : Position;
         Close : Position;
         Label : out League.Strings.Universal_String;
         Ok    : out Boolean);

      procedure To_Inline_Link
        (Text  : Plain_Texts.Plain_Text;
         From  : Position;
         To    : in out Position;
         Ok    : out Boolean);

      --------------------
      -- To_Inline_Link --
      --------------------

      procedure To_Inline_Link
        (Text  : Plain_Texts.Plain_Text;
         From  : Position;
         To    : in out Position;
         Ok    : out Boolean)
      is
         Start : Wide_Wide_Character;  --  Title quote character
         pragma Unreferenced (Start);
         Skip  : Positive := From.Column;
         Last  : Natural;
         Line  : League.Strings.Universal_String :=
           Text.Line ((From.Line, Skip + 1));

         Match : League.Regexps.Regexp_Match :=
           Link_Start_Pattern.Find_Match (Line);

         Complete   : Boolean; --  We have whole inline_link matched
         Has_Title  : Boolean; --  Is title complete

      begin
         if not Match.Is_Matched then
            Ok := False;  --  No '('
            return;
         end if;

         Skip := Skip + Match.Last_Index;

         Line := Line.Tail_From (Match.Last_Index + 1);  --  drop '('
         Markdown.Common_Patterns.Parse_Link_Destination (Line, Last, URL);

         if Last > 0 then
            Skip := Skip + Last;
            Line := Line.Tail_From (Last + 1);  --  drop link destination
         end if;

         Match := Title_Pattern.Find_Match (Line);
         Complete := Match.Last_Index (6) >= Match.First_Index (6);

         if not Complete and Match.Last_Index /= Line.Length then
            Ok := False;  --  unmatched text before ')'
            return;
         elsif Last > 0
           and Match.Last_Index (1) >= Match.First_Index (1)
           and Match.First_Index (1) = 1
         then
            Ok := False;  --  No space between destinationa and title
            return;
         end if;

         Has_Title :=
           (for some J of Title_Close_Group =>
              Match.Last_Index (J) >= Match.First_Index (J));

         Line := Match.Capture (1);

         if Has_Title then
            Title.Append (Line.Slice (2, Line.Length - 1));
         elsif not Line.Is_Empty then
            if Complete then
               Ok := False;  --  No closing ', " or ')' in link title
               return;
            end if;

            Start := Line (1).To_Wide_Wide_Character;
            Title.Append (Line.Tail_From (2));
         end if;

         if Complete then
            To.Column := Skip + Match.Last_Index;
            Ok := True;
            return;
         end if;
--           for J in To.Line + 1 .. Text.Length loop
--              Line := Text (J);
--           end loop;

         Ok := False;
      end To_Inline_Link;

      -------------------
      -- To_Link_Label --
      -------------------

      procedure To_Link_Label
        (Text  : Plain_Texts.Plain_Text;
         Open  : Position;
         Close : Position;
         Label : out League.Strings.Universal_String;
         Ok    : out Boolean)
      is
         Line : constant League.Strings.Universal_String :=
           Text.Line (Open.Line);
      begin
         if Open.Line = Close.Line then
            Label := Line.Slice (Open.Column, Close.Column);
            Ok := True;
         else
            Ok := False;
         end if;
      end To_Link_Label;

      Label      : League.Strings.Universal_String;
   begin
      To_Inline_Link (Text, DL (Close).From, DL (Close).To, Ok);

      if not Ok then
         To_Link_Label (Text, DL (Open).From, DL (Close).From, Label, Ok);

         if Ok then
            Register.Resolve (Label, Ok, URL, Title);
         end if;
      end if;
   end Parse_Link_Ahead;

   ----------------------
   -- Process_Emphasis --
   ----------------------

   procedure Process_Emphasis
     (DL     : in out Delimiter_Lists.Delimiter_List;
      Markup : in out Markup_Vectors.Vector;
      From   : Delimiter_Lists.Delimiter_Index := 1;
      To     : Delimiter_Lists.Delimiter_Index :=
        Delimiter_Lists.Delimiter_Index'Last)
   is
      use Delimiter_Lists;
      Openers_Bottom : array (Emphasis_Kind, Natural range 0 .. 2) of
        Extended_Delimiter_Index := (others => (others => 0));
   begin
      for J in DL.Each ((Kind => Emphasis_Close), From, To) loop
         declare
            Closer : Delimiter renames DL (J);
            Found  : Boolean := False;
         begin
            Each_Open_Emphasis :
            for K in reverse DL.Each
              ((Emphasis_Open, Closer.Kind),
               From => Delimiter_Index'Max
                 (From,
                  Openers_Bottom (Closer.Kind, Closer.Count mod 3)),
               To   => J - 1)
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
                       ((Emphasis,
                         Opener.From + (Opener.Count - Count),
                         Count));

                     Markup.Append ((Emphasis, Closer.From, Count));

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
               if not Closer.Can_Open then
                  Closer.Is_Deleted := True;
               end if;

               Openers_Bottom (Closer.Kind, Closer.Count mod 3) := J;
            end if;
         end;
      end loop;
   end Process_Emphasis;

   -------------------
   -- Process_Links --
   -------------------

   procedure Process_Links
     (Register : Markdown.Link_Registers.Link_Register'Class;
      Text     : Plain_Texts.Plain_Text;
      DL       : in out Delimiter_Lists.Delimiter_List;
      Markup   : in out Markup_Vectors.Vector;
      Bottom   : Delimiter_Lists.Delimiter_Index := 1)
   is
      use Delimiter_Lists;
   begin
      for J in DL.Each ((Kind_Of, ']')) loop
         declare
            Closer : Delimiter renames DL (J);
         begin
            for K in reverse DL.Each
              ((Kind_Of, '['),
               From => Bottom,
               To   => J - 1)
            loop
               declare
                  Opener : Delimiter renames DL (K);
                  URL    : League.Strings.Universal_String;
                  Title  : League.String_Vectors.Universal_String_Vector;
                  Ok     : Boolean;
               begin
                  Parse_Link_Ahead (Register, Text, DL, K, J, URL, Title, Ok);

                  if Ok then
                     Markup.Append ((Link, Opener.From, 1, URL, Title));
                     Markup.Append
                       ((Link,
                        Closer.From,
                        Closer.To.Column - Closer.From.Column + 1,
                        URL,
                        Title));

                     Process_Emphasis (DL, Markup, K + 1, J - 1);

                     for M in DL.Each
                       ((Kind_Of, '['),
                        From => Bottom,
                        To   => J - 1)
                     loop
                        DL (M).Is_Deleted := True;
                     end loop;
                  end if;
               end;
            end loop;

            Closer.Is_Deleted := True;
         end;
      end loop;
   end Process_Links;

   -----------------
   -- Find_Markup --
   -----------------

   procedure Find_Markup
     (Register : Markdown.Link_Registers.Link_Register'Class;
      Text     : Plain_Texts.Plain_Text;
      Markup   : in out Markup_Vectors.Vector)
   is
      State        : Scanner_State;
      List         : Delimiter_Lists.Delimiter_List;
      Cursor       : Position := Text.First;
      Item         : Delimiter;
      Is_Delimiter : Boolean;
   begin
      while Cursor <= Text.Last loop
         Read_Delimiter (Text, Cursor, State, Item, Is_Delimiter);

         if Is_Delimiter then
            List.Append (Item);
         end if;
      end loop;

      Process_Links (Register, Text, List, Markup);
      Process_Emphasis (List, Markup);
   end Find_Markup;

   ---------------
   -- Get_State --
   ---------------

   function Get_State
     (Text   : Plain_Texts.Plain_Text;
      Cursor : Position) return Scanner_State
   is
      Line : League.Strings.Universal_String;
      --  FIXME: use Zs and Pc, Pd, Pe, Pf, Pi, Po, or Ps
   begin
      if Cursor > Text.Last
        or else Text.Line (Cursor.Line).Is_Empty
      then
         return (Is_White_Space => True, Is_Punctuation => False);
      else
         Line := Text.Line (Cursor.Line);
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
      Lines    : League.String_Vectors.Universal_String_Vector)
        return Annotated_Text
   is
      Result : Annotated_Text;
      Text   : Plain_Texts.Plain_Text;
      State  : Inline_Parser_State := (Known_Inline'Range => <>);
      Cursor : Position;
   begin
      Text.Initialize (Lines);
      Cursor := Text.First;
      Find_All_Inlines (Text, Cursor, State);

      while Cursor <= Text.Last loop
         declare
            Markup : Markup_Vectors.Vector;
            Value  : Optional_Inline_State;
         begin
            Parse_Inline (Text, State, Value);

            if Value.Is_Set then
               if Cursor /= Value.Span.From then
                  declare
                     Nested : Plain_Texts.Plain_Text;
                  begin
                     Nested.Initialize
                       (Text, Cursor, Value.Span.From);
                     Find_Markup (Register, Nested, Markup);
                     Append (Result, To_Annotated_Text (Nested, Markup));
                  end;
               end if;

               Append (Result, Value.Value);
               Cursor := Value.Span.To;
               Text.Step (1, Cursor);
            else
               declare
                  Nested : Plain_Texts.Plain_Text;
               begin
                  Nested.Initialize (Text, Cursor);
                  Find_Markup (Register, Nested, Markup);
                  Append (Result, To_Annotated_Text (Nested, Markup));
                  Cursor := Text.Last;
                  Text.Step (1, Cursor);
               end;
            end if;
         end;
      end loop;

      return Result;

   end Parse;

   --------------------
   -- Read_Character --
   --------------------

   procedure Read_Character
     (Text   : Plain_Texts.Plain_Text;
      Cursor : in out Position;
      Result : in out League.Strings.Universal_String)
   is
      Line : constant League.Strings.Universal_String :=
        Text.Line (Cursor.Line);
   begin
      if Line.Is_Empty then
         Text.Step (1, Cursor);
         return;
      end if;

      case Line (Cursor.Column).To_Wide_Wide_Character is
         when '\' =>
            Text.Step (1, Cursor);
            if Cursor > Text.Last then
               Result.Append ('\');
            elsif Get_State (Text, Cursor).Is_Punctuation then
               Result.Append (Line (Cursor.Column));
               Text.Step (1, Cursor);
            else
               Result.Append ('\');
            end if;
         when others =>
            Result.Append (Line (Cursor.Column));
            Text.Step (1, Cursor);
      end case;
   end Read_Character;

   --------------------
   -- Read_Delimiter --
   --------------------

   procedure Read_Delimiter
     (Text         : Plain_Texts.Plain_Text;
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

      Line   : constant League.Strings.Universal_String :=
        Text.Line (Cursor.Line);
      Follow : Scanner_State;
   begin
      if Line.Is_Empty then
         State := Get_State (Text, Cursor);
         Text.Step (1, Cursor);
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
               Text.Step (Next.Count, Cursor);
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
               Text.Step (Next.Count, Cursor);
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
            Item := (Kind       => '[',
                     From       => Cursor,
                     Is_Deleted => False);
            Text.Step (1, Cursor);
            Is_Delimiter := True;

         when ']' =>
            State := Get_State (Text, Cursor);
            Item := (Kind       => ']',
                     From       => Cursor,
                     To         => Cursor,
                     Is_Deleted => False);
            Text.Step (1, Cursor);
            Is_Delimiter := True;
         when '\' =>
            State := Get_State (Text, Cursor);
            Text.Step (2, Cursor);
            Is_Delimiter := False;

         when others =>
            State := Get_State (Text, Cursor);
            Text.Step (1, Cursor);
            Is_Delimiter := False;

      end case;

      if Cursor.Column = 1 then
         State := (Is_White_Space => True, Is_Punctuation => False);
      end if;
   end Read_Delimiter;

   -----------------------
   -- To_Annotated_Text --
   -----------------------

   function To_Annotated_Text
     (Text   : Plain_Texts.Plain_Text;
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

      Result     : League.Strings.Universal_String;
      Cursor     : Position := Text.First;
      Index      : Positive := Map'First;
      Annotation : Annotation_Vectors.Vector;

   begin
      for J in 1 .. Map'Last loop
         Map (J) := Markup_Index (J);
      end loop;

      Sort (1, Map'Last);

      while Cursor <= Text.Last loop
         if Index in Map'Range and then
           Cursor = Markup (Map (Index)).From
         then
            declare
               Item : Inline_Parsers.Markup renames Markup (Map (Index));
            begin
               if Map (Index) mod 2 = 1 then  --  Open markup
                  Annotation.Append
                    (To_Annotation (Item, Result.Length + 1));
                  Annotation_Map ((Map (Index) + 1) / 2) :=
                    Annotation.Last_Index;
               else  --  Close markup
                  Annotation
                    (Annotation_Map ((Map (Index)) / 2)).To :=
                      Result.Length;
               end if;

               Text.Step (Item.Length, Cursor);

               Index := Index + 1;
            end;
         else
            Read_Character (Text, Cursor, Result);
         end if;

         if Cursor <= Text.Last and Cursor.Column = 1 then
            Result.Append (' ');
            Annotation.Append
              ((Soft_Line_Break, Result.Length, Result.Length));
         end if;
      end loop;

      return (Result, Annotation);
   end To_Annotated_Text;

   -------------------
   -- To_Annotation --
   -------------------

   function To_Annotation (Item : Markup; Pos  : Positive) return Annotation is
   begin
      if Item.Kind = Link then
         return (Link, Pos, Pos, Item.URL, Item.Title);
      elsif Item.Length = 1 then
         return (Emphasis, Pos, Pos);
      else
         return (Strong, Pos, Pos);
      end if;
   end To_Annotation;

end Markdown.Inline_Parsers;
