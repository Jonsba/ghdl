--  GHDL Run Time (GRT) - Wave option file package for reading the tree.
--  Copyright (C) 2016 Jonas Baggett
--
--  GHDL is free software; you can redistribute it and/or modify it under
--  the terms of the GNU General Public License as published by the Free
--  Software Foundation; either version 2, or (at your option) any later
--  version.
--
--  GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.

-- Description: See package specifications

with Grt.Errors; use Grt.Errors;
with Grt.Wave_Opt.File; use Grt.Wave_Opt.File;

package body Grt.Wave_Opt.Design is

   -- Find the element that matches the name given. Starts with the element
   -- given, then go thru all its siblings
   function Find_Cursor (Name : String;
                         Parent : Match_List;
                         Is_Signal : Boolean := False)
                        return Match_List;

   -- (...)
   procedure Match_List_Append (List : in out Match_List; Tree_Elem : Elem_Acc);

   function Get_Top_Cursor (Tree_Index : Tree_Index_Type; Name : Ghdl_C_String)
                           return Match_List
   is
      Root : Match_List;
   begin
      if State = Write_File and then Trees (Tree_Index).Next_Child = null then
         Write_Tree_Comment (Tree_Index);
      end if;
      Root := new Match_Elem_Type'(Trees (Tree_Index), null);
      return Get_Cursor (Root, Name);
   end Get_Top_Cursor;

   function Get_Cursor (Parent : Match_List;
                        Name : Ghdl_C_String;
                        Is_Signal : Boolean := False) return Match_List
   is
      Tree_Elem_Cursor : Elem_Acc;
      Last_Updated : Boolean;
      Str_Name : constant String := Name (1 .. strlen (Name));
   begin
      case State is
         when Write_File =>
            Tree_Elem_Cursor := Parent.Tree_Elem;
            Last_Updated := True;
            Update_Tree (Cursor => Tree_Elem_Cursor,
                         Last_Updated => Last_Updated,
                         Elem_Expr => Str_Name,
                         Level => Tree_Elem_Cursor.Level + 1);
            if Is_Signal then
               Write_Signal_Path (Tree_Elem_Cursor);
            end if;
            return new Match_Elem_Type'(Tree_Elem_Cursor, null);
         when Display_Tree =>
            return Find_Cursor (Str_Name, Parent, Is_Signal);
         when Display_All =>
            return null;
      end case;
   end Get_Cursor;

   function Find_Cursor (Name : String;
                         Parent : Match_List;
                         Is_Signal : Boolean := False)
                        return Match_List
   is
      Tree_Elem_Cursor : Elem_Acc;
      Parent_Cursor, List : Match_List;
      --
      function Match_Expr return Boolean is
      begin
         if Tree_Elem_Cursor.Expr.all = Name then
            return True;
         elsif Tree_Elem_Cursor.Expr.all = "*" then
            -- Returns true in the following cases :
            -- Tree : /top/*     | Design : /top/a
            -- Tree : /top/*/... | Design : /top/sub/...
            if Is_Signal xor Tree_Elem_Cursor.Next_Child /= null then
               return True;
            end if;
         elsif Tree_Elem_Cursor.Expr.all = "**" then
            if not Is_Signal or else Tree_Elem_Cursor.Next_Child = null then
               return True;
            end if;
         end if;
         return False;
      end Match_Expr;

      function Get_Cursor_Kind return Elem_Kind_Type is
      begin
         if Tree_Elem_Cursor.Expr.all = "**" then
            return Recursion;
         elsif Is_Signal then
            return Signal;
         else
            return Pkg_Entity;
         end if;
      end Get_Cursor_Kind;
   begin
      Parent_Cursor := Parent;
      loop
         exit when Parent_Cursor = null;
         Tree_Elem_Cursor := Parent_Cursor.Tree_Elem.Next_Child;
         if Parent_Cursor.Tree_Elem.Expr.all = "**" then
            -- In the following cases : /.../**/a | /.../**/sub/...
            -- where a and sub match Name, we can return a cursor to a | sub
            if Tree_Elem_Cursor /= null and then Tree_Elem_Cursor.Expr.all = Name then
               Match_List_Append (List, Tree_Elem_Cursor);
            -- In the following cases : /.../**/a | /.../**
            -- where a doesn't match Name
            elsif not Is_Signal or else Tree_Elem_Cursor = null then
               Match_List_Append (List, Parent_Cursor.Tree_Elem);
            end if;
         end if;
         loop
            exit when Tree_Elem_Cursor = null;
            if Match_Expr then
               Tree_Elem_Cursor.Kind := Get_Cursor_Kind;
               Match_List_Append (List, Tree_Elem_Cursor);
            end if;
            Tree_Elem_Cursor := Tree_Elem_Cursor.Next_Sibling;
         end loop;
         Parent_Cursor := Parent_Cursor.Next;
      end loop;
      return List;
   end Find_Cursor;

   procedure Match_List_Append (List : in out Match_List; Tree_Elem : Elem_Acc) is
   begin
      List := new Match_Elem_Type'(Tree_Elem => Tree_Elem, Next => List);
   end Match_List_Append;

   function Is_Displayed (Cursor : Match_List) return Boolean is
   begin
      if State /= Display_Tree or else Cursor /= null then
         return True;
      end if;
      return False;
   end Is_Displayed;

   -- Read the whole sub tree given and check if every element was found in
   -- design.  Called by Last_Checks
   procedure Check_Sub_Tree_If_All_Found (Previous_Cursor : Elem_Acc);

   procedure Last_Checks is
   begin
      if Wave_Opt.State = Display_Tree then
         for Index in Tree_Index_Type'Range loop
            Check_Sub_Tree_If_All_Found (Trees (Index).Next_Child);
         end loop;
      end if;
   end Last_Checks;

   procedure Check_Sub_Tree_If_All_Found (Previous_Cursor : Elem_Acc)
   is
      Cursor : Elem_Acc;
   begin
      Cursor := Previous_Cursor;
      while Cursor /= null loop
         if Cursor.Kind = Not_Found then
            Print_Context (Cursor, Warning);
            Report_C (Cursor.Expr.all);
            Report_E (" : first element of the path not found in design");
         elsif Cursor.Next_Child = null and then Cursor.Kind = Pkg_Entity then
            Print_Context (Cursor, Warning);
            Report_C (Cursor.Expr.all);
            Report_E (" is not a signal");
         else
            Check_Sub_Tree_If_All_Found (Cursor.Next_Child);
         end if;
         Cursor := Cursor.Next_Sibling;
      end loop;

   end Check_Sub_Tree_If_All_Found;

end Grt.Wave_Opt.Design;
