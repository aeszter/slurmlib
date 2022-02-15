with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Slurm.C_Types;
with Slurm.C_Types;
with POSIX.C;
with Ada.Containers;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Doubly_Linked_Lists;

package Slurm.Utils is
   Version : String := "0.12";
   type Tri_State is (False, True, Undecided);
   type User_Name is new String (1 .. 8);

   Assumption_Error : exception;
   Operator_Error   : exception;
   Unsupported_Error : exception;

   type Fixed is delta 0.0001 digits 5;
   --  a general fixed type, especially useful for SGE resources

   type Usage_Number is delta 0.0001 digits 18;
   type Usage_Integer is range 0 .. 10 ** 12;

   type Gigs is delta 0.001 digits 9;
   function To_String (Memory : Gigs) return String;
   function MiB_To_Gigs (Source : Slurm.C_Types.uint64_t) return Gigs;
   function MiB_To_Gigs (Source : Slurm.C_Types.uint32_t) return Gigs;

--     function To_Tri_State (Truth : String) return Tri_State;
--     function To_Tri_State (Truth : Boolean) return Tri_State;
--
   package String_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Unbounded_String);
--
   package String_Sets is
     new Ada.Containers.Ordered_Sets (Element_Type => Unbounded_String);
--
--     package String_Pairs is
--       new Ada.Containers.Ordered_Maps (Key_Type => Unbounded_String,
--                                        Element_Type => Unbounded_String);
--
--     package Hash_Strings is
--       new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 10);
--
--     package ID_Lists is
--       new Ada.Containers.Ordered_Sets (Element_Type => Natural,
--                                        "<"          => "<",
--                                        "="          => "=");
--
--
   subtype String_List is String_Lists.List;
--     subtype ID_List is ID_Lists.Set;
--     subtype Hash_String_Type is Hash_Strings.Bounded_String;
--
--     function To_Hash_String (S : String) return Hash_String_Type;
--
--      procedure To_String_List (Source  : String; Dest : out POSIX_String_List);
   function To_String_Set (Source  : String) return String_Sets.Set;
   function To_User_Name (User : String) return User_Name;
   function To_String (User : User_Name) return String;
   function To_String (Source : Interfaces.C.Strings.chars_ptr) return String;
   function Convert_String (Source : chars_ptr) return Unbounded_String;
   function Convert_Time (Source : POSIX.C.time_t) return Ada.Calendar.Time;
   function Convert_User (UID : Slurm.C_Types.uint32_t) return User_Name;

--
--     function To_Time (Time_String : String) return Ada.Calendar.Time;
--     function User_Is_Manager (User : String) return Boolean;
--     function User_Is_Operator (User : String) return Boolean;
--     function Get_User_Tickets (User : String) return Long_Integer;

end Slurm.Utils;
