with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Doubly_Linked_Lists;
with Slurm.Parser; use Slurm.Parser;
with Slurm.Spread_Sheets; use Slurm.Spread_Sheets;
with Slurm.Taint; use Slurm.Taint;

package body Slurm.Share_Tree is
   The_List : Lists.List;
   Expected_Header : array (1 .. 7) of Unbounded_String;

   procedure Iterate (Process : not null access procedure (Position : Lists.Cursor)) is
   begin
      The_List.Iterate (Process);
   end Iterate;

   procedure Load is
      use Slurm.Spread_Sheets;

      Data : Spread_Sheet;
      Status : Natural;
      Arguments : Trusted_String_List;
   begin
      Arguments.Append (Implicit_Trust ("-P"));
      Arguments.Append (Implicit_Trust ("-U"));
      Arguments.Append (Implicit_Trust ("-a"));
      Slurm.Parser.Setup (Command     => Cmd_Sshare,
                          Arguments   => Arguments,
                          Output      => Data,
                          Exit_Status => Status,
                          Field_Separator => '|',
                          Standard_Separator => False);
      if Status /= 0 then
         -- assume no data returned
         return;
      end if;

      Data.Rewind;
      -- header line
      for Column in Expected_Header'Range loop
         if Data.Current /= Expected_Header (Column) then
            raise Format_Error with "Header: expected " &
               To_String (Expected_Header (Column)) & ", got " & Data.Current;
         end if;
         Data.Next;
      end loop;
      Data.Next; -- new line

      while not Data.At_End loop
         declare
            U : User_Node;
         begin
-- Output
-- Account|User|RawShares|NormShares|RawUsage|EffectvUsage|FairShare
-- root|root|1|0.500000|0|0.000000|1.000000
            Data.Next; -- Account
            U.User_Name := To_User_Name (Data.Current);
            Data.Next;
            U.Raw_Shares := Usage_Number'Value (Data.Current);
            Data.Next;
            U.Norm_Shares := Usage_Number'Value (Data.Current);
            Data.Next;
            U.Raw_Usage := Usage_Integer'Value (Data.Current);
            Data.Next;
            U.Effective_Usage := Usage_Number'Value (Data.Current);
            Data.Next;
            U.Fairshare := Usage_Number'Value (Data.Current);
            Data.Next;
            The_List.Append (U);
            Data.Next;
         end;
      end loop;
   end Load;

   function Precedes_By_Fairshare (Left, Right : User_Node) return Boolean is
   begin
      return Left.Fairshare < Right.Fairshare;
   end Precedes_By_Fairshare;

   function Precedes_By_Shares (Left, Right : User_Node) return Boolean is
   begin
      return Left.Norm_Shares < Right.Norm_Shares;
   end Precedes_By_Shares;

   function Precedes_By_Usage (Left, Right : User_Node) return Boolean is
   begin
      return Left.Effective_Usage < Right.Effective_Usage;
   end Precedes_By_Usage;

   function Precedes_By_User (Left, Right : User_Node) return Boolean is
   begin
      return Left.User_Name < Right.User_Name;
   end Precedes_By_User;

   procedure Reverse_Order is
   begin
      The_List.Reverse_Elements;
   end Reverse_Order;

   procedure Sort_By_Fairshare is
   begin
      Sorting_By_Fairshare.Sort (The_List);
   end Sort_By_Fairshare;

   procedure Sort_By_Shares is
   begin
      Sorting_By_Shares.Sort (The_List);
   end Sort_By_Shares;

   procedure Sort_By_Usage is
   begin
      Sorting_By_Usage.Sort (The_List);
   end Sort_By_Usage;

   procedure Sort_By_User is
   begin
      Sorting_By_User.Sort (The_List);
   end Sort_By_User;

begin
   Expected_Header (1) := To_Unbounded_String ("Account");
   Expected_Header (2) := To_Unbounded_String ("User");
   Expected_Header (3) := To_Unbounded_String ("RawShares");
   Expected_Header (4) := To_Unbounded_String ("NormShares");
   Expected_Header (5) := To_Unbounded_String ("RawUsage");
   Expected_Header (6) := To_Unbounded_String ("EffectvUsage");
   Expected_Header (7) := To_Unbounded_String ("FairShare");
end Slurm.Share_Tree;
