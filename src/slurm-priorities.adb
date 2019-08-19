with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Slurm.Parser;
with Slurm.Spread_Sheets;
with Slurm.Taint; use Slurm.Taint;

package body Slurm.Priorities is

   All_Preloaded : Boolean := False;

   The_List : Maps.Map;
   Expected_Header : array (1 .. 8) of Unbounded_String;

   procedure Load_One (J : Positive);
   procedure Internal_Load (Parameter : Trusted_String);

   function Get_Priority (J : Positive) return Priority is
   begin
      if not All_Preloaded then
         Load_One (J);
      end if;
      if not The_List.Contains (J) then
         return Undefined;
      end if;
      return The_List.Element (J);
   end Get_Priority;

   procedure Internal_Load (Parameter : Trusted_String) is
      use Slurm.Spread_Sheets;

      Data : Spread_Sheet;
      Status : Natural;
      Arguments : Trusted_String_List;
   begin
      Arguments.Append (Implicit_Trust ("-o%i %r %Y %A %F %J %P %Q"));
      if Parameter /= Implicit_Trust ("") then
         Arguments.Append (Parameter);
      end if;
      Slurm.Parser.Setup (Command     => Cmd_Sprio,
                          Arguments   => Arguments,
                          Output      => Data,
                          Exit_Status => Status);
      if Status /= 0 then
         -- assume no data returned
         return;
      end if;

      Data.Rewind;
      -- header line
      for Column in Expected_Header'Range loop
         if Data.Current /= Expected_Header (Column) then
            if Data.Current = "Unable" then
               return; -- probably Unable to find jobs matching user/id(s) specified
                       -- this happens for jobs that are not eligible for running, so
                       -- it's ok
            end if;
            raise Format_Error with "Header: expected " &
               To_String (Expected_Header (Column)) & ", got " & Data.Current;
         end if;
         Data.Next;
      end loop;
      Data.Next; -- new line

      while not Data.At_End loop
         declare
            Prio : Priority;
            J : Positive;
         begin
-- Output
--          JOBID PARTITION   PRIORITY        AGE  FAIRSHARE    JOBSIZE  PARTITION
--          11758 short           1992          0          0        992       1000
            J := Integer'Value (Data.Current);
            Data.Next;
            Data.Next; -- partition
            Prio.Total := Integer'Value (Data.Current);
            Data.Next;
            Prio.Age := Integer'Value (Data.Current);
            Data.Next;
            Prio.Fairshare := Integer'Value (Data.Current);
            Data.Next;
            Prio.Job_Size := Integer'Value (Data.Current);
            Data.Next;
            Prio.Partition := Integer'Value (Data.Current);
            Data.Next;
            Prio.QOS := Integer'Value (Data.Current);
            Data.Next;
            if not Data.At_Separator then
               raise Format_Error with "Long line";
            end if;
            The_List.Include (J, Prio);
            Data.Next;
         end;
      end loop;
   end Internal_Load;

   procedure Load is
   begin
      Internal_Load (Implicit_Trust (""));
      All_Preloaded := True;
   end Load;

   procedure Load_One (J : Positive) is
   begin
      Internal_Load (Implicit_Trust ("--jobs=") & Sanitise_Number (J'Img));
   end Load_One;

begin
   Expected_Header (1) := To_Unbounded_String ("JOBID");
   Expected_Header (2) := To_Unbounded_String ("PARTITION");
   Expected_Header (3) := To_Unbounded_String ("PRIORITY");
   Expected_Header (4) := To_Unbounded_String ("AGE");
   Expected_Header (5) := To_Unbounded_String ("FAIRSHARE");
   Expected_Header (6) := To_Unbounded_String ("JOBSIZE");
   Expected_Header (7) := To_Unbounded_String ("PARTITION");
   Expected_Header (8) := To_Unbounded_String ("QOS");
end Slurm.Priorities;
