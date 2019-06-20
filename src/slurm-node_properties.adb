with Ada.Strings.Fixed;

package body Slurm.Node_Properties is

   overriding function "<" (Left, Right : Node_Name) return Boolean is
   begin
      return Ada.Strings.Unbounded."<" (Unbounded_String (Left), Unbounded_String (Right));
   end "<";

   overriding function "=" (Left, Right : Node_Name) return Boolean is
   begin
      return Ada.Strings.Unbounded."=" (Unbounded_String (Left), Unbounded_String (Right));
   end "=";

   function Get_CPU_Model (Item : Set_Of_Properties) return String is
   begin
      return To_String (Item.CPU_Model);
   end Get_CPU_Model;

   function Get_CPUs (Item : Set_Of_Properties) return Natural is
   begin
      return Item.CPUs;
   end Get_CPUs;

   function Get_GPU (Item : Set_Of_Properties) return String is
   begin
      return To_String (Item.GPU_Model);
   end Get_GPU;

   function Get_GPU_Memory (Item : Set_Of_Properties) return String is
   begin
      return To_String (Item.GPU_Memory);
   end Get_GPU_Memory;

   function Get_Memory (Item : Set_Of_Properties) return String is
   begin
      return To_String (Item.Memory);
   end Get_Memory;

   function Get_Network (Item : Set_Of_Properties) return String is
   begin
      return To_String (Item.Network);
   end Get_Network;

   function To_Name_Set (Source : String) return Name_Set is
      Result : Name_Set := Name_Sets.Empty_Set;
      Last   : Integer := Source'First - 1;
      Next   : Integer;
   begin
      while Last < Source'Last loop
         Next := Ada.Strings.Fixed.Index (Source  => Source,
                                          Pattern => ",",
                                          From    => Last + 1);
         if Next = 0 then
            Next := Source'Last + 1;
         end if;
         Result.Include (To_Unbounded_String (Source (Last + 1 ..  Next - 1)));
         Last := Next;
      end loop;
      return Result;
   end To_Name_Set;

   overriding function To_String (Source : Node_Name) return String is
   begin
      return To_String (Unbounded_String (Source));
   end To_String;

end Slurm.Node_Properties;
