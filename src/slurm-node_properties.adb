with Ada.Strings.Fixed;

package body Slurm.Node_Properties is

   overriding function "<" (Left, Right : Node_Name) return Boolean is
   begin
      return Ada.Strings.Unbounded."<" (Unbounded_String (Left), Unbounded_String (Right));
   end "<";

   function "<" (Left, Right : Set_Of_Properties) return Boolean is
      use Slurm.Gres;
      use Slurm.Tres;
   begin
      if Left. CPUs < Right.CPUs then
         return True;
      elsif Left. CPUs > Right.CPUs then
         return False;
      elsif Left.Memory < Right.Memory then
         return True;
      elsif Left.Memory > Right.Memory then
         return False;
      elsif Left.GRES < Right.GRES then
         return True;
      elsif Left.GRES > Right.GRES then
         return False;
      elsif Left.Features < Right.Features then
         return True;
      elsif Left.Features > Right.Features then
         return True;
      elsif Left.TRES < Right.TRES then
         return True;
      elsif Left. TRES > Right.TRES then
         return False;
      end if;
      return False;
   end "<";

   overriding function "=" (Left, Right : Node_Name) return Boolean is
   begin
      return Ada.Strings.Unbounded."=" (Unbounded_String (Left), Unbounded_String (Right));
   end "=";

   function Get_CPUs (Item : Set_Of_Properties) return Natural is
   begin
      return Item.CPUs;
   end Get_CPUs;

   function Get_Features (From : Set_Of_Properties) return String is
   begin
      return To_String (From.Features);
   end Get_Features;

   function Get_GRES (From : Set_Of_Properties) return Slurm.Gres.List is
   begin
      return From.GRES;
   end Get_GRES;

   function Get_Memory (From : Set_Of_Properties) return Gigs is
   begin
      return From.Memory;
   end Get_Memory;

   function Get_TRES (From : Set_Of_Properties) return Slurm.Tres.List is
   begin
      return From.TRES;
   end Get_TRES;

   procedure Init_CPUs (Item : in out Set_Of_Properties; Source : Natural) is
   begin
      Item.CPUs := Source;
   end Init_CPUs;

   procedure Init_Features (Item : in out Set_Of_Properties; Source : String) is
   begin
      Item.Features := To_Unbounded_String (Source);
   end Init_Features;

   procedure Init_GRES (Item : in out Set_Of_Properties; Source : Slurm.Gres.List) is
   begin
      Item.GRES := Source;
   end Init_GRES;

   procedure Init_Memory (Item : in out Set_Of_Properties; Source : Gigs) is
   begin
      Item.Memory := Source;
   end Init_Memory;

   procedure Init_TRES (Item : in out Set_Of_Properties; Source : Slurm.Tres.List) is
   begin
      Item.TRES := Source;
   end Init_TRES;

   procedure Iterate_GRES (Item    : Set_Of_Properties;
                           Process : not null access procedure (R : Slurm.Gres.Resource)) is
      procedure Wrapper (Position : Slurm.Gres.Lists.Cursor);

      procedure Wrapper (Position : Slurm.Gres.Lists.Cursor) is
      begin
         Process (Slurm.Gres.Lists.Element (Position));
      end Wrapper;

   begin
      Item.GRES.Iterate (Wrapper'Access);
   end Iterate_GRES;

   procedure Iterate_TRES (Item    : Set_Of_Properties;
                           Process : not null access procedure (R : Slurm.Tres.Resource)) is
      procedure Wrapper (Position : Slurm.Tres.Lists.Cursor);

      procedure Wrapper (Position : Slurm.Tres.Lists.Cursor) is
      begin
         Process (Slurm.Tres.Lists.Element (Position));
      end Wrapper;

   begin
      Item.TRES.Iterate (Wrapper'Access);
   end Iterate_TRES;

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
