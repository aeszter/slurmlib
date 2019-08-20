with Slurm.Utils;
use Slurm.Utils;
with Ada.Containers.Doubly_Linked_Lists;

package Slurm.Share_Tree is

   subtype User_Name_String is Slurm.Utils.User_Name;

   type User_Node is record
      User_Name       : User_Name_String;
      Raw_Shares      : Usage_Number;
      Norm_Shares     : Usage_Number;
      Raw_Usage       : Usage_Integer;
      Effective_Usage : Usage_Number;
      Fairshare       : Usage_Number;
   end record;

   package Lists is new Ada.Containers.Doubly_Linked_Lists (Element_Type => User_Node);

   Format_Error : exception; -- unexpected data from sshare

   procedure Load;
   procedure Iterate (Process : not null access procedure (Position : Lists.Cursor));

   procedure Sort_By_User;
   procedure Sort_By_Usage;
   procedure Sort_By_Fairshare;
   procedure Sort_By_Shares;
   procedure Reverse_Order;

private
   function Precedes_By_User (Left, Right : User_Node) return Boolean;
   function Precedes_By_Usage (Left, Right : User_Node) return Boolean;
   function Precedes_By_Fairshare (Left, Right : User_Node) return Boolean;
   function Precedes_By_Shares (Left, Right : User_Node) return Boolean;

   package Sorting_By_User is new Lists.Generic_Sorting ("<" => Precedes_By_User);
   package Sorting_By_Usage is new Lists.Generic_Sorting ("<" => Precedes_By_Usage);
   package Sorting_By_Fairshare is new Lists.Generic_Sorting ("<" => Precedes_By_Fairshare);
   package Sorting_By_Shares is new Lists.Generic_Sorting ("<" => Precedes_By_Shares);

end Slurm.Share_Tree;
