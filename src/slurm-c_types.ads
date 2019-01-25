with POSIX.C; use POSIX.C;

package Slurm.C_Types is
   subtype uint64_t is unsigned_long;
   subtype uint32_t is unsigned_int;
   subtype uint16_t is unsigned_short;
   type uint8_t is mod 2**8;
   for uint8_t'Size use 8;
   subtype int32_t is int;
--     type double is digits 15;
--     for double'Size use 64;
end Slurm.C_Types;
