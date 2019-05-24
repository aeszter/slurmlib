package Slurm.Node_Properties is
   type Fixed is delta 0.01 digits 6 range 0.0 .. 1000.0;
   type Load is new Fixed range 0.0 .. 1000.0;
end Slurm.Node_Properties;
