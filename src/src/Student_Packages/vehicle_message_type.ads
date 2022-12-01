-- Suggestions for packages which might be useful:

with Ada.Calendar;              use Ada.Calendar;
--  with Ada.Text_IO;                use Ada.Text_IO;
--  with Real_Type;                  use Real_Type;
--  with Generic_Sliding_Statistics;
--  with Rotations;                  use Rotations;
--  with Vectors_3D;                 use Vectors_3D;
--  with Ada.Text_IO; use Ada.Text_IO;
with Swarm_Structures_Base;      use Swarm_Structures_Base;


package Vehicle_Message_Type is

   type Inter_Vehicle_Messages is record
      Global_Globe : Energy_Globe;
      Global_Time : Time;
   end record;
end Vehicle_Message_Type;
