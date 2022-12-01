-- Suggestions for packages which might be useful:

--  with Ada.Text_IO;                use Ada.Text_IO;
with Exceptions;                 use Exceptions;
--  with Real_Type;                  use Real_Type;
--  with Generic_Sliding_Statistics;
--  with Rotations;                  use Rotations;
with Vectors_3D;                 use Vectors_3D;
with Vehicle_Interface;          use Vehicle_Interface;
with Vehicle_Message_Type;       use Vehicle_Message_Type;
-- with Swarm_Structures;           use Swarm_Structures;
with Swarm_Structures_Base;      use Swarm_Structures_Base;
with Real_Type;             use Real_Type;
with Ada.Calendar; use Ada.Calendar;
-- with Ada.Numerics.Generic_Elementary_Functions;

package body Vehicle_Task_Type is

   function distance_from_globe (Globe, Ship : Positions) return Real;

   function distance_from_globe (Globe, Ship : Positions) return Real is

         x_comp : constant Distances := Globe (x) - Ship (x);
         y_comp : constant Distances := Globe (y) - Ship (y);
         z_comp : constant Distances := Globe (z) - Ship (z);
         distance : Real;
   begin
         distance := (x_comp * x_comp) + (y_comp * y_comp) + (z_comp * z_comp);
         return distance;
   end distance_from_globe;

   type Globe_Record is record
      Globe : Energy_Globe;
      Distance : Real;
   end record;

   type Globe_Record_Array is array (Positive range <>) of Globe_Record;

   function generate_globe_records (Globes_Array : Energy_Globes; Ship_Pos : Positions) return Globe_Record_Array;

   function generate_globe_records (Globes_Array : Energy_Globes; Ship_Pos : Positions) return Globe_Record_Array is
      Records_Array : Globe_Record_Array (Globes_Array'Range);
   begin
      for i in Globes_Array'Range loop
         Records_Array (i).Globe := Globes_Array (i);
         Records_Array (i).Distance := distance_from_globe (Globe => Globes_Array (i).Position, Ship  => Ship_Pos);
      end loop;
      return Records_Array;
   end generate_globe_records;

   function find_minimum_globe (Record_Array : Globe_Record_Array) return Energy_Globe;

   function find_minimum_globe (Record_Array : Globe_Record_Array) return Energy_Globe is
      minimum_distance : Real := Record_Array (1).Distance;
      best_record : Globe_Record := Record_Array (1);
   begin
      for i in Record_Array'Range loop
         if Record_Array (i).Distance < minimum_distance then
            minimum_distance := Record_Array (i).Distance;
            best_record := Record_Array (i);
         end if;
      end loop;
      return best_record.Globe;
   end find_minimum_globe;

   task body Vehicle_Task is

      Vehicle_No : Positive;
      pragma Unreferenced (Vehicle_No);
      -- You will want to take the pragma out, once you use the "Vehicle_No"

   begin

      -- You need to react to this call and provide your task_id.
      -- You can e.g. employ the assigned vehicle number (Vehicle_No)
      -- in communications with other vehicles.

      accept Identify (Set_Vehicle_No : Positive; Local_Task_Id : out Task_Id) do
         Vehicle_No     := Set_Vehicle_No;
         Local_Task_Id  := Current_Task;
      end Identify;

      -- Replace the rest of this task with your own code.
      -- Maybe synchronizing on an external event clock like "Wait_For_Next_Physics_Update",
      -- yet you can synchronize on e.g. the real-time clock as well.

      -- Without control this vehicle will go for its natural swarming instinct.

      select

         Flight_Termination.Stop;

      then abort

         Outer_task_loop : loop

            Wait_For_Next_Physics_Update;

            declare
               Globe : constant Energy_Globes := Energy_Globes_Around;
               Local_Globe : Energy_Globe;
               Write_Message : Inter_Vehicle_Messages;
               Recieved_Message : Inter_Vehicle_Messages;
               Charge : constant Vehicle_Charges := Current_Charge;
               Radius_Globe : constant Real := 0.02;
               Local_Time : Time := Clock;
               Recieved_Time : Time;
               Time_Threshold : constant Duration := 0.50;
               Ship_Position : constant Positions := Position;
               Time_Difference : Duration;
               Best_Globe : Energy_Globe;
               Best_Time : Time;
               pragma Unreferenced (Best_Time);
               Best_Time_Diff : Duration;

            begin
               if Globe'Length /= 0 then
                  Local_Globe := find_minimum_globe (Record_Array => generate_globe_records (Globes_Array => Globe,
                                                                                           Ship_Pos     => Ship_Position));
                  for i in Globe'Range loop
                     Local_Time := Clock;
                     Write_Message.Global_Globe := Globe (i);
                     Write_Message.Global_Time := Local_Time;
                     Send (Message => Write_Message);
                  end loop;

                  Best_Globe := Local_Globe;

               else

                  Local_Time := Clock;
                  if Messages_Waiting then
                     Receive (Recieved_Message);
                     Local_Globe := Recieved_Message.Global_Globe;
                     Recieved_Time := Recieved_Message.Global_Time;
                     Time_Difference := Local_Time - Recieved_Time;
                     if Time_Difference < Time_Threshold then
                        Best_Globe := Local_Globe;
                        Best_Time := Recieved_Time;
                        Best_Time_Diff := Time_Difference;
                     else
                        while Messages_Waiting loop
                           Local_Time := Clock;
                           Receive (Recieved_Message);
                           Local_Globe := Recieved_Message.Global_Globe;
                           Recieved_Time := Recieved_Message.Global_Time;
                           Time_Difference := Local_Time - Recieved_Time;
                           exit when (Time_Difference <= Best_Time_Diff) and then (Time_Difference <= Time_Threshold);

                        end loop;

                     end if;
                  end if;

               end if;
               Best_Globe := Local_Globe;
               Best_Time := Recieved_Time;
               Best_Time_Diff := Time_Difference;

              -- for i in Globe'Range loop
              --    Local_Time := Clock;
              --    Write_Message.Global_Globe := Globe(i);
              --    Write_Message.Global_Time := Local_Time;
              --    Send (Message => Write_Message);
              -- end loop;

               Local_Time := Clock;
               Write_Message.Global_Globe := Local_Globe;
               Write_Message.Global_Time := Local_Time;
               Send (Message => Write_Message);

               declare
                  x_value : constant Distances := Best_Globe.Position (x) - Ship_Position (x);
                  y_value : constant Distances := Best_Globe.Position (y) - Ship_Position (y);
                  z_value : constant Distances := Best_Globe.Position (z) - Ship_Position (z);
                  distance_globe : constant Real := (x_value * x_value) + (y_value * y_value) + (z_value * z_value);
               begin

                  if distance_globe > Radius_Globe then
                     if Charge <= 0.7 then
                        Set_Destination (V => Best_Globe.Position);
                        Set_Throttle (T => 1.0);
                     else
                        Set_Destination (V => Best_Globe.Position);
                        Set_Throttle (T => 0.8);
                     end if;
                  else
                     if Charge <= 0.7 then
                        Set_Destination (V => Best_Globe.Position);
                        Set_Throttle (T => 1.0);
                     else
                        Set_Throttle (T => 0.0);
                     end if;
                  end if;

               end;
            end;

         end loop Outer_task_loop;

      end select;

   exception
      when E : others => Show_Exception (E);

   end Vehicle_Task;

end Vehicle_Task_Type;
