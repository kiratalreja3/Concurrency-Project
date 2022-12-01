------------------------------------------------------------------------------
--  File :            GLUT - Windows.adb
--  Description :     models a GLUT window
--  Copyright (c) Gautier de Montmollin/Rod Kay 2006 .. 2007
------------------------------------------------------------------------------

-- with opengl.glx;

with GL, GL.IO, GL.Frustums, GLU,  GLUT;  use GL, GL.IO, GL.Frustums, GLU,  GLUT;

--  with GLOBE_3D;
--  with GLOBE_3D.IO;
with GLOBE_3D.Math;                       use GLOBE_3D.Math;
with GLOBE_3D.Software_Anti_Aliasing;

with Actors;
with GLUT_2D;  --, GLUT_Exit;

-- with Ada.Text_IO;

with Ada.Numerics;                      use Ada.Numerics;
with Ada.Unchecked_Conversion;

-- with Ada.Containers.Generic_Array_Sort;

with Ada.Calendar;

-- with System.Storage_Elements;

package body GLUT.Windows is

   package G3D  renames GLOBE_3D;

   use Ada.Strings.Unbounded;

   deg2rad       : constant := Pi / 180.0;
   GLUT_Problem  : exception;

   -- current_Window  : - for accessing the current GLUT window
   --                  - used by GLUT callbacks to determine the Window to which a callback event relates.
   --
   function current_Window return Window_view is

      function to_Window is new Ada.Unchecked_Conversion (System.Address, GLOBE_3D.p_Window);

   begin
      return GLUT.Windows.Window_view (to_Window (GetWindowData));
   end current_Window;

   procedure Name_is (Self : in out Window; Now  : String) is

   begin
      Self.Name := To_Unbounded_String (Now);
   end Name_is;

   function Name (Self : Window) return String is (To_String (Self.Name));

   function is_Closed (Self : Window) return Boolean is (Self.is_Closed);

   procedure Prepare_default_lighting (Self  : in out Window;
                                       fact  :        GL.C_Float) is

      proto_light  : G3D.Light_definition := (position => (0.0, 500.0,  0.0,  1.0),
                                              ambient  => (0.3,   0.3,  0.3,  fact),
                                              diffuse  => (0.9,   0.9,  0.9,  fact),
                                              specular => (0.05,  0.05, 0.01, fact));
   begin
      GL.Enable (GL.LIGHTING);

      G3D.Define (1, proto_light);
      Self.frontal_light   := proto_light;

      proto_light.diffuse  := (0.5, 0.9, 0.5, fact);
      G3D.Define (2, proto_light);

      proto_light.diffuse  := (0.2, 0.0, 0.9, fact);
      proto_light.specular := (1.0, 1.0, 1.0, fact);
      G3D.Define (3, proto_light);

      proto_light.position := (-3.0, 4.0, 10.0, 1.0);
      G3D.Define (4, proto_light);

      proto_light.position := (3.0, -4.0, 10.0, 1.0);
      proto_light.ambient  := (0.6, 0.6, 0.6, 0.1);
      G3D.Define (5, proto_light);

      proto_light.ambient  := (0.6, 0.0, 0.0, 0.1);
      G3D.Define (6, proto_light);

      proto_light.ambient  := (0.0, 0.6, 0.0, 0.1);
      G3D.Define (7, proto_light);

      proto_light.ambient  := (0.0, 0.0, 0.6, 0.1);
      G3D.Define (8, proto_light);

      G3D.Switch_lights (True);

      G3D.Switch_light (2, False);
      G3D.Switch_light (3, False);
      G3D.Switch_light (4, False);
      G3D.Switch_light (5, False);
      G3D.Switch_light (6, False);
      G3D.Switch_light (7, False);
      G3D.Switch_light (8, False);

   end Prepare_default_lighting;

   procedure Clear_modes is

   begin
      Disable (BLEND);
      Disable (LIGHTING);
      Disable (AUTO_NORMAL);
      Disable (NORMALIZE);
      Disable (DEPTH_TEST);
   end Clear_modes;

   procedure Reset_for_3D (Self : in out Window'Class) is

   begin
      pragma Unreferenced (Self);
      MatrixMode (MODELVIEW);    -- (tbd : still needed ?) . .. The matrix generated by GLU.Perspective is multipled by the current matrix
      ShadeModel (SMOOTH);       -- GL's default is SMOOTH, vs FLAT
      -- ShadeModel (FLAT);       -- GL's default is SMOOTH, vs FLAT

      ClearColor (0.0, 0.0, 0.0, 0.0);    -- Specifies clear values for color buffer (s)
      ClearAccum (0.0, 0.0, 0.0, 0.0);    -- Specifies clear values for the accumulation buffer
   end Reset_for_3D;

   procedure enable_Viewport_and_Perspective (Self : in out Window'Class) is  -- tbd : move projection matrix to 'window resize'.

   begin
      Viewport (0,  0, Self.main_size_x, Self.main_size_y);

      MatrixMode (PROJECTION);
      LoadIdentity;

      GLU.Perspective (fovy   => Self.Camera.FOVy,                    -- field of view angle (deg) in the y direction
                       aspect => Self.Camera.Aspect,                  -- x/y aspect ratio
                       zNear  => Self.Camera.near_plane_Distance,     -- distance from the viewer to the near clipping plane
                       zFar   => Self.Camera.far_plane_Distance);     -- distance from the viewer to the far clipping plane

      Get (GL.PROJECTION_MATRIX,  Self.Camera.Projection_Matrix (1, 1)'Unchecked_Access);   -- Get the current PROJECTION matrix from OpenGL

      Self.Camera.Projection_Matrix := Transpose (Self.Camera.Projection_Matrix);

      MatrixMode (MODELVIEW);    -- The matrix generated by GLU.Perspective is multipled by the current matrix
   end enable_Viewport_and_Perspective;

   procedure set_Size (Self : in out Window'Class;  width, height : Integer) is

      use G3D;
      use REF;

      half_fov_max_rads         : Real;
      Tan_of_half_fov_max_rads  : Real;

   begin
      Self.main_size_x  := GL.Sizei (width);
      Self.main_size_y  := GL.Sizei (height);

      Self.Camera.Clipper.main_clipping.X1 := 0;
      Self.Camera.Clipper.main_clipping.Y1 := 0;
      Self.Camera.Clipper.main_clipping.X2 := width - 1;
      Self.Camera.Clipper.main_clipping.Y2 := height - 1;

      Self.Camera.Aspect := GL.Double (Self.main_size_x) / GL.Double (Self.main_size_y);
      half_fov_max_rads        := 0.5 * Self.Camera.FOVy * deg2rad;

      Tan_of_half_fov_max_rads := Tan (half_fov_max_rads);

      Self.Camera.near_plane_Height := Self.Camera.near_plane_Distance * Tan_of_half_fov_max_rads;
      Self.Camera.near_plane_Width  := Self.Camera.near_plane_Height   * Self.Camera.Aspect;

      Self.Camera.far_plane_Height  := Self.Camera.far_plane_Distance * Tan_of_half_fov_max_rads;
      Self.Camera.far_plane_Width   := Self.Camera.far_plane_Height   * Self.Camera.Aspect;

      if Self.Camera.Aspect > 1.0 then -- x side angle broader than y side angle
         half_fov_max_rads := Arctan (Self.Camera.Aspect * Tan_of_half_fov_max_rads);
      end if;

      Self.Camera.Clipper.max_dot_product := Sin (half_fov_max_rads);

   end set_Size;

   -- Procedures passed to GLUT:
   --   Window_Resize, Keyboard, Motion, Menu, Mouse, Display

   procedure Window_Resize (width, height  : Integer) is

      the_Window  : constant GLUT.Windows.Window_view := current_Window;

   begin
      the_Window.all.forget_mouse := 5;
      set_Size     (the_Window.all,  width, height);
      Reset_for_3D (the_Window.all);
   end Window_Resize;

   procedure Menu (value  : Integer) is

   begin
      case value is
         when 1 => -- GLUT.GameModeString (Full_Screen_Mode);
            GLUT.FullScreen;
            -- res := GLUT.EnterGameMode;
            GLUT.SetCursor (GLUT.CURSOR_NONE);
            current_Window.all.forget_mouse := 10;
            current_Window.all.full_screen  := True;
         when 2 => null; -- GLUT_exit;
         when others => null;
      end case;
   end Menu;
   pragma Unreferenced (Menu);

   procedure Display_status (Self  : in out Window;
                             sec   : GLOBE_3D.Real) is

--        use G3D, G3D.REF;
      use G3D;

      light_info  : String (1 .. 8);

   begin
      PushMatrix;

      Disable (LIGHTING);
      Disable (TEXTURE_2D);

      Color (red   => 0.7,
             green => 0.7,
             blue  => 0.6);

      GLUT_2D.Text_output ((1.0, 0.0, 0.0),  " (x)",  GLUT_2D.Times_Roman_24);
      GLUT_2D.Text_output ((0.0, 1.0, 0.0),  " (y)",  GLUT_2D.Times_Roman_24);
      GLUT_2D.Text_output ((0.0, 0.0, 1.0),  " (z)",  GLUT_2D.Times_Roman_24);

      GLUT_2D.Text_output (0,  50,  Self.main_size_x,  Self.main_size_y,
                           "Eye : " & Coords (Self.Camera.Clipper.Eye_Position),
                           GLUT_2D.Helvetica_10);

      GLUT_2D.Text_output (0,  60,  Self.main_size_x,  Self.main_size_y,
                           "View direction : " & Coords (Self.Camera.Clipper.view_direction),
                           GLUT_2D.Helvetica_10);

      for i in light_info'Range loop

         if Is_light_switched (i) then
            light_info (i) := Character'Val (Character'Pos ('0') + i);
         else
            light_info (i) := 'x';
         end if;
      end loop;

      GLUT_2D.Text_output (0, 70, Self.main_size_x, Self.main_size_y, "Lights : (" & light_info & ')', GLUT_2D.Helvetica_10);

      if sec > 0.0 then
         GLUT_2D.Text_output (0, 130, Self.main_size_x, Self.main_size_y, "FPS : " & Integer'Image (Integer (1.0 / sec)), GLUT_2D.Helvetica_10);
      end if;

      if Self.is_capturing_Video then
         GLUT_2D.Text_output (0, 150, Self.main_size_x, Self.main_size_y, "*recording*", GLUT_2D.Helvetica_10);
      end if;

      PopMatrix;

   end Display_status;

   function Frames_per_second (Self : Window) return Float is (Float (1.0 / (Self.Average * 0.001)));

   procedure Graphic_display (Self    : in out Window'Class;
                              Extras  :        GLOBE_3D.Visual_array := GLOBE_3D.null_Visuals) is

      use G3D;

   begin
      G3D.render (Self.Objects (1 .. Self.object_Count) & Extras, Self.Camera);

      if Self.Show_Status then
         Display_status (Self,  Self.Average * 0.001);
      end if;
   end Graphic_display;

   procedure Fill_screen (Self    : in out Window'Class;
                          Extras  :        GLOBE_3D.Visual_array := GLOBE_3D.null_Visuals) is

      procedure Display_Local is

      begin
         Graphic_display (Self, Extras);
      end Display_Local;

      package SAA is new GLOBE_3D.Software_Anti_Aliasing (Display_Local);
   begin

      case Self.Smoothing is

         when Software =>
            SAA.Set_Quality (SAA.Q3);
            for SAA_Phase in 1 .. SAA.Anti_Alias_phases loop
               SAA.Display_with_Anti_Aliasing (SAA_Phase);
            end loop;

         when Hardware =>
            Enable (MULTISAMPLE_ARB); -- (if not done yet)

            -- ClearColor (0.0, 0.0, 0.0, 1.0);    -- Specifies clear values for color buffer (s)
            -- ClearColor (0.15, 0.4, 0.15, 1.0);    -- Specifies clear values for color buffer (s)  -- tbd : make clear color user - settable
            ClearColor (0.0, 0.0, 0.0, 1.0);    -- Specifies clear values for color buffer (s)  -- tbd : make clear color user - settable
            ClearAccum (0.0,  0.0, 0.0,  0.0);    -- Specifies clear values for the accumulation buffer

            Graphic_display (Self, Extras);
            Flush;

         when None =>
            Graphic_display (Self, Extras);
            Flush;
      end case;

      GLUT.SwapBuffers;
   end Fill_screen;

   procedure Reset_eye  (Self  : in out Window'Class) is

   begin
      Self.Camera.Clipper.Eye_Position := (0.0,  5.0,  4.0);
      Self.Camera.World_Rotation       := GLOBE_3D.Id_33;
   end Reset_eye;

   function Image (Date : Ada.Calendar.Time) return String;
   -- Proxy for Ada 2005 Ada.Calendar.Formatting.Image

   procedure Main_Operations (Self       : access Window;
                              time_Step  :        G3D.Real;
                              Extras     :        GLOBE_3D.Visual_array := GLOBE_3D.null_Visuals) is

      use G3D, G3D.REF, Game_Control;

      elaps, time_now     : Integer;
      gx,    gy           : GL.Double;   -- mouse movement since last call
      seconds             : GL.Double;   -- seconds since last image
      alpha_correct       : Boolean;
      attenu_t, attenu_r  : Real;

   begin
      if not Self.all.is_Visible or else Self.all.is_Closed then
         return;
      end if;

      enable_Viewport_and_Perspective (Self.all);   -- nb : must be done prior to setting frustum planes (when using GL.frustums.current_Planes)

      -- Control of lighting
      --
      --        self.frontal_light.position := (GL.Float (self.Camera.Clipper.eye_Position (0)),
      --                                              GL.Float (self.Camera.Clipper.eye_Position (1)),
      --                                              GL.Float (self.Camera.Clipper.eye_Position (2)),
      --                                              1.0);
      --        G3D.Define (1, self.frontal_light);

      for c in n1 .. n8 loop
         if Self.all.game_command (c) then
            Reverse_light_switch (1 + Command'Pos (c) - Command'Pos (n1));
         end if;
      end loop;

      -- Display screen
      --
      Fill_screen (Self.all, Extras);

      -- Timer management
      --
      time_now := GLUT.Get (GLUT.ELAPSED_TIME);   -- Number of milliseconds since GLUT.Init

      if Self.all.new_scene then
         Self.all.new_scene := False;
         elaps          := 0;
      else
         elaps          := time_now - Self.all.last_time;
      end if;

      Self.all.last_time := time_now;
      Self.all.Average   := 0.0;

      for i in reverse Self.all.sample'First + 1 .. Self.all.sample'Last loop
         Self.all.sample (i) := Self.all.sample (i - 1);
         Self.all.Average    := Self.all.Average + Real (Self.all.sample (i));
      end loop;

      Self.all.sample (Self.all.sample'First) := elaps;

      Self.all.Average := Self.all.Average + Real (elaps);
      Self.all.Average := Self.all.Average / Real (Self.all.sample'Length);

      seconds  := Real (elaps) * 0.001;
      attenu_t := Real'Min (0.96, Real'Max (0.04,  1.0 - seconds * 4.0));
      attenu_r := attenu_t ** 0.5;

      -- Game control management
      --
      Self.all.game_command := no_command;

      Game_Control.Append_Commands (size_x     => Integer (Self.all.main_size_x),
                                    size_y     => Integer (Self.all.main_size_y),
                                    warp_mouse => Self.all.full_screen,
                                    c          => Self.all.game_command,
                                    gx         => gx,
                                    gy         => gy,
                                    Keyboard   => Self.all.Keyboard'Access,
                                    Mouse      => Self.all.Mouse'Access);

      if Self.all.forget_mouse > 0 then -- mouse coords disturbed by resize
         gx := 0.0;
         gy := 0.0;
         Self.all.forget_mouse := Self.all.forget_mouse - 1;
      end if;

      if Self.all.game_command (interrupt_game) then
         null; -- GLUT_exit;                     -- tbd : how to handle this best ?
      end if;

      alpha_correct := False;

      if Self.all.game_command (special_plus)  then
         Self.all.Alpha := Self.all.Alpha + seconds; alpha_correct := True;
      end if;
      if Self.all.game_command (special_minus) then
         Self.all.Alpha := Self.all.Alpha - seconds; alpha_correct := True;
      end if;

      if alpha_correct then
         if    Self.all.Alpha < 0.0 then
            Self.all.Alpha := 0.0;
         elsif Self.all.Alpha > 1.0 then
            Self.all.Alpha := 1.0;
         end if;

         for Each in 1 .. Self.all.object_Count loop
            set_Alpha (Self.all.Objects (Each).all,  Self.all.Alpha);
         end loop;
      end if;

      -- Camera/Eye - nb : camera movement is done after rendering, so camera is in a state ready for the next frame.
      --            -     (important for Impostors)

      -- Rotating the eye

      Actors.Rotation (Self.all.Camera,
                       gc => Self.all.game_command,
                       gx => gx,
                       gy => gy,
                       unitary_change => seconds,
                       deceleration   => attenu_r,
                       time_step      => time_Step);

      -- Moving the eye

      Actors.Translation (Self.all.Camera,
                          gc => Self.all.game_command,
                          gx => gx,
                          gy => gy,
                          unitary_change     => seconds,
                          deceleration       => attenu_t,
                          time_step          => time_Step);

      if Self.all.game_command (n0) then
         Reset_eye (Self.all);
      end if;

      Self.all.Camera.Clipper.view_direction := Transpose (Self.all.Camera.World_Rotation) * (0.0, 0.0, -1.0);

      -- update camera frustum
      --
      MatrixMode    (MODELVIEW);
      Set_GL_Matrix (Self.all.Camera.World_Rotation);
      Translate     (-Self.all.Camera.Clipper.Eye_Position (0), -Self.all.Camera.Clipper.Eye_Position (1), -Self.all.Camera.Clipper.Eye_Position (2));

      Self.all.Camera.frustum_Planes := GL.Frustums.Current_Planes;  -- tbd : getting frustum planes from camera, might be quicker,
      -- set_frustum_Planes (Self.Camera);                        --      but 'set_frustum_Planes' seems buggy :/.

      -- video management
      --
      if Self.all.game_command (video) then
         if Self.all.is_capturing_Video then
            GL.IO.Stop_Capture;
            Self.all.is_capturing_Video := False;
         else
            GL.IO.Start_Capture (AVI_Name   => To_String (Self.all.Name) & "." & Image (Ada.Calendar.Clock) & ".avi",
                                 frame_rate => 8); -- Integer (self.Frames_per_second));
            Self.all.is_capturing_Video := True;
         end if;
      end if;

      if Self.all.is_capturing_Video then
         GL.IO.Capture_Frame;
      end if;

      -- photo management
      --
      if Self.all.game_command (photo) then
         GL.IO.Screenshot (Name => To_String (Self.all.Name) & "." & Image (Ada.Calendar.Clock) & ".bmp");
      end if;

   end Main_Operations;

   procedure Close_Window is

   begin
      current_Window.all.is_Closed := True;
   end Close_Window;

   procedure Update_Visibility (State  : Integer) is

   begin
      --      ada.text_io.put_line ("in update_Visibility callback state : " & integer'image (State));
      --
      -- tbd : this callback is not being called when a window is iconicised !!

      current_Window.all.is_Visible := not (State = GLUT.HIDDEN or else State = GLUT.FULLY_COVERED);
   end Update_Visibility;

   procedure Start_GLUTs (Self  : in out Window) is

--        use GLUT;

      function to_Address is new Ada.Unchecked_Conversion (GLOBE_3D.p_Window, System.Address);

      GLUT_options : GLUT.Unsigned := GLUT.DOUBLE or GLUT.RGBA or GLUT.ALPHA or GLUT.DEPTH;

   begin
      if Self.Smoothing = Hardware then
         GLUT_options := GLUT_options or GLUT.MULTISAMPLE;
      end if;

      InitDisplayMode (GLUT_options);

      set_Size (Self,  500, 400);

      InitWindowSize     (Integer (Self.main_size_x),  Integer (Self.main_size_y));
      InitWindowPosition (120, 120);

      Self.glut_Window := CreateWindow ("GLOBE_3D/GLUT Window");

      if Self.glut_Window = 0 then
         raise GLUT_Problem;
      end if;

      GLUT.CloseFunc        (Close_Window'Access);
      GLUT.ReshapeFunc      (Window_Resize'Access);
      GLUT.WindowStatusFunc (Update_Visibility'Access);
      GLUT.SetWindowData    (to_Address (GLOBE_3D.Window'Class (Self)'Unchecked_Access));

      GLUT.Devices.Initialize;

      --        if CreateMenu (Menu'Access) = 0 then         -- tdb : deferred
      --           raise GLUT_Problem;
      --        end if;

      --      AttachMenu (MIDDLE_BUTTON);

      --      AddMenuEntry (" * Full Screen", 1);
      --      AddMenuEntry ("--> Exit (Esc)", 2);

   end Start_GLUTs;

   procedure Start_GLs (Self  : in out Window) is

      fog_colour  : GL.Light_Float_vector := (0.2, 0.2, 0.2, 0.1);

   begin

      Clear_modes;
      Prepare_default_lighting (Self, 0.9);

      if Self.foggy then
         Enable (FOG);
         Fogfv  (FOG_COLOR,   fog_colour (0)'Unchecked_Access);
         Fogf   (FOG_DENSITY, 0.02);
      end if;

      Reset_for_3D (Self);

      if Self.Smoothing = Hardware then
         Enable (MULTISAMPLE_ARB);
         Enable (SAMPLE_COVERAGE_ARB); -- Hope it helps switching on the AA .. .
      end if;

   end Start_GLs;

   procedure Initialize is

   begin
      GLUT.Init;
      GLUT.SetOption (GLUT.GLUT_RENDERING_CONTEXT, GLUT.GLUT_USE_CURRENT_CONTEXT);
      GLUT.SetOption (GLUT.ACTION_ON_WINDOW_CLOSE, ACTION_CONTINUE_EXECUTION);
   end Initialize;

   procedure Define (Self  : in out Window) is

   begin
      Start_GLUTs (Self);    -- Initialize the GLUT things
      Start_GLs   (Self);    -- Initialize the (Open)GL things
      Reset_eye   (Self);

      Freshen     (Self, 0.02);    -- do an initial freshen, to initialise Camera, etc.
   end Define;

   procedure Destroy (Self  : in out Window) is

   begin
      DestroyWindow (Self.glut_Window);
   end Destroy;

   overriding procedure Enable (Self  : in out Window) is

   begin
      GLUT.SetWindow (Self.glut_Window);
      --      opengl.glx.glXMakeCurrent;
   end Enable;

   overriding procedure Freshen (Self      : in out Window;
                                 time_Step :        G3D.Real;
                                 Extras    :        GLOBE_3D.Visual_array := GLOBE_3D.null_Visuals) is

   begin
      Enable (Self);  -- for multi - window operation.
      Main_Operations (Self'Access, time_Step, Extras);
   end Freshen;

   -- traits
   --

   function Smoothing (Self : Window) return Smoothing_method is (Self.Smoothing);

   procedure Smoothing_is (Self : in out Window;
                           Now  :        Smoothing_method) is

   begin
      Self.Smoothing := Now;
   end Smoothing_is;

   procedure Add (Self : in out Window; the_Object : GLOBE_3D.p_Visual) is

   begin
      Self.object_Count                := Self.object_Count + 1;
      Self.Objects (Self.object_Count) := the_Object.all'Access;
   end Add;

   procedure Rid (Self : in out Window; the_Object : GLOBE_3D.p_Visual) is

      use G3D;

   begin
      for Each in 1 .. Self.object_Count loop

         if Self.Objects (Each) = the_Object then

            if Each /= Self.object_Count then
               Self.Objects (Each .. Self.object_Count - 1) := Self.Objects (Each + 1 .. Self.object_Count);
            end if;

            Self.object_Count := Self.object_Count - 1;
            return;
         end if;

      end loop;

      raise no_such_Object;
   end Rid;

   function Object_Count (Self : Window) return Natural is (Self.object_Count);

   -- status display
   --

   function  Show_Status (Self : Window) return Boolean is (Self.Show_Status);

   procedure Show_Status (Self  : in out Window;
                          Show  :        Boolean := True) is

   begin
      Self.Show_Status := Show;
   end Show_Status;

   -- Devices
   --

   function Keyboard (Self : access Window'Class) return Devices.p_Keyboard is (Self.all.Keyboard'Access);

   function Mouse (Self : access Window'Class) return Devices.p_Mouse is (Self.all.Mouse'Access);

   -- Proxy for Ada 2005 Ada.Calendar.Formatting.Image
   function Image (Date : Ada.Calendar.Time) return String is

      use Ada.Calendar;

      subtype Sec_int is Long_Integer; -- must contain 86_400

      m, s  : Sec_int;

   begin
      s := Sec_int (Seconds (Date));
      m := s / 60;

      declare
         -- + 100 : trick for obtaining 0x
         sY  : constant String := Integer'Image (Year (Date));
         sM  : constant String := Integer'Image (Month (Date) + 100);
         sD  : constant String := Integer'Image (Day (Date)  + 100);
         shr : constant String := Sec_int'Image (m  /  60 + 100);
         smn : constant String := Sec_int'Image (m mod 60 + 100);
         ssc : constant String := Sec_int'Image (s mod 60 + 100);

      begin
         return
           sY (sY'Last - 3 .. sY'Last) & '-' &  -- not Year 10'000 compliant.
           sM (sM'Last - 1 .. sM'Last) & '-' &
           sD (sD'Last - 1 .. sD'Last) &
           " " &
           shr (shr'Last - 1 .. shr'Last) & '.' &
           smn (smn'Last - 1 .. smn'Last) & '.' &
           ssc (ssc'Last - 1 .. ssc'Last);
      end;
   end Image;

end GLUT.Windows;