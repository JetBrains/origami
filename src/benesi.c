# Mandelbulber settings file
# version 2.11
# only modified parameters
[main_parameters]
background_color_1 0000 0000 0000;
background_color_2 0000 0000 0000;
background_color_3 0000 0000 0000;
box_folding_limit 1,19;
camera -1,708715502097254 -0,006790331013323624 0,002362354162198877;
camera_distance_to_target 0,02163924852646606;
camera_rotation 72,42810878425402 -27,0200663530116 -31,26006986910304;
camera_top -0,526890644712197 -0,3774670750905936 0,761514842755752;
DE_factor 0,301995;
DOF_enabled true;
DOF_focus 0,02373302727937698;
flight_last_to_render 0;
formula_1 66;
formula_iterations_1 9;
glow_color_1 ff00 0000 0000;
glow_color_2 ff00 e800 7000;
glow_intensity 6,0256;
hybrid_fractal_enable true;
image_height 1080;
image_width 1920;
keyframe_last_to_render 0;
mat1_is_defined true;
spherical_folding_inner 0,18;
spherical_folding_outer 1,54;
target -1,72709327317538 -0,0009704803065685025 -0,007468411054265049;
view_distance_max 13,81473177672692;
[fractal_1]
info true;





void BenesiPineTreeIteration(CVector3 &z, CVector3 c, const cFractal *fractal, sExtendedAux &aux)
{
  CVector3 temp = z;
  aux.r = z.Length();
  z *= z;
  double t = 2.0 * temp.x;
  if (z.y + z.z > 0.0)
    t = t / sqrt(z.y + z.z);
  else
    t = 1.0;

  z.x = (z.x - z.y - z.z) + c.x * fractal->transformCommon.constantMultiplier100.x;
  z.z = (t * (z.y - z.z)) + c.y * fractal->transformCommon.constantMultiplier100.y;
  z.y = (2.0 * t * temp.y * temp.z) + c.z * fractal->transformCommon.constantMultiplier100.z;
  aux.r_dz = aux.r * aux.r_dz * 2.0 + 1.0;
}
