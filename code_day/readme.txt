This version is the finnal pixed_based WaSSI-C model:
1. it can input dynamic vegetation code for each pixels -- MODIS land cover is from 2001 to 2012 now;
2. it can simulate maximum 6000 pixels and maximum 32 years at once;

3. it can run in gfortran without any error	;
 ------ c:\TDM\bin>gfortran general.f90 pet.f90 output.f90 carbonbal.f90 summary.f90 warmup.f90 waterbal.f90
 ------ c:\TDM\bin>a    ! a is a.exe, the program of WaSSI-C model,which is automatically created by gfortran 
4.