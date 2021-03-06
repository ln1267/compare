# ---------------------------------------------------------------------------
# 功能：1、基于裁剪数据边界进行LAI数据的裁剪
#       2、提取出LAI栅格中”<100"的有效值
#		3、合并八天数据到月
# Created on: 星期日 十二月 18 2011 02:37:43 下午
#   (generated by ArcGIS/ModelBuilder)
# ---------------------------------------------------------------------------

# Import arcpy module
import arcpy

# Check out any necessary licenses
arcpy.CheckOutExtension("spatial")

#循环开始
for n in xrange(2001,2012):
	i=0
	for d in xrange(1,365,8):
		d="%03d" % d
		data_class="LAI"
##########################-----操作目录---------##############################################
		input_mulu="K:\\Data\\MODIS_LAI\\"		#输入TIF文件目录		
		out_mulu="K:\\Data\\LAI\\linshi\\"	#输出有效值文件目录
		out_lai_mu="K:\\Data\\LAI\\"	
##########################-----输入文件---------##############################################
		input_tif = input_mulu+"MOD15A2.A"+str(n)+d+".Lai_1km.tif" 
	
		output_extra = out_mulu+"lai"+str(n)+d+".tif"	#提取后输出的tif文件

			
		print output_extra
		
		
# Process: Extract by Attributes...
		arcpy.gp.ExtractByAttributes_sa(input_tif ,"\"value\" < 100 ", output_extra)
		print "成功提取出“<100“的值！"

# Local variables:
	input_lai_1= out_mulu+"lai"+str(n)+"001.tif"
	input_lai_2= out_mulu+"lai"+str(n)+"009.tif"
	input_lai_3= out_mulu+"lai"+str(n)+"017.tif"
	input_lai_4= out_mulu+"lai"+str(n)+"025.tif"
	
	input_lai_5= out_mulu+"lai"+str(n)+"033.tif"
	input_lai_6= out_mulu+"lai"+str(n)+"041.tif"
	input_lai_7= out_mulu+"lai"+str(n)+"049.tif"
	input_lai_8= out_mulu+"lai"+str(n)+"057.tif"
	
	input_lai_9= out_mulu+"lai"+str(n)+"065.tif"
	input_lai_10= out_mulu+"lai"+str(n)+"073.tif"
	input_lai_11= out_mulu+"lai"+str(n)+"081.tif"
	input_lai_12= out_mulu+"lai"+str(n)+"089.tif"
	
	input_lai_13= out_mulu+"lai"+str(n)+"097.tif"
	input_lai_14= out_mulu+"lai"+str(n)+"105.tif"
	input_lai_15= out_mulu+"lai"+str(n)+"113.tif"
	
	input_lai_16= out_mulu+"lai"+str(n)+"121.tif"
	input_lai_17= out_mulu+"lai"+str(n)+"129.tif"
	input_lai_18= out_mulu+"lai"+str(n)+"137.tif"
	input_lai_19= out_mulu+"lai"+str(n)+"145.tif"
	
	input_lai_20= out_mulu+"lai"+str(n)+"153.tif"
	input_lai_21= out_mulu+"lai"+str(n)+"161.tif"
	input_lai_22= out_mulu+"lai"+str(n)+"169.tif"
	input_lai_23= out_mulu+"lai"+str(n)+"177.tif"
	
	input_lai_24= out_mulu+"lai"+str(n)+"185.tif"
	input_lai_25= out_mulu+"lai"+str(n)+"193.tif"
	input_lai_26= out_mulu+"lai"+str(n)+"201.tif"
	input_lai_27= out_mulu+"lai"+str(n)+"209.tif"
	
	input_lai_28= out_mulu+"lai"+str(n)+"217.tif"
	input_lai_29= out_mulu+"lai"+str(n)+"225.tif"
	input_lai_30= out_mulu+"lai"+str(n)+"233.tif"
	input_lai_31= out_mulu+"lai"+str(n)+"241.tif"
	
	input_lai_32= out_mulu+"lai"+str(n)+"249.tif"
	input_lai_33= out_mulu+"lai"+str(n)+"257.tif"
	input_lai_34= out_mulu+"lai"+str(n)+"265.tif"
	input_lai_35= out_mulu+"lai"+str(n)+"273.tif"
	
	input_lai_36= out_mulu+"lai"+str(n)+"281.tif"
	input_lai_37= out_mulu+"lai"+str(n)+"289.tif"
	input_lai_38= out_mulu+"lai"+str(n)+"297.tif"
	
	input_lai_39= out_mulu+"lai"+str(n)+"305.tif"
	input_lai_40= out_mulu+"lai"+str(n)+"313.tif"
	input_lai_41= out_mulu+"lai"+str(n)+"321.tif"
	input_lai_42= out_mulu+"lai"+str(n)+"329.tif"

	input_lai_43= out_mulu+"lai"+str(n)+"337.tif"
	input_lai_44= out_mulu+"lai"+str(n)+"345.tif"
	input_lai_45= out_mulu+"lai"+str(n)+"353.tif"
	input_lai_46= out_mulu+"lai"+str(n)+"361.tif"

	
	
	out_lai_01_tif=out_lai_mu+"lai"+str(n)+"01.tif"
	out_lai_02_tif=out_lai_mu+"lai"+str(n)+"02.tif"
	out_lai_03_tif=out_lai_mu+"lai"+str(n)+"03.tif"
	out_lai_04_tif=out_lai_mu+"lai"+str(n)+"04.tif"
	out_lai_05_tif=out_lai_mu+"lai"+str(n)+"05.tif"
	out_lai_06_tif=out_lai_mu+"lai"+str(n)+"06.tif"
	out_lai_07_tif=out_lai_mu+"lai"+str(n)+"07.tif"
	out_lai_08_tif=out_lai_mu+"lai"+str(n)+"08.tif"
	out_lai_09_tif=out_lai_mu+"lai"+str(n)+"09.tif"
	out_lai_10_tif=out_lai_mu+"lai"+str(n)+"10.tif"
	out_lai_11_tif=out_lai_mu+"lai"+str(n)+"11.tif"
	out_lai_12_tif=out_lai_mu+"lai"+str(n)+"12.tif"
# Process: 像元统计数据
	arcpy.gp.CellStatistics_sa([input_lai_1,input_lai_2,input_lai_3,input_lai_4],out_lai_01_tif,"MEAN","DATA")
	arcpy.gp.CellStatistics_sa([input_lai_5,input_lai_6,input_lai_7,input_lai_8], out_lai_02_tif, "MEAN", "DATA")
	arcpy.gp.CellStatistics_sa([input_lai_9,input_lai_10,input_lai_11,input_lai_12], out_lai_03_tif, "MEAN", "DATA")
	arcpy.gp.CellStatistics_sa([input_lai_13,input_lai_14,input_lai_15], out_lai_04_tif, "MEAN", "DATA")
	arcpy.gp.CellStatistics_sa([input_lai_16,input_lai_17,input_lai_18,input_lai_19], out_lai_05_tif, "MEAN", "DATA")
	arcpy.gp.CellStatistics_sa([input_lai_20,input_lai_21,input_lai_22,input_lai_23], out_lai_06_tif, "MEAN", "DATA")
	arcpy.gp.CellStatistics_sa([input_lai_24,input_lai_25,input_lai_26,input_lai_27], out_lai_07_tif, "MEAN", "DATA")
	arcpy.gp.CellStatistics_sa([input_lai_28,input_lai_29,input_lai_30,input_lai_31], out_lai_08_tif, "MEAN", "DATA")
	arcpy.gp.CellStatistics_sa([input_lai_32,input_lai_33,input_lai_34,input_lai_35], out_lai_09_tif, "MEAN", "DATA")
	arcpy.gp.CellStatistics_sa([input_lai_36,input_lai_37,input_lai_38], out_lai_10_tif, "MEAN", "DATA")
	arcpy.gp.CellStatistics_sa([input_lai_39,input_lai_40,input_lai_41,input_lai_42], out_lai_11_tif, "MEAN", "DATA")
	arcpy.gp.CellStatistics_sa([input_lai_43,input_lai_44,input_lai_45,input_lai_46], out_lai_12_tif, "MEAN", "DATA")
	print "完成年内的各月LAI合并！"
