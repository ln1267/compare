# ---------------------------------------------------------------------------
# 生成气象数据————————arcgis9.3
# Created on: 星期五 十二月 09 2011 11:27:55 上午
#   (generated by ArcGIS/ModelBuilder)
# ---------------------------------------------------------------------------

# Import arcpy module
import arcpy

# Check out any necessary licenses
arcpy.CheckOutExtension("spatial")


#循环开始
for n in xrange(2001,2007):
	for m in xrange(1,13):
		m="%02d" % m		#转换为两位日期
# Local variables:	
												#如果处理气象数据统计字段为"HUC_8"，统计LAI是则为”VEG_ID"
		data_class="lai"							#待处理的数据类型”TM“，”PRE“，”LAI“
		data_class2="veg_"					#判断是处理气象数据还是LAI数据；分别用“basin_"和“VEG_"
##########################-----流域划分个数---------##############################################			
		basin_num_1="10"
		basin_num_2="125"
		basin_num_3="15"
		basin_num_4="25"
		basin_num_5="35"
		basin_num_6="40"
		basin_num_7="50"
		basin_num_8="85"
		basin_num_9="100"
		basin_num_10="160"
		basin_num_11="200"
		basin_num_12="300"
		basin_num_13="650"
		basin_num_14="1500"
		basin_num_15="4000"
		basin_num_16="20000"
		basin_num_17="20000"
		basin_num_18="2000"
		################################################################################	
		
##########################-----公用目录---------##############################################		
		input_dir="E:\\lai\\"		#气象数据所在的目录
		output_raster_dir="E:\\HUC\\"+data_class+"\\"			#转换生成的栅格所在的目录
		basins_dir="E:\\HUC\\VEG\\" 	#流域边界所在目录
################################################################################		
		
#######################---转换后的每个流域结果的输出目录------#################################
		
		output_data_dir_basin_1=output_raster_dir+basin_num_1	#统计结果输出的目录
		output_data_dir_basin_2=output_raster_dir+basin_num_2	#统计结果输出的目录
		output_data_dir_basin_3=output_raster_dir+basin_num_3	#统计结果输出的目录
		output_data_dir_basin_4=output_raster_dir+basin_num_4	#统计结果输出的目录
		output_data_dir_basin_5=output_raster_dir+basin_num_5	#统计结果输出的目录
		output_data_dir_basin_6=output_raster_dir+basin_num_6	#统计结果输出的目录
		output_data_dir_basin_7=output_raster_dir+basin_num_7	#统计结果输出的目录
		output_data_dir_basin_8=output_raster_dir+basin_num_8	#统计结果输出的目录
		output_data_dir_basin_9=output_raster_dir+basin_num_9	#统计结果输出的目录
		output_data_dir_basin_10=output_raster_dir+basin_num_10	#统计结果输出的目录
		output_data_dir_basin_11=output_raster_dir+basin_num_11	#统计结果输出的目录
		output_data_dir_basin_12=output_raster_dir+basin_num_12	#统计结果输出的目录
		output_data_dir_basin_13=output_raster_dir+basin_num_13	#统计结果输出的目录
		output_data_dir_basin_14=output_raster_dir+basin_num_14	#统计结果输出的目录
		output_data_dir_basin_15=output_raster_dir+basin_num_15	#统计结果输出的目录
		output_data_dir_basin_16=output_raster_dir+basin_num_16	#统计结果输出的目录
		output_data_dir_basin_17=output_raster_dir+basin_num_17	#统计结果输出的目录
		output_data_dir_basin_18=output_raster_dir+basin_num_18	#统计结果输出的目录

		################################################################################

		name1=data_class+str(n)+m+".txt"
		name2=data_class+str(n)+m+".tif"
		name4="_"+data_class+str(n)+m+".dbf"
		
######################-----待预处理的气象数据文件------##############################################		
		input_file = input_dir+name1							#输入气象数据
#		input_file = "d:\\tm196101.txt"
		output_asctoras = input_dir+name2				#处理后的气象数据
################################################################################
		
######################-----分流域处理后的结果文件------##############################################		
		output_data_basin_1=output_data_dir_basin_1+name4	#按表格统计结果输出的文件
		output_data_basin_2=output_data_dir_basin_2+name4	#按表格统计结果输出的文件
		output_data_basin_3=output_data_dir_basin_3+name4	#按表格统计结果输出的文件
		output_data_basin_4=output_data_dir_basin_4+name4	#按表格统计结果输出的文件
		output_data_basin_5=output_data_dir_basin_5+name4	#按表格统计结果输出的文件
		output_data_basin_6=output_data_dir_basin_6+name4	#按表格统计结果输出的文件
		output_data_basin_7=output_data_dir_basin_7+name4	#按表格统计结果输出的文件
		output_data_basin_8=output_data_dir_basin_8+name4	#按表格统计结果输出的文件
		output_data_basin_9=output_data_dir_basin_9+name4	#按表格统计结果输出的文件
		output_data_basin_10=output_data_dir_basin_10+name4	#按表格统计结果输出的文件
		output_data_basin_11=output_data_dir_basin_11+name4	#按表格统计结果输出的文件
		output_data_basin_12=output_data_dir_basin_12+name4	#按表格统计结果输出的文件
		output_data_basin_13=output_data_dir_basin_13+name4	#按表格统计结果输出的文件
		output_data_basin_14=output_data_dir_basin_14+name4	#按表格统计结果输出的文件
		output_data_basin_15=output_data_dir_basin_15+name4	#按表格统计结果输出的文件
		output_data_basin_16=output_data_dir_basin_16+name4	#按表格统计结果输出的文件
		output_data_basin_17=output_data_dir_basin_17+name4	#按表格统计结果输出的文件
		output_data_basin_18=output_data_dir_basin_18+name4	#按表格统计结果输出的文件
		################################################################################
		
		print input_dir,output_asctoras,output_data_basin_1
		
##########################待处理流域边界文件##############################################		
		basin_1=basins_dir+data_class2+basin_num_1+".shp"	# 待统计流域边界
		basin_2=basins_dir+data_class2+basin_num_2+".shp"	# 待统计流域边界
		basin_3=basins_dir+data_class2+basin_num_3+".shp"	# 待统计流域边界
		basin_4=basins_dir+data_class2+basin_num_4+".shp"	# 待统计流域边界
		basin_5=basins_dir+data_class2+basin_num_5+".shp"	# 待统计流域边界
		basin_6=basins_dir+data_class2+basin_num_6+".shp"	# 待统计流域边界
		basin_7=basins_dir+data_class2+basin_num_7+".shp"	# 待统计流域边界
		basin_8=basins_dir+data_class2+basin_num_8+".shp"	# 待统计流域边界
		basin_9=basins_dir+data_class2+basin_num_9+".shp"	# 待统计流域边界
		basin_10=basins_dir+data_class2+basin_num_10+".shp"	# 待统计流域边界
		basin_11=basins_dir+data_class2+basin_num_11+".shp"	# 待统计流域边界
		basin_12=basins_dir+data_class2+basin_num_12+".shp"	# 待统计流域边界
		basin_13=basins_dir+data_class2+basin_num_13+".shp"	# 待统计流域边界
		basin_14=basins_dir+data_class2+basin_num_14+".shp"	# 待统计流域边界
		basin_15=basins_dir+data_class2+basin_num_15+".shp"	# 待统计流域边界
		basin_16=basins_dir+data_class2+basin_num_16+".shp"	# 待统计流域边界
		basin_17=basins_dir+data_class2+basin_num_17+".shp"	# 待统计流域边界
		basin_18=basins_dir+data_class2+basin_num_18+".shp"	# 待统计流域边界
################################################################################



# Process: 以表格显示分区统计岷江流域
# Process: Zonal Statistics as Table...
		arcpy.gp.ZonalStatisticsAsTable_sa(basin_1, "VEG_ID", output_asctoras, output_data_basin_1, "DATA", "MEAN")
		print "成功对basin_250进行统计"
		
# Process: 以表格显示分区统计岷江流域
# Process: Zonal Statistics as Table...
		arcpy.gp.ZonalStatisticsAsTable_sa(basin_2, "VEG_ID", output_asctoras, output_data_basin_2, "DATA", "MEAN")
		print "成功对basin_500进行统计"

# Process: 以表格显示分区统计岷江流域
# Process: Zonal Statistics as Table...
		arcpy.gp.ZonalStatisticsAsTable_sa(basin_3, "VEG_ID", output_asctoras, output_data_basin_3, "DATA", "MEAN")
		print "成功对basin_800进行统计"

# Process: 以表格显示分区统计岷江流域
# Process: Zonal Statistics as Table...
		arcpy.gp.ZonalStatisticsAsTable_sa(basin_4, "VEG_ID", output_asctoras, output_data_basin_4, "DATA", "MEAN")
		print "成功对basin_1200进行统计"

# Process: 以表格显示分区统计岷江流域
# Process: Zonal Statistics as Table...
		arcpy.gp.ZonalStatisticsAsTable_sa(basin_5, "VEG_ID", output_asctoras, output_data_basin_5, "DATA", "MEAN")
		print "成功对basin_1500进行统计"


# Process: 以表格显示分区统计岷江流域
# Process: Zonal Statistics as Table...
		arcpy.gp.ZonalStatisticsAsTable_sa(basin_6, "VEG_ID", output_asctoras, output_data_basin_6, "DATA", "MEAN")
		print "成功对basin_2000进行统计"
		
# Process: 以表格显示分区统计岷江流域
# Process: Zonal Statistics as Table...
		arcpy.gp.ZonalStatisticsAsTable_sa(basin_7, "VEG_ID", output_asctoras, output_data_basin_7, "DATA", "MEAN")
		print "成功对basin_250进行统计"
		
# Process: 以表格显示分区统计岷江流域
# Process: Zonal Statistics as Table...
		arcpy.gp.ZonalStatisticsAsTable_sa(basin_8, "VEG_ID", output_asctoras, output_data_basin_8, "DATA", "MEAN")
		print "成功对basin_500进行统计"

# Process: 以表格显示分区统计岷江流域
# Process: Zonal Statistics as Table...
		arcpy.gp.ZonalStatisticsAsTable_sa(basin_9, "VEG_ID", output_asctoras, output_data_basin_9, "DATA", "MEAN")
		print "成功对basin_800进行统计"

# Process: 以表格显示分区统计岷江流域
# Process: Zonal Statistics as Table...
		arcpy.gp.ZonalStatisticsAsTable_sa(basin_10, "VEG_ID", output_asctoras, output_data_basin_10, "DATA", "MEAN")
		print "成功对basin_1200进行统计"

# Process: 以表格显示分区统计岷江流域
# Process: Zonal Statistics as Table...
		arcpy.gp.ZonalStatisticsAsTable_sa(basin_11, "VEG_ID", output_asctoras, output_data_basin_11, "DATA", "MEAN")
		print "成功对basin_1500进行统计"


# Process: 以表格显示分区统计岷江流域
# Process: Zonal Statistics as Table...
		arcpy.gp.ZonalStatisticsAsTable_sa(basin_12, "VEG_ID", output_asctoras, output_data_basin_12, "DATA", "MEAN")
		print "成功对basin_2000进行统计"
# Process: 以表格显示分区统计岷江流域
# Process: Zonal Statistics as Table...
		arcpy.gp.ZonalStatisticsAsTable_sa(basin_13, "VEG_ID", output_asctoras, output_data_basin_13, "DATA", "MEAN")
		print "成功对basin_250进行统计"
		
