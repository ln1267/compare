
/**需更改的内容有
	一般只需将"200"替换为文件名中“Outputs_”和“_n”之间的值；
		eg：Outputs_3的文件名则将"200"替换为“200”
	1 逻辑库名称：“libname W”
	2 逻辑库的目录：libname W "E:\zagunao\OUTPUTS\Outputs";
	3 读入验证数据在XLXS中的SHEET表名RANGE="200$"; RANGE="200$"; 
	4 "if year < 2000 or year >2006 then delete;”
**/


/*----------------------------数据预处理开始-----------------------------------------------*/
/*为每个输出创建逻辑库*/
libname W "C:\WASSICBZB\Outputs";

/*向逻辑库中读入输出的结果文件*/
data W.ANNUALBIO;/* 读入ANNUALBIO（流域年物种丰富度）*/
	infile "C:\WASSICBZB\Outputs\ANNUALBIO.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR TREE MAMMALS BIRD AMPHIB REPTILES VERTEB AET PET;
run;
data W.ANNUALCARBON;/* 读入ANNUALCARBON（流域年碳通量）*/
	infile "C:\WASSICBZB\Outputs\ANNUALCARBON.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR GEP Reco NEE AET PET;
run;
data W.ANNUALFLOW;/* 读入ANNUALFLOW（流域年水通量）*/
	infile "C:\WASSICBZB\Outputs\ANNUALFLOW.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR RAIN PET AET Sun_ET RUNOFF RUN_Pratio ET_Pratio RUN_ETRatio SNWPCKMON RFACTOR;
run;
data W.FLOWVOLBYLANDUSE;/* 读入FLOWVOLBYLANDUSE（流域五种主要植被类型（农田，森林，草地，灌丛，无植被）的年径流）*/
	infile "C:\WASSICBZB\Outputs\FLOWVOLBYLANDUSE.TXT"  dlm=',' dsd missover firstobs=2;;
	input WATERSHEDID YEAR CROPFLOW FORESTFLOW GRASSFLOW SHRUBSAVAFLOW URBANWATERFLOW TFLOW;
run;
data W.HUCBIO;/* 读入HUCBIO（流域年均物种丰富度）*/
	infile "C:\WASSICBZB\Outputs\HUCBIO.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL NO_YR TREE MAMMALS BIRD AMPHIB REPTILES AHUCVERTEB;
run;
data W.HUCCARBON;/* 读入HUCCARBON（流域年均碳通量）*/
	infile "C:\WASSICBZB\Outputs\HUCCARBON.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL NO_YR GEP Reco NEE;
run;
data W.HUCFLOW;/* 读入HUCFLOW（流域年均水通量）*/
	infile "C:\WASSICBZB\Outputs\HUCFLOW.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL RAIN PET AET RUNOFF RUNOFF_P ET_P RUN_ET_PRFACTOR Y_n;
run;
data W.MONTHCARBON;/* 读入MONTHCARBON（流域月碳通量）*/
	infile "C:\WASSICBZB\Outputs\MONTHCARBON.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR MONTH GEP Reco NEE;
run;
data W.MONTHFLOW;/* 读入MONTHFLOW（流域月水通量）*/
	infile "C:\WASSICBZB\Outputs\MONTHFLOW.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR MONTH PRECIP TEMP SMC SNWPK PET AET Sun_ET RUNOFF BASEFLOW FLOWMCMMon;
run;
data W.RUNOFFBYLANDUSE;/* 读入RUNOFFBYLANDUSE（流域各个植被类型的年径流）*/
	infile "C:\WASSICBZB\Outputs\RUNOFFBYLANDUSE.TXT"  dlm=',' dsd missover firstobs=2;;
	input WATERSHEDID YEAR LADUSEID HUCRUNOFF FLOWVOL LANDratio HUCAREA;
run;
data W.SOILSTORAGE;/* 读入RUNOFFBYLANDUSE（流域各个植被类型的年径流）*/
	infile "C:\WASSICBZB\Outputs\SOILSTORAGE.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR MONTH UZTWC UZFWC LZTWC LZFPC LZFSC;
run;
/*模拟结果文件读入结束*/



/*输出所有的到output.xlsx文件*/
PROC EXPORT DATA= W.ANNUALCARBON 
            OUTFILE= "C:\WASSICBZB\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="ANNUALCARBON"; 
RUN;

PROC EXPORT DATA= W.ANNUALFLOW 
            OUTFILE= "C:\WASSICBZB\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="ANNUALFLOW"; 
RUN;
PROC EXPORT DATA= W.FLOWVOLBYLANDUSE 
            OUTFILE= "C:\WASSICBZB\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="FLOWVOLBYLANDUSE"; 
RUN;
PROC EXPORT DATA= W.RUNOFFBYLANDUSE 
            OUTFILE= "C:\WASSICBZB\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="RUNOFFBYLANDUSE"; 
RUN;
PROC EXPORT DATA= W.HUCFLOW 
            OUTFILE= "C:\WASSICBZB\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="HUCFLOW"; 
RUN;
PROC EXPORT DATA= W.MONTHCARBON 
            OUTFILE= "C:\WASSICBZB\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="MONTHCARBON"; 
RUN;
PROC EXPORT DATA= W.MONTHFLOW 
            OUTFILE= "C:\WASSICBZB\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="MONTHFLOW"; 
RUN;
PROC EXPORT DATA= W.RUNOFFBYLANDUSE 
            OUTFILE= "C:\WASSICBZB\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="RUNOFFBYLANDUSE"; 
RUN;
PROC EXPORT DATA= W.SOILSTORAGE 
            OUTFILE= "C:\WASSICBZB\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="SOILSTORAGE"; 
RUN;






/*读入验证数据*/
/*Monthly validation database*/
/*读入月MODIS月GEP验证数据*/
PROC IMPORT OUT= W.MODGEP 
            DATAFILE= "E:\zagunao\VALID\MODGEP.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="sheet1$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;

	/*Zhang ET database*/
PROC IMPORT OUT= W.ZHANGET 
            DATAFILE= "E:\zagunao\VALID\ZHANGET.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="sheet1$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
	/*MODIS ET database*/
PROC IMPORT OUT= W.MODET
            DATAFILE= "E:\zagunao\VALID\MODET.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="sheet1$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
PROC IMPORT OUT= W.MODPET
            DATAFILE= "E:\zagunao\VALID\MODPET.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="sheet1$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
PROC IMPORT OUT= W.FLOW_V
            DATAFILE= "E:\zagunao\VALID\FLOW_O.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="sheet2$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
PROC IMPORT OUT= W.BASEFLOW_V
            DATAFILE= "E:\zagunao\VALID\BASEFLOW_O.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="baseflow$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
/*Annual validation database*/
PROC IMPORT OUT=W.MODGPP_Y 
            DATAFILE= "E:\zagunao\VALID\MODGPP_Y.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="sheet1$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
PROC IMPORT OUT=W.MODET_Y 
            DATAFILE= "E:\zagunao\VALID\MODET_Y.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="sheet1$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
PROC IMPORT OUT=W.ZHANGET_Y 
            DATAFILE= "E:\zagunao\VALID\ZHET_Y.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="sheet1$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;

/*验证数据读入结束*/

/*----------------------------数据预处理结束-----------------------------------------------*/


/*----------------------------数据处理开始-----------------------------------------------*/
libname W "C:\WASSICBZB\Outputs";


/*处理月验证数据*/

	/*处理月GEP数据*/
DATA W.GEP_VERIFY;/*先清除数据集中存在的数据*/
	delete;
RUN;
DATA GEP_VERIFY; /*从Monthcarbon数据集中提取出2000-20011年的GEP数据*/
	set W.Monthcarbon(drop=reco);
	if year < 2000 or year >2006 then delete;
RUN;
proc sort data=work.GEP_VERIFY out=W.GEP_VERIFY;/*对提取出的GEP数据进行排序*/
     by cell YEAR MONTH;
run; 
proc sort data=W.MODGEP out=W.MODGEP;/*对验证用的GEP数据进行排序*/
     by HUC_8 YEAR MONTH;
run; 
DATA W.Gep_val;
	set W.MODGEP;
	if year < 2000 or year >2006 then delete;
run;
DATA W.GEP_VERIFY;/* 将验证数据添加到模拟数据后面*/
	MERGE W.GEP_VERIFY W.Gep_val (keep=MODIS_GEP);
RUN;

DATA W.GEP_VERIFY_G;/*删除验证数据不存在的值*/
	set W.GEP_VERIFY;
	if month<4 or month > 10 then delete ;
RUN;

	/*处理ET数据*/
DATA W.ET_VERIFY;/*先清除数据集中存在的数据*/
	delete;
RUN;
DATA ET_VERIFY; /*从Monthcarbon数据集中提取出2000-20011年的ET数据*/
	set W.Monthflow(keep=CELL YEAR MONTH PET AET Sun_ET);
	if year < 2000 or year >2006 then delete;
RUN;
proc sort data=work.ET_VERIFY out=W.ET_VERIFY;/*对提取出的ET数据进行排序*/
     by cell YEAR MONTH;
run; 

		/*处理MODIS_ET数据*/
DATA W.ET_val;
	set W.MODET;
	if year < 2000 or year >2006 then delete;
run;
proc sort data=W.ET_Val out=W.ET_Val;/*对验证用的ET数据进行排序*/
     by HUC_8 YEAR MONTH;
run; 
DATA W.ET_VERIFY;/* 将验证数据添加到模拟数据后面*/
	MERGE W.ET_VERIFY W.ET_val (keep=MODIS_ET);
RUN;

DATA W.ET_VERIFY_G;/*删除验证数据不存在的值*/
	set W.ET_VERIFY;
	if month<4 or month > 10 then delete ;
RUN;

		/*处理MODIS_PET数据*/
DATA W.ET_val;
	set W.MODPET;
	if year < 2000 or year >2006 then delete;
run;
proc sort data=W.ET_Val out=W.ET_Val;/*对验证用的ET数据进行排序*/
     by HUC_8 YEAR MONTH;
run; 
DATA W.ET_VERIFY;/* 将验证数据添加到模拟数据后面*/
	MERGE W.ET_VERIFY W.ET_val (keep=MODIS_PET);
RUN;
DATA W.ET_VERIFY_G;/*删除验证数据不存在的值*/
	set W.ET_VERIFY;
	if month<4 or month > 10 then delete ;
RUN;

		/*处理Zhang ET数据*/
DATA W.ET_val;
	set W.ZHANGET;
	if year < 2000 or year >2006 then delete;
run;
proc sort data=W.ET_Val out=W.ET_Val;/*对验证用的ET数据进行排序*/
     by HUC_8 YEAR MONTH;
run; 

DATA W.ET_VERIFY;/* 将验证数据添加到模拟数据后面*/
	MERGE W.ET_VERIFY W.ET_val (keep=ZHANG_ET);
RUN;

DATA W.ET_VERIFY_G;/*删除验证数据不存在的值*/
	set W.ET_VERIFY;
	if month<4 or month > 10 then delete ;
RUN;
	/*合并月验证数据*/

DATA W.MONTH_VERIFY;/* 将验证数据添加到模拟数据后面*/
	MERGE W.ET_VERIFY W.GEP_VERIFY;
RUN;


/*处理年验证数据*/
	/*处理年GEP数据*/
DATA W.GEP_VERIFY_Y;/*先清除数据集中存在的数据*/
	delete;
RUN;
DATA GEP_VERIFY_Y; /*从Monthcarbon数据集中提取出2000-20011年的GEP数据*/
	set W.Annualcarbon;
	if year < 2000 or year >2006 then delete;
RUN;
proc sort data=work.GEP_VERIFY_Y out=W.GEP_VERIFY_Y;/*对提取出的GEP数据进行排序*/
     by cell YEAR ;
run; 
DATA W.Gep_val_Y;
	set W.MODGPP_Y;
	if year < 2000 or year >2006 then delete;
run;
proc sort data=W.Gep_val_Y out=W.Gep_val_Y;/*对验证用的GEP数据进行排序*/
     by HUC_8 YEAR ;
run; 

DATA W.GEP_VERIFY_Y;/* 将验证数据添加到模拟数据后面*/
	MERGE W.GEP_VERIFY_Y W.Gep_val_Y(keep=MODGPP_Y) ;
RUN;

	/*处理年ET数据*/
DATA W.ET_VERIFY_Y;/*先清除数据集中存在的数据*/
	delete;
RUN;
DATA ET_VERIFY_Y; /*从Monthcarbon数据集中提取出2000-20011年的ET数据*/
	set W.Annualflow(keep=CELL YEAR RAIN PET AET Sun_ET RUNOFF);
	if year < 2000 or year >2006 then delete;
RUN;
proc sort data=work.ET_VERIFY_Y out=W.ET_VERIFY_Y;/*对提取出的ET数据进行排序*/
     by cell YEAR ;
run; 

		/*处理MODIS_ET数据*/
DATA W.ET_val_Y;
	set W.MODET_Y;
	if year < 2000 or year >2006 then delete;
run;
proc sort data=W.ET_Val_Y out=W.ET_Val_Y;/*对验证用的ET数据进行排序*/
     by HUC_8 YEAR ;
run; 
DATA W.ET_VERIFY_Y;/* 将验证数据添加到模拟数据后面*/
	MERGE W.ET_VERIFY_Y W.ET_val_Y (keep=MODET_Y);
RUN;

		/*处理Zhang ET数据*/
DATA W.ET_val_Y;
	set W.ZHANGET_Y;
	if year < 2000 or year >2006 then delete;
run;
proc sort data=W.ET_Val_Y out=W.ET_Val_Y;/*对验证用的ET数据进行排序*/
     by HUC_8 YEAR MONTH;
run; 

DATA W.ET_VERIFY_Y;/* 将验证数据添加到模拟数据后面*/
	MERGE W.ET_VERIFY_Y W.ET_val_Y (keep=ZHANGET_Y);
RUN;
	/*合并年验证数据*/

DATA W.ANNUAL_VERIFY;/* 将验证数据添加到模拟数据后面*/
	MERGE W.ET_VERIFY_Y W.GEP_VERIFY_Y;
RUN;

/*向验证数据中添加流域属性字段*/
PROC IMPORT OUT= W.basin 
            DATAFILE= "E:\zagunao\basin_pro.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;
data W.basin;
	set W.basin(keep= HUC_8  shape_area);
	ratio=shape_area/2397709885;
	cell=HUC_8;
run;
proc sort data=W.basin out=W.basin;/*对验证用的ET数据进行排序*/
     by cell;
run; 
data W.month_VERIFY;
 	merge W.month_VERIFY W.basin;
	by cell;
	drop huc_8;
run;
data W.ANNUAL_VERIFY;
 	merge W.ANNUAL_VERIFY W.basin;
	by cell;
	drop huc_8;
run;


/*流域总结果的处理*/
	/*月流域结果*/
data W.BASIN_month_VERIFY;
	set W.month_VERIFY;
	B_PET=PET*ratio;
	B_AET=AET*ratio;
	B_SunET=Sun_ET*ratio;
	B_MODET=MODIS_ET*ratio;
	B_MODPET=MODIS_PET*ratio;
	B_ZHANGET=ZHANG_ET*ratio;
	B_GEP=GEP*ratio;
	B_NEE=NEE*ratio;
	B_MODGEP=MODIS_GEP*ratio;
	drop PET AET Sun_ET MODIS_ET MODIS_PET ZHANG_ET GEP NEE MODIS_GEP shape_area ratio;
run;

proc sort data=W.BASIN_month_VERIFY out=BASIN_month_VERIFY;
	by year month ;
run;
proc means data=BASIN_month_VERIFY noprint; /*分别求算每个流域的均值*/                                                                                             
        by year month ;                                                                                                                   
        output out=W.BASIN_month_VERIFY(drop=_type_ _freq_) sum=;                                                                                    
run;  

	/*年流域结果*/
data W.BASIN_annual_VERIFY;
	set W.annual_VERIFY;
	B_PET=PET*ratio;
	B_AET=AET*ratio;
	B_SunET=Sun_ET*ratio;
	B_MODET=MODET_Y*ratio;
	B_ZHANGET=ZHANGET_Y*ratio;
	B_GEP=GEP*ratio;
	B_RECO=reco*ratio;
	B_NEE=NEE*ratio;
	B_MODGPP=MODGPP_Y*ratio;
	B_RUNOFF=RUNOFF*ratio;
	B_RAIN=RAIN*ratio;
	drop PET AET Sun_ET MODET_Y rain ZHANGET_Y GEP NEE MODGPP_Y RECO shape_area ratio RUNOFF;
run;

proc sort data=W.BASIN_annual_VERIFY out=BASIN_annual_VERIFY;
	by year  ;
run;
proc means data=BASIN_annual_VERIFY noprint; /*分别求算每个流域的均值*/                                                                                             
        by year;                                                                                                                   
        output out=W.BASIN_annual_VERIFY(drop=_type_ _freq_) sum=;                                                                                    
run;  



/*----------------------------GEP和ET的验证数据处理结束--------------------------*/


ods printer pdf file ='C:\WASSICBZB\Validation.pdf';/*为每个输出创建逻辑库*/
/*---------------------------------显示验证结果------------------------------*/

/*显示cell月验证结果*/
proc sort data=W.Month_verify out=Month_verify;
	by cell year ;
run;
proc sort data=W.Annual_verify out=Annual_verify;
	by cell year;
run;
	/*GEP数据显示*/
/*
proc reg data=Month_verify;
	model GEP=MODIS_GEP;
	plot GEP*MODIS_GEP;
	title "A monthly comparison of GEP between WASSI-C and MODIS of each HRU";
run;
quit; 

proc reg data=Annual_verify;
	model GEP=MODGPP_Y;
	plot GEP*MODGPP_Y;
	title "A annual comparison of GEP between WASSI-C and MODIS of each HRU";

run;
quit; 
	/*ET数据显示*/
/*
proc reg data=Month_verify;
	model AET=MODIS_ET;
	plot AET*MODIS_ET;
	title "A monthly comparison of ET between WASSI-C and MODIS of each HRU";
run;
quit; 
data ZHANG_ET;
	set Month_verify;
	if ZHANG_ET <= 0 then delete;
run;
proc reg data=zhang_et;
	model AET=ZHANG_ET;
	plot AET*ZHANG_ET;
	title "A monthly comparison of ET between WASSI-C and ZHANG KE of each HRU";
run;
quit; 
proc reg data=Annual_verify;
	model AET=MODET_Y;
	plot AET*MODET_Y;
	title "A annual comparison of ET between WASSI-C and MODIS of each HRU";
run;
quit; 


/*显示BASIN月验证结果*/
proc sort data=W.BASIN_Month_verify out=BASIN_Month_verify;
	by cell year ;
run;
proc sort data=W.BASIN_Annual_verify out=BASIN_Annual_verify;
	by cell year;
run;
	/*GEP数据显示*/
/*
proc reg data=BASIN_Month_verify;
	model B_GEP=B_MODGEP;
	plot B_GEP*B_MODGEP;
	title "A monthly comparison of GEP between WASSI-C and MODIS of the whole watershed";
run;
quit; 

proc reg data=BASIN_Annual_verify;
	model B_GEP=B_MODGPP;
	plot B_GEP*B_MODGPP;
	title "A monthly comparison of GEP between WASSI-C and MODIS of the whole watershed";

run;
quit; 
	/*ET数据显示*/
/*
proc reg data=BASIN_Month_verify;
	model B_AET=B_MODET;
	plot B_AET*B_MODET;
	title "A monthly comparison of ET between WASSI-C and MODIS of the whole watershed";
run;
quit; 
proc reg data=BASIN_Month_verify;
	model B_AET=B_ZHANGET;
	plot B_AET*B_ZHANGET;
	title "A monthly comparison of ET between WASSI-C and ZHANG KE of the whole watershed";
run;
quit; 
proc reg data=BASIN_Annual_verify;
	model B_AET=B_MODET;
	plot B_AET*B_MODET;
	title "A annual comparison of ET between WASSI-C and MODIS of the whole watershed";
run;
quit; 
*/

/*导出处理好的验证数据集*/

PROC EXPORT DATA= W.Month_verify 
            OUTFILE= "C:\WASSICBZB\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="MONTHLY"; 
RUN;
PROC EXPORT DATA= W.Annual_verify 
            OUTFILE= "C:\WASSICBZB\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="ANNUAL"; 
RUN;

PROC EXPORT DATA= W.BASIN_Month_verify 
            OUTFILE= "C:\WASSICBZB\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="BASIN_MONTHLY"; 
RUN;
PROC EXPORT DATA= W.BASIN_Annual_verify 
            OUTFILE= "C:\WASSICBZB\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="BASIN_ANNUAL"; 
RUN;
/*------------------------处理结束---------------------------*/




/*------------------------开始处理FLOW数据---------------------------*/
proc sort data=W.monthflow out=run_v ;
	by cell;
run;
data run_v;
 	merge run_v W.basin;
	by cell;
run;

data W.run_v;
	set run_v;
	B_SNWPK=SNWPK*ratio;
	B_SMC=SMC*ratio;
	B_RUNOFF=RUNOFF*ratio;
	B_BASEFLOW=BASEFLOW*ratio;
	drop PRECIP TEMP SMC SNWPK PET AET Sun_ET RUNOFF BASEFLOW FLOWMCMMon HUC_8 shape_area ratio ;
run;
proc sort data=W.run_v out=run_v ;
	by year month;
run;
proc sort data=W.monthflow out=flow_v ;
	by year month;
run;
proc means data=run_v noprint; /*分别求算每个流域的均值*/                                                                                             
        by year month ;                                                                                                                   
        output out=out_run(drop=_type_ _freq_) sum=;                                                                                    
run;  
proc means data=flow_v noprint; /*分别求算每个流域的均值*/                                                                                             
        by year month ;                                                                                                                   
        output out=out_flow(drop=_type_ _freq_) sum=;                                                                                    
run;  

DATA W.FLOW_verify;
	set out_flow(keep=YEAR MONTH FLOWMCMMon);
	FLOW=FLOWMCMMon;
	RUNOFF=FLOWMCMMon*1000/2904.38;
	if year < 2000 or year >2006 then delete;
	
	drop FLOWMCMMon;
run;
DATA W.run;
	set out_run;
	if year < 2000 or year >2006 then delete;
run;
proc sort data=W.FLOW_verify out=W.FLOW_verify;/*对验证用的GEP数据进行排序*/
     by  YEAR MONTH ;
run; 
DATA W.FLOW_v;
	set W.flow_V;
	if year < 2000 or year >2006 then delete;
run;
DATA W.BASEFLOW_v;
	set W.baseflow_V;
	if year < 2000 or year >2006 then delete;
run;
DATA W.FLOW_verify;/* 将验证数据添加到模拟数据后面*/
	MERGE W.FLOW_verify W.flow_V W.BASEFLOW_V W.run;
	drop cell;
RUN;
     
/*
proc reg data=W.FLOW_verify;
	model RUNOFF=RUNOFF_O;
	plot RUNOFF*RUNOFF_O;
	title "A monthly comparison of RUNOFF between WASSI-C and Observed of the whole watershed";
run;
quit;
*/ 
proc reg data=W.FLOW_verify;
	model B_baseflow=baseflow_O;
	plot B_baseflow*baseflow_O;
	title "A monthly comparison of BASEFLOW between WASSI-C and Observed of the whole watershed";
run;
quit; 
/*
proc reg data=flow_v_g;
	model FLOWMCMMon=run_V;
	plot FLOWMCMMon*run_V;
	title "FLOW生长季验证结果";
run;
quit; 
*/
	/*导出处理好的FLOW数据集*/
PROC EXPORT DATA= W.FLOW_verify 
            OUTFILE= "C:\WASSICBZB\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="FLOW"; 
RUN;

/*------------------------FLOW处理结束---------------------------*/

ods printer close;
