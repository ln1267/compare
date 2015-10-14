
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
libname W "E:\zagunao\OUTPUTS\Outputs";

/*向逻辑库中读入输出的结果文件*/
data W.ANNUALBIO;/* 读入ANNUALBIO（流域年物种丰富度）*/
	infile "E:\zagunao\OUTPUTS\Outputs\ANNUALBIO.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR TREE MAMMALS BIRD AMPHIB REPTILES VERTEB AET PET;
run;
data W.ANNUALCARBON;/* 读入ANNUALCARBON（流域年碳通量）*/
	infile "E:\zagunao\OUTPUTS\Outputs\ANNUALCARBON.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR GEP Reco NEE AET PET;
run;
data W.ANNUALFLOW;/* 读入ANNUALFLOW（流域年水通量）*/
	infile "E:\zagunao\OUTPUTS\Outputs\ANNUALFLOW.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR RAIN PET AET Sun_ET RUNOFF RUN_Pratio ET_Pratio RUN_ETRatio SNWPCKMON RFACTOR;
run;
data W.FLOWVOLBYLANDUSE;/* 读入FLOWVOLBYLANDUSE（流域五种主要植被类型（农田，森林，草地，灌丛，无植被）的年径流）*/
	infile "E:\zagunao\OUTPUTS\Outputs\FLOWVOLBYLANDUSE.TXT"  dlm=',' dsd missover firstobs=2;;
	input WATERSHEDID YEAR CROPFLOW FORESTFLOW GRASSFLOW SHRUBSAVAFLOW URBANWATERFLOW TFLOW;
run;
data W.HUCBIO;/* 读入HUCBIO（流域年均物种丰富度）*/
	infile "E:\zagunao\OUTPUTS\Outputs\HUCBIO.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL NO_YR TREE MAMMALS BIRD AMPHIB REPTILES AHUCVERTEB;
run;
data W.HUCCARBON;/* 读入HUCCARBON（流域年均碳通量）*/
	infile "E:\zagunao\OUTPUTS\Outputs\HUCCARBON.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL NO_YR GEP Reco NEE;
run;
data W.HUCFLOW;/* 读入HUCFLOW（流域年均水通量）*/
	infile "E:\zagunao\OUTPUTS\Outputs\HUCFLOW.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL RAIN PET AET RUNOFF RUNOFF_P ET_P RUN_ET_PRFACTOR;
run;
data W.MONTHCARBON;/* 读入MONTHCARBON（流域月碳通量）*/
	infile "E:\zagunao\OUTPUTS\Outputs\MONTHCARBON.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR MONTH GEP Reco NEE;
run;
data W.MONTHFLOW;/* 读入MONTHFLOW（流域月水通量）*/
	infile "E:\zagunao\OUTPUTS\Outputs\MONTHFLOW.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR MONTH PRECIP TEMP SMC SNWPK PET AET Sun_ET RUNOFF FLOWMCMMon;
run;
data W.RUNOFFBYLANDUSE;/* 读入RUNOFFBYLANDUSE（流域各个植被类型的年径流）*/
	infile "E:\zagunao\OUTPUTS\Outputs\RUNOFFBYLANDUSE.TXT"  dlm=',' dsd missover firstobs=2;;
	input WATERSHEDID YEAR LADUSEID HUCRUNOFF FLOWVOL LANDratio HUCAREA;
run;
/*模拟结果文件读入结束*/


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
libname W "E:\zagunao\OUTPUTS\Outputs";
ods printer pdf file ='E:\zagunao\OUTPUTS\Validation.pdf';/*为每个输出创建逻辑库*/

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



/*---------------------------------显示验证结果------------------------------*/

/*显示cell月验证结果*/
proc sort data=W.Month_verify out=Month_verify;
	by cell year ;
run;
proc sort data=W.Annual_verify out=Annual_verify;
	by cell year;
run;
	/*GEP数据显示*/
proc reg data=Month_verify;
	model GEP=MODIS_GEP;
	plot GEP*MODIS_GEP;
	title "GEP月验证结果";
run;
quit; 

proc reg data=Annual_verify;
	model GEP=MODGPP_Y;
	plot GEP*MODGPP_Y;
	title "GEP年验证结果";

run;
quit; 
	/*ET数据显示*/
proc reg data=Month_verify;
	model AET=MODIS_ET;
	plot AET*MODIS_ET;
	title "MODIS_ET月验证结果";
run;
quit; 
proc reg data=Month_verify;
	model AET=ZHANG_ET;
	plot AET*ZHANG_ET;
	title "ZHANG_ET月验证结果";
run;
quit; 
proc reg data=Annual_verify;
	model AET=MODET_Y;
	plot AET*MODET_Y;
	title "MODIS_ET年验证结果";
run;
quit; 
proc reg data=Annual_verify;
	model AET=ZHANGET_Y;
	plot AET*ZHANGET_Y;
	title "ZHANG_ET年验证结果";
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
proc reg data=BASIN_Month_verify;
	model B_GEP=B_MODGEP;
	plot B_GEP*B_MODGEP;
	title "BASIN_GEP月验证结果";
run;
quit; 

proc reg data=BASIN_Annual_verify;
	model B_GEP=B_MODGPP;
	plot B_GEP*B_MODGPP;
	title "BASIN_GEP年验证结果";

run;
quit; 
	/*ET数据显示*/
proc reg data=BASIN_Month_verify;
	model B_AET=B_MODET;
	plot B_AET*B_MODET;
	title "BASIN_MODIS_ET月验证结果";
run;
quit; 
proc reg data=BASIN_Month_verify;
	model B_AET=B_ZHANGET;
	plot B_AET*B_ZHANGET;
	title "BASIN_ZHANG_ET月验证结果";
run;
quit; 
proc reg data=BASIN_Annual_verify;
	model B_AET=B_MODET;
	plot B_AET*B_MODET;
	title "BASIN_MODIS_ET年验证结果";
run;
quit; 
proc reg data=BASIN_Annual_verify;
	model B_AET=B_ZHANGET;
	plot B_AET*B_ZHANGET;
	title "BASIN_ZHANG_ET年验证结果";
run;
quit; 

/*导出处理好的验证数据集*/

PROC EXPORT DATA= W.Month_verify 
            OUTFILE= "E:\zagunao\OUTPUTS\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="MONTHLY"; 
RUN;
PROC EXPORT DATA= W.Annual_verify 
            OUTFILE= "E:\zagunao\OUTPUTS\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="ANNUAL"; 
RUN;

PROC EXPORT DATA= W.BASIN_Month_verify 
            OUTFILE= "E:\zagunao\OUTPUTS\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="BASIN_MONTHLY"; 
RUN;
PROC EXPORT DATA= W.BASIN_Annual_verify 
            OUTFILE= "E:\zagunao\OUTPUTS\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="BASIN_ANNUAL"; 
RUN;
/*------------------------处理结束---------------------------*/




/*------------------------开始处理FLOW数据---------------------------*/
proc sort data=W.monthflow out=flow_v ;
	by year month;
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
proc sort data=W.FLOW_verify out=W.FLOW_verify;/*对验证用的GEP数据进行排序*/
     by  YEAR MONTH ;
run; 
DATA W.FLOW_v;
	set W.flow_V;
	if year < 2000 or year >2006 then delete;
run;

DATA W.FLOW_verify;/* 将验证数据添加到模拟数据后面*/
	MERGE W.FLOW_verify W.flow_V;
RUN;
     
proc reg data=W.FLOW_verify;
	model FLOW=FLOW_O;
	plot FLOW*FLOW_O;
	title "FLOW验证结果";
run;
quit; 
proc reg data=W.FLOW_verify;
	model RUNOFF=RUNOFF_O;
	plot RUNOFF*RUNOFF_O;
	title "RUNOFF验证结果";
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
            OUTFILE= "E:\zagunao\OUTPUTS\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="FLOW"; 
RUN;

/*------------------------FLOW处理结束---------------------------*/

ods printer close;
