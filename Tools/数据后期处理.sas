
/**需更改的内容有
	一般只需将"200"替换为文件名中“Outputs_”和“_n”之间的值；
		eg：Outputs_250_3的文件名则将"200"替换为“200”
	1 逻辑库名称：“libname W_250”
	2 逻辑库的目录：libname W_250 "E:\HUC\Outputs_union\Outputs_250";
	3 读入验证数据在XLXS中的SHEET表名RANGE="200$"; RANGE="200$"; 
**/



/*----------------------------数据预处理开始-----------------------------------------------*/
/*为每个输出创建逻辑库*/
libname W_250 "E:\HUC\Outputs_union\Outputs_250";

/*向逻辑库中读入输出的结果文件*/
data W_250.ANNUALBIO;/* 读入ANNUALBIO（流域年物种丰富度）*/
	infile "E:\HUC\Outputs_union\Outputs_250\ANNUALBIO.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR TREE MAMMALS BIRD AMPHIB REPTILES VERTEB AET PET;
run;
data W_250.ANNUALCARBON;/* 读入ANNUALCARBON（流域年碳通量）*/
	infile "E:\HUC\Outputs_union\Outputs_250\ANNUALCARBON.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR GEP Reco NEE AET PET;
run;
data W_250.ANNUALFLOW;/* 读入ANNUALFLOW（流域年水通量）*/
	infile "E:\HUC\Outputs_union\Outputs_250\ANNUALFLOW.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR RAIN PET AET RUNOFF RUN_Pratio ET_Pratio RUN_ETRatio SNWPCKMON RFACTOR;
run;
data W_250.FLOWVOLBYLANDUSE;/* 读入FLOWVOLBYLANDUSE（流域五种主要植被类型（农田，森林，草地，灌丛，无植被）的年径流）*/
	infile "E:\HUC\Outputs_union\Outputs_250\FLOWVOLBYLANDUSE.TXT"  dlm=',' dsd missover firstobs=2;;
	input WATERSHEDID YEAR CROPFLOW FORESTFLOW GRASSFLOW SHRUBSAVAFLOW URBANWATERFLOW TFLOW;
run;
data W_250.HUCBIO;/* 读入HUCBIO（流域年均物种丰富度）*/
	infile "E:\HUC\Outputs_union\Outputs_250\HUCBIO.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL NO_YR TREE MAMMALS BIRD AMPHIB REPTILES AHUCVERTEB;
run;
data W_250.HUCCARBON;/* 读入HUCCARBON（流域年均碳通量）*/
	infile "E:\HUC\Outputs_union\Outputs_250\HUCCARBON.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL NO_YR GEP Reco NEE;
run;
data W_250.HUCFLOW;/* 读入HUCFLOW（流域年均水通量）*/
	infile "E:\HUC\Outputs_union\Outputs_250\HUCFLOW.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL RAIN PET AET RUNOFF RUNOFF_P ET_P RUN_ET_PRFACTOR;
run;
data W_250.MONTHCARBON;/* 读入MONTHCARBON（流域月碳通量）*/
	infile "E:\HUC\Outputs_union\Outputs_250\MONTHCARBON.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR MONTH GEP Reco NEE;
run;
data W_250.MONTHFLOW;/* 读入MONTHFLOW（流域月水通量）*/
	infile "E:\HUC\Outputs_union\Outputs_250\MONTHFLOW.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR MONTH PRECIP TEMP SMC SNWPK PET AET RUNOFF FLOWMCMMon;
run;
data W_250.RUNOFFBYLANDUSE;/* 读入RUNOFFBYLANDUSE（流域各个植被类型的年径流）*/
	infile "E:\HUC\Outputs_union\Outputs_250\RUNOFFBYLANDUSE.TXT"  dlm=',' dsd missover firstobs=2;;
	input WATERSHEDID YEAR LADUSEID HUCRUNOFF FLOWVOL LANDratio HUCAREA;
run;
/*模拟结果文件读入结束*/


/*读入验证数据*/
PROC IMPORT OUT= W_250.GEP_VAL 
            DATAFILE= "E:\HUC\gep\GEP_250.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="sheet1$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;

PROC IMPORT OUT=GEP_V_A 
            DATAFILE= "E:\HUC\npp\250_npp2006.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;
PROC IMPORT OUT=GPP_V_A 
            DATAFILE= "E:\HUC\gpp\250_gpp2006.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;


PROC IMPORT OUT= W_250.ET_VAL 
            DATAFILE= "E:\HUC\ET\ET_250.xlsx" 
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

/*处理GEP数据*/
DATA W_250.GEP_VERIFY;/*先清除数据集中存在的数据*/
	delete;
RUN;
DATA GEP_VERIFY; /*从Monthcarbon数据集中提取出2000-20011年的GEP数据*/
	set W_250.Monthcarbon(drop=reco);
	*where (year < 20012 and year >= 2000);
RUN;
proc sort data=work.GEP_VERIFY out=W_250.GEP_VERIFY;/*对提取出的GEP数据进行排序*/
     by cell YEAR MONTH;
run; 
proc sort data=W_250.GEP_Val out=W_250.GEP_Val;/*对验证用的GEP数据进行排序*/
     by HUC_8 YEAR MONTH;
run; 
DATA W_250.GEP_VERIFY;/* 将验证数据添加到模拟数据后面*/
	MERGE W_250.GEP_VERIFY W_250.Gep_val (keep=GEP_V);
	GEP_V=GEP_V*0.1;
RUN;

DATA W_250.GEP_VERIFY;/*删除验证数据不存在的值*/
	set W_250.GEP_VERIFY;
	if GEP_V < 0 then delete ;
RUN;

PROC IMPORT OUT= W_250.basin_250 
            DATAFILE= "E:\HUC\basins\basin_250.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;
data basin;
	set W_250.basin_250(keep=liuyu HUC_8);
	cell=HUC_8;
run;
data W_250.GEP_VERIFY;
 	merge W_250.GEP_VERIFY basin;
	by cell;
	drop huc_8;
run;
data gep_V_A;
	merge gep_V_A basin;
	by HUC_8;
	GEP_V=MEAN*0.1;
	keep cell GEP_V liuyu;
run;
data W_250.GEP_V_A;
	merge W_250.Annualcarbon GEP_V_A;
	by cell;
run;

data gpp_V_A;
	set gpp_V_A;
	cell=HUC_8;
	GPP=MEAN*0.1;
	keep cell GpP ;
run;
data W_250.GpP_V_A;
	merge W_250.Annualcarbon GpP_V_A;
	by cell;
	keep cell gep gpp;
run;


/*处理ET数据*/
DATA W_250.ET_VERIFY;/*先清除数据集中存在的数据*/
	delete;
RUN;
DATA ET_VERIFY; /*从Monthcarbon数据集中提取出2000-20011年的ET数据*/
	set W_250.Monthflow(keep=CELL YEAR MONTH PET AET);
	*where (year <=2006 and year >= 1983);
RUN;
proc sort data=work.ET_VERIFY out=W_250.ET_VERIFY;/*对提取出的ET数据进行排序*/
     by cell YEAR MONTH;
run; 
proc sort data=W_250.ET_Val out=W_250.ET_Val;/*对验证用的ET数据进行排序*/
     by HUC_8 YEAR MONTH;
run; 
DATA W_250.ET_VERIFY;/* 将验证数据添加到模拟数据后面*/
	MERGE W_250.ET_VERIFY W_250.ET_val (keep=ET_V);
RUN;


DATA W_250.ET_VERIFY;/*删除验证数据不存在的值*/
	set W_250.ET_VERIFY;
	if ET_V < 0 then delete ;
RUN;
data W_250.ET_VERIFY;
 	merge W_250.ET_VERIFY basin;
	by cell;
	drop huc_8;
run;
/*----------------------------数据处理结束-----------------------------------------------*/

/*----------------------------处理结果显示-----------------------------------------------*/
libname W_250 "E:\HUC\Outputs_union\Outputs_250";
ods printer pdf file ='E:\HUC\Outputs_union\Validation\Validation_250.pdf';/*为每个输出创建逻辑库*/
/*GEP结果散点图*/

proc reg data=W_250.GEP_VERIFY;/*验证GEP数据的散点图及其回归线*/
	model GEP=GEP_V;
	plot GEP*GEP_V;
	title "GEP_250验证结果";
run;
quit;
/**利用年数据的验证结果
proc reg data=W_250.GEP_V_A;
	model GEP=GEP_V;
	plot GEP*GEP_V;
	title "GEP_250的年验证结果";
run;
quit;
proc sort data=W_250.GEP_V_A out=GEP_V_A;
     by liuyu;
run; 
proc reg data=GEP_V_A;
	model GEP=GEP_V;
	plot GEP*GEP_V;
	title "GEP_250的年验证结果";
	by liuyu;
run;
quit;

**/
/*对三个流域分别进行验证分析*/
proc sort data=W_250.GEP_VERIFY out=GEP_VERIFY;
     by liuyu;
run; 

proc reg data=GEP_VERIFY;
	model GEP=GEP_V;
	plot GEP*GEP_V;
	title "GEP_250三个子流域验证结果";
	by liuyu;
run;
quit;

/*分流域单元验证GEP数据的散点图及其回归线*/
/**
proc reg data=W_250.GEP_VERIFY;
	model GEP=GEP_V;
	plot GEP*GEP_V;
	title "GEP_250分流域验证结果";
	by cell;
run;
quit;
**/
/*分月验证GEP数据的散点图及其回归线*/
proc sort data=W_250.GEP_VERIFY out=GEP_VERIFY;
	by month;
run;
proc reg data=GEP_VERIFY;
	model GEP=GEP_V;
	plot GEP*GEP_V;
	title "GEP_250分月验证结果";
	by month;
run;
quit;

/*分别求算每个流域的年均值*/
data out_GEP ;
delete;
run;
proc means data=W_250.GEP_VERIFY noprint;   /*分别求算每个流域的均值*/                                                                                           
        by cell year;                                                                                                                   
        output out=out_GEP(drop=_type_ _freq_) mean=;                                                                                    
run;        
proc reg data=out_GEP;/*分年份验证GEP数据的散点图及其回归线*/
	model GEP=GEP_V;
	plot GEP*GEP_V;
	title "GEP_250流域年均值验证结果";
run;
quit;

proc reg data=W_250.GpP_V_A;/*分年份验证GEP数据的散点图及其回归线*/
	model GEP=GpP;
	plot GEP*GpP;
	title "GpP_250流域年验证结果";
run;
quit;

/*分别求算每个流域的均值*/
data out_GEP1 ;
delete;
run;
proc means data=W_250.GEP_VERIFY noprint; /*分别求算每个流域的均值*/                                                                                             
        by cell;                                                                                                                   
        output out=out_GEP(drop=_type_ _freq_) mean=;                                                                                    
run;     
proc reg data=out_GEP;/*分年份验证GEP数据的散点图及其回归线*/
	model GEP=GEP_V;
	plot GEP*GEP_V;
	title "GEP_250流域均值验证结果";
	
run;
quit;



/*ET结果散点图*/
proc reg data=W_250.ET_VERIFY;/*验证ET数据的散点图及其回归线*/
	model PET=ET_V;
	plot PET*ET_V;
	title "ET_250验证结果";
run;
quit;

/*对三个流域分别进行验证分析*/
proc sort data=W_250.ET_VERIFY out=ET_VERIFY;
     by liuyu;
run; 

proc reg data=ET_VERIFY;
	model PET=ET_V;
	plot PET*ET_V;
	title "ET_250三个子流域验证结果";
	by liuyu;
run;
quit;

/*分流域验证ET数据的散点图及其回归线*/
/**
proc reg data=W_250.ET_VERIFY;
	model PET=ET_V;
	plot PET*ET_V;
	title "ET_250分流域验证结果";
	by cell;
run;
quit;
**/



/*分别求算每个流域的年均值*/
data out_ET ;
delete;
run;
proc means data=W_250.ET_VERIFY noprint;   /*分别求算每个流域的均值*/                                                                                           
        by cell year;                                                                                                                   
        output out=out_ET(drop=_type_ _freq_) mean=;                                                                                    
run;        
proc reg data=out_ET;/*分年份验证ET数据的散点图及其回归线*/
	model PET=ET_V;
	plot PET*ET_V;
	title "ET_250流域年均值验证结果";
run;
quit;
/*分别求算每个流域的均值*/
data out_ET1 ;
delete;
run;
proc means data=W_250.ET_VERIFY noprint; /*分别求算每个流域的均值*/                                                                                             
        by cell;                                                                                                                   
        output out=out_ET1(drop=_type_ _freq_) mean=;                                                                                    
run;     
proc reg data=out_ET1;/*分年份验证ET数据的散点图及其回归线*/
	model PET=ET_V;
	plot PET*ET_V;
	title "ET_250流域均值验证结果";
run;
quit;
/*----------------------------处理结果显示结束-----------------------------------------------*/
ods printer close;
