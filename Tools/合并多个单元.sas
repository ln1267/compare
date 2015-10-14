libname W_2_1 "C:\WASSICBZB\Outputs_2_1";

/*向逻辑库中读入输出的结果文件*/
data W_2_1.ANNUALBIO;/* 读入ANNUALBIO（流域年物种丰富度）*/
	infile "C:\WASSICBZB\Outputs_2_1\ANNUALBIO.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR TREE MAMMALS BIRD AMPHIB REPTILES VERTEB AET PET;
run;
data W_2_1.ANNUALCARBON;/* 读入ANNUALCARBON（流域年碳通量）*/
	infile "C:\WASSICBZB\Outputs_2_1\ANNUALCARBON.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR GEP Reco NEE AET PET;
run;
data W_2_1.ANNUALFLOW;/* 读入ANNUALFLOW（流域年水通量）*/
	infile "C:\WASSICBZB\Outputs_2_1\ANNUALFLOW.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR RAIN PET AET Sun_ET RUNOFF RUN_Pratio ET_Pratio RUN_ETRatio SNWPCKMON RFACTOR;
run;
data W_2_1.FLOWVOLBYLANDUSE;/* 读入FLOWVOLBYLANDUSE（流域五种主要植被类型（农田，森林，草地，灌丛，无植被）的年径流）*/
	infile "C:\WASSICBZB\Outputs_2_1\FLOWVOLBYLANDUSE.TXT"  dlm=',' dsd missover firstobs=2;;
	input WATERSHEDID YEAR CROPFLOW FORESTFLOW GRASSFLOW SHRUBSAVAFLOW URBANWATERFLOW TFLOW;
run;
data W_2_1.HUCBIO;/* 读入HUCBIO（流域年均物种丰富度）*/
	infile "C:\WASSICBZB\Outputs_2_1\HUCBIO.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL NO_YR TREE MAMMALS BIRD AMPHIB REPTILES AHUCVERTEB;
run;
data W_2_1.HUCCARBON;/* 读入HUCCARBON（流域年均碳通量）*/
	infile "C:\WASSICBZB\Outputs_2_1\HUCCARBON.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL NO_YR GEP Reco NEE;
run;
data W_2_1.HUCFLOW;/* 读入HUCFLOW（流域年均水通量）*/
	infile "C:\WASSICBZB\Outputs_2_1\HUCFLOW.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL RAIN PET AET RUNOFF RUNOFF_P ET_P RUN_ET_PRFACTOR Y_n;
run;
data W_2_1.MONTHCARBON;/* 读入MONTHCARBON（流域月碳通量）*/
	infile "C:\WASSICBZB\Outputs_2_1\MONTHCARBON.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR MONTH GEP Reco NEE;
run;
data W_2_1.MONTHFLOW;/* 读入MONTHFLOW（流域月水通量）*/
	infile "C:\WASSICBZB\Outputs_2_1\MONTHFLOW.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR MONTH PRECIP TEMP SMC SNWPK PET AET Sun_ET RUNOFF BASEFLOW FLOWMCMMon;
run;
data W_2_1.RUNOFFBYLANDUSE;/* 读入RUNOFFBYLANDUSE（流域各个植被类型的年径流）*/
	infile "C:\WASSICBZB\Outputs_2_1\RUNOFFBYLANDUSE.TXT"  dlm=',' dsd missover firstobs=2;;
	input WATERSHEDID YEAR LADUSEID HUCRUNOFF FLOWVOL LANDratio HUCAREA;
run;
data W_2_1.SOILSTORAGE;/* 读入RUNOFFBYLANDUSE（流域各个植被类型的年径流）*/
	infile "C:\WASSICBZB\Outputs_2_1\SOILSTORAGE.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR MONTH UZTWC UZFWC LZTWC LZFPC LZFSC;
run;
/*模拟结果文件读入结束*/

/*合并几个文件*/
libname W_2_1 "C:\WASSICBZB\Outputs_2_1";
libname W_2_601 "C:\WASSICBZB\Outputs_2_601";
libname W_2_1201 "C:\WASSICBZB\Outputs_2_1201";
libname W "C:\WASSICBZB\Out\one_et";
data W.ANNUALBIO;

set W_2_1.ANNUALBIO W_2_601.ANNUALBIO W_2_1201.ANNUALBIO;

Run;

data W.ANNUALCARBON;

set W_2_1.ANNUALCARBON W_2_601.ANNUALCARBON W_2_1201.ANNUALCARBON;

Run;

data W.ANNUALFLOW;

set W_2_1.ANNUALFLOW W_2_601.ANNUALFLOW W_2_1201.ANNUALFLOW;

Run;

data W.FLOWVOLBYLANDUSE;

set W_2_1.FLOWVOLBYLANDUSE W_2_601.FLOWVOLBYLANDUSE W_2_1201.FLOWVOLBYLANDUSE;

Run;

data W.HUCBIO;

set W_2_1.HUCBIO W_2_601.HUCBIO W_2_1201.HUCBIO;

Run;

data W.HUCCARBON;

set W_2_1.HUCCARBON W_2_601.HUCCARBON W_2_1201.HUCCARBON;

Run;

data W.HUCFLOW;

set W_2_1.HUCFLOW W_2_601.HUCFLOW W_2_1201.HUCFLOW;

Run;

data W.MONTHCARBON;

set W_2_1.MONTHCARBON W_2_601.MONTHCARBON W_2_1201.MONTHCARBON;

Run;

data W.MONTHFLOW;

set W_2_1.MONTHFLOW W_2_601.MONTHFLOW W_2_1201.MONTHFLOW;

Run;

data W.RUNOFFBYLANDUSE;

set W_2_1.RUNOFFBYLANDUSE W_2_601.RUNOFFBYLANDUSE W_2_1201.RUNOFFBYLANDUSE;

Run;

data W.SOILSTORAGE;

set W_2_1.SOILSTORAGE W_2_601.SOILSTORAGE W_2_1201.SOILSTORAGE;

Run;






/*输出所有的到output.xlsx文件*/
PROC EXPORT DATA= W.ANNUALCARBON 
            OUTFILE= "C:\WASSICBZB\out\one_et\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="ANNUALCARBON"; 
RUN;

PROC EXPORT DATA= W.ANNUALFLOW 
            OUTFILE= "C:\WASSICBZB\out\one_et\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="ANNUALFLOW"; 
RUN;
PROC EXPORT DATA= W.FLOWVOLBYLANDUSE 
            OUTFILE= "C:\WASSICBZB\out\one_et\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="FLOWVOLBYLANDUSE"; 
RUN;
PROC EXPORT DATA= W.RUNOFFBYLANDUSE 
            OUTFILE= "C:\WASSICBZB\out\one_et\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="RUNOFFBYLANDUSE"; 
RUN;
PROC EXPORT DATA= W.HUCFLOW 
            OUTFILE= "C:\WASSICBZB\out\one_et\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="HUCFLOW"; 
RUN;
PROC EXPORT DATA= W.HUCCARBON 
            OUTFILE= "C:\WASSICBZB\out\one_et\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="HUCCARBON"; 
RUN;
PROC EXPORT DATA= W.MONTHCARBON 
            OUTFILE= "C:\WASSICBZB\out\one_et\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="MONTHCARBON"; 
RUN;
PROC EXPORT DATA= W.MONTHFLOW 
            OUTFILE= "C:\WASSICBZB\out\one_et\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="MONTHFLOW"; 
RUN;
PROC EXPORT DATA= W.RUNOFFBYLANDUSE 
            OUTFILE= "C:\WASSICBZB\out\one_et\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="RUNOFFBYLANDUSE"; 
RUN;
PROC EXPORT DATA= W.SOILSTORAGE 
            OUTFILE= "C:\WASSICBZB\out\one_et\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="SOILSTORAGE"; 
RUN;



libname W "C:\WASSICBZB\Out\one_et";
/*设置输出的年份或者是cell*/
data MONTHFLOW;

set W.MONTHFLOW;

if year >2011 or year < 2000 then delete;

Run;

data MONTHCARBON;

set W.MONTHCARBON;

if year >2011 or year < 2000 then delete;

Run;
data HUCFLOW;

set W.HUCFLOW;

if year >2011 or year < 2000 then delete;

Run;
data HUCCARBON;

set W.HUCCARBON;

if year >2011 or year < 2000 then delete;

Run;
data ANNUALFLOW;

set W.ANNUALFLOW;

if year >2011 or year < 2000 then delete;

Run;

data ANNUALCARBON;

set W.ANNUALCARBON;

if year >2011 or year < 2000 then delete;

Run;


/*输出所有的到output.xlsx文件*/
PROC EXPORT DATA= ANNUALCARBON 
            OUTFILE= "C:\WASSICBZB\out\one_et\output_00_11.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="ANNUALCARBON"; 
RUN;

PROC EXPORT DATA= ANNUALFLOW 
            OUTFILE= "C:\WASSICBZB\out\one_et\output_00_11.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="ANNUALFLOW"; 
RUN;

PROC EXPORT DATA= HUCFLOW 
            OUTFILE= "C:\WASSICBZB\out\one_et\output_00_11.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="HUCFLOW"; 
RUN;
PROC EXPORT DATA= HUCCARBON 
            OUTFILE= "C:\WASSICBZB\out\one_et\output_00_11.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="HUCCARBON"; 
RUN;
PROC EXPORT DATA= MONTHCARBON 
            OUTFILE= "C:\WASSICBZB\out\one_et\output_00_11.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="MONTHCARBON"; 
RUN;
PROC EXPORT DATA= MONTHFLOW 
            OUTFILE= "C:\WASSICBZB\out\one_et\output_00_11.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="MONTHFLOW"; 
RUN;
