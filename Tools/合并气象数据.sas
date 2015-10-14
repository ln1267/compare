
data d df1 df2 df3 df4 df5 df6 df7 df8 df9 df10 df11 df12 pre tm climate;
delete;
run;

PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\basin_20000\PRE\pre200601.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df1 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=1;
run;

PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\basin_20000\PRE\pre200602.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df2 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=2;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\basin_20000\PRE\pre200603.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df3 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=3;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\basin_20000\PRE\pre200604.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df4 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=4;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\basin_20000\PRE\pre200605.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df5 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=5;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\basin_20000\PRE\pre200606.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df6 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=6;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\basin_20000\PRE\pre200607.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df7 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=7;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\basin_20000\PRE\pre200608.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df8 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=8;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\basin_20000\PRE\pre200609.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df9 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=9;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\basin_20000\PRE\pre200610.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df10 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=10;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\basin_20000\PRE\pre200611.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df11 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=11;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\basin_20000\PRE\pre200612.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df12 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=12;
run;
data pre;
set df1 df2 df3 df4 df5 df6 df7 df8 df9 df10 df11 df12 ;
run;



PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\basin_20000\TM\TM200601.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df1 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=1;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\basin_20000\TM\TM200602.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df2 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=2;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\basin_20000\TM\TM200603.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df3 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=3;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\basin_20000\TM\TM200604.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df4 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=4;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\basin_20000\TM\TM200605.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df5 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=5;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\basin_20000\TM\TM200606.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df6 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=6;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\basin_20000\TM\TM200607.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df7 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=7;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\basin_20000\TM\TM200608.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df8 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=8;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\basin_20000\TM\TM200609.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df9 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=9;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\basin_20000\TM\TM200610.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df10 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=10;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\basin_20000\TM\TM200611.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df11 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=11;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\basin_20000\TM\TM200612.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df12 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=12;
run;
data TM;
set df1 df2 df3 df4 df5 df6 df7 df8 df9 df10 df11 df12 ;
run;

data pre;/*将GEP验证数据的变量名命名为GEP_V*/
	set pre;
	Ppt_mm=MEAN;
	drop MEAN;
run;

data tm;/*将GEP验证数据的变量名命名为GEP_V*/
	set tm;
	Tavg_C=MEAN;
	drop MEAN;
run;
data climate ;
 merge pre tm;
run;
*proc sort data=climate;
*	by HUC_8 year month ;
*run;
PROC EXPORT DATA= WORK.climate 
            OUTFILE= "E:\HUC\basin_20000\climate.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="climate_20000"; 
RUN;
