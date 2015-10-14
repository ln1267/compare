PROC IMPORT OUT= WORK.T45 
            DATAFILE= "E:\HUC\TM\UNION\tm_45.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
PROC IMPORT OUT= WORK.P45 
            DATAFILE= "E:\HUC\PRE\UNION\PRE_45.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data C45;
	set P45(keep=HUC_8 MEAN);
	Ppt_mm=MEAN;
	drop MEAN;
	set T45(keep=HUC_8 MEAN);
	Tavg_C=MEAN;
	DROP MEAN;
run;
PROC EXPORT DATA= C45 
            OUTFILE= "E:\HUC\climate\climate_45.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="climate"; 
RUN;
PROC IMPORT OUT= WORK.T50 
            DATAFILE= "E:\HUC\TM\UNION\tm_50.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
PROC IMPORT OUT= WORK.P50 
            DATAFILE= "E:\HUC\PRE\UNION\PRE_50.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data C50;
	set P50(keep=HUC_8 MEAN);
	Ppt_mm=MEAN;
	drop MEAN;
	set T50(keep=HUC_8 MEAN);
	Tavg_C=MEAN;
	DROP MEAN;
run;
PROC EXPORT DATA= C50 
            OUTFILE= "E:\HUC\climate\climate_50.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="climate"; 
RUN;
PROC IMPORT OUT= WORK.T55 
            DATAFILE= "E:\HUC\TM\UNION\tm_55.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
PROC IMPORT OUT= WORK.P55 
            DATAFILE= "E:\HUC\PRE\UNION\PRE_55.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data C55;
	set P55(keep=HUC_8 MEAN);
	Ppt_mm=MEAN;
	drop MEAN;
	set T55(keep=HUC_8 MEAN);
	Tavg_C=MEAN;
	DROP MEAN;
run;
PROC EXPORT DATA= C55 
            OUTFILE= "E:\HUC\climate\climate_55.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="climate"; 
RUN;
PROC IMPORT OUT= WORK.T60 
            DATAFILE= "E:\HUC\TM\UNION\tm_60.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
PROC IMPORT OUT= WORK.P60 
            DATAFILE= "E:\HUC\PRE\UNION\PRE_60.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data C60;
	set P60(keep=HUC_8 MEAN);
	Ppt_mm=MEAN;
	drop MEAN;
	set T60(keep=HUC_8 MEAN);
	Tavg_C=MEAN;
	DROP MEAN;
run;
PROC EXPORT DATA= C60 
            OUTFILE= "E:\HUC\climate\climate_60.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="climate"; 
RUN;
PROC IMPORT OUT= WORK.T65 
            DATAFILE= "E:\HUC\TM\UNION\tm_65.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
PROC IMPORT OUT= WORK.P65 
            DATAFILE= "E:\HUC\PRE\UNION\PRE_65.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data C65;
	set P65(keep=HUC_8 MEAN);
	Ppt_mm=MEAN;
	drop MEAN;
	set T65(keep=HUC_8 MEAN);
	Tavg_C=MEAN;
	DROP MEAN;
run;
PROC EXPORT DATA= C65 
            OUTFILE= "E:\HUC\climate\climate_65.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="climate"; 
RUN;
PROC IMPORT OUT= WORK.T70 
            DATAFILE= "E:\HUC\TM\UNION\tm_70.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
PROC IMPORT OUT= WORK.P70 
            DATAFILE= "E:\HUC\PRE\UNION\PRE_70.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data C70;
	set P70(keep=HUC_8 MEAN);
	Ppt_mm=MEAN;
	drop MEAN;
	set T70(keep=HUC_8 MEAN);
	Tavg_C=MEAN;
	DROP MEAN;
run;
PROC EXPORT DATA= C70 
            OUTFILE= "E:\HUC\climate\climate_70.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="climate"; 
RUN;
PROC IMPORT OUT= WORK.T75 
            DATAFILE= "E:\HUC\TM\UNION\tm_75.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
PROC IMPORT OUT= WORK.P75 
            DATAFILE= "E:\HUC\PRE\UNION\PRE_75.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data C75;
	set P75(keep=HUC_8 MEAN);
	Ppt_mm=MEAN;
	drop MEAN;
	set T75(keep=HUC_8 MEAN);
	Tavg_C=MEAN;
	DROP MEAN;
run;
PROC EXPORT DATA= C75 
            OUTFILE= "E:\HUC\climate\climate_75.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="climate"; 
RUN;
PROC IMPORT OUT= WORK.T80 
            DATAFILE= "E:\HUC\TM\UNION\tm_80.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
PROC IMPORT OUT= WORK.P80 
            DATAFILE= "E:\HUC\PRE\UNION\PRE_80.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data C80;
	set P80(keep=HUC_8 MEAN);
	Ppt_mm=MEAN;
	drop MEAN;
	set T80(keep=HUC_8 MEAN);
	Tavg_C=MEAN;
	DROP MEAN;
run;
PROC EXPORT DATA= C80 
            OUTFILE= "E:\HUC\climate\climate_80.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="climate"; 
RUN;
PROC IMPORT OUT= WORK.T90 
            DATAFILE= "E:\HUC\TM\UNION\tm_90.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
PROC IMPORT OUT= WORK.P90 
            DATAFILE= "E:\HUC\PRE\UNION\PRE_90.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data C90;
	set P90(keep=HUC_8 MEAN);
	Ppt_mm=MEAN;
	drop MEAN;
	set T90(keep=HUC_8 MEAN);
	Tavg_C=MEAN;
	DROP MEAN;
run;
PROC EXPORT DATA= C90 
            OUTFILE= "E:\HUC\climate\climate_90.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="climate"; 
RUN;
PROC IMPORT OUT= WORK.T100 
            DATAFILE= "E:\HUC\TM\UNION\tm_100.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
PROC IMPORT OUT= WORK.P100 
            DATAFILE= "E:\HUC\PRE\UNION\PRE_100.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data C100;
	set P100(keep=HUC_8 MEAN);
	Ppt_mm=MEAN;
	drop MEAN;
	set T100(keep=HUC_8 MEAN);
	Tavg_C=MEAN;
	DROP MEAN;
run;
PROC EXPORT DATA= C100 
            OUTFILE= "E:\HUC\climate\climate_100.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="climate"; 
RUN;
PROC IMPORT OUT= WORK.T120 
            DATAFILE= "E:\HUC\TM\UNION\tm_120.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
PROC IMPORT OUT= WORK.P120 
            DATAFILE= "E:\HUC\PRE\UNION\PRE_120.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data C120;
	set P120(keep=HUC_8 MEAN);
	Ppt_mm=MEAN;
	drop MEAN;
	set T120(keep=HUC_8 MEAN);
	Tavg_C=MEAN;
	DROP MEAN;
run;
PROC EXPORT DATA= C120 
            OUTFILE= "E:\HUC\climate\climate_120.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="climate"; 
RUN;
PROC IMPORT OUT= WORK.T150 
            DATAFILE= "E:\HUC\TM\UNION\tm_150.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
PROC IMPORT OUT= WORK.P150 
            DATAFILE= "E:\HUC\PRE\UNION\PRE_150.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data C150;
	set P150(keep=HUC_8 MEAN);
	Ppt_mm=MEAN;
	drop MEAN;
	set T150(keep=HUC_8 MEAN);
	Tavg_C=MEAN;
	DROP MEAN;
run;
PROC EXPORT DATA= C150 
            OUTFILE= "E:\HUC\climate\climate_150.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="climate"; 
RUN;
PROC IMPORT OUT= WORK.T200 
            DATAFILE= "E:\HUC\TM\UNION\tm_200.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
PROC IMPORT OUT= WORK.P200 
            DATAFILE= "E:\HUC\PRE\UNION\PRE_200.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data C200;
	set P200(keep=HUC_8 MEAN);
	Ppt_mm=MEAN;
	drop MEAN;
	set T200(keep=HUC_8 MEAN);
	Tavg_C=MEAN;
	DROP MEAN;
run;
PROC EXPORT DATA= C200 
            OUTFILE= "E:\HUC\climate\climate_200.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="climate"; 
RUN;
PROC IMPORT OUT= WORK.T250 
            DATAFILE= "E:\HUC\TM\UNION\tm_250.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
PROC IMPORT OUT= WORK.P250 
            DATAFILE= "E:\HUC\PRE\UNION\PRE_250.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data C250;
	set P250(keep=HUC_8 MEAN);
	Ppt_mm=MEAN;
	drop MEAN;
	set T250(keep=HUC_8 MEAN);
	Tavg_C=MEAN;
	DROP MEAN;
run;
PROC EXPORT DATA= C250 
            OUTFILE= "E:\HUC\climate\climate_250.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="climate"; 
RUN;
PROC IMPORT OUT= WORK.T300 
            DATAFILE= "E:\HUC\TM\UNION\tm_300.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
PROC IMPORT OUT= WORK.P300 
            DATAFILE= "E:\HUC\PRE\UNION\PRE_300.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data C300;
	set P300(keep=HUC_8 MEAN);
	Ppt_mm=MEAN;
	drop MEAN;
	set T300(keep=HUC_8 MEAN);
	Tavg_C=MEAN;
	DROP MEAN;
run;
PROC EXPORT DATA= C300 
            OUTFILE= "E:\HUC\climate\climate_300.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="climate"; 
RUN;
PROC IMPORT OUT= WORK.T380 
            DATAFILE= "E:\HUC\TM\UNION\tm_380.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
PROC IMPORT OUT= WORK.P380 
            DATAFILE= "E:\HUC\PRE\UNION\PRE_380.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data C380;
	set P380(keep=HUC_8 MEAN);
	Ppt_mm=MEAN;
	drop MEAN;
	set T380(keep=HUC_8 MEAN);
	Tavg_C=MEAN;
	DROP MEAN;
run;
PROC EXPORT DATA= C380 
            OUTFILE= "E:\HUC\climate\climate_380.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="climate"; 
RUN;
PROC IMPORT OUT= WORK.T458 
            DATAFILE= "E:\HUC\TM\UNION\tm_458.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
PROC IMPORT OUT= WORK.P458 
            DATAFILE= "E:\HUC\PRE\UNION\PRE_458.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data C458;
	set P458(keep=HUC_8 MEAN);
	Ppt_mm=MEAN;
	drop MEAN;
	set T458(keep=HUC_8 MEAN);
	Tavg_C=MEAN;
	DROP MEAN;
run;
PROC EXPORT DATA= C458 
            OUTFILE= "E:\HUC\climate\climate_458.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="climate"; 
RUN;
PROC IMPORT OUT= WORK.T650 
            DATAFILE= "E:\HUC\TM\UNION\tm_650.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
PROC IMPORT OUT= WORK.P650 
            DATAFILE= "E:\HUC\PRE\UNION\PRE_650.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data C650;
	set P650(keep=HUC_8 MEAN);
	Ppt_mm=MEAN;
	drop MEAN;
	set T650(keep=HUC_8 MEAN);
	Tavg_C=MEAN;
	DROP MEAN;
run;
PROC EXPORT DATA= C650 
            OUTFILE= "E:\HUC\climate\climate_650.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="climate"; 
RUN;
PROC IMPORT OUT= WORK.T800 
            DATAFILE= "E:\HUC\TM\UNION\tm_800.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
PROC IMPORT OUT= WORK.P800 
            DATAFILE= "E:\HUC\PRE\UNION\PRE_800.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data C800;
	set P800(keep=HUC_8 MEAN);
	Ppt_mm=MEAN;
	drop MEAN;
	set T800(keep=HUC_8 MEAN);
	Tavg_C=MEAN;
	DROP MEAN;
run;
PROC EXPORT DATA= C800 
            OUTFILE= "E:\HUC\climate\climate_800.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="climate"; 
RUN;
PROC IMPORT OUT= WORK.T1500 
            DATAFILE= "E:\HUC\TM\UNION\tm_1500.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
PROC IMPORT OUT= WORK.P1500 
            DATAFILE= "E:\HUC\PRE\UNION\PRE_1500.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data C1500;
	set P1500(keep=HUC_8 MEAN);
	Ppt_mm=MEAN;
	drop MEAN;
	set T1500(keep=HUC_8 MEAN);
	Tavg_C=MEAN;
	DROP MEAN;
run;
PROC EXPORT DATA= C1500 
            OUTFILE= "E:\HUC\climate\climate_1500.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="climate"; 
RUN;
PROC IMPORT OUT= WORK.T4000 
            DATAFILE= "E:\HUC\TM\UNION\tm_4000.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
PROC IMPORT OUT= WORK.P4000 
            DATAFILE= "E:\HUC\PRE\UNION\PRE_4000.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data C4000;
	set P4000(keep=HUC_8 MEAN);
	Ppt_mm=MEAN;
	drop MEAN;
	set T4000(keep=HUC_8 MEAN);
	Tavg_C=MEAN;
	DROP MEAN;
run;
PROC EXPORT DATA= C4000 
            OUTFILE= "E:\HUC\climate\climate_4000.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="climate"; 
RUN;
