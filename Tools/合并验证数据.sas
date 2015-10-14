
data d df1 df2 df3 df4 df5 df6 df7 df8 df9 df10 df11 df12 GEP ET ;
delete;
run;

PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\GEP\4000_GEP200601.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df1 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=1;
run;

PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\GEP\4000_GEP200602.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df2 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=2;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\GEP\4000_GEP200603.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df3 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=3;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\GEP\4000_GEP200604.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df4 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=4;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\GEP\4000_GEP200605.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df5 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=5;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\GEP\4000_GEP200606.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df6 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=6;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\GEP\4000_GEP200607.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df7 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=7;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\GEP\4000_GEP200608.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df8 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=8;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\GEP\4000_GEP200609.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df9 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=9;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\GEP\4000_GEP200610.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df10 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=10;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\GEP\4000_GEP200611.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df11 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=11;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\GEP\4000_GEP200612.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df12 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=12;
run;
data GEP;
set df1 df2 df3 df4 df5 df6 df7 df8 df9 df10 df11 df12 ;
run;

PROC EXPORT DATA= GEP 
            OUTFILE= "E:\HUC\gep\GEP_4000.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="gep"; 
RUN;

data d df1 df2 df3 df4 df5 df6 df7 df8 df9 df10 df11 df12 GEP ET ;
delete;
run;

PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\ET\4000_ET200601.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df1 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=1;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\ET\4000_ET200602.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df2 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=2;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\ET\4000_ET200603.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df3 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=3;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\ET\4000_ET200604.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df4 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=4;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\ET\4000_ET200605.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df5 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=5;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\ET\4000_ET200606.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df6 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=6;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\ET\4000_ET200607.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df7 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=7;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\ET\4000_ET200608.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df8 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=8;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\ET\4000_ET200609.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df9 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=9;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\ET\4000_ET200610.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df10 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=10;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\ET\4000_ET200611.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df11 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=11;
run;
PROC IMPORT OUT= WORK.d 
            DATAFILE= "E:\HUC\ET\4000_ET200612.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data df12 ;
merge d(keep=HUC_8 MEAN);
year=2006;
month=12;
run;
data ET;
set df1 df2 df3 df4 df5 df6 df7 df8 df9 df10 df11 df12 ;
run;
PROC EXPORT DATA= ET 
            OUTFILE= "E:\HUC\ET\ET_4000.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="et"; 
RUN;
