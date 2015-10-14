libname W_1995 "C:\WASSICBZB\Outputs_1995";

/*??????????????*/
data W_1995.ANNUALBIO;/* ??ANNUALBIO(????????)*/
	infile "C:\WASSICBZB\Outputs_1995\ANNUALBIO.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR TREE MAMMALS BIRD AMPHIB REPTILES VERTEB AET PET;
run;
data W_1995.ANNUALCARBON;/* ??ANNUALCARBON(??????)*/
	infile "C:\WASSICBZB\Outputs_1995\ANNUALCARBON.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR GEP Reco NEE AET PET;
run;
data W_1995.ANNUALFLOW;/* ??ANNUALFLOW(??????)*/
	infile "C:\WASSICBZB\Outputs_1995\ANNUALFLOW.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR RAIN PET AET Sun_ET RUNOFF RUN_Pratio ET_Pratio RUN_ETRatio SNWPCKMON RFACTOR;
run;
data W_1995.FLOWVOLBYLANDUSE;/* ??FLOWVOLBYLANDUSE(??????????(??,??,??,??,???)????)*/
	infile "C:\WASSICBZB\Outputs_1995\FLOWVOLBYLANDUSE.TXT"  dlm=',' dsd missover firstobs=2;;
	input WATERSHEDID YEAR CROPFLOW FORESTFLOW GRASSFLOW SHRUBSAVAFLOW URBANWATERFLOW TFLOW;
run;
data W_1995.HUCBIO;/* ??HUCBIO(?????????)*/
	infile "C:\WASSICBZB\Outputs_1995\HUCBIO.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL NO_YR TREE MAMMALS BIRD AMPHIB REPTILES AHUCVERTEB;
run;
data W_1995.HUCCARBON;/* ??HUCCARBON(???????)*/
	infile "C:\WASSICBZB\Outputs_1995\HUCCARBON.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL NO_YR GEP Reco NEE;
run;
data W_1995.HUCFLOW;/* ??HUCFLOW(???????)*/
	infile "C:\WASSICBZB\Outputs_1995\HUCFLOW.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL RAIN PET AET RUNOFF RUNOFF_P ET_P RUN_ET_PRFACTOR Y_n;
run;
data W_1995.MONTHCARBON;/* ??MONTHCARBON(??????)*/
	infile "C:\WASSICBZB\Outputs_1995\MONTHCARBON.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR MONTH GEP Reco NEE;
run;
data W_1995.MONTHFLOW;/* ??MONTHFLOW(??????)*/
	infile "C:\WASSICBZB\Outputs_1995\MONTHFLOW.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR MONTH PRECIP TEMP SMC SNWPK PET AET Sun_ET RUNOFF BASEFLOW FLOWMCMMon;
run;
data W_1995.RUNOFFBYLANDUSE;/* ??RUNOFFBYLANDUSE(????????????)*/
	infile "C:\WASSICBZB\Outputs_1995\RUNOFFBYLANDUSE.TXT"  dlm=',' dsd missover firstobs=2;;
	input WATERSHEDID YEAR LADUSEID HUCRUNOFF FLOWVOL LANDratio HUCAREA;
run;
data W_1995.SOILSTORAGE;/* ??RUNOFFBYLANDUSE(????????????)*/
	infile "C:\WASSICBZB\Outputs_1995\SOILSTORAGE.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR MONTH UZTWC UZFWC LZTWC LZFPC LZFSC;
run;
/*??????????*/



/*??????output.xlsx??*/
PROC EXPORT DATA= W_1995.ANNUALCARBON 
            OUTFILE= "C:\WASSICBZB\out\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="ANNUALCARBON"; 
RUN;

PROC EXPORT DATA= W_1995.ANNUALFLOW 
            OUTFILE= "C:\WASSICBZB\out\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="ANNUALFLOW"; 
RUN;
PROC EXPORT DATA= W_1995.FLOWVOLBYLANDUSE 
            OUTFILE= "C:\WASSICBZB\out\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="FLOWVOLBYLANDUSE"; 
RUN;
PROC EXPORT DATA= W_1995.RUNOFFBYLANDUSE 
            OUTFILE= "C:\WASSICBZB\out\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="RUNOFFBYLANDUSE"; 
RUN;
PROC EXPORT DATA= W_1995.HUCFLOW 
            OUTFILE= "C:\WASSICBZB\out\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="HUCFLOW"; 
RUN;
PROC EXPORT DATA= W_1995.HUCCARBON 
            OUTFILE= "C:\WASSICBZB\out\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="HUCCARBON"; 
RUN;
PROC EXPORT DATA= W_1995.MONTHCARBON 
            OUTFILE= "C:\WASSICBZB\out\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="MONTHCARBON"; 
RUN;
PROC EXPORT DATA= W_1995.MONTHFLOW 
            OUTFILE= "C:\WASSICBZB\out\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="MONTHFLOW"; 
RUN;
PROC EXPORT DATA= W_1995.RUNOFFBYLANDUSE 
            OUTFILE= "C:\WASSICBZB\out\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="RUNOFFBYLANDUSE"; 
RUN;
PROC EXPORT DATA= W_1995.SOILSTORAGE 
            OUTFILE= "C:\WASSICBZB\out\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="SOILSTORAGE"; 
RUN;



libname W "C:\WASSICBZB\Out";
/*??????????cell*/
data MONTHFLOW;

set W_1995.MONTHFLOW;

if year >2011 or year < 2000 then delete;

Run;

data MONTHCARBON;

set W_1995.MONTHCARBON;

if year >2011 or year < 2000 then delete;

Run;
data HUCFLOW;

set W_1995.HUCFLOW;

if year >2011 or year < 2000 then delete;

Run;
data HUCCARBON;

set W_1995.HUCCARBON;

if year >2011 or year < 2000 then delete;

Run;
data ANNUALFLOW;

set W_1995.ANNUALFLOW;

if year >2011 or year < 2000 then delete;

Run;

data ANNUALCARBON;

set W_1995.ANNUALCARBON;

if year >2011 or year < 2000 then delete;

Run;


/*??????output.xlsx??*/
PROC EXPORT DATA= ANNUALCARBON 
            OUTFILE= "C:\WASSICBZB\out\output_00_11.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="ANNUALCARBON"; 
RUN;

PROC EXPORT DATA= ANNUALFLOW 
            OUTFILE= "C:\WASSICBZB\out\output_00_11.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="ANNUALFLOW"; 
RUN;

PROC EXPORT DATA= HUCFLOW 
            OUTFILE= "C:\WASSICBZB\out\output_00_11.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="HUCFLOW"; 
RUN;
PROC EXPORT DATA= HUCCARBON 
            OUTFILE= "C:\WASSICBZB\out\output_00_11.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="HUCCARBON"; 
RUN;
PROC EXPORT DATA= MONTHCARBON 
            OUTFILE= "C:\WASSICBZB\out\output_00_11.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="MONTHCARBON"; 
RUN;
PROC EXPORT DATA= MONTHFLOW 
            OUTFILE= "C:\WASSICBZB\out\output_00_11.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="MONTHFLOW"; 
RUN;
