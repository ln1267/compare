
/**???????
	?????"200"???????“Outputs_”?“_n”????;
		eg:Outputs_3??????"200"???“200”
	1 ?????:“libname W”
	2 ??????:libname W "E:\zagunao\OUTPUTS\Outputs";
	3 ???????XLXS??SHEET??RANGE="200$"; RANGE="200$"; 
	4 "if year < 2000 or year >2006 then delete;”
**/



/*----------------------------???????-----------------------------------------------*/
/*??????????*/
libname W "C:\WASSICBZB\Outputs";

/*??????????????*/
data W_1995.ANNUALBIO;/* ??ANNUALBIO(????????)*/
	infile "C:\WASSICBZB\Outputs\ANNUALBIO.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR TREE MAMMALS BIRD AMPHIB REPTILES VERTEB AET PET;
run;
data W_1995.ANNUALCARBON;/* ??ANNUALCARBON(??????)*/
	infile "C:\WASSICBZB\Outputs\ANNUALCARBON.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR GEP Reco NEE AET PET;
run;
data W_1995.ANNUALFLOW;/* ??ANNUALFLOW(??????)*/
	infile "C:\WASSICBZB\Outputs\ANNUALFLOW.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR RAIN PET AET Sun_ET RUNOFF RUN_Pratio ET_Pratio RUN_ETRatio SNWPCKMON RFACTOR;
run;
data W_1995.FLOWVOLBYLANDUSE;/* ??FLOWVOLBYLANDUSE(??????????(??,??,??,??,???)????)*/
	infile "C:\WASSICBZB\Outputs\FLOWVOLBYLANDUSE.TXT"  dlm=',' dsd missover firstobs=2;;
	input WATERSHEDID YEAR CROPFLOW FORESTFLOW GRASSFLOW SHRUBSAVAFLOW URBANWATERFLOW TFLOW;
run;
data W_1995.HUCBIO;/* ??HUCBIO(?????????)*/
	infile "C:\WASSICBZB\Outputs\HUCBIO.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL NO_YR TREE MAMMALS BIRD AMPHIB REPTILES AHUCVERTEB;
run;
data W_1995.HUCCARBON;/* ??HUCCARBON(???????)*/
	infile "C:\WASSICBZB\Outputs\HUCCARBON.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL NO_YR GEP Reco NEE;
run;
data W_1995.HUCFLOW;/* ??HUCFLOW(???????)*/
	infile "C:\WASSICBZB\Outputs\HUCFLOW.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL RAIN PET AET RUNOFF RUNOFF_P ET_P RUN_ET_PRFACTOR;
run;
data W_1995.MONTHCARBON;/* ??MONTHCARBON(??????)*/
	infile "C:\WASSICBZB\Outputs\MONTHCARBON.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR MONTH GEP Reco NEE;
run;
data W_1995.MONTHFLOW;/* ??MONTHFLOW(??????)*/
	infile "C:\WASSICBZB\Outputs\MONTHFLOW.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR MONTH PRECIP TEMP SMC SNWPK PET AET Sun_ET RUNOFF BASEFLOW FLOWMCMMon;
run;
data W_1995.RUNOFFBYLANDUSE;/* ??RUNOFFBYLANDUSE(????????????)*/
	infile "C:\WASSICBZB\Outputs\RUNOFFBYLANDUSE.TXT"  dlm=',' dsd missover firstobs=2;;
	input WATERSHEDID YEAR LADUSEID HUCRUNOFF FLOWVOL LANDratio HUCAREA;
run;
/*??????????*/


/*??????*/
/*Monthly validation database*/
/*???MODIS?GEP????*/
PROC IMPORT OUT= W_1995.MODGEP 
            DATAFILE= "D:\Minjiang\valid\MODGEP.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="sheet1$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
	/*Zhang ET database*/
PROC IMPORT OUT= W_1995.ZHANGET 
            DATAFILE= "D:\Minjiang\valid\zhet.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="sheet1$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
	/*MODIS ET database*/
PROC IMPORT OUT= W_1995.MODET
            DATAFILE= "D:\Minjiang\valid\MODET.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="sheet1$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;

/**
PROC IMPORT OUT= W_1995.MODPET
            DATAFILE= "E:\zagunao\VALID\MODPET.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="sheet1$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
PROC IMPORT OUT= W_1995.FLOW_V
            DATAFILE= "E:\zagunao\VALID\FLOW_O.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="sheet2$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
PROC IMPORT OUT= W_1995.BASEFLOW_V
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

/**
PROC IMPORT OUT=W_1995.MODGPP_Y 
            DATAFILE= "E:\zagunao\VALID\MODGPP_Y.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="sheet1$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
PROC IMPORT OUT=W_1995.MODET_Y 
            DATAFILE= "E:\zagunao\VALID\MODET_Y.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="sheet1$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
PROC IMPORT OUT=W_1995.ZHANGET_Y 
            DATAFILE= "E:\zagunao\VALID\ZHET_Y.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="sheet1$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;

**/

/*????????*/

/*----------------------------???????-----------------------------------------------*/


/*----------------------------??????-----------------------------------------------*/
libname W_1995 "C:\WASSICBZB\Outputs";


/*???????*/

	/*???GEP??*/
DATA W_1995.GEP_VERIFY;/*????????????*/
	delete;
RUN;
DATA GEP_VERIFY; /*?Monthcarbon???????2000-20011??GEP??*/
	set W_1995.Monthcarbon(drop=reco);
	if year < 2000 or year >2011 then delete;
RUN;
proc sort data=work.GEP_VERIFY out=W_1995.GEP_VERIFY;/*?????GEP??????*/
     by cell YEAR MONTH;
run; 
proc sort data=W_1995.MODGEP out=W_1995.MODGEP;/*?????GEP??????*/
     by HUC_8 YEAR MONTH;
run; 
DATA W_1995.Gep_val;
	set W_1995.MODGEP;
	if year < 2000 or year >2011 then delete;
run;
DATA W_1995.GEP_VERIFY;/* ??????????????*/
	MERGE W_1995.GEP_VERIFY W_1995.Gep_val (keep=MODIS_GEP);
RUN;

DATA W_1995.GEP_VERIFY_G;/*???????????*/
	set W_1995.GEP_VERIFY;
	if month<4 or month > 10 then delete ;
RUN;

	/*??ET??*/
DATA W_1995.ET_VERIFY;/*????????????*/
	delete;
RUN;
DATA ET_VERIFY; /*?Monthcarbon???????2000-20011??ET??*/
	set W_1995.Monthflow(keep=CELL YEAR MONTH PET AET Sun_ET);
	if year < 2000 or year >2011 then delete;
RUN;
proc sort data=work.ET_VERIFY out=W_1995.ET_VERIFY;/*?????ET??????*/
     by cell YEAR MONTH;
run; 

		/*??MODIS_ET??*/
DATA W_1995.ET_val;
	set W_1995.MODET;
	if year < 2000 or year >2011 then delete;
run;
proc sort data=W_1995.ET_Val out=W_1995.ET_Val;/*?????ET??????*/
     by HUC_8 YEAR MONTH;
run; 
DATA W_1995.ET_VERIFY;/* ??????????????*/
	MERGE W_1995.ET_VERIFY W_1995.ET_val (keep=MODIS_ET);
RUN;

DATA W_1995.ET_VERIFY_G;/*???????????*/
	set W_1995.ET_VERIFY;
	if month<4 or month > 10 then delete ;
RUN;

		/*??MODIS_PET??*/
DATA W_1995.ET_val;
	set W_1995.MODPET;
	if year < 2000 or year >2006 then delete;
run;
proc sort data=W_1995.ET_Val out=W_1995.ET_Val;/*?????ET??????*/
     by HUC_8 YEAR MONTH;
run; 
DATA W_1995.ET_VERIFY;/* ??????????????*/
	MERGE W_1995.ET_VERIFY W_1995.ET_val (keep=MODIS_PET);
RUN;
DATA W_1995.ET_VERIFY_G;/*???????????*/
	set W_1995.ET_VERIFY;
	if month<4 or month > 10 then delete ;
RUN;

		/*??Zhang ET??*/
DATA W_1995.ET_val;
	set W_1995.ZHANGET;
	if year < 2000 or year >2006 then delete;
run;
proc sort data=W_1995.ET_Val out=W_1995.ET_Val;/*?????ET??????*/
     by HUC_8 YEAR MONTH;
run; 

DATA W_1995.ET_VERIFY;/* ??????????????*/
	MERGE W_1995.ET_VERIFY W_1995.ET_val (keep=ZHANG_ET);
RUN;

DATA W_1995.ET_VERIFY_G;/*???????????*/
	set W_1995.ET_VERIFY;
	if month<4 or month > 10 then delete ;
RUN;
	/*???????*/

DATA W_1995.MONTH_VERIFY;/* ??????????????*/
	MERGE W_1995.ET_VERIFY W_1995.GEP_VERIFY;
RUN;


/*???????*/
	/*???GEP??*/
DATA W_1995.GEP_VERIFY_Y;/*????????????*/
	delete;
RUN;
DATA GEP_VERIFY_Y; /*?Monthcarbon???????2000-20011??GEP??*/
	set W_1995.Annualcarbon;
	if year < 2000 or year >2006 then delete;
RUN;
proc sort data=work.GEP_VERIFY_Y out=W_1995.GEP_VERIFY_Y;/*?????GEP??????*/
     by cell YEAR ;
run; 
DATA W_1995.Gep_val_Y;
	set W_1995.MODGPP_Y;
	if year < 2000 or year >2006 then delete;
run;
proc sort data=W_1995.Gep_val_Y out=W_1995.Gep_val_Y;/*?????GEP??????*/
     by HUC_8 YEAR ;
run; 

DATA W_1995.GEP_VERIFY_Y;/* ??????????????*/
	MERGE W_1995.GEP_VERIFY_Y W_1995.Gep_val_Y(keep=MODGPP_Y) ;
RUN;

	/*???ET??*/
DATA W_1995.ET_VERIFY_Y;/*????????????*/
	delete;
RUN;
DATA ET_VERIFY_Y; /*?Monthcarbon???????2000-20011??ET??*/
	set W_1995.Annualflow(keep=CELL YEAR RAIN PET AET Sun_ET RUNOFF);
	if year < 2000 or year >2006 then delete;
RUN;
proc sort data=work.ET_VERIFY_Y out=W_1995.ET_VERIFY_Y;/*?????ET??????*/
     by cell YEAR ;
run; 

		/*??MODIS_ET??*/
DATA W_1995.ET_val_Y;
	set W_1995.MODET_Y;
	if year < 2000 or year >2006 then delete;
run;
proc sort data=W_1995.ET_Val_Y out=W_1995.ET_Val_Y;/*?????ET??????*/
     by HUC_8 YEAR ;
run; 
DATA W_1995.ET_VERIFY_Y;/* ??????????????*/
	MERGE W_1995.ET_VERIFY_Y W_1995.ET_val_Y (keep=MODET_Y);
RUN;

		/*??Zhang ET??*/
DATA W_1995.ET_val_Y;
	set W_1995.ZHANGET_Y;
	if year < 2000 or year >2006 then delete;
run;
proc sort data=W_1995.ET_Val_Y out=W_1995.ET_Val_Y;/*?????ET??????*/
     by HUC_8 YEAR MONTH;
run; 

DATA W_1995.ET_VERIFY_Y;/* ??????????????*/
	MERGE W_1995.ET_VERIFY_Y W_1995.ET_val_Y (keep=ZHANGET_Y);
RUN;
	/*???????*/

DATA W_1995.ANNUAL_VERIFY;/* ??????????????*/
	MERGE W_1995.ET_VERIFY_Y W_1995.GEP_VERIFY_Y;
RUN;

/*??????????????*/
PROC IMPORT OUT= W_1995.basin 
            DATAFILE= "D:\Minjiang\basin.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;
data W_1995.basin;
	set W_1995.basin(keep= HUC_8  shape_area);
	ratio=shape_area/22317364709.1043;
	cell=HUC_8;
run;
proc sort data=W_1995.basin out=W_1995.basin;/*?????ET??????*/
     by cell;
run; 
data W_1995.month_VERIFY;
 	merge W_1995.month_VERIFY W_1995.basin;
	by cell;
	drop huc_8;
run;
data W_1995.ANNUAL_VERIFY;
 	merge W_1995.ANNUAL_VERIFY W_1995.basin;
	by cell;
	drop huc_8;
run;


/*????????*/
	/*?????*/
data W_1995.BASIN_month_VERIFY;
	set W_1995.month_VERIFY;
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

proc sort data=W_1995.BASIN_month_VERIFY out=BASIN_month_VERIFY;
	by year month ;
run;
proc means data=BASIN_month_VERIFY noprint; /*???????????*/                                                                                             
        by year month ;                                                                                                                   
        output out=W_1995.BASIN_month_VERIFY(drop=_type_ _freq_) sum=;                                                                                    
run;  

	/*?????*/
data W_1995.BASIN_annual_VERIFY;
	set W_1995.annual_VERIFY;
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

proc sort data=W_1995.BASIN_annual_VERIFY out=BASIN_annual_VERIFY;
	by year  ;
run;
proc means data=BASIN_annual_VERIFY noprint; /*???????????*/                                                                                             
        by year;                                                                                                                   
        output out=W_1995.BASIN_annual_VERIFY(drop=_type_ _freq_) sum=;                                                                                    
run;  



/*----------------------------GEP?ET?????????--------------------------*/


ods printer pdf file ='C:\WASSICBZB\Validation.pdf';/*??????????*/
/*---------------------------------??????------------------------------*/

/*??cell?????*/
proc sort data=W_1995.Month_verify out=Month_verify;
	by cell year ;
run;
proc sort data=W_1995.Annual_verify out=Annual_verify;
	by cell year;
run;
	/*GEP????*/
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
	/*ET????*/
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


/*??BASIN?????*/
proc sort data=W_1995.BASIN_Month_verify out=BASIN_Month_verify;
	by cell year ;
run;
proc sort data=W_1995.BASIN_Annual_verify out=BASIN_Annual_verify;
	by cell year;
run;
	/*GEP????*/
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
	/*ET????*/
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

/*???????????*/

PROC EXPORT DATA= W_1995.Month_verify 
            OUTFILE= "C:\WASSICBZB\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="MONTHLY"; 
RUN;
PROC EXPORT DATA= W_1995.Annual_verify 
            OUTFILE= "C:\WASSICBZB\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="ANNUAL"; 
RUN;

PROC EXPORT DATA= W_1995.BASIN_Month_verify 
            OUTFILE= "C:\WASSICBZB\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="BASIN_MONTHLY"; 
RUN;
PROC EXPORT DATA= W_1995.BASIN_Annual_verify 
            OUTFILE= "C:\WASSICBZB\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="BASIN_ANNUAL"; 
RUN;
/*------------------------????---------------------------*/




/*------------------------????FLOW??---------------------------*/
proc sort data=W_1995.monthflow out=run_v ;
	by cell;
run;
data run_v;
 	merge run_v W_1995.basin;
	by cell;
run;

data W_1995.run_v;
	set run_v;
	B_SNWPK=SNWPK*ratio;
	B_SMC=SMC*ratio;
	B_RUNOFF=RUNOFF*ratio;
	B_BASEFLOW=BASEFLOW*ratio;
	drop PRECIP TEMP SMC SNWPK PET AET Sun_ET RUNOFF BASEFLOW FLOWMCMMon HUC_8 shape_area ratio ;
run;
proc sort data=W_1995.run_v out=run_v ;
	by year month;
run;
proc sort data=W_1995.monthflow out=flow_v ;
	by year month;
run;
proc means data=run_v noprint; /*???????????*/                                                                                             
        by year month ;                                                                                                                   
        output out=out_run(drop=_type_ _freq_) sum=;                                                                                    
run;  
proc means data=flow_v noprint; /*???????????*/                                                                                             
        by year month ;                                                                                                                   
        output out=out_flow(drop=_type_ _freq_) sum=;                                                                                    
run;  

DATA W_1995.FLOW_verify;
	set out_flow(keep=YEAR MONTH FLOWMCMMon);
	FLOW=FLOWMCMMon;
	RUNOFF=FLOWMCMMon*1000/2904.38;
	if year < 2000 or year >2006 then delete;
	
	drop FLOWMCMMon;
run;
DATA W_1995.run;
	set out_run;
	if year < 2000 or year >2006 then delete;
run;
proc sort data=W_1995.FLOW_verify out=W_1995.FLOW_verify;/*?????GEP??????*/
     by  YEAR MONTH ;
run; 
DATA W_1995.FLOW_v;
	set W_1995.flow_V;
	if year < 2000 or year >2006 then delete;
run;
DATA W_1995.BASEFLOW_v;
	set W_1995.baseflow_V;
	if year < 2000 or year >2006 then delete;
run;
DATA W_1995.FLOW_verify;/* ??????????????*/
	MERGE W_1995.FLOW_verify W_1995.flow_V W_1995.BASEFLOW_V W_1995.run;
	drop cell;
RUN;
     

proc reg data=W_1995.FLOW_verify;
	model RUNOFF=RUNOFF_O;
	plot RUNOFF*RUNOFF_O;
	title "A monthly comparison of RUNOFF between WASSI-C and Observed of the whole watershed";
run;
quit; 
proc reg data=W_1995.FLOW_verify;
	model B_baseflow=baseflow_O;
	plot B_baseflow*baseflow_O;
	title "A monthly comparison of BASEFLOW between WASSI-C and Observed of the whole watershed";
run;
quit; 
/*
proc reg data=flow_v_g;
	model FLOWMCMMon=run_V;
	plot FLOWMCMMon*run_V;
	title "FLOW???????";
run;
quit; 
*/
	/*??????FLOW???*/
PROC EXPORT DATA= W_1995.FLOW_verify 
            OUTFILE= "C:\WASSICBZB\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="FLOW"; 
RUN;

/*------------------------FLOW????---------------------------*/

ods printer close;
