
/**����ĵ�������
	һ��ֻ�轫"200"�滻Ϊ�ļ����С�Outputs_���͡�_n��֮���ֵ��
		eg��Outputs_3���ļ�����"200"�滻Ϊ��200��
	1 �߼������ƣ���libname W��
	2 �߼����Ŀ¼��libname W "E:\zagunao\OUTPUTS\Outputs";
	3 ������֤������XLXS�е�SHEET����RANGE="200$"; RANGE="200$"; 
	4 "if year < 2000 or year >2006 then delete;��
**/



/*----------------------------����Ԥ����ʼ-----------------------------------------------*/
/*Ϊÿ����������߼���*/
libname W "E:\zagunao\OUTPUTS\Outputs";

/*���߼����ж�������Ľ���ļ�*/
data W.ANNUALBIO;/* ����ANNUALBIO�����������ַḻ�ȣ�*/
	infile "E:\zagunao\OUTPUTS\Outputs\ANNUALBIO.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR TREE MAMMALS BIRD AMPHIB REPTILES VERTEB AET PET;
run;
data W.ANNUALCARBON;/* ����ANNUALCARBON��������̼ͨ����*/
	infile "E:\zagunao\OUTPUTS\Outputs\ANNUALCARBON.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR GEP Reco NEE AET PET;
run;
data W.ANNUALFLOW;/* ����ANNUALFLOW��������ˮͨ����*/
	infile "E:\zagunao\OUTPUTS\Outputs\ANNUALFLOW.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR RAIN PET AET Sun_ET RUNOFF RUN_Pratio ET_Pratio RUN_ETRatio SNWPCKMON RFACTOR;
run;
data W.FLOWVOLBYLANDUSE;/* ����FLOWVOLBYLANDUSE������������Ҫֲ�����ͣ�ũ�ɭ�֣��ݵأ���ԣ���ֲ�������꾶����*/
	infile "E:\zagunao\OUTPUTS\Outputs\FLOWVOLBYLANDUSE.TXT"  dlm=',' dsd missover firstobs=2;;
	input WATERSHEDID YEAR CROPFLOW FORESTFLOW GRASSFLOW SHRUBSAVAFLOW URBANWATERFLOW TFLOW;
run;
data W.HUCBIO;/* ����HUCBIO������������ַḻ�ȣ�*/
	infile "E:\zagunao\OUTPUTS\Outputs\HUCBIO.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL NO_YR TREE MAMMALS BIRD AMPHIB REPTILES AHUCVERTEB;
run;
data W.HUCCARBON;/* ����HUCCARBON���������̼ͨ����*/
	infile "E:\zagunao\OUTPUTS\Outputs\HUCCARBON.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL NO_YR GEP Reco NEE;
run;
data W.HUCFLOW;/* ����HUCFLOW���������ˮͨ����*/
	infile "E:\zagunao\OUTPUTS\Outputs\HUCFLOW.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL RAIN PET AET RUNOFF RUNOFF_P ET_P RUN_ET_PRFACTOR;
run;
data W.MONTHCARBON;/* ����MONTHCARBON��������̼ͨ����*/
	infile "E:\zagunao\OUTPUTS\Outputs\MONTHCARBON.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR MONTH GEP Reco NEE;
run;
data W.MONTHFLOW;/* ����MONTHFLOW��������ˮͨ����*/
	infile "E:\zagunao\OUTPUTS\Outputs\MONTHFLOW.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR MONTH PRECIP TEMP SMC SNWPK PET AET Sun_ET RUNOFF FLOWMCMMon;
run;
data W.RUNOFFBYLANDUSE;/* ����RUNOFFBYLANDUSE���������ֲ�����͵��꾶����*/
	infile "E:\zagunao\OUTPUTS\Outputs\RUNOFFBYLANDUSE.TXT"  dlm=',' dsd missover firstobs=2;;
	input WATERSHEDID YEAR LADUSEID HUCRUNOFF FLOWVOL LANDratio HUCAREA;
run;
/*ģ�����ļ��������*/


/*������֤����*/
/*Monthly validation database*/
/*������MODIS��GEP��֤����*/
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

/*��֤���ݶ������*/

/*----------------------------����Ԥ�������-----------------------------------------------*/


/*----------------------------���ݴ���ʼ-----------------------------------------------*/
libname W "E:\zagunao\OUTPUTS\Outputs";
ods printer pdf file ='E:\zagunao\OUTPUTS\Validation.pdf';/*Ϊÿ����������߼���*/

/*��������֤����*/

	/*������GEP����*/
DATA W.GEP_VERIFY;/*��������ݼ��д��ڵ�����*/
	delete;
RUN;
DATA GEP_VERIFY; /*��Monthcarbon���ݼ�����ȡ��2000-20011���GEP����*/
	set W.Monthcarbon(drop=reco);
	if year < 2000 or year >2006 then delete;
RUN;
proc sort data=work.GEP_VERIFY out=W.GEP_VERIFY;/*����ȡ����GEP���ݽ�������*/
     by cell YEAR MONTH;
run; 
proc sort data=W.MODGEP out=W.MODGEP;/*����֤�õ�GEP���ݽ�������*/
     by HUC_8 YEAR MONTH;
run; 
DATA W.Gep_val;
	set W.MODGEP;
	if year < 2000 or year >2006 then delete;
run;
DATA W.GEP_VERIFY;/* ����֤������ӵ�ģ�����ݺ���*/
	MERGE W.GEP_VERIFY W.Gep_val (keep=MODIS_GEP);
RUN;

DATA W.GEP_VERIFY_G;/*ɾ����֤���ݲ����ڵ�ֵ*/
	set W.GEP_VERIFY;
	if month<4 or month > 10 then delete ;
RUN;

	/*����ET����*/
DATA W.ET_VERIFY;/*��������ݼ��д��ڵ�����*/
	delete;
RUN;
DATA ET_VERIFY; /*��Monthcarbon���ݼ�����ȡ��2000-20011���ET����*/
	set W.Monthflow(keep=CELL YEAR MONTH PET AET Sun_ET);
	if year < 2000 or year >2006 then delete;
RUN;
proc sort data=work.ET_VERIFY out=W.ET_VERIFY;/*����ȡ����ET���ݽ�������*/
     by cell YEAR MONTH;
run; 

		/*����MODIS_ET����*/
DATA W.ET_val;
	set W.MODET;
	if year < 2000 or year >2006 then delete;
run;
proc sort data=W.ET_Val out=W.ET_Val;/*����֤�õ�ET���ݽ�������*/
     by HUC_8 YEAR MONTH;
run; 
DATA W.ET_VERIFY;/* ����֤������ӵ�ģ�����ݺ���*/
	MERGE W.ET_VERIFY W.ET_val (keep=MODIS_ET);
RUN;

DATA W.ET_VERIFY_G;/*ɾ����֤���ݲ����ڵ�ֵ*/
	set W.ET_VERIFY;
	if month<4 or month > 10 then delete ;
RUN;

		/*����MODIS_PET����*/
DATA W.ET_val;
	set W.MODPET;
	if year < 2000 or year >2006 then delete;
run;
proc sort data=W.ET_Val out=W.ET_Val;/*����֤�õ�ET���ݽ�������*/
     by HUC_8 YEAR MONTH;
run; 
DATA W.ET_VERIFY;/* ����֤������ӵ�ģ�����ݺ���*/
	MERGE W.ET_VERIFY W.ET_val (keep=MODIS_PET);
RUN;
DATA W.ET_VERIFY_G;/*ɾ����֤���ݲ����ڵ�ֵ*/
	set W.ET_VERIFY;
	if month<4 or month > 10 then delete ;
RUN;

		/*����Zhang ET����*/
DATA W.ET_val;
	set W.ZHANGET;
	if year < 2000 or year >2006 then delete;
run;
proc sort data=W.ET_Val out=W.ET_Val;/*����֤�õ�ET���ݽ�������*/
     by HUC_8 YEAR MONTH;
run; 

DATA W.ET_VERIFY;/* ����֤������ӵ�ģ�����ݺ���*/
	MERGE W.ET_VERIFY W.ET_val (keep=ZHANG_ET);
RUN;

DATA W.ET_VERIFY_G;/*ɾ����֤���ݲ����ڵ�ֵ*/
	set W.ET_VERIFY;
	if month<4 or month > 10 then delete ;
RUN;
	/*�ϲ�����֤����*/

DATA W.MONTH_VERIFY;/* ����֤������ӵ�ģ�����ݺ���*/
	MERGE W.ET_VERIFY W.GEP_VERIFY;
RUN;


/*��������֤����*/
	/*������GEP����*/
DATA W.GEP_VERIFY_Y;/*��������ݼ��д��ڵ�����*/
	delete;
RUN;
DATA GEP_VERIFY_Y; /*��Monthcarbon���ݼ�����ȡ��2000-20011���GEP����*/
	set W.Annualcarbon;
	if year < 2000 or year >2006 then delete;
RUN;
proc sort data=work.GEP_VERIFY_Y out=W.GEP_VERIFY_Y;/*����ȡ����GEP���ݽ�������*/
     by cell YEAR ;
run; 
DATA W.Gep_val_Y;
	set W.MODGPP_Y;
	if year < 2000 or year >2006 then delete;
run;
proc sort data=W.Gep_val_Y out=W.Gep_val_Y;/*����֤�õ�GEP���ݽ�������*/
     by HUC_8 YEAR ;
run; 

DATA W.GEP_VERIFY_Y;/* ����֤������ӵ�ģ�����ݺ���*/
	MERGE W.GEP_VERIFY_Y W.Gep_val_Y(keep=MODGPP_Y) ;
RUN;

	/*������ET����*/
DATA W.ET_VERIFY_Y;/*��������ݼ��д��ڵ�����*/
	delete;
RUN;
DATA ET_VERIFY_Y; /*��Monthcarbon���ݼ�����ȡ��2000-20011���ET����*/
	set W.Annualflow(keep=CELL YEAR RAIN PET AET Sun_ET RUNOFF);
	if year < 2000 or year >2006 then delete;
RUN;
proc sort data=work.ET_VERIFY_Y out=W.ET_VERIFY_Y;/*����ȡ����ET���ݽ�������*/
     by cell YEAR ;
run; 

		/*����MODIS_ET����*/
DATA W.ET_val_Y;
	set W.MODET_Y;
	if year < 2000 or year >2006 then delete;
run;
proc sort data=W.ET_Val_Y out=W.ET_Val_Y;/*����֤�õ�ET���ݽ�������*/
     by HUC_8 YEAR ;
run; 
DATA W.ET_VERIFY_Y;/* ����֤������ӵ�ģ�����ݺ���*/
	MERGE W.ET_VERIFY_Y W.ET_val_Y (keep=MODET_Y);
RUN;

		/*����Zhang ET����*/
DATA W.ET_val_Y;
	set W.ZHANGET_Y;
	if year < 2000 or year >2006 then delete;
run;
proc sort data=W.ET_Val_Y out=W.ET_Val_Y;/*����֤�õ�ET���ݽ�������*/
     by HUC_8 YEAR MONTH;
run; 

DATA W.ET_VERIFY_Y;/* ����֤������ӵ�ģ�����ݺ���*/
	MERGE W.ET_VERIFY_Y W.ET_val_Y (keep=ZHANGET_Y);
RUN;
	/*�ϲ�����֤����*/

DATA W.ANNUAL_VERIFY;/* ����֤������ӵ�ģ�����ݺ���*/
	MERGE W.ET_VERIFY_Y W.GEP_VERIFY_Y;
RUN;

/*����֤������������������ֶ�*/
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
proc sort data=W.basin out=W.basin;/*����֤�õ�ET���ݽ�������*/
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


/*�����ܽ���Ĵ���*/
	/*��������*/
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
proc means data=BASIN_month_VERIFY noprint; /*�ֱ�����ÿ������ľ�ֵ*/                                                                                             
        by year month ;                                                                                                                   
        output out=W.BASIN_month_VERIFY(drop=_type_ _freq_) sum=;                                                                                    
run;  

	/*��������*/
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
proc means data=BASIN_annual_VERIFY noprint; /*�ֱ�����ÿ������ľ�ֵ*/                                                                                             
        by year;                                                                                                                   
        output out=W.BASIN_annual_VERIFY(drop=_type_ _freq_) sum=;                                                                                    
run;  



/*----------------------------GEP��ET����֤���ݴ������--------------------------*/



/*---------------------------------��ʾ��֤���------------------------------*/

/*��ʾcell����֤���*/
proc sort data=W.Month_verify out=Month_verify;
	by cell year ;
run;
proc sort data=W.Annual_verify out=Annual_verify;
	by cell year;
run;
	/*GEP������ʾ*/
proc reg data=Month_verify;
	model GEP=MODIS_GEP;
	plot GEP*MODIS_GEP;
	title "GEP����֤���";
run;
quit; 

proc reg data=Annual_verify;
	model GEP=MODGPP_Y;
	plot GEP*MODGPP_Y;
	title "GEP����֤���";

run;
quit; 
	/*ET������ʾ*/
proc reg data=Month_verify;
	model AET=MODIS_ET;
	plot AET*MODIS_ET;
	title "MODIS_ET����֤���";
run;
quit; 
proc reg data=Month_verify;
	model AET=ZHANG_ET;
	plot AET*ZHANG_ET;
	title "ZHANG_ET����֤���";
run;
quit; 
proc reg data=Annual_verify;
	model AET=MODET_Y;
	plot AET*MODET_Y;
	title "MODIS_ET����֤���";
run;
quit; 
proc reg data=Annual_verify;
	model AET=ZHANGET_Y;
	plot AET*ZHANGET_Y;
	title "ZHANG_ET����֤���";
run;
quit; 

/*��ʾBASIN����֤���*/
proc sort data=W.BASIN_Month_verify out=BASIN_Month_verify;
	by cell year ;
run;
proc sort data=W.BASIN_Annual_verify out=BASIN_Annual_verify;
	by cell year;
run;
	/*GEP������ʾ*/
proc reg data=BASIN_Month_verify;
	model B_GEP=B_MODGEP;
	plot B_GEP*B_MODGEP;
	title "BASIN_GEP����֤���";
run;
quit; 

proc reg data=BASIN_Annual_verify;
	model B_GEP=B_MODGPP;
	plot B_GEP*B_MODGPP;
	title "BASIN_GEP����֤���";

run;
quit; 
	/*ET������ʾ*/
proc reg data=BASIN_Month_verify;
	model B_AET=B_MODET;
	plot B_AET*B_MODET;
	title "BASIN_MODIS_ET����֤���";
run;
quit; 
proc reg data=BASIN_Month_verify;
	model B_AET=B_ZHANGET;
	plot B_AET*B_ZHANGET;
	title "BASIN_ZHANG_ET����֤���";
run;
quit; 
proc reg data=BASIN_Annual_verify;
	model B_AET=B_MODET;
	plot B_AET*B_MODET;
	title "BASIN_MODIS_ET����֤���";
run;
quit; 
proc reg data=BASIN_Annual_verify;
	model B_AET=B_ZHANGET;
	plot B_AET*B_ZHANGET;
	title "BASIN_ZHANG_ET����֤���";
run;
quit; 

/*��������õ���֤���ݼ�*/

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
/*------------------------�������---------------------------*/




/*------------------------��ʼ����FLOW����---------------------------*/
proc sort data=W.monthflow out=flow_v ;
	by year month;
run;

proc means data=flow_v noprint; /*�ֱ�����ÿ������ľ�ֵ*/                                                                                             
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
proc sort data=W.FLOW_verify out=W.FLOW_verify;/*����֤�õ�GEP���ݽ�������*/
     by  YEAR MONTH ;
run; 
DATA W.FLOW_v;
	set W.flow_V;
	if year < 2000 or year >2006 then delete;
run;

DATA W.FLOW_verify;/* ����֤������ӵ�ģ�����ݺ���*/
	MERGE W.FLOW_verify W.flow_V;
RUN;
     
proc reg data=W.FLOW_verify;
	model FLOW=FLOW_O;
	plot FLOW*FLOW_O;
	title "FLOW��֤���";
run;
quit; 
proc reg data=W.FLOW_verify;
	model RUNOFF=RUNOFF_O;
	plot RUNOFF*RUNOFF_O;
	title "RUNOFF��֤���";
run;
quit; 
/*
proc reg data=flow_v_g;
	model FLOWMCMMon=run_V;
	plot FLOWMCMMon*run_V;
	title "FLOW��������֤���";
run;
quit; 
*/
	/*��������õ�FLOW���ݼ�*/
PROC EXPORT DATA= W.FLOW_verify 
            OUTFILE= "E:\zagunao\OUTPUTS\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="FLOW"; 
RUN;

/*------------------------FLOW�������---------------------------*/

ods printer close;
