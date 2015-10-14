
/**����ĵ�������
	һ��ֻ�轫"200"�滻Ϊ�ļ����С�Outputs_���͡�_n��֮���ֵ��
		eg��Outputs_3���ļ�����"200"�滻Ϊ��200��
	1 �߼������ƣ���libname W��
	2 �߼����Ŀ¼��libname W "E:\chuanxi\OUTPUTS\Outputs";
	3 ������֤������XLXS�е�SHEET����RANGE="200$"; RANGE="200$"; 
	4 "if year < 2000 or year >2006 then delete;��
**/



/*----------------------------����Ԥ����ʼ-----------------------------------------------*/
/*Ϊÿ����������߼���*/
libname W "E:\chuanxi\OUTPUTS\Outputs";

/*���߼����ж�������Ľ���ļ�*/
data W.ANNUALBIO;/* ����ANNUALBIO�����������ַḻ�ȣ�*/
	infile "E:\chuanxi\OUTPUTS\Outputs\ANNUALBIO.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR TREE MAMMALS BIRD AMPHIB REPTILES VERTEB AET PET;
run;
data W.ANNUALCARBON;/* ����ANNUALCARBON��������̼ͨ����*/
	infile "E:\chuanxi\OUTPUTS\Outputs\ANNUALCARBON.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR GEP Reco NEE AET PET;
run;
data W.ANNUALFLOW;/* ����ANNUALFLOW��������ˮͨ����*/
	infile "E:\chuanxi\OUTPUTS\Outputs\ANNUALFLOW.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR RAIN PET AET Sun_ET RUNOFF RUN_Pratio ET_Pratio RUN_ETRatio SNWPCKMON RFACTOR;
run;
data W.FLOWVOLBYLANDUSE;/* ����FLOWVOLBYLANDUSE������������Ҫֲ�����ͣ�ũ�ɭ�֣��ݵأ���ԣ���ֲ�������꾶����*/
	infile "E:\chuanxi\OUTPUTS\Outputs\FLOWVOLBYLANDUSE.TXT"  dlm=',' dsd missover firstobs=2;;
	input WATERSHEDID YEAR CROPFLOW FORESTFLOW GRASSFLOW SHRUBSAVAFLOW URBANWATERFLOW TFLOW;
run;
data W.HUCBIO;/* ����HUCBIO������������ַḻ�ȣ�*/
	infile "E:\chuanxi\OUTPUTS\Outputs\HUCBIO.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL NO_YR TREE MAMMALS BIRD AMPHIB REPTILES AHUCVERTEB;
run;
data W.HUCCARBON;/* ����HUCCARBON���������̼ͨ����*/
	infile "E:\chuanxi\OUTPUTS\Outputs\HUCCARBON.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL NO_YR GEP Reco NEE;
run;
data W.HUCFLOW;/* ����HUCFLOW���������ˮͨ����*/
	infile "E:\chuanxi\OUTPUTS\Outputs\HUCFLOW.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL RAIN PET AET RUNOFF RUNOFF_P ET_P RUN_ET_PRFACTOR;
run;
data W.MONTHCARBON;/* ����MONTHCARBON��������̼ͨ����*/
	infile "E:\chuanxi\OUTPUTS\Outputs\MONTHCARBON.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR MONTH GEP Reco NEE;
run;
data W.MONTHFLOW;/* ����MONTHFLOW��������ˮͨ����*/
	infile "E:\chuanxi\OUTPUTS\Outputs\MONTHFLOW.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR MONTH PRECIP TEMP SMC SNWPK PET AET Sun_ET RUNOFF FLOWMCMMon;
run;
data W.RUNOFFBYLANDUSE;/* ����RUNOFFBYLANDUSE���������ֲ�����͵��꾶����*/
	infile "E:\chuanxi\OUTPUTS\Outputs\RUNOFFBYLANDUSE.TXT"  dlm=',' dsd missover firstobs=2;;
	input WATERSHEDID YEAR LADUSEID HUCRUNOFF FLOWVOL LANDratio HUCAREA;
run;
/*ģ�����ļ��������*/


/*������֤����*/
/*Monthly validation database*/
/*������MODIS��GEP��֤����*/
PROC IMPORT OUT= W.MODGEP 
            DATAFILE= "E:\chuanxi\VALID\MODGEP.xlsx" 
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
            DATAFILE= "E:\chuanxi\VALID\ZHANGET.xlsx" 
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
            DATAFILE= "E:\chuanxi\VALID\MODET.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="sheet1$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
PROC IMPORT OUT= W.MODPET
            DATAFILE= "E:\chuanxi\VALID\MODPET.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="sheet1$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
/*Annual validation database*/
PROC IMPORT OUT=W.MODGPP_Y 
            DATAFILE= "E:\chuanxi\VALID\MODGPP_Y.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="sheet1$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
PROC IMPORT OUT=W.MODET_Y 
            DATAFILE= "E:\chuanxi\VALID\MODET_Y.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="sheet1$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
PROC IMPORT OUT=W.ZHANGET_Y 
            DATAFILE= "E:\chuanxi\VALID\ZHET_Y.xlsx" 
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
libname W "E:\chuanxi\OUTPUTS\Outputs";
ods printer pdf file ='E:\chuanxi\OUTPUTS\Validation.pdf';/*Ϊÿ����������߼���*/

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
            DATAFILE= "E:\chuanxi\basin.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;
data basin;
	set W.basin(keep= HUC_8 liuyu shape_area);
	cell=HUC_8;
run;

data W.month_VERIFY;
 	merge W.month_VERIFY basin;
	by cell;
	drop huc_8;
run;
data W.ANNUAL_VERIFY;
 	merge W.ANNUAL_VERIFY basin;
	by cell;
	drop huc_8;
run;
/*----------------------------GEP��ET����֤���ݴ������--------------------------*/



/*---------------------------------��ʾ��֤���------------------------------*/

/*��ʾ����֤���*/
proc sort data=W.Month_verify out=Month_verify;
	by liuyu year ;
run;
proc sort data=W.Annual_verify out=Annual_verify;
	by liuyu year;
run;
proc reg data=Month_verify;
	model GEP=MODIS_GEP;
	plot GEP*MODIS_GEP;
	title "�����������GEP����֤���";
	by liuyu year;
run;
quit; 
proc reg data=Annual_verify;
	model GEP=MODGPP_Y;
	plot GEP*MODGPP_Y;
	title "�����������GEP����֤���";
	by liuyu year;
run;
quit; 

proc sort data=W.Month_verify out=Month_verify;
	by liuyu  ;
run;
proc sort data=W.Annual_verify out=Annual_verify;
	by liuyu ;
run;
proc reg data=Month_verify;
	model GEP=MODIS_GEP;
	plot GEP*MODIS_GEP;
	title "��������GEP����֤���";
	by liuyu;
run;
quit; 

proc reg data=Annual_verify;
	model GEP=MODGPP_Y;
	plot GEP*MODGPP_Y;
	title "��������GEP����֤���";
	by liuyu;
run;
quit; 
proc sort data=W.Month_verify out=Month_verify;
	by year  ;
run;
proc sort data=W.Annual_verify out=Annual_verify;
	by year ;
run;
proc reg data=Month_verify;
	model GEP=MODIS_GEP;
	plot GEP*MODIS_GEP;
	title "����GEP����֤���";
	by year;
run;
quit; 
proc reg data=Annual_verify;
	model GEP=MODGPP_Y;
	plot GEP*MODGPP_Y;
	title "����GEP����֤���";
	by year;
run;
quit; 


/*��������õ���֤���ݼ�*/

PROC EXPORT DATA= W.Month_verify 
            OUTFILE= "E:\chuanxi\OUTPUTS\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="MONTHLY"; 
RUN;
PROC EXPORT DATA= W.Annual_verify 
            OUTFILE= "E:\chuanxi\OUTPUTS\output.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="ANNUAL"; 
RUN;

/*------------------------�������---------------------------*/

ods printer close;


/*------------------------��ʼ����FLOW����---------------------------*/
proc sort data=W.monthflow out=flow_v;
	by year month;
run;

proc means data=flow_v noprint; /*�ֱ�����ÿ������ľ�ֵ*/                                                                                             
        by year month ;                                                                                                                   
        output out=out_flow(drop=_type_ _freq_) sum=;                                                                                    
run;  
data flow_v;
	merge out_flow val;
	keep year month FLOWMCMMon run_v;
	if month <1 then delete;
run;

data flow_v_g;
	set flow_v;
	if month<4 or month >10 then delete;
run;

proc reg data=flow_v;
	model FLOWMCMMon=run_V;
	plot FLOWMCMMon*run_V;
	title "FLOW��֤���";
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
PROC EXPORT DATA= WORK.flow_v 
            OUTFILE= "E:\chuanxi\OUTPUTS\excle\A.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="flow"; 
RUN;
PROC EXPORT DATA= WORK.flow_v_g 
            OUTFILE= "E:\chuanxi\OUTPUTS\excle\A.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="flow_g"; 
RUN;

/*------------------------FLOW�������---------------------------*/

ods printer close;
