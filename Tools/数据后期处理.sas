
/**����ĵ�������
	һ��ֻ�轫"200"�滻Ϊ�ļ����С�Outputs_���͡�_n��֮���ֵ��
		eg��Outputs_250_3���ļ�����"200"�滻Ϊ��200��
	1 �߼������ƣ���libname W_250��
	2 �߼����Ŀ¼��libname W_250 "E:\HUC\Outputs_union\Outputs_250";
	3 ������֤������XLXS�е�SHEET����RANGE="200$"; RANGE="200$"; 
**/



/*----------------------------����Ԥ����ʼ-----------------------------------------------*/
/*Ϊÿ����������߼���*/
libname W_250 "E:\HUC\Outputs_union\Outputs_250";

/*���߼����ж�������Ľ���ļ�*/
data W_250.ANNUALBIO;/* ����ANNUALBIO�����������ַḻ�ȣ�*/
	infile "E:\HUC\Outputs_union\Outputs_250\ANNUALBIO.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR TREE MAMMALS BIRD AMPHIB REPTILES VERTEB AET PET;
run;
data W_250.ANNUALCARBON;/* ����ANNUALCARBON��������̼ͨ����*/
	infile "E:\HUC\Outputs_union\Outputs_250\ANNUALCARBON.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR GEP Reco NEE AET PET;
run;
data W_250.ANNUALFLOW;/* ����ANNUALFLOW��������ˮͨ����*/
	infile "E:\HUC\Outputs_union\Outputs_250\ANNUALFLOW.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR RAIN PET AET RUNOFF RUN_Pratio ET_Pratio RUN_ETRatio SNWPCKMON RFACTOR;
run;
data W_250.FLOWVOLBYLANDUSE;/* ����FLOWVOLBYLANDUSE������������Ҫֲ�����ͣ�ũ�ɭ�֣��ݵأ���ԣ���ֲ�������꾶����*/
	infile "E:\HUC\Outputs_union\Outputs_250\FLOWVOLBYLANDUSE.TXT"  dlm=',' dsd missover firstobs=2;;
	input WATERSHEDID YEAR CROPFLOW FORESTFLOW GRASSFLOW SHRUBSAVAFLOW URBANWATERFLOW TFLOW;
run;
data W_250.HUCBIO;/* ����HUCBIO������������ַḻ�ȣ�*/
	infile "E:\HUC\Outputs_union\Outputs_250\HUCBIO.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL NO_YR TREE MAMMALS BIRD AMPHIB REPTILES AHUCVERTEB;
run;
data W_250.HUCCARBON;/* ����HUCCARBON���������̼ͨ����*/
	infile "E:\HUC\Outputs_union\Outputs_250\HUCCARBON.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL NO_YR GEP Reco NEE;
run;
data W_250.HUCFLOW;/* ����HUCFLOW���������ˮͨ����*/
	infile "E:\HUC\Outputs_union\Outputs_250\HUCFLOW.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL RAIN PET AET RUNOFF RUNOFF_P ET_P RUN_ET_PRFACTOR;
run;
data W_250.MONTHCARBON;/* ����MONTHCARBON��������̼ͨ����*/
	infile "E:\HUC\Outputs_union\Outputs_250\MONTHCARBON.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR MONTH GEP Reco NEE;
run;
data W_250.MONTHFLOW;/* ����MONTHFLOW��������ˮͨ����*/
	infile "E:\HUC\Outputs_union\Outputs_250\MONTHFLOW.TXT"  dlm=',' dsd missover firstobs=2;;
	input CELL YEAR MONTH PRECIP TEMP SMC SNWPK PET AET RUNOFF FLOWMCMMon;
run;
data W_250.RUNOFFBYLANDUSE;/* ����RUNOFFBYLANDUSE���������ֲ�����͵��꾶����*/
	infile "E:\HUC\Outputs_union\Outputs_250\RUNOFFBYLANDUSE.TXT"  dlm=',' dsd missover firstobs=2;;
	input WATERSHEDID YEAR LADUSEID HUCRUNOFF FLOWVOL LANDratio HUCAREA;
run;
/*ģ�����ļ��������*/


/*������֤����*/
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
/*��֤���ݶ������*/

/*----------------------------����Ԥ�������-----------------------------------------------*/


/*----------------------------���ݴ���ʼ-----------------------------------------------*/

/*����GEP����*/
DATA W_250.GEP_VERIFY;/*��������ݼ��д��ڵ�����*/
	delete;
RUN;
DATA GEP_VERIFY; /*��Monthcarbon���ݼ�����ȡ��2000-20011���GEP����*/
	set W_250.Monthcarbon(drop=reco);
	*where (year < 20012 and year >= 2000);
RUN;
proc sort data=work.GEP_VERIFY out=W_250.GEP_VERIFY;/*����ȡ����GEP���ݽ�������*/
     by cell YEAR MONTH;
run; 
proc sort data=W_250.GEP_Val out=W_250.GEP_Val;/*����֤�õ�GEP���ݽ�������*/
     by HUC_8 YEAR MONTH;
run; 
DATA W_250.GEP_VERIFY;/* ����֤������ӵ�ģ�����ݺ���*/
	MERGE W_250.GEP_VERIFY W_250.Gep_val (keep=GEP_V);
	GEP_V=GEP_V*0.1;
RUN;

DATA W_250.GEP_VERIFY;/*ɾ����֤���ݲ����ڵ�ֵ*/
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


/*����ET����*/
DATA W_250.ET_VERIFY;/*��������ݼ��д��ڵ�����*/
	delete;
RUN;
DATA ET_VERIFY; /*��Monthcarbon���ݼ�����ȡ��2000-20011���ET����*/
	set W_250.Monthflow(keep=CELL YEAR MONTH PET AET);
	*where (year <=2006 and year >= 1983);
RUN;
proc sort data=work.ET_VERIFY out=W_250.ET_VERIFY;/*����ȡ����ET���ݽ�������*/
     by cell YEAR MONTH;
run; 
proc sort data=W_250.ET_Val out=W_250.ET_Val;/*����֤�õ�ET���ݽ�������*/
     by HUC_8 YEAR MONTH;
run; 
DATA W_250.ET_VERIFY;/* ����֤������ӵ�ģ�����ݺ���*/
	MERGE W_250.ET_VERIFY W_250.ET_val (keep=ET_V);
RUN;


DATA W_250.ET_VERIFY;/*ɾ����֤���ݲ����ڵ�ֵ*/
	set W_250.ET_VERIFY;
	if ET_V < 0 then delete ;
RUN;
data W_250.ET_VERIFY;
 	merge W_250.ET_VERIFY basin;
	by cell;
	drop huc_8;
run;
/*----------------------------���ݴ������-----------------------------------------------*/

/*----------------------------��������ʾ-----------------------------------------------*/
libname W_250 "E:\HUC\Outputs_union\Outputs_250";
ods printer pdf file ='E:\HUC\Outputs_union\Validation\Validation_250.pdf';/*Ϊÿ����������߼���*/
/*GEP���ɢ��ͼ*/

proc reg data=W_250.GEP_VERIFY;/*��֤GEP���ݵ�ɢ��ͼ����ع���*/
	model GEP=GEP_V;
	plot GEP*GEP_V;
	title "GEP_250��֤���";
run;
quit;
/**���������ݵ���֤���
proc reg data=W_250.GEP_V_A;
	model GEP=GEP_V;
	plot GEP*GEP_V;
	title "GEP_250������֤���";
run;
quit;
proc sort data=W_250.GEP_V_A out=GEP_V_A;
     by liuyu;
run; 
proc reg data=GEP_V_A;
	model GEP=GEP_V;
	plot GEP*GEP_V;
	title "GEP_250������֤���";
	by liuyu;
run;
quit;

**/
/*����������ֱ������֤����*/
proc sort data=W_250.GEP_VERIFY out=GEP_VERIFY;
     by liuyu;
run; 

proc reg data=GEP_VERIFY;
	model GEP=GEP_V;
	plot GEP*GEP_V;
	title "GEP_250������������֤���";
	by liuyu;
run;
quit;

/*������Ԫ��֤GEP���ݵ�ɢ��ͼ����ع���*/
/**
proc reg data=W_250.GEP_VERIFY;
	model GEP=GEP_V;
	plot GEP*GEP_V;
	title "GEP_250��������֤���";
	by cell;
run;
quit;
**/
/*������֤GEP���ݵ�ɢ��ͼ����ع���*/
proc sort data=W_250.GEP_VERIFY out=GEP_VERIFY;
	by month;
run;
proc reg data=GEP_VERIFY;
	model GEP=GEP_V;
	plot GEP*GEP_V;
	title "GEP_250������֤���";
	by month;
run;
quit;

/*�ֱ�����ÿ����������ֵ*/
data out_GEP ;
delete;
run;
proc means data=W_250.GEP_VERIFY noprint;   /*�ֱ�����ÿ������ľ�ֵ*/                                                                                           
        by cell year;                                                                                                                   
        output out=out_GEP(drop=_type_ _freq_) mean=;                                                                                    
run;        
proc reg data=out_GEP;/*�������֤GEP���ݵ�ɢ��ͼ����ع���*/
	model GEP=GEP_V;
	plot GEP*GEP_V;
	title "GEP_250�������ֵ��֤���";
run;
quit;

proc reg data=W_250.GpP_V_A;/*�������֤GEP���ݵ�ɢ��ͼ����ع���*/
	model GEP=GpP;
	plot GEP*GpP;
	title "GpP_250��������֤���";
run;
quit;

/*�ֱ�����ÿ������ľ�ֵ*/
data out_GEP1 ;
delete;
run;
proc means data=W_250.GEP_VERIFY noprint; /*�ֱ�����ÿ������ľ�ֵ*/                                                                                             
        by cell;                                                                                                                   
        output out=out_GEP(drop=_type_ _freq_) mean=;                                                                                    
run;     
proc reg data=out_GEP;/*�������֤GEP���ݵ�ɢ��ͼ����ع���*/
	model GEP=GEP_V;
	plot GEP*GEP_V;
	title "GEP_250�����ֵ��֤���";
	
run;
quit;



/*ET���ɢ��ͼ*/
proc reg data=W_250.ET_VERIFY;/*��֤ET���ݵ�ɢ��ͼ����ع���*/
	model PET=ET_V;
	plot PET*ET_V;
	title "ET_250��֤���";
run;
quit;

/*����������ֱ������֤����*/
proc sort data=W_250.ET_VERIFY out=ET_VERIFY;
     by liuyu;
run; 

proc reg data=ET_VERIFY;
	model PET=ET_V;
	plot PET*ET_V;
	title "ET_250������������֤���";
	by liuyu;
run;
quit;

/*��������֤ET���ݵ�ɢ��ͼ����ع���*/
/**
proc reg data=W_250.ET_VERIFY;
	model PET=ET_V;
	plot PET*ET_V;
	title "ET_250��������֤���";
	by cell;
run;
quit;
**/



/*�ֱ�����ÿ����������ֵ*/
data out_ET ;
delete;
run;
proc means data=W_250.ET_VERIFY noprint;   /*�ֱ�����ÿ������ľ�ֵ*/                                                                                           
        by cell year;                                                                                                                   
        output out=out_ET(drop=_type_ _freq_) mean=;                                                                                    
run;        
proc reg data=out_ET;/*�������֤ET���ݵ�ɢ��ͼ����ع���*/
	model PET=ET_V;
	plot PET*ET_V;
	title "ET_250�������ֵ��֤���";
run;
quit;
/*�ֱ�����ÿ������ľ�ֵ*/
data out_ET1 ;
delete;
run;
proc means data=W_250.ET_VERIFY noprint; /*�ֱ�����ÿ������ľ�ֵ*/                                                                                             
        by cell;                                                                                                                   
        output out=out_ET1(drop=_type_ _freq_) mean=;                                                                                    
run;     
proc reg data=out_ET1;/*�������֤ET���ݵ�ɢ��ͼ����ع���*/
	model PET=ET_V;
	plot PET*ET_V;
	title "ET_250�����ֵ��֤���";
run;
quit;
/*----------------------------��������ʾ����-----------------------------------------------*/
ods printer close;
