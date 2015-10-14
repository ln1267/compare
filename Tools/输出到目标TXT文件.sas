/*
ͨ����ȡ�����������ļ�

*/
data v1 v2 v3 cellinfo s s1 cellinfo1 hucarea ;
delete;
run;

/*��ȡ���������ļ��������TXT------�ֱ�ͬ���ļ�*/
PROC IMPORT OUT= WORK.S 
            DATAFILE= "E:\zagunao\soilinfo.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=YES;
RUN;
data s1;
	set S(keep=HUC);
	ID=HUC;
	Watershed=HUC;
	drop HUC;
run;
data S;
	set S1;
	set s;
	drop OID_ OID1 DELETE_FLG HUC;
run;
PROC EXPORT DATA= WORK.S 
            OUTFILE= "E:\zagunao\INPUTS\Inputs\soilinfo.txt" 
            dbms=csv REPLACE;
     PUTNAMES=YES;
RUN;
/*���������������*/



/**  -------����������ͬһ��EXLCE�ļ�ʱ��ȡ��ʽ
��ȡSOIL�ļ��������TXT��ʽ
PROC IMPORT OUT= WORK.v1 
            DATAFILE= "E:\HUC\soil_union.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="'$'"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
PROC EXPORT DATA= WORK.V1 
            OUTFILE= "E:\zagunao\INPUTS\Inputs\soilinfo.txt" 
            dbms=csv REPLACE;
     PUTNAMES=YES;
RUN;

/*��ȡLAI�ļ��������TXT��ʽ*/
PROC IMPORT OUT= WORK.v2 
            DATAFILE= "E:\zagunao\INPUTS\lai.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="sheet3$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
data v2_3;
	SET v2;
	if year > 2008  then delete;
run;
PROC EXPORT DATA= WORK.V2_3 
            OUTFILE= "E:\zagunao\INPUTS\Inputs\landlai.txt" 
            dbms=csv REPLACE;
     PUTNAMES=YES;
RUN;

/*��ȡCLIMATE�ļ��������TXT��ʽ*/
PROC IMPORT OUT= WORK.v3 
            DATAFILE= "E:\zagunao\INPUTS\climate-2km.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="Sheet1$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
data v3_3;
	set v3;
	keep BasinID YEAR MONTH Ppt_mm Tavg_C;
	if year >2008  then delete;
run;
 
PROC EXPORT DATA= WORK.V3_3 
            OUTFILE= "E:\zagunao\INPUTS\Inputs\climate.txt" 
            dbms=csv REPLACE;
     PUTNAMES=YES;
RUN;


/*��ȡ����basin�ļ���VEG_RATIO�ļ������CELLINFO��HUCAREA��TXT��ʽ*/
PROC IMPORT OUT= WORK.basin_ 
            DATAFILE= "E:\zagunao\basin_pro.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;
PROC IMPORT OUT= WORK.Vatio_ 
            DATAFILE= "E:\zagunao\INPUTS\VEG_ratio.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="sheet2$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
proc sort data=work.basin_  out=basin_ ;/*����ȡ����GEP���ݽ�������*/
     by HUC_8;
run; 
data cellinfo1;
set basin_;
	ID=HUC_8;
	BasinID=huc_8;
	lat_=lat;
	long_=long;
	area=shape_area;
	drop shape_area liuyu HUC_8 lat long;
run;
data cellinfo;
	merge cellinfo1;
	lat=lat_;
	long=long_;
	merge vatio_;
run;
data cellinfo;
	set cellinfo;
	keep ID BasinID lat long VEG_1 VEG_2 VEG_3 VEG_4 VEG_5 VEG_6 VEG_7 VEG_8 VEG_9 VEG_10 VEG_11 VEG_12;
run;
proc sort data=work.cellinfo  out=cellinfo ;/*����ȡ����GEP���ݽ�������*/
     by ID;
run; 
PROC EXPORT DATA= cellinfo 
            OUTFILE= "E:\zagunao\INPUTS\Inputs\cellinfo.txt" 
            dbms=csv REPLACE;
     PUTNAMES=YES;
RUN;
/*�����������ļ�*/
data HUCAREA;
	set cellinfo1;
	WatershedID=ID;
	Area_m2=area;
	drop basinID lat_ long_ area;
run;
data HUCAREA;
set HUCAREA(keep=ID WatershedID Area_m2);
run;

PROC EXPORT DATA= HUCAREA 
            OUTFILE= "E:\zagunao\INPUTS\Inputs\hucarea.txt" 
            dbms=csv REPLACE;
     PUTNAMES=YES;
RUN;



