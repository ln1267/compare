/*
通过读取制作的数据文件

*/
data v1 v2 v3 v4 cellinfo s s1 cellinfo1 hucarea ;
delete;
run;

/*读取土壤数据文件并输出到TXT------分别不同的文件*/
PROC IMPORT OUT= WORK.s 
            DATAFILE= "d:\zagunao\INPUTS\soilinfo.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="soilinfo10$"; /*其中带$符号表示sheet的名称*/
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
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
data work.S;
	set work.S;
	if ID <1201 or ID >2200 then delete;
run;
PROC EXPORT DATA= WORK.S 
            OUTFILE= "d:\zagunao\INPUTS\Inputs\soilinfo.txt" 
            dbms=csv REPLACE;
     PUTNAMES=YES;
RUN;
/*结束输出土壤数据*/



/**  -------土壤数据在同一个EXLCE文件时读取方式
读取SOIL文件并输出到TXT格式
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

/*读取LAI文件并输出到TXT格式*/
PROC IMPORT OUT= WORK.v1 
            DATAFILE= "d:\zagunao\INPUTS\lai.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="sheet1$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
PROC IMPORT OUT= WORK.v2 
            DATAFILE= "d:\zagunao\INPUTS\lai.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="sheet2$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
PROC IMPORT OUT= WORK.v3 
            DATAFILE= "d:\zagunao\INPUTS\lai.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="sheet3$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
PROC IMPORT OUT= WORK.v4 
            DATAFILE= "d:\zagunao\INPUTS\lai.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="sheet4$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
data LAI;
	set v1 v2 v3 v4;

	if cell <1201 or cell>2200 then delete;

run;
proc sort data=LAI   out=LAI;/*对提取出的GEP数据进行排序*/
     by cell year month;
run; 

PROC EXPORT DATA= LAI 
            OUTFILE= "d:\zagunao\INPUTS\Inputs\landlai.txt" 
            dbms=csv REPLACE;
     PUTNAMES=YES;
RUN;

/*读取CLIMATE文件并输出到TXT格式*/
PROC IMPORT OUT= WORK.v3 
            DATAFILE= "d:\zagunao\INPUTS\climate.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="Sheet1$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;

data work.v3;
	set work.v3;

	if BasinID >2200 or BasinID<1201  then delete;
run;
proc sort data=work.v3   out=work.climate;/*对提取出的GEP数据进行排序*/
     by basinID year month;
run; 
 
PROC EXPORT DATA= WORK.climate 
            OUTFILE= "d:\zagunao\INPUTS\Inputs\climate.txt" 
            dbms=csv REPLACE;
     PUTNAMES=YES;
RUN;


/*读取流域basin文件和VEG_RATIO文件并输出CELLINFO和HUCAREA到TXT格式*/
PROC IMPORT OUT= WORK.basin_
            DATAFILE= "d:\zagunao\INPUTS\basininfo.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="basin1$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
PROC IMPORT OUT= WORK.Vatio_ 
            DATAFILE= "d:\zagunao\INPUTS\VEG.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="sheet2$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
proc sort data=work.basin_  out=basin_ ;/*对提取出的GEP数据进行排序*/
     by HUC_8;
run; 
data cellinfo1;
set basin_;
	ID=HUC_8;
	BasinID=huc_8;
	lat_=lat;
	long_=long;
	area=shape_area;
	dem1=dem_mean;
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
	keep ID BasinID lat long VEG_0 VEG_1 VEG_2 VEG_3 VEG_4 VEG_5 VEG_6 VEG_7 VEG_8 VEG_9 VEG_10 VEG_11 VEG_12;
	if ID >2200 or ID <1201 then delete;
run;
proc sort data=work.cellinfo  out=cellinfo ;/*对提取出的GEP数据进行排序*/
     by ID;
	
run; 
PROC EXPORT DATA= cellinfo 
            OUTFILE= "d:\zagunao\INPUTS\Inputs\cellinfo.txt" 
            dbms=csv REPLACE;
     PUTNAMES=YES;
RUN;
/*输出流域面积文件*/
data HUCAREA;
	set cellinfo1;
	WatershedID=ID;
	Area_m2=area;
	dem=dem1;
	drop basinID lat_ long_ area;
run;
data HUCAREA;
set HUCAREA(keep=ID WatershedID dem Area_m2);
if ID >2200 or ID <1201 then delete;
run;

PROC EXPORT DATA= HUCAREA 
            OUTFILE= "d:\zagunao\INPUTS\Inputs\hucarea.txt" 
            dbms=csv REPLACE;
     PUTNAMES=YES;
RUN;



