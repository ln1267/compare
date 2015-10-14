data FLOW;
	infile "C:\WASSICBZB\Outputs\DATA_V_F.TXT"  dlm=',' dsd missover firstobs=2;;
	input YEAR MONTH RUNOFF RUNOFF_V BASEFLOW BASEFLOW_V ;
run;
proc reg data=FLOW;
	model RUNOFF=RUNOFF_V;
	plot RUNOFF*RUNOFF_V;
	title "A monthly comparison of RUNOFF between WASSI-C and Observed of the whole watershed";
run;
quit;

proc reg data=FLOW;
	model baseflow=baseflow_V;
	plot baseflow*baseflow_V;
	title "A monthly comparison of BASEFLOW between WASSI-C and Observed of the whole watershed";
run;
quit; 
PROC EXPORT DATA= FLOW 
            OUTFILE= "C:\WASSICBZB\FLOW.xlsx" 
            DBMS=EXCEL REPLACE;
     SHEET="FLOW"; 
RUN;
