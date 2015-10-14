
	  
      PROGRAM WaSSICBZB    
c       use myvar
      COMMON/BASIC/NGRID,NYEAR, NLC,BYEAR,IYSTART,IYEND

      COMMON/VAL/VAL_1(100), VAL_2(100), VAL_3,VAL_4,VAL_5,VAL_6

      INTEGER NLC£¨BYEAR 
           
      INTEGER MONTHD(12),MONTHL(12)
      
      CHARACTER PRESS
      REAL VAL_L1 ,VAL_L2      
      
C --- Number of days for each month during regular year
      DATA MONTHD/31,28,31,30,31,30,31,31,30,31,30,31/

C --- Number of days for each month during leap year
      DATA MONTHL/31,29,31,30,31,30,31,31,30,31,30,31/
      
C --- Write introductory information to screen
      
      WRITE(*,10)
   10 FORMAT(' *************************************************'//,
     >       '                   *** WaSSI-CB Za Gu Nao ***'//,
     >  '   Water Supply Stress Index Modeling System'//,
     >    ' Eastern Forest Environmental Threat Assessment Center'/,
     >    ' USDA Forest Service Southern Research Station '/,
     >       ' Raleigh, NC'//,
     >       ' June 2011 -'//,
     >       ' Press Y OR y to continue :>' //)
      READ(*,20) PRESS
	IF (press.eq."y" .or.press.eq."Y") THEN
   20 FORMAT(A1)
      WRITE(*,30)
   30 FORMAT('       *** PROGRAM IS RUNNING, PLEASE WAIT ***')
C       PRINT*,"«Î ‰»ÎVAL_1, VAL_2, VAL_3,VAL_4,VAL_5,VAL_6µƒ÷µ"
C      READ (*,*) VAL_1(1), VAL_1(2), VAL_3,VAL_4,VAL_5,VAL_6
C      PRINT*,VAL_1(1), VAL_2, VAL_3,VAL_4,VAL_5,VAL_6
C      READ(*,*) 
C--Open Input files----------------------------------------------
    
      OPEN(1,FILE='C:\WaSSICBZB\Inputs\GENERAL.TXT')
      OPEN(2,FILE='C:\WaSSICBZB\Inputs\CELLINFO.TXT') 

      OPEN(4,FILE='C:\WaSSICBZB\Inputs\CLIMATE.TXT')

      OPEN(7,FILE='C:\WaSSICBZB\Inputs\SOILINFO.TXT')
      OPEN(8,FILE='C:\WaSSICBZB\Inputs\LANDLAI.TXT')

      OPEN(11,FILE='C:\WaSSICBZB\Inputs\HUCAREA.TXT')

      OPEN(20,FILE='C:\WaSSICBZB\Inputs\V_month.TXT')
      OPEN(21,FILE='C:\WaSSICBZB\Inputs\V_year.TXT')

      OPEN(22,FILE='C:\WaSSICBZB\Inputs\V_flow.TXT')

C ---Open Output files---------------------------------------- 
 
       
      OPEN(77,FILE='C:\WaSSICBZB\Outputs\BASICOUT.TXT')
      
     
      OPEN(78,FILE='C:\WaSSICBZB\Outputs\MONTHFLOW.TXT')
      OPEN(79,FILE='C:\WaSSICBZB\Outputs\ANNUALFLOW.TXT')
      OPEN(80,FILE='C:\WaSSICBZB\Outputs\HUCFLOW.TXT')
      
      OPEN(400,FILE='C:\WaSSICBZB\Outputs\MONTHCARBON.TXT')
      OPEN(500,FILE='C:\WaSSICBZB\Outputs\ANNUALCARBON.TXT')
      OPEN(600,FILE='C:\WaSSICBZB\Outputs\HUCCARBON.TXT')
      OPEN(700,FILE='C:\WaSSICBZB\Outputs\ANNUALBIO.TXT')
      OPEN(800,FILE='C:\WaSSICBZB\Outputs\HUCBIO.TXT')    
c      OPEN(900,FILE='C:\WaSSICBZB\Outputs\SOILSTORAGE.TXT')
      OPEN(910,FILE='C:\WaSSICBZB\Outputs\RUNOFFBYLANDUSE.TXT')
      OPEN(920,FILE='C:\WaSSICBZB\Outputs\FLOWVOLBYLANDUSE.TXT')     
C      OPEN(1000,FILE='C:\WaSSICBZB\Outputs\RUNLAND.TXT')
C --- READ INPUT DATA FILES (WARMUP.FOR)
      OPEN(2000,FILE='C:\WaSSICBZB\Outputs\DATA_V_M.TXT') 
      OPEN(2001,FILE='C:\WaSSICBZB\Outputs\DATA_V_A.TXT') 
      OPEN(2002,FILE='C:\WaSSICBZB\Outputs\DATA_V_F.TXT')    
      OPEN(2003,FILE='C:\WaSSICBZB\Outputs\VALIDATION.TXT')    
       
      CALL RPSDF
      CALL RPSINT
      
      CALL RPSWATERUSE  
      
      CALL RPSLAI
      
      CALL RPSCLIMATE

      CALL RPSVALID


C--------------------------------------------------------------------------------------------      

C --- START SIMULATION LOOPS
      VAL_L1=11.14
      VAL_L2=1.85
      TUN=0
      DO 1111 TUN1=1, 1
        VAL_L1=VAL_L1+0.5
        VAL_1(TUN1)=VAL_L1
      DO 1112 TUN2=1, 1
        VAL_L2=VAL_L2+0.05
        VAL_2(TUN2)=VAL_L2
        TUN=TUN+1
      DO 200 ICELL=1, NGRID
         ICOUNT=0 
                
         DO 300 IYEAR=1, NYEAR
                  
            
            YEAR = IYSTART + ICOUNT
			ICOUNT=ICOUNT+1 
            NDAY = 365
            IF(YEAR/4*4.NE.YEAR) GO TO 110
            IF(YEAR/400*400.EQ.YEAR) GO TO 110
            NDAY=366   
             
110         CONTINUE 
        

            DO 400 IM=1, 12
               
               IF (NDAY .EQ. 365) THEN
                 MNDAY=MONTHD(IM)
               ELSE
                 MNDAY=MONTHL(IM)
               ENDIF
               
               
               
                              
               CALL WARMPET(ICELL, IYEAR, IM, MNDAY)
               
                 
               CALL WATERBAL(TUN1,TUN2,ICELL, IYEAR, IM, MNDAY)
               


400         CONTINUE 
 
C     WRITE MONTHLY WATER BALANCE OUTPUT TO MONTHRUNOFF.TXT            
C     WRITE MONTHLY SOIL STORAGE OUTPUT TO SOILSTORAGE.TXT             
C     CALCULATE TOTAL ANNUAL RAIN, PET, AET, DISCHARGE, INT, SNOWP  
C     PRINT ANNUAL WATER BALANCE COMPONENTS TO ANNUALFLOW.TXT  

c     CALCULATE R FACTOR AND OUTPUT TO ANNUALFLOW.TXT           

            CALL OUTPUT(ICELL,IYEAR)
            
300      CONTINUE

C     CALCULATE AVERAGE WATER BALANCE COMPONENTS FROM IYSTART TO IYEND
C     WRITE TO SUMMARRUNOFF.TXT   
                                 
    
         CALL SUMMARY(ICELL)
           

200   CONTINUE
       
	     
                
            PRINT *, 'WATER BALANCE SECTION SUCCEEDED!'                      

C     SIMULATE TOTAL FLOW FOR EACH MONTH AND EACH HUC                  
C     WRITE ACCUMULATED FLOW TO MONTHACCFLOW.TXT                       
C     PERFORM WATER SUPPLY/DEMAND AND WASSI CALCULATIONS               
C     WRITE WASSI OUTPUT TO ANNUALWaSSI.TXT                            
C     WRITE WASSI OUTPUT TO HUCWaSSI.TXT                               

c            CALL FLOWROUTING
                    
c            PRINT *, 'FLOW ROUTING DONE'
            
C     Simulate GEP AND NEE for selected HUC                            
C     WRITE GEP AND NEE TO MONTHCARBON.TXT, ANNUALCARBON.TXT, HUCCARBON.TXT        
C     SIMULATE BIODIVERSITY FOR SELECTED HUC                           
C     WRITE BIODIVERSITY TO HUCBIO.TXT                                 


            CALL CARBONBAL
            
            PRINT *, 'CARBON BALANCE AND BIODIVERSITY SIMULATION ENDS'
                       
            CALL VALIDATION(TUN1,TUN2)
           
         WRITE(*,75)
75         FORMAT('  CALCULATING FLOW BY LANDCOVER'/)

1112   CONTINUE
1111   CONTINUE
            CALL FLOWBYLAND      

  
            PRINT *, '-------------PROGRAM RUN ENDS----------------!'
		Stop
      ELSE
    	    	STOP 
	ENDIF		
      END

