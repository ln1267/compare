
	  
      PROGRAM WaSSICBZB    
c       use myvar
      COMMON/BASIC/NGRID,NYEAR, NLC,BYEAR,IYSTART,IYEND

      COMMON/VAL/VAL_1(100), VAL_2(100), VAL_3,VAL_4,VAL_5,VAL_6

        COMMON/SOIL/LZTWM(1000), LZFPM(1000), LZFSM(1000), LZSK(1000),
     >  LZPK(1000), UZTWM(1000), UZFWM(1000), UZK(1000), ZPERC(1000),
     >  REXP(1000), PFREE(1000), SMC(12)

      INTEGER NLC，BYEAR ,TUN
           
      INTEGER MONTHD(12),MONTHL(12)
      REAL LZTWM, LZFPM, LZFSM, LZSK,
     >  LZPK, UZTWM, UZFWM, UZK, ZPERC,
     >  REXP, PFREE, SMC

      CHARACTER PRESS
      REAL VAL_L1 ,VAL_L2      
      
C --- Number of days for each month during regular year
      DATA MONTHD/31,28,31,30,31,30,31,31,30,31,30,31/

C --- Number of days for each month during leap year
      DATA MONTHL/31,29,31,30,31,30,31,31,30,31,30,31/
      
C --- Write introductory information to screen
      
      WRITE(*,10)
   10 FORMAT(' *************************************************'//,
     >       '                   *** WaSSI-CB Chuan Xi  ***'//,
     >  '   Water Supply Stress Index Modeling System'//,
     >    ' Eastern Forest Environmental Threat Assessment Center'/,
     >    ' USDA Forest Service Southern Research Station '/,
     >       ' Raleigh, NC'//,
     >       ' June 2011 -'//,
     >       ' Press Y OR y to continue :>' //)
C      READ(*,20) PRESS
      PRESS = "Y"
	IF (press.eq."y" .or.press.eq."Y") THEN
   20 FORMAT(A1)
      WRITE(*,30)
   30 FORMAT('       *** PROGRAM IS RUNNING, PLEASE WAIT ***')
C       PRINT*,"请输入VAL_1, VAL_2, VAL_3,VAL_4,VAL_5,VAL_6的值"
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
      OPEN(22,FILE='C:\WaSSICBZB\Inputs\V_FLOW.TXT')


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
!      VAL_L1=30
!      VAL_L2=1
!      TUN=2
!       PRINT*,"请输入“变量起始值”, “变量增量”, “模拟次数”的值"
!       PRINT*,"以逗号或者回车隔开"
!      READ (*,*) VAL_L1,VAL_L2,TUN
!      PRINT*,VAL_L1,VAL_L2,TUN


      DO 1111 TUN1=1, 1
        VAL_L1=VAL_L1+VAL_L2
        VAL_1(TUN1)=VAL_L1
!      DO 1112 TUN2=1, 1
!        VAL_L2=VAL_L2+0.05
!        VAL_2(TUN2)=VAL_L2
!        TUN=TUN+1 

      WRITE(77,2051)
2051  FORMAT(/'SOIL PARAMETERS FOR EACH SIMULATION CELL'/)
!      READ (7,550) DUMY
550   FORMAT (30A8)
      
      
      WRITE (77,550) DUMY
          
      DO 15 I=1, NGRID
      UZTWM(I)=30
      UZFWM(I)=22
      UZK(I)=0.15 !0.19 
      ZPERC(I)=80
      REXP(I)=2.4 !3
      LZTWM(I)=162
      LZFSM(I)=36
      LZFPM(I)=65 !85
      LZSK(I)=0.06
      LZPK(I)=0.016
      PFREE(I)=0.2
C      READ(7,*) ID, HUCNO(I), UZTWM(I), UZFWM(I), UZK(I), ZPERC(I),
C     &REXP(I), LZTWM(I), LZFSM(I), LZFPM(I), LZSK(I),
C     &LZPK(I), PFREE(I)
            
C      WRITE(77,1150) HUCNO(I), UZTWM(I), UZFWM(I), UZK(I), ZPERC(I),
C     &REXP(I), LZTWM(I), LZFSM(I), LZFPM(I), LZSK(I),
C     &LZPK(I), PFREE(I)
     
1150  FORMAT(I12, 11F10.4)    
     
15    CONTINUE







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
               
                 
               CALL WATERBAL(TUN1,ICELL, IYEAR, IM, MNDAY)
               


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
                       
            CALL VALIDATION(TUN1)    
           
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

