
	  
      PROGRAM WaSSICBZB    
c       use myvar
      COMMON/BASIC/NGRID,NYEAR, NLC,BYEAR,IYSTART,IYEND

      COMMON/VAL/VAL_1(100), VAL_2(100), VAL_3,VAL_4,VAL_5,VAL_6

        COMMON/SOIL/LZTWM(1000), LZFPM(1000), LZFSM(1000), LZSK(1000),
     >  LZPK(1000), UZTWM(1000), UZFWM(1000), UZK(1000), ZPERC(1000),
     >  REXP(1000), PFREE(1000), SMC(366)

      INTEGER NLC��BYEAR ,TUN,NGRID,NYEAR, NLC,IYSTART,IYEND,YEAR
           
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
C       PRINT*,"������VAL_1, VAL_2, VAL_3,VAL_4,VAL_5,VAL_6��ֵ"
C      READ (*,*) VAL_1(1), VAL_1(2), VAL_3,VAL_4,VAL_5,VAL_6
C      PRINT*,VAL_1(1), VAL_2, VAL_3,VAL_4,VAL_5,VAL_6
C      READ(*,*) 
C--Open Input files----------------------------------------------
    
      OPEN(1,FILE='D:\WaSSICBZB\Inputs\GENERAL.TXT')
C      OPEN(2,FILE='D:\WaSSICBZB\Inputs\CELLINFO.TXT') 

      OPEN(4,FILE='D:\WaSSICBZB\Inputs\CLIMATE_day.TXT')

      OPEN(7,FILE='D:\WaSSICBZB\Inputs\SOILINFO.TXT')
C      OPEN(8,FILE='D:\WaSSICBZB\Inputs\LANDLAI.TXT')

      OPEN(11,FILE='D:\WaSSICBZB\Inputs\HUCAREA.TXT')
      OPEN(22,FILE='D:\WaSSICBZB\Inputs\V_FLOW_day.TXT')


C ---Open Output files---------------------------------------- 
 
       
      OPEN(77,FILE='D:\WaSSICBZB\Outputs\BASICOUT.TXT')
      
     
      OPEN(78,FILE='D:\WaSSICBZB\Outputs\MONTHFLOW.TXT')
      OPEN(79,FILE='D:\WaSSICBZB\Outputs\ANNUALFLOW.TXT')
      OPEN(80,FILE='D:\WaSSICBZB\Outputs\FLOW_day.TXT')
C      OPEN(80,FILE='D:\WaSSICBZB\Outputs\HUCFLOW.TXT')     
C     OPEN(400,FILE='D:\WaSSICBZB\Outputs\MONTHCARBON.TXT')
C      OPEN(500,FILE='D:\WaSSICBZB\Outputs\ANNUALCARBON.TXT')
C      OPEN(600,FILE='D:\WaSSICBZB\Outputs\HUCCARBON.TXT')
C      OPEN(700,FILE='D:\WaSSICBZB\Outputs\ANNUALBIO.TXT')
C      OPEN(800,FILE='D:\WaSSICBZB\Outputs\HUCBIO.TXT')    
      OPEN(900,FILE='D:\WaSSICBZB\Outputs\SOILSTORAGE.TXT')
C      OPEN(910,FILE='D:\WaSSICBZB\Outputs\RUNOFFBYLANDUSE.TXT')
C      OPEN(920,FILE='D:\WaSSICBZB\Outputs\FLOWVOLBYLANDUSE.TXT')     
C      OPEN(930,FILE='D:\WaSSICBZB\Outputs\RUNOFFBYLANDUSE_m.TXT')
C      OPEN(940,FILE='D:\WaSSICBZB\Outputs\FLOWVOLBYLANDUSE_m.TXT')
        
C      OPEN(1000,FILE='D:\WaSSICBZB\Outputs\RUNLAND.TXT')



C --- READ INPUT DATA FILES (WARMUP.FOR)
       OPEN(2002,FILE='D:\WaSSICBZB\Outputs\DATA_V_F.TXT') 
       OPEN(2003,FILE='D:\WaSSICBZB\Outputs\VALIDATION.TXT')  
       
      CALL RPSDF    ! Input general data and add heading to outputs.

C      CALL RPSINT       ! Input Soil parameters
      CALL RPSWATERUSE  !Input HUC Area data
      
C      CALL RPSLAI
      
      CALL RPSCLIMATE   !Input (pre, T, PET) data  

      CALL RPSVALID    ! Input Validating Flow data


C--------------------------------------------------------------------------------------------      

C --- START SIMULATION LOOPS
!      VAL_L1=30
!      VAL_L2=1
!      TUN=2
       PRINT*,"�����롰������ʼֵ��, ������������, ��ģ���������ֵ"
       PRINT*,"�Զ��Ż��߻س�����"
      READ (*,*) VAL_L1,VAL_L2,TUN
      PRINT*,VAL_L1,VAL_L2,TUN


      DO 1111 TUN1=1, TUN
        VAL_L1=VAL_L1+VAL_L2
        VAL_1(TUN1)=VAL_L1
!      DO 1112 TUN2=1, 1
!        VAL_L2=VAL_L2+0.05
!        VAL_2(TUN2)=VAL_L2
!        TUN=TUN+1 

C assign  the default value for the soil parameters
!!      READ (7,550) DUMY
!550   FORMAT (30A8)
!      
!      
!      WRITE (77,550) DUMY
!          
      DO 15 I=1, NGRID
      UZTWM(I)=VAL_1(TUN1) !33
      UZFWM(I)= 140 !22
      UZK(I)=0.10 !0.15 
      ZPERC(I)= 150 !80
      REXP(I)= 1.5 !2.4 
      LZTWM(I)=140 !162
      LZFSM(I)= 225 !36
      LZFPM(I)=100 !65 
      LZSK(I)= 0.05 ! 0.06
      LZPK(I)= 0.028 !0.016
      PFREE(I)= 0.15 !0.2
     
     
15    CONTINUE



      DO 200 ICELL=1, NGRID
         ICOUNT=0 
                
         DO 300 IYEAR=1, NYEAR
                  
            
            YEAR = IYSTART + ICOUNT
			ICOUNT=ICOUNT+1 
            NDAY = 365
            IF(YEAR/4*4.NE.YEAR) GO TO 110
C            IF(YEAR/400*400.EQ.YEAR) GO TO 110
            NDAY=366   
             
110         CONTINUE 
        
             
             Do 500 L_DAY= 1,NDAY
               
                             
C               CALL WARMPET(ICELL, IYEAR, IM, DAY)
               
                 
               CALL WATERBAL(TUN1,ICELL,IYEAR,L_DAY)
               

500         CONTINUE

 
C     WRITE MONTHLY WATER BALANCE OUTPUT TO MONTHRUNOFF.TXT            
C     WRITE MONTHLY SOIL STORAGE OUTPUT TO SOILSTORAGE.TXT             
C     CALCULATE TOTAL ANNUAL RAIN, PET, AET, DISCHARGE, INT, SNOWP  
C     PRINT ANNUAL WATER BALANCE COMPONENTS TO ANNUALFLOW.TXT  

c     CALCULATE R FACTOR AND OUTPUT TO ANNUALFLOW.TXT           

            CALL OUTPUT(ICELL,IYEAR)
            
300      CONTINUE

C     CALCULATE AVERAGE WATER BALANCE COMPONENTS FROM IYSTART TO IYEND
C     WRITE TO SUMMARRUNOFF.TXT   
                                 
    
C         CALL SUMMARY(ICELL)
           

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


C           CALL CARBONBAL
            
C            PRINT *, 'CARBON BALANCE AND BIODIVERSITY SIMULATION ENDS'
                       
            CALL VALIDATION(TUN1)    
           
         WRITE(*,75)
75         FORMAT('  CALCULATING FLOW BY LANDCOVER'/)

1112   CONTINUE
1111   CONTINUE
C            CALL FLOWBYLAND      

  
            PRINT *, '-------------PROGRAM RUN ENDS----------------!'
		Stop
      ELSE
    	    	STOP 
	ENDIF		
      END

