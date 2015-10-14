
!******************Defining Common Variables for the whole program********

      Module common_var

! BASIC
      INTEGER NGRID,NYEAR,NLC,BYEAR,IYSTART,IYEND
      COMMON/BASIC/NGRID,NYEAR,NLC,BYEAR,IYSTART,IYEND

! VAL
      COMMON/VAL/VAL_1(100), VAL_2(100), VAL_3,VAL_4,VAL_5,VAL_6

! VALID
      REAL GEP_V,ET_V,GPP_V,NPP_V,FLOW_V,FLOW,RUNOFF_V,RUN_OFF,BASEFLOW_V,&
        BASEFLOW
      COMMON/VALID/ GEP_V(1000,200,12), ET_V(1000,200,12)&
          ,GPP_V(1000,200),NPP_V(1000,200), FLOW_V(200,12),FLOW(200,12)&
          ,RUNOFF_V(1000,200),RUN_OFF(1000,200), &
          BASEFLOW_V(200,12),BASEFLOW(200,12)

! CELLINFO
      REAL LADUSE,LATUDE,LONGI
      INTEGER HUCNO
      COMMON/CELLINFO/LADUSE(1000,20),HUCNO(1000),LATUDE(1000),LONGI(1000)


! OUTPUT1
      REAL PET,APET,PAET,APAET,AET,RUNOFF,INTER,PRIBF,SECBF,INTF, &
         AVUZTWC,TPAET,AVUZFWC,AVLZTWC,AVLZFPC,A_ET,P_ET,Sun_ET,RUN_HRU,BASE_HRU
      COMMON/OUTPUT1/ PET(200,12,20),APET(12),PAET(200,12,20),APAET(12),&
         AET(12), RUNOFF(12), INTER(12), PRIBF(12), SECBF(12), INTF(12), &
         AVUZTWC(12), AVUZFWC(12), AVLZTWC(12), AVLZFPC(12),&
         A_ET(1000,200, 12),P_ET(1000,200, 12),Sun_ET(1000,200, 12),&
         RUN_HRU(1000,200, 12),BASE_HRU(1000,200, 12)      
      
! CLIMATE      
      REAL  RAIN,TEMP,AAPPT
      COMMON/CLIMATE/ RAIN(1000,200,12), TEMP(1000,200, 12), AAPPT(1000)
      
! LAI        
      REAL LAI_1,LAI_2,LAI_3,LAI_4,LAI_5,LAI_6,LAI_7,LAI_8,LAI_9,LAI_10,&
            LAI_11,LAI_12,LAI_13,LAI_14,LAI_15,LAI_16,LAI_17,LAI_18,LAI_19
      COMMON/LAI/LAI_1(1000,200,12), LAI_2(1000,200,12), &
         LAI_3(1000,200,12),LAI_4(1000,200,12),LAI_5(1000,200,12),& 
         LAI_6(1000,200,12), LAI_7(1000,200,12),LAI_8(1000,200,12),&
         LAI_9(1000,200,12),LAI_10(1000,200,12),LAI_11(1000,200,12), &
         LAI_12(1000,200,12), LAI_13(1000,200,12), LAI_14(1000,200,12),&
         LAI_15(1000,200,12),LAI_16(1000,200,12), LAI_17(1000,200,12),&
         LAI_18(1000,200,12), LAI_19(1000,200,12)

! SNOWPACK	
      REAL SP,SNOWPACK,NSPM
      COMMON/SNOWPACK/SP(12),SNOWPACK, NSPM(200)

! SUMMARY1 
      REAL ANURAIN,ANURUN,ANUPET,ANUAET,ANUPAET
      COMMON/SUMMARY1/ANURAIN(200),ANURUN(200),ANUPET(200),ANUAET(200),&
        ANUPAET(200)

! SOIL
      REAL LZTWM, LZFPM, LZFSM,LZSK,LZPK, UZTWM, UZFWM, UZK, ZPERC,&
        REXP, PFREE, SMC        
      COMMON/SOIL/LZTWM(1000), LZFPM(1000), LZFSM(1000), LZSK(1000),&
        LZPK(1000), UZTWM(1000), UZFWM(1000), UZK(1000), ZPERC(1000),&
        REXP(1000), PFREE(1000), SMC(12)

! HUC
      Double Precision HUCAREA
      REAL HUCELE
      COMMON/HUC/ HUCAREA(1000),HUCELE(1000)

! FLOW 
      REAL STRFLOW,STRET,STRGEP     
      COMMON/FLOW/STRFLOW(1000, 200, 12),STRET(1000, 200, 12)&
     ,STRGEP(1000, 200, 12)

! CARBON
        REAL GEPM,RECOM,NEEM,GEPA,NEEA
      COMMON/CARBON/ GEPM(1000, 200, 12),RECOM(1000,200,12), &
      NEEM(1000,200,12),GEPA(1000,200),NEEA(1000,200)
      
! BYLAND     
      REAL RUNLAND,ETLAND,GEPLAND    
      COMMON/BYLAND/ RUNLAND(1000,30,12, 31,13), &
        ETLAND(650, 40,12, 31,13), GEPLAND(650, 40,12, 31,13)

! HUCPETAET
      REAL HUCAET, HUCPET,HUCPAET
      COMMON/HUCPETAET/HUCAET(1000,200), HUCPET(1000,200),&
      HUCPAET(1000,200)

! LANDCHANGE     
      REAL FPERD, FPERDLAI  
      COMMON/LANDCHANGE/FPERD,FPERDLAI
        
! R	  
	  REAL RFACTOR 
      COMMON/R/ RFACTOR(200)
      end

!**************---Program Starting--------*************************
     
      
      PROGRAM WaSSICBZB 
         
       use common_var  
         
           
      INTEGER MONTHD(12),MONTHL(12)


      CHARACTER PRESS
      REAL VAL_L1 ,VAL_L2      
      
! --- Number of days for each month during regular year
      DATA MONTHD/31,28,31,30,31,30,31,31,30,31,30,31/

! --- Number of days for each month during leap year
      DATA MONTHL/31,29,31,30,31,30,31,31,30,31,30,31/
      
! --- Write introductory information to screen
      
      WRITE(*,10)
   10 FORMAT(' *************************************************'//,&
            '                   *** WaSSI-CB Minjiang  ***'//,&
       '   Water Supply Stress Index Modeling System'//,&
         ' Eastern Forest Environmental Threat Assessment Center'/,&
         ' USDA Forest Service Southern Research Station '/,&
            ' Raleigh, NC'//,&
            ' June 2011 -'//,&
            ' Press Y OR y to continue :>1 ' //)
      READ(*,20) PRESS
!      PRESS = "Y"
!	IF (press.eq."y" .or.press.eq."Y") THEN
   20 FORMAT(A1)
      WRITE(*,30)
   30 FORMAT('       *** PROGRAM IS RUNNING, PLEASE WAIT ***')

!--Open Input files----------------------------------------------
    
      OPEN(1,FILE='C:\WaSSICBZB\Inputs\GENERAL.TXT')
      OPEN(2,FILE='C:\WaSSICBZB\Inputs\CELLINFO.TXT') 

      OPEN(4,FILE='C:\WaSSICBZB\Inputs\CLIMATE.TXT')

      OPEN(7,FILE='C:\WaSSICBZB\Inputs\SOILINFO.TXT')
      OPEN(8,FILE='C:\WaSSICBZB\Inputs\LANDLAI.TXT')

      OPEN(11,FILE='C:\WaSSICBZB\Inputs\HUCAREA.TXT')
!      OPEN(22,FILE='C:\WaSSICBZB\Inputs\V_FLOW.TXT')


! ---Open Output files---------------------------------------- 
 
       
      OPEN(77,FILE='C:\WaSSICBZB\Outputs\BASICOUT.TXT')
      
     
      OPEN(78,FILE='C:\WaSSICBZB\Outputs\MONTHFLOW.TXT')
      OPEN(79,FILE='C:\WaSSICBZB\Outputs\ANNUALFLOW.TXT')
      OPEN(80,FILE='C:\WaSSICBZB\Outputs\HUCFLOW.TXT')
      
      OPEN(400,FILE='C:\WaSSICBZB\Outputs\MONTHCARBON.TXT')
      OPEN(500,FILE='C:\WaSSICBZB\Outputs\ANNUALCARBON.TXT')
      OPEN(600,FILE='C:\WaSSICBZB\Outputs\HUCCARBON.TXT')
      OPEN(700,FILE='C:\WaSSICBZB\Outputs\ANNUALBIO.TXT')
!      OPEN(800,FILE='C:\WaSSICBZB\Outputs\HUCBIO.TXT')    
      OPEN(900,FILE='C:\WaSSICBZB\Outputs\SOILSTORAGE.TXT')
      OPEN(910,FILE='C:\WaSSICBZB\Outputs\RUNOFFBYLANDUSE.TXT')
      OPEN(920,FILE='C:\WaSSICBZB\Outputs\FLOWVOLBYLANDUSE.TXT')     
!      OPEN(1000,FILE='C:\WaSSICBZB\Outputs\RUNLAND.TXT')
! --- Open Output FILES (WARMUP.FOR)
!       OPEN(2002,FILE='C:\WaSSICBZB\Outputs\DATA_V_F.TXT') 
!       OPEN(2003,FILE='C:\WaSSICBZB\Outputs\VALIDATION.TXT')  
       
      CALL RPSDF

      CALL RPSINT       
      CALL RPSWATERUSE  
      
      CALL RPSLAI
      
      CALL RPSCLIMATE

!      CALL RPSVALID


!--------------------------------------------------------------------------------------------      

! --- START SIMULATION LOOPS
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
 
!    WRITE MONTHLY WATER BALANCE OUTPUT TO MONTHRUNOFF.TXT            
!     WRITE MONTHLY SOIL STORAGE OUTPUT TO SOILSTORAGE.TXT             
!     CALCULATE TOTAL ANNUAL RAIN, PET, AET, DISCHARGE, INT, SNOWP  
!     PRINT ANNUAL WATER BALANCE COMPONENTS TO ANNUALFLOW.TXT  

!     CALCULATE R FACTOR AND OUTPUT TO ANNUALFLOW.TXT           

            CALL OUTPUT(ICELL,IYEAR)
            
300      CONTINUE

!     CALCULATE AVERAGE WATER BALANCE COMPONENTS FROM IYSTART TO IYEND
!     WRITE TO SUMMARRUNOFF.TXT   
                                 
    
         CALL SUMMARY(ICELL)
           

200   CONTINUE
       
	     
                
            PRINT *, 'WATER BALANCE SECTION SUCCEEDED!'                      

!     SIMULATE TOTAL FLOW FOR EACH MONTH AND EACH HUC                  
!     WRITE ACCUMULATED FLOW TO MONTHACCFLOW.TXT                       
!     PERFORM WATER SUPPLY/DEMAND AND WASSI CALCULATIONS               
!     WRITE WASSI OUTPUT TO ANNUALWaSSI.TXT                            
!     WRITE WASSI OUTPUT TO HUCWaSSI.TXT                               

!            CALL FLOWROUTING
                    
!            PRINT *, 'FLOW ROUTING DONE'
            
!     Simulate GEP AND NEE for selected HUC                            
!     WRITE GEP AND NEE TO MONTHCARBON.TXT, ANNUALCARBON.TXT, HUCCARBON.TXT        
!     SIMULATE BIODIVERSITY FOR SELECTED HUC                           
!     WRITE BIODIVERSITY TO HUCBIO.TXT                                 


            CALL CARBONBAL
            
            PRINT *, 'CARBON BALANCE AND BIODIVERSITY SIMULATION ENDS'
                       
!            CALL VALIDATION(TUN1)    
           
         WRITE(*,75)
75         FORMAT('  CALCULATING FLOW BY LANDCOVER'/)

1112   CONTINUE
1111   CONTINUE
            CALL FLOWBYLAND      

  
            PRINT *, '-------------PROGRAM RUN ENDS----------------!'
		Stop
!      ELSE
!    	    	STOP 
!	ENDIF		
      END

