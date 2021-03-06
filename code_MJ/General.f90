
!******************Defining Common Variables for the whole program********

      Module common_var
      implicit none
! BASIC
      INTEGER NGRID,NYEAR,NLC,BYEAR,IYSTART,IYEND
      COMMON/BASIC/NGRID,NYEAR,NLC,BYEAR,IYSTART,IYEND

! VAL       
      REAL VAL_1, VAL_2, VAL_3,VAL_4,VAL_5,VAL_6 
      COMMON/VAL/VAL_1(100), VAL_2(100), VAL_3,VAL_4,VAL_5,VAL_6

! VALID
      REAL GEP_V,ET_V,GPP_V,NPP_V,FLOW_V,FLOW,RUNOFF_V,RUN_OFF,BASEFLOW_V,&
        BASEFLOW
      COMMON/VALID/ GEP_V(6000,200,12), ET_V(6000,200,12)&
          ,GPP_V(6000,200),NPP_V(6000,200), FLOW_V(200,12),FLOW(200,12)&
          ,RUNOFF_V(6000,200),RUN_OFF(6000,200), &
          BASEFLOW_V(200,12),BASEFLOW(6000,200,12)

! CELLINFO
      REAL LADUSE,LATUDE,LONGI,HUCELE
      INTEGER HUCNO
      COMMON/CELLINFO/LADUSE(6000),HUCNO(6000),LATUDE(6000),LONGI(6000),HUCELE(6000)

! VEGINFO
      INTEGER veg
      COMMON/VEGINFO/VEG(6000,50)


! OUTPUT1
      REAL PET,APET,PAET,APAET,AET,RUNOFF,INTER,PRIBF,SECBF,INTF, &
         AVUZTWC,TPAET,AVUZFWC,AVLZTWC,AVLZFPC,AVLZFSC,A_ET,P_ET,Sun_ET,RUN_HRU,BASE_HRU
      COMMON/OUTPUT1/ PET(200,12),APET(12),PAET(200,12),APAET(12),&
         AET(12), RUNOFF(12), INTER(12), PRIBF(12), SECBF(12), INTF(12), &
         AVUZTWC(12), AVUZFWC(12), AVLZTWC(12), AVLZFPC(12),AVLZFSC(12),&
         A_ET(6000,200, 12),P_ET(6000,200, 12),Sun_ET(6000,200, 12),&
         RUN_HRU(6000,200, 12),BASE_HRU(6000,200, 12)      
      
! CLIMATE      
      REAL  RAIN,TEMP,AAPPT
      COMMON/CLIMATE/ RAIN(6000,200,12), TEMP(6000,200, 12), AAPPT(6000)
      
! LAI        
      REAL LAI
      COMMON/LAI/LAI(6000,200,12)

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
      COMMON/SOIL/LZTWM(6000), LZFPM(6000), LZFSM(6000), LZSK(6000),&
        LZPK(6000), UZTWM(6000), UZFWM(6000), UZK(6000), ZPERC(6000),&
        REXP(6000), PFREE(6000), SMC(12)

! Soil Mositure
       REAL UZTWC, UZFWC, LZTWC, LZFSC, LZFPC
       COMMON/SMC/UZTWC, UZFWC, LZTWC, LZFSC, LZFPC
! FLOW 
      REAL STRFLOW,STRET,STRGEP     
      COMMON/FLOW/STRFLOW(6000, 200, 12),STRET(6000, 200, 12)&
     ,STRGEP(6000, 200, 12)

! CARBON
        REAL GEPM,RECOM,NEEM,GEPA,NEEA
      COMMON/CARBON/ GEPM(6000, 200, 12),RECOM(6000,200,12), &
      NEEM(6000,200,12),GEPA(6000,200),NEEA(6000,200)
      
! BYLAND     
      REAL RUNLAND,ETLAND,GEPLAND    
      COMMON/BYLAND/ RUNLAND(6000,32,12,31), &
        ETLAND(6000,32,12,31), GEPLAND(6000,32,12,31)

! HUCPETAET
      REAL HUCAET, HUCPET,HUCPAET
      COMMON/HUCPETAET/HUCAET(6000,200), HUCPET(6000,200),&
      HUCPAET(6000,200)

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
        implicit none 
           
      INTEGER ICELL,ICOUNT,IYEAR,MONTHD(12),MONTHL(12)
      INTEGER YEAR,NDAY,IM,MNDAY

      CHARACTER PRESS
      REAL VAL_L1 ,VAL_L2      
      
! --- Number of days for each month during regular year
      DATA MONTHD/31,28,31,30,31,30,31,31,30,31,30,31/

! --- Number of days for each month during leap year
      DATA MONTHL/31,29,31,30,31,30,31,31,30,31,30,31/
      
! --- Write introductory information to screen
      
      WRITE(*,10)
   10 FORMAT(' *************************************************'//,&
            '                   *** Revised WaSSI-CB by Ning Liu  ***'//,&
       '   Water Supply Stress Index Modeling System'//,&
         ' Eastern Forest Environmental Threat Assessment Center'/,&
         ' USDA Forest Service Southern Research Station '/,&
            ' Raleigh, NC'//,&
            ' April 2015 -'//,&
            ' Press Y OR y to continue : ' //)
      READ(*,20) PRESS
!      PRESS = "Y"
!	IF (press.eq."y" .or.press.eq."Y") THEN
   20 FORMAT(A1)
      WRITE(*,30)
   30 FORMAT('       *** PROGRAM IS RUNNING, PLEASE WAIT ***')

!--Open Input files----------------------------------------------
    
      OPEN(1,FILE='F:\Data_WaSSIC\WaSSI_ZA\inputs_82_12\GENERAL.TXT')
      OPEN(2,FILE='F:\Data_WaSSIC\WaSSI_ZA\inputs_82_12\CELLINFO.TXT') 
      OPEN(3,FILE='F:\Data_WaSSIC\WaSSI_ZA\inputs_82_12\vegINFO.TXT')
      OPEN(4,FILE='F:\Data_WaSSIC\WaSSI_ZA\inputs_82_12\CLIMATE.TXT')

      OPEN(7,FILE='F:\Data_WaSSIC\WaSSI_ZA\inputs_82_12\SOILINFO.TXT')
      OPEN(8,FILE='F:\Data_WaSSIC\WaSSI_ZA\inputs_82_12\LANDLAI.TXT')

!      OPEN(11,FILE='F:\Data_WaSSIC\WaSSI_ZA\inputs_82_12\HUCAREA.TXT')
!      OPEN(22,FILE='F:\Data_WaSSIC\WaSSI_ZA\inputs_82_12\V_FLOW.TXT')


! ---Open Output files---------------------------------------- 
 
       
      OPEN(77,FILE='F:\Data_WaSSIC\WaSSI_ZA\outputs_82_12\BASICOUT.TXT')
      
     
      OPEN(78,FILE='F:\Data_WaSSIC\WaSSI_ZA\outputs_82_12\MONTHFLOW.TXT')
      OPEN(79,FILE='F:\Data_WaSSIC\WaSSI_ZA\outputs_82_12\ANNUALFLOW.TXT')
      OPEN(80,FILE='F:\Data_WaSSIC\WaSSI_ZA\outputs_82_12\HUCFLOW.TXT')
      OPEN(99,FILE='F:\Data_WaSSIC\WaSSI_ZA\outputs_82_12\ceshi.TXT')
      OPEN(400,FILE='F:\Data_WaSSIC\WaSSI_ZA\outputs_82_12\MONTHCARBON.TXT')
      OPEN(500,FILE='F:\Data_WaSSIC\WaSSI_ZA\outputs_82_12\ANNUALCARBON.TXT')
      OPEN(600,FILE='F:\Data_WaSSIC\WaSSI_ZA\outputs_82_12\HUCCARBON.TXT')
      OPEN(700,FILE='F:\Data_WaSSIC\WaSSI_ZA\outputs_82_12\ANNUALBIO.TXT')
!      OPEN(800,FILE='F:\Data_WaSSIC\WaSSI_ZA\outputs_82_12\HUCBIO.TXT')    
      OPEN(900,FILE='F:\Data_WaSSIC\WaSSI_ZA\outputs_82_12\SOILSTORAGE.TXT')
      OPEN(910,FILE='F:\Data_WaSSIC\WaSSI_ZA\outputs_82_12\RUNOFFBYLANDUSE.TXT')
      OPEN(920,FILE='F:\Data_WaSSIC\WaSSI_ZA\outputs_82_12\FLOWVOLBYLANDUSE.TXT')     
!      OPEN(1000,FILE='F:\Data_WaSSIC\WaSSI_ZA\outputs_82_12\RUNLAND.TXT')
! --- Open Output FILES (WARMUP.FOR)
!       OPEN(2002,FILE='F:\Data_WaSSIC\WaSSI_ZA\outputs_82_12\DATA_V_F.TXT') 
!       OPEN(2003,FILE='F:\Data_WaSSIC\WaSSI_ZA\outputs_82_12\VALIDATION.TXT')  

!  --------- Read input data -------------------------------
       
      CALL RPSDF       ! Set up column headings for each output files

      CALL RPSINT      ! Read Landuse, elevation and Soil parameters
          
!      CALL RPSWATERUSE  ! Read HUC area, elevation, and slope
      
      CALL RPSLAI     ! Read LAI data
      
      CALL RPSCLIMATE  ! Read calimate data

!      CALL RPSVALID   ! Read Runoff validation data


!----------------------Auto calibration part------------------------------------      

! --- START SIMULATION LOOPS
!      VAL_L1=30
!      VAL_L2=1
!      TUN=2
!       PRINT*,"请输入“变量起始值”, “变量增量”, “模拟次数”的值"
!       PRINT*,"以逗号或者回车隔开"
!      READ (*,*) VAL_L1,VAL_L2,TUN
!      PRINT*,VAL_L1,VAL_L2,TUN


 !     DO 1111 TUN1=1, 1
 !       VAL_L1=VAL_L1+VAL_L2
  !      VAL_1(TUN1)=VAL_L1
!      DO 1112 TUN2=1, 1
!        VAL_L2=VAL_L2+0.05
!        VAL_2(TUN2)=VAL_L2
!        TUN=TUN+1 

      WRITE(77,2051)
2051  FORMAT(/'SOIL PARAMETERS FOR EACH SIMULATION CELL'/)
!      READ (7,550) DUMY
!550   FORMAT (30A8)
!  
!      WRITE (77,550) DUMY
!          
!----------------------Modelling for each Cell and year start------------------------------------   
   
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
               
                                             
               CALL WARMPET(ICELL, IYEAR, IM, MNDAY)  ! Caculate MONTHLY PET AND POTENTIAL AET 
               
                 
               CALL WATERBAL(ICELL, IYEAR, IM, MNDAY) ! Caculate MONTHLY GPP and ET
               

400         CONTINUE 
 
!    WRITE MONTHLY WATER BALANCE OUTPUT TO MONTHRUNOFF.TXT            
!     WRITE MONTHLY SOIL STORAGE OUTPUT TO SOILSTORAGE.TXT             
!     CALCULATE TOTAL ANNUAL RAIN, PET, AET, DISCHARGE, INT, SNOWP  
!     PRINT ANNUAL WATER BALANCE COMPONENTS TO ANNUALFLOW.TXT  

!     CALCULATE R FACTOR AND OUTPUT TO ANNUALFLOW.TXT           

            CALL OUTPUT(ICELL,IYEAR)  ! Output Annual water and carbon balances
            
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

!1112   CONTINUE
!1111   CONTINUE
 !           CALL FLOWBYLAND      

  
            PRINT *, '-------------PROGRAM RUN ENDS----------------!'
		Stop
!      ELSE
!    	    	STOP 
!	ENDIF		
      END

