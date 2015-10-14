 Module common_var
      implicit none
! BASIC ok
      INTEGER NGRID,NYEAR,NLC,BYEAR,IYSTART,IYEND
      COMMON/BASIC/NGRID,NYEAR,NLC,BYEAR,IYSTART,IYEND

! VAL ok
      REAL VAL_1, VAL_2, VAL_3,VAL_4,VAL_5,VAL_6 
      COMMON/VAL/VAL_1(100), VAL_2(100), VAL_3,VAL_4,VAL_5,VAL_6

! VALID ok
      REAL GEP_V,ET_V,GPP_V,NPP_V,FLOW_V,FLOW,RUNOFF_V,RUN_OFF,BASEFLOW_V,&
        BASEFLOW
      COMMON/VALID/ GEP_V(1000,200,12), ET_V(1000,200,12)&
          ,GPP_V(1000,200),NPP_V(1000,200), FLOW_V(200,12),FLOW(200,12)&
          ,RUNOFF_V(1000,200),RUN_OFF(1000,200), &
          BASEFLOW_V(200,12),BASEFLOW(200,12)

! CELLINFO ok
      REAL LADUSE,LATUDE,LONGI,HUCELE
      INTEGER HUCNO
      Double Precision HUCAREA
      COMMON/CELLINFO/LADUSE(1000,20),HUCNO(1000),LATUDE(1000),LONGI(1000),HUCELE(1000),HUCAREA(1000)


! OUTPUT1 ok
      REAL PET,PAET,APET,APAET,AET,RUNOFF,INTER,PRIBF,SECBF,INTF, &
         AVUZTWC,TPAET,AVUZFWC,AVLZTWC,AVLZFPC,AVLZFSC,A_ET,P_ET,Sun_ET,RUN_HRU,BASE_HRU
      COMMON/OUTPUT1/ PET(200,12,20),PAET(200,12,20),APET(12),APAET(12),&
         AET(12), RUNOFF(12), INTER(12), PRIBF(12), SECBF(12), INTF(12), &
         AVUZTWC(12), AVUZFWC(12), AVLZTWC(12), AVLZFPC(12),AVLZFSC(12),&
         A_ET(1000,200, 12),P_ET(1000,200, 12),Sun_ET(1000,200, 12),&
         RUN_HRU(1000,200, 12),BASE_HRU(1000,200, 12)      
 
! CLIMATE  ok    
      REAL  RAIN,TEMP,AAPPT
      COMMON/CLIMATE/ RAIN(1000,200,12), TEMP(1000,200, 12), AAPPT(1000)
      
! LAI  ok      
      REAL LAI_1, LAI_2, LAI_3,LAI_4,LAI_5,LAI_6, LAI_7,LAI_8,&
         LAI_9,LAI_10,LAI_11,LAI_12, LAI_13, LAI_14,&
         LAI_15,LAI_16, LAI_17,LAI_18, LAI_19
      COMMON/LAI/LAI_1(1000,200,12), LAI_2(1000,200,12), &
         LAI_3(1000,200,12),LAI_4(1000,200,12),LAI_5(1000,200,12), &
         LAI_6(1000,200,12), LAI_7(1000,200,12),LAI_8(1000,200,12),&
         LAI_9(1000,200,12),LAI_10(1000,200,12),LAI_11(1000,200,12),& 
         LAI_12(1000,200,12), LAI_13(1000,200,12), LAI_14(1000,200,12),&
         LAI_15(1000,200,12),LAI_16(1000,200,12), LAI_17(1000,200,12),&
         LAI_18(1000,200,12), LAI_19(1000,200,12)

! SNOWPACK	ok
      REAL SP,SNOWPACK
      INTEGER NSPM
      COMMON/SNOWPACK/SP(12),SNOWPACK,NSPM(200)

! SUMMARY1 ok
      REAL ANURAIN,ANURUN,ANUPET,ANUAET,ANUPAET
      COMMON/SUMMARY1/ANURAIN(200),ANURUN(200),ANUPET(200),ANUAET(200),&
        ANUPAET(200)

! SOIL ok
      REAL LZTWM, LZFPM, LZFSM,LZSK,LZPK, UZTWM, UZFWM, UZK, ZPERC,&
        REXP, PFREE, SMC        
      COMMON/SOIL/LZTWM(1000), LZFPM(1000), LZFSM(1000), LZSK(1000),&
        LZPK(1000), UZTWM(1000), UZFWM(1000), UZK(1000), ZPERC(1000),&
        REXP(1000), PFREE(1000), SMC(12)

! Soil Mositure ok
       REAL UZTWC, UZFWC, LZTWC, LZFSC, LZFPC
       COMMON/SMC/UZTWC(20), UZFWC(20), LZTWC(20), LZFSC(20), LZFPC(20)

! Monthly Soil Mositure ok
       REAL MonUZTWC, MonUZFWC, MonLZTWC, MonLZFSC, MonLZFPC
       COMMON/MONSMC/MonUZTWC(1000, 200, 12), MonUZFWC(1000, 200, 12), MonLZTWC(1000, 200, 12), MonLZFSC(1000, 200, 12), MonLZFPC(1000, 200, 12)


! FLOW ok
      REAL STRFLOW,STRET,STRGEP     
      COMMON/FLOW/STRFLOW(1000, 200, 12),STRET(1000, 200, 12)&
     ,STRGEP(1000, 200, 12)

! CARBON ok
        REAL GEPM,RECOM,NEEM,GEPA,NEEA
      COMMON/CARBON/ GEPM(1000, 200, 12),RECOM(1000,200,12), &
      NEEM(1000,200,12),GEPA(1000,200),NEEA(1000,200)
      
! BYLAND ok    
      REAL RUNLAND,ETLAND,GEPLAND,NEELAND     
      COMMON/BYLAND/ RUNLAND(50,200,12,10), &
        ETLAND(50, 200,12,10), GEPLAND(50,200,12,10),NEELAND(50,200,12,10) 

! HUCPETAET
      REAL HUCAET, HUCPET,HUCPAET
      COMMON/HUCPETAET/HUCAET(1000,200), HUCPET(1000,200),&
      HUCPAET(1000,200)

! LANDCHANGE  ok   
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


      INTEGER TUN1,TUN2
           
      INTEGER I,ID,ICELL,ICOUNT,IYEAR,MONTHD(12),MONTHL(12)
      INTEGER YEAR,NDAY,IM,MNDAY

      CHARACTER PRESS,DUMY
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
!      READ(*,20) PRESS
      PRESS = "Y"
	IF (press.eq."y" .or.press.eq."Y") THEN
   20 FORMAT(A1)
      WRITE(*,30)
   30 FORMAT('       *** PROGRAM IS RUNNING, PLEASE WAIT ***')

!--Open Input files----------------------------------------------
    
      OPEN(1,FILE='D:\YUN\Baidu_sina\WaSSI_month\Inputs\GENERAL.TXT')
      OPEN(2,FILE='D:\YUN\Baidu_sina\WaSSI_month\Inputs\CELLINFO.TXT') 

      OPEN(4,FILE='D:\YUN\Baidu_sina\WaSSI_month\Inputs\CLIMATE.TXT')

      OPEN(7,FILE='D:\YUN\Baidu_sina\WaSSI_month\Inputs\SOILINFO.TXT')
      OPEN(8,FILE='D:\YUN\Baidu_sina\WaSSI_month\Inputs\LANDLAI.TXT')

      OPEN(11,FILE='D:\YUN\Baidu_sina\WaSSI_month\Inputs\HUCAREA.TXT')
      OPEN(22,FILE='D:\YUN\Baidu_sina\WaSSI_month\Inputs\V_FLOW.TXT')


! ---Open Output files---------------------------------------- 
 
       
      OPEN(77,FILE='D:\YUN\Baidu_sina\WaSSI_month\Outputs\BASICOUT.TXT')
      
     
      OPEN(78,FILE='D:\YUN\Baidu_sina\WaSSI_month\Outputs\MONTHFLOW.TXT')
      OPEN(79,FILE='D:\YUN\Baidu_sina\WaSSI_month\Outputs\ANNUALFLOW.TXT')
      OPEN(80,FILE='D:\YUN\Baidu_sina\WaSSI_month\Outputs\HUCFLOW.TXT')
      OPEN(99,FILE='D:\YUN\Baidu_sina\WaSSI_month\Outputs\ceshi.TXT')
      OPEN(400,FILE='D:\YUN\Baidu_sina\WaSSI_month\Outputs\MONTHCARBON.TXT')
      OPEN(500,FILE='D:\YUN\Baidu_sina\WaSSI_month\Outputs\ANNUALCARBON.TXT')
      OPEN(600,FILE='D:\YUN\Baidu_sina\WaSSI_month\Outputs\HUCCARBON.TXT')
      OPEN(700,FILE='D:\YUN\Baidu_sina\WaSSI_month\Outputs\ANNUALBIO.TXT')
      OPEN(800,FILE='D:\YUN\Baidu_sina\WaSSI_month\Outputs\HUCBIO.TXT')    
      OPEN(900,FILE='D:\YUN\Baidu_sina\WaSSI_month\Outputs\SOILSTORAGE.TXT')
      OPEN(910,FILE='D:\YUN\Baidu_sina\WaSSI_month\Outputs\RUNOFFBYLANDUSE.TXT')
      OPEN(920,FILE='D:\YUN\Baidu_sina\WaSSI_month\Outputs\FLOWVOLBYLANDUSE.TXT')     
      OPEN(930,FILE='D:\YUN\Baidu_sina\WaSSI_month\Outputs\RUNOFFBYLANDUSE_m.TXT')
      OPEN(940,FILE='D:\YUN\Baidu_sina\WaSSI_month\Outputs\FLOWVOLBYLANDUSE_m.TXT')
        
!      OPEN(1000,FILE='D:\YUN\Baidu_sina\WaSSI_month\Outputs\RUNLAND.TXT')
! --- READ INPUT DATA FILES (WARMUP.FOR)
       OPEN(2002,FILE='D:\YUN\Baidu_sina\WaSSI_month\Outputs\DATA_V_F.TXT') 
       OPEN(2003,FILE='D:\YUN\Baidu_sina\WaSSI_month\Outputs\VALIDATION.TXT')  
       
!  --------- Read input data -------------------------------
       
      CALL RPSDF       ! Set up column headings for each output files

      CALL RPSINT      ! Read Landuse, elevation and Soil parameters
          
      CALL RPSWATERUSE  ! Read HUC area, elevation, and slope
      
      CALL RPSLAI     ! Read LAI data
      
      CALL RPSCLIMATE  ! Read calimate data

      CALL RPSVALID   ! Read Runoff validation data


!----------------------Auto calibration part------------------------------------    

!--------------------------------------------------------------------------------------------      

! --- START SIMULATION LOOPS
      VAL_L1=0
      VAL_L2=0
!      TUN=2
!       PRINT*,"请输入“变量起始值”, “变量增量”, “模拟次数”的值"
!       PRINT*,"以逗号或者回车隔开"
!      READ (*,*) VAL_L1,VAL_L2,TUN
!      PRINT*,VAL_L1,VAL_L2,TUN


      DO 1111 TUN1=1, 1
        VAL_L1=VAL_L1+0.
        VAL_1(TUN1)=VAL_L1
        VAL_L2=0
      DO 1112 TUN2=1, 1
        VAL_L2=VAL_L2+0.0
        VAL_2(TUN2)=VAL_L2
!        TUN=TUN+1 

!      WRITE(77,2051)
!2051  FORMAT(/'SOIL PARAMETERS FOR EACH SIMULATION CELL'/)
!
!
!! ----Read Soil parameters from SOILINFO.TXT
      READ (7,550) DUMY
550   FORMAT (30A8)

      WRITE (77,550) DUMY

      DO 15 I=1, NGRID

      READ(7,*) ID, HUCNO(I), UZTWM(I), UZFWM(I), UZK(I), ZPERC(I),&
     REXP(I), LZTWM(I), LZFSM(I), LZFPM(I), LZSK(I),&
     LZPK(I), PFREE(I)
            
!      WRITE(77,1150) HUCNO(I), UZTWM(I), UZFWM(I), UZK(I), ZPERC(I),&
!     REXP(I), LZTWM(I), LZFSM(I), LZFPM(I), LZSK(I),&
!     LZPK(I), PFREE(I)
!     
!1150  FORMAT(I12, 11F10.4)

! ----Assign Soil parameters for each HRU by artification
           
!      UZTWM(I)=UZTWM(I)*22 
!!      UZFWM(I)=22
!      UZK(I)=UZK(I)*22 
!      ZPERC(I)=ZPERC(I)*22
!!      REXP(I)=REXP(I)
!      LZTWM(I)=LZTWM(I)*22 
!!      LZFSM(I)=36
!      LZFPM(I)=LZFPM(I)*1.5
!      LZSK(I)= LZSK(I)*1
!      LZPK(I)=LZPK(I)*2
!      PFREE(I)=0.2

      UZTWM(I)=30 
      UZFWM(I)=22
      UZK(I)=0.15 
      ZPERC(I)=80
      REXP(I)=2.4
      LZTWM(I)=162
      LZFSM(I)=36
      LZFPM(I)=65
      LZSK(I)= 0.06
      LZPK(I)=0.016
      PFREE(I)=0.2



15    CONTINUE

!----------------------Modelling for each HRU and year start------------------------------------   
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
               
                 
               CALL WATERBAL(TUN1,TUN2,ICELL, IYEAR, IM, MNDAY) ! Caculate MONTHLY GPP and ET
 
400         CONTINUE 
 
!C     WRITE MONTHLY WATER BALANCE OUTPUT TO MONTHRUNOFF.TXT            
!C     WRITE MONTHLY SOIL STORAGE OUTPUT TO SOILSTORAGE.TXT             
!C     CALCULATE TOTAL ANNUAL RAIN, PET, AET, DISCHARGE, INT, SNOWP  
!C     PRINT ANNUAL WATER BALANCE COMPONENTS TO ANNUALFLOW.TXT  
!
!c     CALCULATE R FACTOR AND OUTPUT TO ANNUALFLOW.TXT           

            CALL OUTPUT(ICELL,IYEAR) ! Output Annual water and carbon balances
            
300      CONTINUE

!C     CALCULATE AVERAGE WATER BALANCE COMPONENTS FROM IYSTART TO IYEND
!C     WRITE TO SUMMARRUNOFF.TXT   
!                                 
    
         CALL SUMMARY(ICELL)
           

200   CONTINUE
        
                
            PRINT *, 'WATER BALANCE SECTION SUCCEEDED!'                      
!
!C     SIMULATE TOTAL FLOW FOR EACH MONTH AND EACH HUC                  
!C     WRITE ACCUMULATED FLOW TO MONTHACCFLOW.TXT                       
!C     PERFORM WATER SUPPLY/DEMAND AND WASSI CALCULATIONS               
!C     WRITE WASSI OUTPUT TO ANNUALWaSSI.TXT                            
!C     WRITE WASSI OUTPUT TO HUCWaSSI.TXT                               
!
!c            CALL FLOWROUTING
!                    
!c            PRINT *, 'FLOW ROUTING DONE'
!            
!C     Simulate GEP AND NEE for selected HUC                            
!C     WRITE GEP AND NEE TO MONTHCARBON.TXT, ANNUALCARBON.TXT, HUCCARBON.TXT        
!C     SIMULATE BIODIVERSITY FOR SELECTED HUC                           
!C     WRITE BIODIVERSITY TO HUCBIO.TXT                                 


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

