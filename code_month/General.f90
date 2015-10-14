 ! This is the latest version of Watershed daily WASSIC 

 !******************Defining Common Variables for the whole program********
 Module common_var
      implicit none
	  
! Grid numbers
      INTEGER MAX_GRIDS, MAX_YEARS,MAX_VEGS
      PARAMETER (MAX_GRIDS=30,MAX_YEARS=70,MAX_VEGS=12)	  
	  	  
! BASIC ok
      INTEGER NGRID,NYEAR,NLC,BYEAR,IYSTART,IYEND
      COMMON/BASIC/NGRID,NYEAR,NLC,BYEAR,IYSTART,IYEND

! VAL ok
      REAL VAL_1, VAL_2, VAL_3,VAL_4,VAL_5,VAL_6 
      COMMON/VAL/VAL_1(100), VAL_2(100), VAL_3,VAL_4,VAL_5,VAL_6

! VALID ok
      REAL GEP_V,ET_V,GPP_V,NPP_V,FLOW_V,FLOW,RUNOFF_V,RUN_OFF,BASEFLOW_V,BASE_FLOW
        
      COMMON/VALID/ GEP_V(MAX_GRIDS,MAX_YEARS,12), ET_V(MAX_GRIDS,MAX_YEARS,12)&
          ,GPP_V(MAX_GRIDS,MAX_YEARS),NPP_V(MAX_GRIDS,MAX_YEARS), FLOW_V(MAX_YEARS,12),FLOW(MAX_YEARS,12)&
          ,RUNOFF_V(MAX_YEARS,12),RUN_OFF(MAX_YEARS,12), &
          BASEFLOW_V(MAX_YEARS,12),BASE_FLOW(MAX_YEARS,12)

! CELLINFO ok
      REAL LADUSE,LATUDE,LONGI,HUCELE
      INTEGER HUCNO
      Double Precision HUCAREA
      COMMON/CELLINFO/LADUSE(MAX_GRIDS,MAX_VEGS),HUCNO(MAX_GRIDS),LATUDE(MAX_GRIDS),LONGI(MAX_GRIDS),&
	  HUCELE(MAX_GRIDS),HUCAREA(MAX_GRIDS)


! OUTPUT1 ok

		REAL PET,PAET,AET,RUNOFF,BASEFLOW,PRIBF,SECBF,INTF,&
         RUN_HRU,BASE_HRU,APET,APAET,A_ET,P_ET,Sun_ET
		COMMON/OUTPUT1/ PET(MAX_YEARS,12,MAX_VEGS),PAET(MAX_YEARS,12,MAX_VEGS),AET(MAX_GRIDS,MAX_YEARS, 12),&
          RUNOFF(MAX_GRIDS,MAX_YEARS,12) ,BASEFLOW(MAX_GRIDS,MAX_YEARS,12),PRIBF(MAX_GRIDS,MAX_YEARS,12),&
		  SECBF(MAX_GRIDS,MAX_YEARS,12),&
         INTF(MAX_GRIDS,MAX_YEARS,12), RUN_HRU(MAX_GRIDS,MAX_YEARS,12),BASE_HRU(MAX_GRIDS,MAX_YEARS,12)&
		,APET(MAX_GRIDS,MAX_YEARS,12),APAET(MAX_GRIDS,MAX_YEARS,12),A_ET(MAX_GRIDS,MAX_YEARS,12),&
		P_ET(MAX_GRIDS,MAX_YEARS,12),Sun_ET(MAX_GRIDS,MAX_YEARS,12)
	
 
! CLIMATE  ok    
      REAL  RAIN,TEMP,AAPPT
      COMMON/CLIMATE/ RAIN(MAX_GRIDS,MAX_YEARS,12), TEMP(MAX_GRIDS,MAX_YEARS,12), AAPPT(MAX_GRIDS)
      
! LAI  ok      
      REAL LAI_VEG
	  INTEGER LAI_S_Y,LAI_E_Y
      COMMON/LAI/LAI_VEG(MAX_GRIDS,MAX_YEARS,12,MAX_VEGS),LAI_S_Y,LAI_E_Y

! SNOWPACK	ok
      REAL SP,SNOWPACK
      INTEGER NSPM
      COMMON/SNOWPACK/SP(MAX_GRIDS,MAX_YEARS,12),SNOWPACK, NSPM(MAX_GRIDS,MAX_YEARS)

! SUMMARY1 
      REAL ANURAIN,ANURUN,ANUPET,ANUAET,ANUPAET
      COMMON/SUMMARY1/ANURAIN(MAX_GRIDS,MAX_YEARS),ANURUN(MAX_GRIDS,MAX_YEARS),ANUPET(MAX_GRIDS,MAX_YEARS),&
	  ANUAET(MAX_GRIDS,MAX_YEARS),ANUPAET(MAX_GRIDS,MAX_YEARS)

! SOIL parameters input 
      REAL LZTWM, LZFPM, LZFSM,LZSK,LZPK, UZTWM, UZFWM, UZK, ZPERC,&
        REXP, PFREE        
      COMMON/SOIL/LZTWM(MAX_GRIDS), LZFPM(MAX_GRIDS), LZFSM(MAX_GRIDS), LZSK(MAX_GRIDS),LZPK(MAX_GRIDS), UZTWM(MAX_GRIDS), &
        UZFWM(MAX_GRIDS),UZK(MAX_GRIDS), ZPERC(MAX_GRIDS),REXP(MAX_GRIDS), PFREE(MAX_GRIDS)
		
		
! Soil Mositure (AV-average,EM-end of month)
		REAL AVSMC,AVUZTWC,AVUZFWC,AVLZTWC,AVLZFPC,AVLZFSC,EMSMC,EMUZTWC,EMUZFWC,EMLZTWC,EMLZFPC,EMLZFSC
	   COMMON/SMC/AVSMC(MAX_GRIDS,MAX_YEARS,12),AVUZTWC(MAX_GRIDS,MAX_YEARS,12), AVUZFWC(MAX_GRIDS,MAX_YEARS,12), &
	   AVLZTWC(MAX_GRIDS,MAX_YEARS,12), AVLZFPC(MAX_GRIDS,MAX_YEARS,12),AVLZFSC(MAX_GRIDS,MAX_YEARS,12), &
	   EMSMC(MAX_GRIDS,MAX_YEARS,12),EMUZTWC(MAX_GRIDS,MAX_YEARS,12),EMUZFWC(MAX_GRIDS,MAX_YEARS,12),&
	   EMLZTWC(MAX_GRIDS,MAX_YEARS,12), EMLZFPC(MAX_GRIDS,MAX_YEARS,12),EMLZFSC(MAX_GRIDS,MAX_YEARS,12)
	   
! FLOW ok
      REAL STRFLOW,STRET,STRGEP     
     COMMON/FLOW/STRFLOW(MAX_GRIDS,MAX_YEARS,12),STRET(MAX_GRIDS,MAX_YEARS,12)&
     ,STRGEP(MAX_GRIDS,MAX_YEARS,12)

! CARBON ok
        REAL GEPM,RECOM,NEEM,GEPA,NEEA
      COMMON/CARBON/ GEPM(MAX_GRIDS,MAX_YEARS,12),RECOM(MAX_GRIDS,MAX_YEARS,12), &
      NEEM(MAX_GRIDS,MAX_YEARS,12),GEPA(MAX_GRIDS,MAX_YEARS),NEEA(MAX_GRIDS,MAX_YEARS)
      
! BYLAND ok    
     
		REAL,ALLOCATABLE :: RUNLAND(:,:,:,:,:),ETLAND(:,:,:,:,:),GEPLAND(:,:,:,:,:)&
		,NEELAND(:,:,:,:,:)

	 ! REAL RUNLAND,ETLAND,GEPLAND,NEELAND     
      ! COMMON/BYLAND/ RUNLAND(MAX_GRIDS,MAX_YEARS,12,31,MAX_VEGS), &
        ! ETLAND(MAX_GRIDS,MAX_YEARS,12,31,MAX_VEGS), GEPLAND(MAX_GRIDS,MAX_YEARS,12,31,MAX_VEGS),NEELAND(MAX_GRIDS,MAX_YEARS,12,31,MAX_VEGS) 

! HUCPETAET
		REAL HUCAET, HUCPET,HUCPAET
		COMMON/HUCPETAET/HUCAET(MAX_GRIDS,MAX_YEARS), HUCPET(MAX_GRIDS,MAX_YEARS),&
		HUCPAET(MAX_GRIDS,MAX_YEARS)

! LANDCHANGE  ok   
      REAL FPERD, FPERDLAI  
      COMMON/LANDCHANGE/FPERD,FPERDLAI
        
! R	  
	  REAL RFACTOR
      COMMON/R/ RFACTOR(MAX_GRIDS,MAX_YEARS)
	  
	  
      end

!**************---Program Starting--------*************************	  
      PROGRAM WaSSICBZB   
	  
      use common_var
      implicit none 


      INTEGER TUN1,TUN2,selection
           
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
            '                   *** Revised Watershed WaSSI-CB by Ning Liu  ***'//,&
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


   print*, "please input 1 for simulating in MAC"

    READ(*,*) selection

   IF (selection==1) then
   

   !--Open Input files----------------------------------------------
    
      OPEN(1,FILE='E:\Github\Watershed\inputs\GENERAL.TXT')
      OPEN(2,FILE='E:\Github\Watershed\inputs\CELLINFO.TXT') 

      OPEN(4,FILE='E:\Github\Watershed\inputs\CLIMATE.TXT')

!      OPEN(7,FILE='E:\Github\Watershed\inputs\SOILINFO.TXT')
      OPEN(8,FILE='E:\Github\Watershed\inputs\LANDLAI.TXT')

      OPEN(11,FILE='E:\Github\Watershed\inputs\HUCAREA.TXT')
      OPEN(22,FILE='E:\Github\Watershed\inputs\V_FLOW.TXT')


! ---Open Output files---------------------------------------- 
 
       
      OPEN(77,FILE='E:\Github\Outputs_W\BASICOUT.TXT')
      OPEN(78,FILE='E:\Github\Outputs_W\MONTHFLOW.TXT')
      OPEN(79,FILE='E:\Github\Outputs_W\ANNUALFLOW.TXT')
      OPEN(80,FILE='E:\Github\Outputs_W\HUCFLOW.TXT')
      OPEN(99,FILE='E:\Github\Outputs_W\ceshi.TXT')
      OPEN(400,FILE='E:\Github\Outputs_W\MONTHCARBON.TXT')
      OPEN(500,FILE='E:\Github\Outputs_W\ANNUALCARBON.TXT')
      OPEN(600,FILE='E:\Github\Outputs_W\HUCCARBON.TXT')
      OPEN(700,FILE='E:\Github\Outputs_W\ANNUALBIO.TXT')
      OPEN(800,FILE='E:\Github\Outputs_W\HUCBIO.TXT')    
      OPEN(900,FILE='E:\Github\Outputs_W\SOILSTORAGE.TXT')
      OPEN(910,FILE='E:\Github\Outputs_W\RUNOFFBYLANDUSE.TXT')
      OPEN(920,FILE='E:\Github\Outputs_W\FLOWVOLBYLANDUSE.TXT')     
      OPEN(930,FILE='E:\Github\Outputs_W\RUNOFFBYLANDUSE_m.TXT')
      OPEN(940,FILE='E:\Github\Outputs_W\FLOWVOLBYLANDUSE_m.TXT')
        
!      OPEN(1000,FILE='D:\YUN\Baidu_sina\WASSICBZB\Outputs_W\RUNLAND.TXT')
! --- READ INPUT DATA FILES (WARMUP.FOR)
       OPEN(2002,FILE='E:\Github\Outputs_W\DATA_V_F.TXT') 
       OPEN(2003,FILE='E:\Github\Outputs_W\VALIDATION.TXT') 

   ELSE

   !--Open Input files----------------------------------------------
    
      OPEN(1,FILE='inputs1\GENERAL.TXT')
      OPEN(2,FILE='inputs1\CELLINFO.TXT') 

      OPEN(4,FILE='inputs1\CLIMATE0.TXT')

!      OPEN(7,FILE='inputs\SOILINFO.TXT')
      OPEN(8,FILE='inputs1\LANDLAI.TXT')

      OPEN(11,FILE='inputs1\HUCAREA.TXT')
      OPEN(22,FILE='inputs1\V_FLOW.TXT')


! ---Open Output files---------------------------------------- 
 
       
      OPEN(77,FILE='..\Outputs_W\BASICOUT.TXT')
      OPEN(78,FILE='..\Outputs_W\MONTHFLOW.TXT')
      OPEN(79,FILE='..\Outputs_W\ANNUALFLOW.TXT')
      OPEN(80,FILE='..\Outputs_W\HUCFLOW.TXT')
      OPEN(99,FILE='..\Outputs_W\ceshi.TXT')
      OPEN(400,FILE='..\Outputs_W\MONTHCARBON.TXT')
      OPEN(500,FILE='..\Outputs_W\ANNUALCARBON.TXT')
      OPEN(600,FILE='..\Outputs_W\HUCCARBON.TXT')
      OPEN(700,FILE='..\Outputs_W\ANNUALBIO.TXT')
      OPEN(800,FILE='..\Outputs_W\HUCBIO.TXT')    
      OPEN(900,FILE='..\Outputs_W\SOILSTORAGE.TXT')
      OPEN(910,FILE='..\Outputs_W\RUNOFFBYLANDUSE.TXT')
      OPEN(920,FILE='..\Outputs_W\FLOWVOLBYLANDUSE.TXT')     
      OPEN(930,FILE='..\Outputs_W\RUNOFFBYLANDUSE_m.TXT')
      OPEN(940,FILE='..\Outputs_W\FLOWVOLBYLANDUSE_m.TXT')
        
!      OPEN(1000,FILE='D:\YUN\Baidu_sina\WASSICBZB\Outputs_W\RUNLAND.TXT')
! --- READ INPUT DATA FILES (WARMUP.FOR)
       OPEN(2002,FILE='..\Outputs_W\DATA_V_F.TXT') 
       OPEN(2003,FILE='..\Outputs_W\VALIDATION.TXT') 

   ENDIF

 
       
!  --------- Read input data -------------------------------
       
      CALL RPSDF       ! Set up column headings for each output files

      CALL RPSINT      ! Read Landuse, elevation and Soil parameters
          
      CALL RPSWATERUSE  ! Read HUC area, elevation, and slope
      
      print*,"finish read Land cover  data"
	  
      CALL RPSLAI     ! Read LAI data
      
      print*,"finish read LAI  data"
	  
      CALL RPSCLIMATE  ! Read calimate data

      print*,"finish read Climate data"

!----------------------Auto calibration part------------------------------------    


! --- START SIMULATION LOOPS
      VAL_L1=-1
      
!      TUN=2
!       PRINT*,"请输入“变量起始值”, “变量增量”, “模拟次数”的值"
!       PRINT*,"以逗号或者回车隔开"
!      READ (*,*) VAL_L1,VAL_L2,TUN
!      PRINT*,VAL_L1,VAL_L2,TUN


      DO 1111 TUN1=1, 1
        VAL_L1=VAL_L1+0.3
        VAL_1(TUN1)=VAL_L1
        VAL_L2=-1
      DO 1112 TUN2=1, 1
        VAL_L2=VAL_L2+0.1
        VAL_2(TUN2)=VAL_L2
!        TUN=TUN+1 

      WRITE(77,2051)
2051  FORMAT(/'SOIL PARAMETERS FOR EACH SIMULATION CELL'/)


! ----Read Soil parameters from SOILINFO.TXT

	IF (selection==1) then
		
		OPEN(7,FILE='E:\Github\Watershed\inputs1\SOILINFO_BNU.TXT')

	ELSE
	
		OPEN(7,FILE='inputs\SOILINFO_BNU.TXT')
		
	ENDIF

      read (7,550) dumy
550   format (30a8)

      write (77,550) dumy
!
      DO 15 I=1, NGRID
!
      READ(7,*) ID, HUCNO(I), UZTWM(I), UZFWM(I), UZK(I), ZPERC(I),&
     REXP(I), LZTWM(I), LZFSM(I), LZFPM(I), LZSK(I),&
     LZPK(I), PFREE(I)
            
      WRITE(77,1150) HUCNO(I), UZTWM(I), UZFWM(I), UZK(I), ZPERC(I),&
     REXP(I), LZTWM(I), LZFSM(I), LZFPM(I), LZSK(I),&
     LZPK(I), PFREE(I)
     
1150  FORMAT(I12, 11F10.4)

! ----Assign Soil parameters for each HRU by artification
           
!        UZTWM(I)=UZTWM(I)*0.3 !1.15 
!       UZFWM(I)=UZFWM(I)*1.1
!       UZK(I)=UZK(I) *0.2!*1.85
!       ZPERC(I)=ZPERC(I)!
!       REXP(I)=REXP(I)*0.6 !*0.9
!       LZTWM(I)=LZTWM(I) !*1.3
!     LZFSM(I)=LZFSM(I) ! *1.15!*
!      LZFPM(I)=LZFPM(I)*3 ! *2.05   ! 2.4 !
!      LZSK(I)= LZSK(I)*1.25 !  *0.9 !
!       LZPK(I)=LZPK(I) *2 ! !*(1+VAL_2(TUN2)) !*1.2
!      PFREE(I)=PFREE(I) !*(1+VAL_1(TUN1)) !*0.4 !
!! !----------------------
!  !      UZTWM(I)=UZTWM(I) !
!  !     UZFWM(I)=UZFWM(I)*1
!  !     UZK(I)=UZK(I)*1 
!   !    ZPERC(I)=ZPERC(I)*1
!    !   REXP(I)=REXP(I)*1
!       LZTWM(I)=LZTWM(I)*1.3 !*(1+VAL_1(TUN1)) 
!    !  LZFSM(I)=LZFSM(I) !*
!       LZFPM(I)=LZFPM(I)*2.4
!       LZSK(I)= LZSK(I)*0.9 !*(1+VAL_2(TUN2)) 
!       LZPK(I)=LZPK(I)*1.2
!!      PFREE(I)=PFREE(I) !


15    CONTINUE
    close(7) !Close Soil input file
	
!---------------------END of auto calibration-----------------------------------------------------------------------      

! ----   Allocates array RUNLAND,

	Print*,  NGRID,NYEAR,"12,31",MAX_VEGS
      ALLOCATE (RUNLAND(NGRID,NYEAR,12,31,MAX_VEGS))
      ALLOCATE (ETLAND(NGRID,NYEAR,12,31,MAX_VEGS))
      ALLOCATE (GEPLAND(NGRID,NYEAR,12,31,MAX_VEGS))
      ALLOCATE (NEELAND(NGRID,NYEAR,12,31,MAX_VEGS))
	

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

1112   CONTINUE ! calibration VAL/VAL_1
1111   CONTINUE ! calibration VAL/VAL_2
 

            CALL FLOWBYLAND      

  
            PRINT *, '-------------PROGRAM RUN ENDS----------------!'
		Stop
      ELSE
    	    	STOP 
	ENDIF		
      END

