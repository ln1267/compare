!**********************************************************************C
!                                                                      C
!     *** SUBROUTINE WATERBAL ***                                      C
!     SIMULATES MONTHLY WATER BALANCE USING 2 LAYER SOIL MOISTURE      C
!     ALGORITHM FROM NOAA NATIONAL WEATHER SERVICE SACRAMENTO SOIL     C
!     MOISTURE ACCOUNTING MODEL (SAC-SMA)                              C
!     Line 591 --- Carbon model                                        C
!     Line 941 --- Area for each cell                                  C
!**********************************************************************C
      
      SUBROUTINE WATERBAL(TUN1,TUN2,I,J,M,MNDAY)
        
        Use Common_var
        implicit none                                     
   
! ----------------------------------------------------------------------------     
      INTEGER I,J,M,K,IAM,DAY,MDAY,MNDAY,TUN1,TUN2
      
      REAL AETTEMP, RUNOFFTEMP, PBFTEMP, SBFTEMP,IFTEMP, GEPTEMP,&
            RECOTEMP, NEETEMP
      
	  REAL UZTWC(MAX_VEGS), UZFWC(MAX_VEGS), LZTWC(MAX_VEGS), LZFSC(MAX_VEGS), LZFPC(MAX_VEGS) ! soil moisture content parameters 
	  
      REAL ETUZTW(MAX_YEARS,12,MAX_VEGS), RESIDET(MAX_YEARS,12,MAX_VEGS), ETUZFW(MAX_YEARS,12,MAX_VEGS)
      
      REAL ETLZTW(MAX_YEARS,12,MAX_VEGS), RATLZT(MAX_VEGS), RATLZ(MAX_VEGS)
	   
	  REAL ET(MAX_YEARS,12,MAX_VEGS), SURFRO(MAX_VEGS), GEP(MAX_YEARS,12,MAX_VEGS), INFIL(MAX_VEGS),&
       RECO(MAX_YEARS,12,MAX_VEGS), NEE(MAX_YEARS,12,MAX_VEGS) 	
      
	  REAL UZRAT(MAX_VEGS), TWX(MAX_VEGS), PERCM(MAX_VEGS), PERC(MAX_VEGS), DEFR(MAX_VEGS), LZDEF(MAX_VEGS) 
      
      REAL PERCT(MAX_VEGS), PERCF(MAX_VEGS)
      
      REAL HPL(MAX_VEGS), RATLP(MAX_VEGS), RATLS(MAX_VEGS), FRACP(MAX_VEGS), PERCP(MAX_VEGS),PERCS(MAX_VEGS)
      
      REAL PBF(MAX_VEGS), SBF(MAX_VEGS), INF(MAX_VEGS)	   
	 
	  REAL SNOW, SNOWW
      
      REAL LTASM,TAREA
      
      REAL DPAET
      
      REAL TAUZTWC, TAUZFWC, TALZTWC, TALZFPC, TALZFSC
     
      REAL TASM,AUZTWC,AUZFWC,ALZTWC,ALZFPC,ALZFSC,ASM           
     
      INTEGER GEPFLAG
	  
	  LOGICAL Wether_Winter
         
! ----------------------------------------------------------------------------     
     
        
! *****************************************************************************************************

! *****************************************************************************************************
! --- INITIALIZE VARIABLES FOR START OF SIMULATION


             AETTEMP =0.
             RUNOFFTEMP = 0.
             PBFTEMP = 0.0
             SBFTEMP = 0.0
             IFTEMP = 0.0
                          
             GEPTEMP = 0.
             RECOTEMP = 0.
             NEETEMP =0.
			 
			Wether_Winter = .FALSE.
		 
        IF (J .EQ. 1 .AND. M .EQ. 1) THEN

        IAM =0     
        SNOWPACK=0
                  
        DO  K=1, NLC
                        
           UZTWC(K) = 0.1*UZTWM(I)
           UZFWC(K) = 0.0
           LZTWC(K) = 0.1*LZTWM(I)
           LZFSC(K) = 0.75*LZFSM(I)
           LZFPC(K) = 0.75*LZFPM(I)
           
               
		END DO
        
        ENDIF 
         
! *****************************************************************************************************
! *****************************************************************************************************
!----- SIMULATE SNOWPACK (SEE VAROSMARTY  ET AL., 1989)
      
        IF (TEMP(I,J, M) .LE.  -1.0) THEN
        
           SNOW = RAIN(I,J, M)
    
           SNOWPACK = SNOWPACK + SNOW
            IAM = 0

! -------based on Temerature calculate ET in Winter
! ------- ------------------------------------------------ 
           ! SNOWW=SNOWPACK*( VAL_1(TUN1)+ VAL_2(TUN2)*TEMP(I,J, M))
           
           ! IF (( VAL_1(TUN1)+ VAL_2(TUN2)*TEMP(I,J, M)) .ge. 1)  SNOWW=0
! !
           ! IF (SNOWW .LE. 0)  SNOWW=0
! !           
           ! SNOWPACK = SNOWPACK - SNOWW
! ------- ------------------------------------------------          
!          SNOWW=0
                                
        ELSE 
        
            IAM = IAM +1 
            
            HUCELE(I) = 1000.
            
           IF (HUCELE(I) .LE. 500.0) THEN
          
              SNOWW = SNOWPACK
              SNOWPACK = 0.
                  
           ELSE 
              
              IF (IAM .EQ. 1) THEN 
              
                 SNOWW = 0.5 * SNOWPACK
              
              ELSEIF (IAM .EQ. 2) THEN
              
                 SNOWW = SNOWPACK
                 
              ELSE
              
                 SNOWW = 0.
                 SNOWPACK = 0.
                                   
              ENDIF
              
              SNOWPACK = SNOWPACK - SNOWW
                            
          ENDIF           
        ENDIF  
                            

! *****************************************************************************************************
! *****************************************************************************************************
! *****************************************************************************************************
! -- LOOP THROUGH DAYS OF CURRENT MONTH AND CALCULATE SOIL WATER STORAGE, BASEFLOW, RUNOFF, AND AET
        
        DO 100 DAY= 1, MNDAY    
        
             TASM = 0.          
             TAREA = 0.
             TAUZTWC = 0.
             TAUZFWC = 0.
             TALZTWC = 0.
             TALZFPC = 0.
             TALZFSC = 0.


! *****************************************************************************************************
! *****************************************************************************************************
! -- LOOP THROUGH LAND COVERS IN THE HUC AND PERFORM WATER BALANCE COMPUTATIONS
! -- ASSUMES OPEN WATER LAND COVER IS NEG.
         
             K=0
          
             DO 40 K=1, NLC
                  

! *****************************************************************************************************
! -- SET ET, SURFACE RUNOFF, INTERFLOW, GEP TO ZERO IF TEMPERATURE IS LE -1.0
! -- BASEFLOW STILL OCCURS

             IF (TEMP (I,J, M) .LE. -1.0) THEN
          
                ET(J,M,K) = 0.
                SURFRO(K) = 0.
                INF(K) = 0.
                GEP (J, M, K) = 0.
				

! define whether calculate Winter RUNOFF		
		
				IF (Wether_Winter) then 

! *****************************************************************************************************               
! *****************计算温度小于-1℃的ET**********************************************    
!!       P+SOIL > PET      ET=PET   
!!       P+SOIL < PET      ET=P
! *****************************************************************************************************       
!             DPAET=0.0
!             DPAET=PAET(J,M,K)/MNDAY
!
!
!            IF (UZTWC(K) .GT. DPAET) THEN 
!                ET(J,M,K)=DPAET
!                UZTWC(K)=UZTWC(K)-ET(J,M,K)
!              ELSE
!                ET(J,M,K)=0
!            ENDIF
!
!       
! *****************************************************************************************************
! -- COMPUTE THE DAILY AVERAGE INFILTRATION FOR A GIVEN MONTH FOR EACH LAND USE
!---流域植被类型日水分输入量=降水+融雪
						INFIL(K) = SNOWW/MNDAY
                 

!     --- COMPUTE WATER IN EXCESS OF UZ TENSION WATER CAPACITY (TWX)
!
						TWX(K) = INFIL(K) + UZTWC(K) - UZTWM(I)
!
! *****************************************************************************************************
! *****************************************************************************************************
! --- COMPUTE PERCOLATION INTO SOIL WATER STORAGES AND SURFACE RUNOFF
!
					  IF (TWX(K).GE.0.0) THEN           	
!     --- IF INFIL EXCEEDS UZ TENSION WATER CAPACITY, SET UZ TENSION WATER STORAGE TO CAPACITY, 
!         REMAINDER OF INFIL GOES TO UZFWC IF EXCEEDS UZFWC EXCESS GOES TO SURFACE RUNOFF
!
						 UZTWC(K) = UZTWM(I)     
					
						 UZFWC(K) = UZFWC(K) + TWX(K)
					
						 IF (UZFWC(K) .GT. UZFWM(I)) THEN
					
							SURFRO(K) = UZFWC(K) - UZFWM(I)
					
							UZFWC(K) = UZFWM(I)
					
						 ELSE
					
							SURFRO(K) = 0.0
					
						 ENDIF 

					  ELSE        	
           
!
!     --- IF INFIL DOES NOT EXCEED UZ TENSION WATER CAPACITY, ALL INFIL GOES TO UZ TENSION WATER STORAGE
	!
						 UZTWC(K) = UZTWC(K) + INFIL(K)
						 SURFRO(K) = 0.0
					
					  ENDIF
           

! --- COMPUTE PERCOLATION TO LZ IF FREE WATER IS AVAILABLE IN UZ
!
!		
					IF (UZFWC(K) .GT. 0.0) THEN
!		
!
!!     --- COMPUTE PERCOLATION DEMAND FROM LZ

						 PERCM(K) = LZFPM(K) * LZPK(I) + LZFSM(K) * LZSK(I)
					
						 PERC(K) = PERCM(K) * (UZFWC(K)/UZFWM(I))
					
						 DEFR(K)=1.0-((LZTWC(K)+LZFPC(K)+LZFSC(K))/ &
						(LZTWM(I)+LZFPM(I)+LZFSM(I)))
					
						 PERC(K) = PERC(K) * (1.0 + ZPERC(I) * (DEFR(K) &
						**REXP(I)))
				
!
!     --- COMPARE LZ PERCOLATION DEMAND TO UZ FREE WATER AVAILABLE AND COMPUTE ACTUAL PERCOLATION

						IF (PERC(K) .LT. UZFWC(K)) THEN
					
						   UZFWC(K) = UZFWC(K) - PERC(K)
					
						ELSE
					
						   PERC(K) = UZFWC(K)
					
						   UZFWC(K) = 0.0
					
						ENDIF
            
!      --- CHECK TO SEE IF PERC EXCEEDS LZ TENSION AND FREE WATER DEFICIENCY, IF SO SET PERC TO LZ DEFICIENCY


						LZDEF(K) = (LZTWC(K) + LZFPC(K) + LZFSC(K)) - &
						(LZTWM(I) + LZFPM(I) + LZFSM(I)) + PERC(K)
						
						IF (LZDEF(K) .GT. 0.0) THEN
						
						   PERC(K) = PERC(K) - LZDEF(K)
			  
						   UZFWC(K) = UZFWC(K) + LZDEF(K)
						   
						ENDIF
					
                
! --- DISRIBUTE PERCOLATED WATER INTO THE LZ STORAGES AND COMPUTE THE REMAINDER IN UZ FREE WATER STORAGE AND RESIDUAL AVAIL FOR RUNOFF
   

!    --- COMPUTE PERC WATER GOING INTO LZ TENSION WATER STORAGE AND COMPARE TO AVAILABLE STORAGE

						PERCT(K) = PERC(K) * (1.0 - PFREE(I))
                
						IF ((PERCT(K) + LZTWC(K)) .GT. LZTWM(I)) THEN
                
!    --- WHEN PERC IS GREATER THAN AVAILABLE TENSION WATER STORAGE, SET TENSION WATER STORAGE TO MAX, REMAINDER OF PERC GETS EVALUATED AGAINST FREE WATER STORAGE

						   PERCF(K) = PERCT(K) + LZTWC(K) - LZTWM(I)
					
						   LZTWC(K) = LZTWM(I)
					
						ELSE
                
!     --- WHEN PERC IS LESS THAN AVAILABLE TENSION WATER STORAGE, UPDATE TENSION WATER STORAGE

						   LZTWC(K) = LZTWC(K) + PERCT(K)
					
						   PERCF(K) = 0.0
					
						ENDIF
!                
!     --- COMPUTE TOTAL PERC WATER GOING INTO LZ FREE WATER STORAGE

						PERCF(K) = PERCF(K) + PERC(K) * PFREE(I)           

						IF(PERCF(K) .GT. 0.0) THEN
                
!     --- COMPUTE RELATIVE SIZE OF LZ PRIMARY FREE WATER STORAGE COMPARED TO LZ TOTAL FREE WATER STORAGE

						HPL(K) = LZFPM(I) / (LZFPM(I) + LZFSM(I))
                
!     --- COMPUTE LZ PRIMARY AND SECONDARY FREE WATER CONTENT TO CAPACITY RATIOS

						   RATLP(K) = LZFPC(K) / LZFPM(I)
					
						   RATLS(K) = LZFSC(K) / LZFSM(I)
                
!     --- COMPUTE FRACTIONS AND PERCENTAGES OF FREE WATER PERC TO GO TO LZ PRIMARY STORAGE

						   FRACP(K) = (HPL(K) * 2.0 * (1.0 - RATLP(K))) &
						  / ((1.0 - RATLP(K)) + (1.0 - RATLS(K)))
					
						   IF (FRACP(K) .GT. 1.0) FRACP(K) = 1.0

							  PERCP(K) = PERCF(K) * FRACP(K)
					
							  PERCS(K) = PERCF(K) - PERCP(K)
                
!     --- COMPUTE NEW PRIMARY AND SECONDARY STORAGE

!         --- COMPUTE NEW SECONDARY FREE WATER STORAGE

							  LZFSC(K) = LZFSC(K) + PERCS(K)

							IF(LZFSC(K) .GT. LZFSM(I)) THEN
                
!         --- IF NEW SECONDARY FREE WATER STORAGE EXCEEDS CAPACITY SET SECONDARY STORAGE TO CAPACITY AND EXCESS GOES TO PRIMARY FREE WATER STORAGE

								 PERCS(K) = PERCS(K) - LZFSC(K) + LZFSM(I)
					
								 LZFSC(K) = LZFSM(I)
							  
							ENDIF
                
            
!        --- IF NEW LZ SECONDARY FREE WATER STORAGE IS LESS THAN CAPACITY MOVE ON TO COMPUTE NEW PRIMARY FREE WATER STORAGE


							LZFPC(K) = LZFPC(K) + (PERCF(K) - PERCS(K))

					
							IF (LZFPC(K) .GT. LZFPM(I)) THEN

!             --- IF LZ FREE PRIMARY WATER STORAGE EXCEEDS CAPACITY SET PRIMARY STORAGE TO CAPACITY AND EVALUATE EXCESS AGAINST LZ TENSION WATER STORAGE

							  LZTWC(K) = LZTWC(K) + LZFPC(K) - LZFPM(I)
					
							  LZFPC(K) = LZFPM(I)
					
								IF (LZTWC(K) .GT. LZTWM(I)) THEN

!           --- IF LZ TENSION WATER EXCEEDS CAPACITY EVALUATE EXCESS AGAINST UZ FREE WATER CAPACITY AND SET LZ TENSION WATER STORAGE TO CAPACITY

								 UZFWC(K) = UZFWC(K) + LZTWC(K) - LZTWM(I)
					
								 LZTWC(K) = LZTWM(I)
								 
								ENDIF
							  
							ENDIF
						   
						ENDIF
					
					ENDIF
		 
! ***************************************************************************************************** 
! *****************************************************************************************************                
! --- COMPUTE BASEFLOW AND UPDATE LZ PRIMARY AND SECONDARY FREE WATER STORAGES
!
!                
!      --- COMPUTE PRIMARY BASEFLOW AND COMPARE TO AVAILABLE FREE PRIMARY STORAGE

					 PBF(K) = LZFPC(K) * LZPK(I)
					
					 LZFPC(K) = LZFPC(K) - PBF(K)
					
					IF (LZFPC(K) .LE. 0.0001) THEN 
					
						PBF(K) = PBF(K) + LZFPC(K)
					
						LZFPC(K) = 0.0
					
					ENDIF
                

!      --- COMPUTE SECONDARY BASEFLOW AND COMPARE TO AVAILABLE FREE PRIMARY STORAGE

					 SBF(K) = LZFSC(K) * LZSK(I)
					
					 LZFSC(K) = LZFSC(K) - SBF(K)
					
					IF (LZFSC(K) .LE. 0.0001) THEN
					
					   SBF(K) = SBF(K) + LZFSC(K)
					
					   LZFSC(K) = 0.0
					
					ENDIF                
                 

! *****************************************************************************************************
! --- COMPUTE INTERFLOW FROM UZ

					 INF(K) = UZFWC(K) * UZK(I)
					
					IF (UZFWC(K) .LT. INF(K)) THEN
					 
						INF(K) = UZFWC(K)
						
						UZFWC(K) = 0.0
					 
					ELSE
						
						UZFWC(K) = UZFWC(K) - INF(K)
					
					ENDIF
!
!	
				ELSE
		
		! -- COMPUTE PRIMARY BASEFLOW WHEN T <= -1.0
	!
					PBF(K) = LZFPC(K) * LZPK(I)
					LZFPC(K) = LZFPC(K) - PBF(K)
					IF (LZFPC(K) .LE. 0.0001) THEN 
					   PBF(K) = PBF(K) + LZFPC(K)
					   LZFPC(K) = 0.0
					ENDIF
					
	! -- COMPUTE SECONDARY BASEFLOW WHEN T <= -1.0

					SBF(K) = LZFSC(K) * LZSK(I)
					LZFSC(K) = LZFSC(K) - SBF(K)
					IF (LZFSC(K) .LE. 0.0001) THEN
					   SBF(K) = SBF(K) + LZFSC(K)
					   LZFSC(K) = 0.0
					ENDIF 

! Test Output
!
!Write(*,9881) 
!9881 Format("Check the following variables: SNOWPACK; PBF;SBF")
!Write(99,*) I,J,M,SNOWPACK,PBF(K),SBF(K) 
		

		
				ENDIF

! ***************************End of ET and Flow calculation in Winter*************************************************** 
                                        

			ELSE
                  
! **************************----Trmperature > -0.1 -------------*******************************************************
! *****************************************************************************************************
! -- COMPUTE THE DAILY AVERAGE INFILTRATION FOR A GIVEN MONTH FOR EACH LAND USE
!---流域植被类型日水分输入量=降水+融雪
                INFIL(K) = RAIN(I,J,M)/MNDAY + SNOWW/MNDAY
          
            
! *****************************************************************************************************
! --- COMPUTE AET GIVEN TOTAL WATER STORED IN UPPER SOIL LAYER STORAGES AND PAET CALCULATED IN PET.FOR
! --- ASSUME ET IS SUPPLIED ONLY FROM UPPER LAYER NO UPWARD FLUX FROM LOWER LAYER TO UPPER LAYER
! --- NOTE THAT SAC-SMA ALLOWS ET TO ALSO BE SUPPLIED UNRESTRICTED BY LZ TENSION WATER STORAGE

                
                   DPAET = PAET(J, M, K)/MNDAY
               
                   ET(J, M, K) = DPAET
!print *,ET(J,M,K)
                
!		开始计算植被类型K 每天的ET、
! --- COMPUTE ET FROM UZ TENSION WATER STORAGE, RECALCULATE UZTWC, CALCULATE RESIDUAL ET DEMAND

                   ETUZTW(J, M, K) = ET(J, M, K) * (UZTWC(K)/UZTWM(I))
                   
                   RESIDET(J, M, K) = ET(J, M, K) - ETUZTW(J, M, K)
                   
                   UZTWC(K) = UZTWC(K) - ETUZTW(J, M, K)
                   
                   ETUZFW(J, M, K) = 0.0
                   
                   IF (UZTWC(K).GE.0.0) GOTO 220
                   
                   ETUZTW(J, M, K) = ETUZTW(J, M, K) + UZTWC(K)
                   
                   UZTWC(K) = 0.0
                   
                   RESIDET(J, M, K) = ET(J, M, K) - ETUZTW(J, M, K)
                   
! --- COMPUTE ET FROM UZ FREE WATER STORAGE, RECALCULATE UZFWC, CALCULATE RESIDUAL ET DEMAND                   
                   
                   IF (UZFWC(K) .GE. RESIDET(J, M, K)) GO TO 221
                   
                   ETUZFW(J, M, K) = UZFWC(K)
                   
                   UZFWC(K) = 0.0
                   
                   RESIDET(J, M, K) = RESIDET(J, M, K) - ETUZFW(J, M, K)
                   
                   GO TO 225
                   
221                ETUZFW(J, M, K) = RESIDET(J, M, K)

                   UZFWC(K) = UZFWC(K) - ETUZFW(J, M, K)
                   
                   RESIDET(J, M, K) = 0.0
                   
! --- REDISTRIBUTE WATER BETWEEN UZ TENSION WATER AND FREE WATER STORAGES

220                IF((UZTWC(K)/UZTWM(I)).GE.(UZFWC(K)/UZFWM(I))) & 
                  GO TO 225

                   UZRAT(K)=(UZTWC(K)+UZFWC(K))/(UZTWM(I)+UZFWM(I))
                      
                   UZTWC(K) = UZTWM(I) * UZRAT(K)
                      
                   UZFWC(K) = UZFWM(I) * UZRAT(K)
                        
225                IF (UZTWC(K) .LT. 0.00001) UZTWC(K) = 0.0

                   IF (UZFWC(K) .LT. 0.00001) UZFWC(K) = 0.0
                   
                   
! --- COMPUTE ET FROM LZ TENSION WATER STORAGE, RECALCULATE LZTWC, CALCULATE RESIDUAL ET DEMAND

                   ETLZTW(J, M, K) = RESIDET(J, M, K) * (LZTWC(K) / &
                  (UZTWM(I) + LZTWM(I)))
                   
                   LZTWC(K) = LZTWC(K) - ETLZTW(J, M, K)
                   
                   IF(LZTWC(K) .GE. 0.0) GO TO 226
                   
                   ETLZTW(J, M, K) = ETLZTW(J, M, K) + LZTWC(K)
                   
                   LZTWC(K) = 0.0
                   
226                RATLZT(K) = LZTWC(K) / LZTWM(I)

					RATLZ(K)= (LZTWC(K) + LZFPC(K) + LZFSC(K))/&
					(LZTWM(I) + LZFPM(I) + LZFSM(I))
	 
	 
                   IF (RATLZT(K) .GE. RATLZ(K)) GO TO 230
                  
                   LZTWC(K) = LZTWC(K) + (RATLZ(K) - RATLZT(K)) * LZTWM(I)
                  

                   
                   LZFSC(K) = LZFSC(K) - (RATLZ(K) - RATLZT(K)) * LZTWM(I)
                
                   
                   IF(LZFSC(K) .GE. 0.0) GO TO 230
                   
                   LZFPC(K) = LZFPC(K) + LZFSC(K)
                   
                   LZFSC(K) = 0.0
                   
230                IF (LZTWC(K) .LT. 0.00001) LZTWC(K) = 0.0

! Test Output
!
!Write(*,9882) 
!9882 Format("Check the following variables: LZTWC; LZFSC;LZFPC;UZFWC,ETLZTW,ETUZTW),ETUZFW")
!Write(*,*) I,J,M,K,LZTWC(K),LZFSC(K),LZFPC(K),UZTWC(K),UZFWC(K),ETLZTW(J,M,K),ETUZTW(J,M,K),ETUZFW(J,M,K)


! --- CALCULATE TOTAL ET SUPPLIED BY UPPER AND LOWER LAYERS

                   ET(J, M, K) = ETUZTW(J, M, K) + ETUZFW(J, M, K) + ETLZTW(J, M, K)

! for check
!WRITE(99,*),I,J,M,K,'ET=',ET(J,M,K),'ETUZTW=',ETUZTW(J,M,K) ,'ETUZFW=',ETUZFW(J,M,K),'ETLZTW=', ETLZTW(J,M,K)
!WRITE(*,*),I,J,M,K,'ET=',ET(J,M,K),'ETUZTW=',ETUZTW(J,M,K) ,'ETUZFW=',ETUZFW(J,M,K),'ETLZTW=', ETLZTW(J,M,K)                  
                  IF (ET(J, M, K) .LT. 0.00001) ET(J, M, K) = 0.0
! *****************************************************************************************************
! *****************************************************************************************************
! --- COMPUTE PERCOLATION INTO SOIL WATER STORAGES AND SURFACE RUNOFF
!
!     --- COMPUTE WATER IN EXCESS OF UZ TENSION WATER CAPACITY (TWX)

                  TWX(K) = INFIL(K) + UZTWC(K) - UZTWM(I)


!Print *, 'TWX=',TWX(K)
           
                  IF (TWX(K).GE.0.0) THEN
             	
!     --- IF INFIL EXCEEDS UZ TENSION WATER CAPACITY, SET UZ TENSION WATER STORAGE TO CAPACITY, 
!         REMAINDER OF INFIL GOES TO UZFWC IF EXCEEDS UZFWC EXCESS GOES TO SURFACE RUNOFF

                     UZTWC(K) = UZTWM(I)     
                
                     UZFWC(K) = UZFWC(K) + TWX(K)
                
                     IF (UZFWC(K) .GT. UZFWM(I)) THEN
                
                        SURFRO(K) = UZFWC(K) - UZFWM(I)
                
                        UZFWC(K) = UZFWM(I)
                
                     ELSE
                
                        SURFRO(K) = 0.0
                
                     ENDIF 

                  ELSE        	
           

!     --- IF INFIL DOES NOT EXCEED UZ TENSION WATER CAPACITY, ALL INFIL GOES TO UZ TENSION WATER STORAGE

                     UZTWC(K) = UZTWC(K) + INFIL(K)
             	
                  ENDIF
           
! --- COMPUTE PERCOLATION TO LZ IF FREE WATER IS AVAILABLE IN UZ

		
	          IF (UZFWC(K) .GT. 0.0) THEN
		

!     --- COMPUTE PERCOLATION DEMAND FROM LZ

                     PERCM(K) = LZFPM(K) * LZPK(I) + LZFSM(K) * LZSK(I)
                
                     PERC(K) = PERCM(K) * (UZFWC(K)/UZFWM(I))
                
                     DEFR(K)=1.0-((LZTWC(K)+LZFPC(K)+LZFSC(K))/ (LZTWM(I)+LZFPM(I)+LZFSM(I)))
                
                     PERC(K) = PERC(K) * (1.0 + ZPERC(I) * (DEFR(K)**REXP(I)))
            

!     --- COMPARE LZ PERCOLATION DEMAND TO UZ FREE WATER AVAILABLE AND COMPUTE ACTUAL PERCOLATION

                    IF (PERC(K) .LT. UZFWC(K)) THEN
                
                       UZFWC(K) = UZFWC(K) - PERC(K)
                
                    ELSE
                
                       PERC(K) = UZFWC(K)
                
                       UZFWC(K) = 0.0
                
                    ENDIF
            
!      --- CHECK TO SEE IF PERC EXCEEDS LZ TENSION AND FREE WATER DEFICIENCY, IF SO SET PERC TO LZ DEFICIENCY


                    LZDEF(K) = (LZTWC(K) + LZFPC(K) + LZFSC(K)) - &
      (LZTWM(I) + LZFPM(I) + LZFSM(I)) + PERC(K)
                    
                    IF (LZDEF(K) .GT. 0.0) THEN
                    
                       PERC(K) = PERC(K) - LZDEF(K)
          
                       UZFWC(K) = UZFWC(K) + LZDEF(K)
                       
                    ENDIF
                
                
! --- DISRIBUTE PERCOLATED WATER INTO THE LZ STORAGES AND COMPUTE THE REMAINDER IN UZ FREE WATER STORAGE AND RESIDUAL AVAIL FOR RUNOFF
   

!    --- COMPUTE PERC WATER GOING INTO LZ TENSION WATER STORAGE AND COMPARE TO AVAILABLE STORAGE

                    PERCT(K) = PERC(K) * (1.0 - PFREE(I))
                
                    IF ((PERCT(K) + LZTWC(K)) .GT. LZTWM(I)) THEN
                
!    --- WHEN PERC IS GREATER THAN AVAILABLE TENSION WATER STORAGE, SET TENSION WATER STORAGE TO MAX, REMAINDER OF PERC GETS EVALUATED AGAINST FREE WATER STORAGE

                       PERCF(K) = PERCT(K) + LZTWC(K) - LZTWM(I)
                
                       LZTWC(K) = LZTWM(I)
                
                    ELSE
                
!     --- WHEN PERC IS LESS THAN AVAILABLE TENSION WATER STORAGE, UPDATE TENSION WATER STORAGE

                       LZTWC(K) = LZTWC(K) + PERCT(K)
                
                       PERCF(K) = 0.0
                
                    ENDIF
                
!     --- COMPUTE TOTAL PERC WATER GOING INTO LZ FREE WATER STORAGE

                    PERCF(K) = PERCF(K) + PERC(K) * PFREE(I)           

                    IF(PERCF(K) .GT. 0.0) THEN
                
!     --- COMPUTE RELATIVE SIZE OF LZ PRIMARY FREE WATER STORAGE COMPARED TO LZ TOTAL FREE WATER STORAGE

                       HPL(K) = LZFPM(I) / (LZFPM(I) + LZFSM(I))
                
!     --- COMPUTE LZ PRIMARY AND SECONDARY FREE WATER CONTENT TO CAPACITY RATIOS

                       RATLP(K) = LZFPC(K) / LZFPM(I)
                
                       RATLS(K) = LZFSC(K) / LZFSM(I)
                
!     --- COMPUTE FRACTIONS AND PERCENTAGES OF FREE WATER PERC TO GO TO LZ PRIMARY STORAGE

                       FRACP(K) = (HPL(K) * 2.0 * (1.0 - RATLP(K))) &
                      / ((1.0 - RATLP(K)) + (1.0 - RATLS(K)))
                
                       IF (FRACP(K) .GT. 1.0) FRACP(K) = 1.0

                          PERCP(K) = PERCF(K) * FRACP(K)
                
                          PERCS(K) = PERCF(K) - PERCP(K)
                
!     --- COMPUTE NEW PRIMARY AND SECONDARY STORAGE

!         --- COMPUTE NEW SECONDARY FREE WATER STORAGE

                          LZFSC(K) = LZFSC(K) + PERCS(K)

                          IF(LZFSC(K) .GT. LZFSM(I)) THEN
                
!         --- IF NEW SECONDARY FREE WATER STORAGE EXCEEDS CAPACITY SET SECONDARY STORAGE TO CAPACITY AND EXCESS GOES TO PRIMARY FREE WATER STORAGE

                             PERCS(K) = PERCS(K) - LZFSC(K) + LZFSM(I)
                
                             LZFSC(K) = LZFSM(I)
                          
                          ENDIF
                
            
!        --- IF NEW LZ SECONDARY FREE WATER STORAGE IS LESS THAN CAPACITY MOVE ON TO COMPUTE NEW PRIMARY FREE WATER STORAGE


                       LZFPC(K) = LZFPC(K) + (PERCF(K) - PERCS(K))

                
                       IF (LZFPC(K) .GT. LZFPM(I)) THEN

!             --- IF LZ FREE PRIMARY WATER STORAGE EXCEEDS CAPACITY SET PRIMARY STORAGE TO CAPACITY AND EVALUATE EXCESS AGAINST LZ TENSION WATER STORAGE

                          LZTWC(K) = LZTWC(K) + LZFPC(K) - LZFPM(I)
                
                          LZFPC(K) = LZFPM(I)
                
                          IF (LZTWC(K) .GT. LZTWM(I)) THEN

!           --- IF LZ TENSION WATER EXCEEDS CAPACITY EVALUATE EXCESS AGAINST UZ FREE WATER CAPACITY AND SET LZ TENSION WATER STORAGE TO CAPACITY

                             UZFWC(K) = UZFWC(K) + LZTWC(K) - LZTWM(I)
                
                             LZTWC(K) = LZTWM(I)
                             
                          ENDIF
                          
                       ENDIF
                       
                    ENDIF
                
		 ENDIF
		 
! ***************************************************************************************************** 
! *****************************************************************************************************                
! --- COMPUTE BASEFLOW AND UPDATE LZ PRIMARY AND SECONDARY FREE WATER STORAGES
!
!                
!      --- COMPUTE PRIMARY BASEFLOW AND COMPARE TO AVAILABLE FREE PRIMARY STORAGE

                 PBF(K) = LZFPC(K) * LZPK(I)
                
                 LZFPC(K) = LZFPC(K) - PBF(K)
                
                 IF (LZFPC(K) .LE. 0.0001) THEN 
                
                    PBF(K) = PBF(K) + LZFPC(K)
                
                    LZFPC(K) = 0.0
                
                 ENDIF
                

!      --- COMPUTE SECONDARY BASEFLOW AND COMPARE TO AVAILABLE FREE PRIMARY STORAGE

                 SBF(K) = LZFSC(K) * LZSK(I)
                
                 LZFSC(K) = LZFSC(K) - SBF(K)
                
                 IF (LZFSC(K) .LE. 0.0001) THEN
                
                   SBF(K) = SBF(K) + LZFSC(K)
                
                   LZFSC(K) = 0.0
                
                 ENDIF                
                 

! *****************************************************************************************************
! --- COMPUTE INTERFLOW FROM UZ

                 INF(K) = UZFWC(K) * UZK(I)
                
                 IF (UZFWC(K) .LT. INF(K)) THEN
                 
                    INF(K) = UZFWC(K)
                    
                    UZFWC(K) = 0.0
                 
                 ELSE
                    
                    UZFWC(K) = UZFWC(K) - INF(K)
                
                 ENDIF

             ENDIF


!Print *, 'Finish calculate Water balances and Soil Water Content'		
! **************************----Finish calculate Water balances and Soil water--------******************************************************


! for check
IF(j<=2) then ! .and. K==1
WRITE(99,*) I,J,M,Day,K,' ET=', ET(J,M,K),'RAIN=',RAIN(I,J,M)/MNDAY,'TEMP=',TEMP (I,J, M),'TWX=',TWX(K),'INFIL=',&
&INFIL(K),'UZTWC=',UZTWC(K),'PBF=',PBF(K),'SBF=',SBF(K),'SNOWW=',SNOWW/MNDAY,&
&'LZFPC=',LZFPC(K) ,'LZFSC=',LZFSC(K),'UZFWC=', UZFWC(K),'SURFRO=', SURFRO(K),'INF=', INF(K)
ENDIF
    

! CALCULATING DAILY GEP G C/M2/DAY
! NOTE  the following is based on New Analysis by Asko (Aug 24, 2010)
!----根据不同的植被类型重编写下面的代码


! -- CROP

               IF (K.EQ.1) THEN 

               GEP(J, M, K) = 4.5 * ET(J,M,K)
!---------论文中的公式
               RECO(J,M,K)= 40.6 + 0.43 * GEP(J, M, K)*MNDAY
!---------原始计算公式 		
!               RECO(J,M,K)= VAL_1(TUN1) + VAL_2(TUN2) * GEP(J,M,K)*MNDAY      !11.14 1.85
			  
			   
! -- Close SHRUBLANDS                   
               
               ELSEIF (K .EQ. 5) THEN                

               GEP(J, M, K) = 1.4 * ET(J,M,K)  
                
               RECO(J,M,K)= 11.4 + 0.69 * GEP(J, M, K)*MNDAY
                
! -- DECIDUOUS Broadleaf FOREST
 
               ELSEIF (K .EQ. 2) THEN 

               GEP(J, M, K) = 2.4* ET(J,M,K) 
!------论文公式				   
               RECO(J,M,K)= 30.8 + 0.45 * GEP(J, M, K)*MNDAY	
!-------原始计算公式
!			   RECO(J,M,K)= 24.12 + 1.49 * ET(J,M,K)*MNDAY	


! -- Evergreen Broadleaf FOREST
 
               ELSEIF (K .EQ. 0) THEN 

               GEP(J, M, K) = 2.6* ET(J,M,K)              
               RECO(J,M,K)= 19.6 + 0.61 * GEP(J, M, K)*MNDAY			   

!--- Evergreen Needleleaf Forest
               ELSEIF (K .EQ. 3) THEN                

               GEP(J, M, K) = 2.14* ET(J,M,K)
               RECO(J,M,K)= 9.9 + 0.68 * GEP(J, M, K)*MNDAY			   
               
! -- GRASSLANDS               
                ELSEIF (K .EQ. 7) THEN                
               
               GEP(J, M, K) = 2.25 * ET(J,M,K)
!------ 论文公式
              RECO(J,M,K)= 18.9 + 0.64*GEP(J, M, K)*MNDAY
!-------原始计算公式	
!			   RECO(J,M,K)= 14.2 + 1.42 * ET(J,M,K)*MNDAY	
               
!---- MIXED FOREST
               
               ELSEIF (K .EQ. 4) THEN 
               
               GEP(J, M, K) =2.5 * ET(J,M,K)
               IF (TEMP(I,J,M) .LE. -1.0) THEN 
                
                GEP(J, M, K) = 0.0
                
               ENDIF 
                           
               RECO(J,M,K)= 24.44 + 0.62 * GEP(J,M,K)*MNDAY

! -- Open Shrublands                   
               
	     ELSEIF (K .EQ. 6) THEN                

               GEP(J, M, K) =  1.42* ET(J,M,K)
               RECO(J,M,K)= 9.7 + 0.56 * GEP(J, M, K)*MNDAY
                                           
!-- SAVANNAS                   
               
	     ELSEIF (K.EQ.  0) THEN                

               GEP(J, M, K) = 1.26* ET(J,M,K) !
               RECO(J,M,K)= 25.2 + 0.53 * GEP(J, M, K)*MNDAY
       
         
! -- Wetlands                     
               
	     ELSEIF (K .EQ. 0) THEN                

               GEP(J, M, K) = 1.66* ET(J,M,K)
               RECO(J,M,K)= 7.8 + 0.56 * GEP(J, M, K)*MNDAY     
!-- Wet Savanna                     
               
	     ELSEIF (K .EQ. 0) THEN                

               GEP(J, M, K) = 1.49* ET(J,M,K)
               RECO(J,M,K)= 14.7 + 0.63 * GEP(J, M, K)*MNDAY
 			   
! -- URBAN/BARRENS/WATRE BODY (SAME AS OPEN SHRUB)                  
               
           ELSE
               
              GEP(J, M, K) = 0.
              RECO(J,M,K) =0.
              
              
           ENDIF

                 IF (GEP(J,M,K) .LE. 0.) THEN
                      GEP(J,M,K) = 0.                  
                 ENDIF
                                 
! **************************----Finish calculate Carbon balances--------************************************************
              
                RECO(J,M,K) = RECO(J,M,K)/MNDAY 
                                                            
                NEE(J, M, K) = -GEP(J,M,K) + RECO(J,M,K)

! for check
!print *, I,J,M,' LANDUSE=', LADUSE(I),' ET=', ET(J,M),' GEP=', GEP(J,M),' RECO=', RECO(J,M)
                                    
! --- COMPUTE FRACTION OF EACH WATER BALANCE COMPONENT AND GEP FOR EACH LAND COVER
!
! *****************************************************************************************************
! -- CALCULATE THE FRACTION OF daily GEP

               GEPTEMP = GEPTEMP + GEP(J,M,K)*LADUSE(I,K)   
               RECOTEMP = RECOTEMP + RECO(J,M,K)*LADUSE(I,K)            
               NEETEMP = NEETEMP + NEE(J,M,K)*LADUSE(I,K)
                            

! *****************************************************************************************************
! --- CALCULATE THE FRACTION OF daily AET
               
               AETTEMP = AETTEMP + ET(J,M,K) * LADUSE(I,K)
               
! *****************************************************************************************************
!--- CALCULATE THE FRACTION OF daily SURFACE RUNOFF
              
               RUNOFFTEMP = RUNOFFTEMP + SURFRO(K) * LADUSE(I,K)
               
! *****************************************************************************************************
!--- CALCULATE THE FRACTION OF DAILY PRIMARY BASEFLOW
              
               PBFTEMP = PBFTEMP + PBF(K) * LADUSE(I,K)               
               
! *****************************************************************************************************
!--- CALCULATE THE FRACTION OF DAILY SECONDARY BASEFLOW
              
               SBFTEMP = SBFTEMP + SBF(K) * LADUSE(I,K)               

! *****************************************************************************************************
!--- CALCULATE THE FRACTION OF DAILY INTERFLOW
              
               IFTEMP = IFTEMP + INF(K) * LADUSE(I,K)                   
              
! *****************************************************************************************************


! ----CALCUALTE TOTAL RUNOFF FOR EACH LANDUSE, K (GE SUN OCT 19, 2010) 
!
!----!!!!! RUNLAND(I,Y,DAY, K)更改
!			RUNLAND_Y=(J-1)*12+M							
            RUNLAND(I,J,M,DAY,K) = SURFRO(K) + PBF(K) + SBF(K) + INF(K)
            ETLAND(I,J,M,DAY,K) = ET(J,M,K)
            GEPLAND(I,J,M,DAY,K) = GEP(J,M,K)
            NEELAND(I,J,M,DAY,K) = NEE(J,M,K)
!--- calculate the fraction of soil moisture 
          
              
              TAUZTWC = TAUZTWC + UZTWC(K) * LADUSE(I,K)
              
              TAUZFWC = TAUZFWC + UZFWC(K) * LADUSE(I,K)
              
              TALZTWC = TALZTWC + LZTWC(K) * LADUSE(I,K)
              
              TALZFPC = TALZFPC + LZFPC(K) * LADUSE(I,K)
              
              TALZFSC = TALZFSC + LZFSC(K) * LADUSE(I,K)
              
              TASM = TASM + (UZTWC(K)+UZFWC(K)+LZTWC(K)+LZFPC(K) +LZFSC(K)) * LADUSE(I,K)
   
              TAREA = TAREA + LADUSE(I,K) 

    
40         CONTINUE  ! K loop

           
! -- CALCULATE AVG SMC

              AUZTWC = TAUZTWC / TAREA
              AUZFWC = TAUZFWC / TAREA
              ALZTWC = TALZTWC / TAREA
              ALZFPC = TALZFPC / TAREA
              ALZFSC = TALZFSC / TAREA
              ASM = TASM/TAREA

100        CONTINUE ! Day loop


           AET(I,J,M) = AETTEMP        
           RUNOFF(I,J,M) = RUNOFFTEMP
           PRIBF(I,J,M) = PBFTEMP
           SECBF(I,J,M) = SBFTEMP
           INTF(I,J,M) = IFTEMP
           SP(I,J,M) = SNOWPACK

!! --  AVERAGE soil moisture		   
!		   AVSMC (I,J,M) = ASM 
!           AVUZTWC(I,J,M) = AUZTWC
!           AVUZFWC(I,J,M) = AUZFWC
 !          AVLZTWC(I,J,M) = ALZTWC
 !          AVLZFPC(I,J,M) = ALZFPC
  !         AVLZFSC(I,J,M) = ALZFSC


!--- End of month soil moisture 
              
		   EMSMC(I,J,M) = ASM 
           EMUZTWC(I,J,M) = AUZTWC
           EMUZFWC(I,J,M) = AUZFWC
           EMLZTWC(I,J,M) = ALZTWC
           EMLZFPC(I,J,M) = ALZFPC
           EMLZFSC(I,J,M) = ALZFSC	 		   
		!	Print*,	"EMSMC(I,J,M)",EMSMC(I,J,M)
				   
				   
           IF (RUNOFF(I,J,M) .LT. 0.) THEN
           
           RUNOFF(I,J,M)=0.
           
           ENDIF            
       
           GEPM(I,J, M) = GEPTEMP
           RECOM(I,J,M)  = RECOTEMP
           NEEM(I,J,M) = NEETEMP

! -- STREAMFLOW IN MILLION M3 FOR EACH HUC FOR MONTH M. HUCAREA IN SQ. METERS 
          STRFLOW(I, J, M) = (RUNOFF(I,J,M) + PRIBF(I,J,M) + SECBF(I,J,M) + INTF(I,J,M)) &
      * HUCAREA(I)/1000./1000000.
!      Print*,"STRFLOW(I, J, M)",STRFLOW(I, J, M),"HUCAREA(I)",HUCAREA(I)


! TEST OUTPUT

    ! WRITE(99,*) 'ICELL=',I,'Year=',J,'Month=',M,'TEMP=',TEMP(I,J,M),'RAINFALL=',RAIN(I,J,M),'AET=',AET(I,J,M),&
    ! 'INTF=',INTF(I,J,M),'EMSMC=',EMSMC (I,J,M),'SP=',SP(I,J,M), 'PRIBF=',PRIBF(I,J,M),'SECBF=',SECBF(I,J,M),&
        ! 'EMUZTWC=',EMUZTWC(I,J,M),'EMUZFWC=',EMUZFWC(I,J,M),'EMLZFPC=',EMLZFPC(I,J,M),'EMLZFSC=',EMLZFSC(I,J,M),&
        ! 'GEPM=',GEPM(I,J, M) 
12133 format(A10,I6,I6,I4,13F10.3)


      RETURN
      END
