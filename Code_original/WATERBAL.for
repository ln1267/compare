

C**********************************************************************C
C                                                                      C
C     *** SUBROUTINE WATERBAL ***                                      C
C     SIMULATES MONTHLY WATER BALANCE USING 2 LAYER SOIL MOISTURE      C
C     ALGORITHM FROM NOAA NATIONAL WEATHER SERVICE SACRAMENTO SOIL     C
C     MOISTURE ACCOUNTING MODEL (SAC-SMA)                              C
C                                                                      C
C**********************************************************************C
      SUBROUTINE WATERBAL(I,J,M,MNDAY)
      
      COMMON/BASIC/NGRID, NYEAR, NLC,BYEAR,IYSTART,IYEND
       

      COMMON/OUTPUT/ PET(200,12,20),APET(12),PAET(200,12,20),APAET(12),
     &AET(12), RUNOFF(12), INTER(12), PRIBF(12), SECBF(12), INTF(12), 
     &AVUZTWC(12), AVUZFWC(12), AVLZTWC(12), AVLZFPC(12)
     
      COMMON/SNOWPACK/SP(12), SNOWPACK, NSPM(200)
       
      COMMON/SUMMARY/ANURAIN(200),ANURUN(200),ANUPET(200),ANUAET(200)
              
      COMMON/SOIL/LZTWM(2200), LZFPM(2200), LZFSM(2200), LZSK(2200),
     >  LZPK(2200), UZTWM(2200), UZFWM(2200), UZK(2200), ZPERC(2200),
     >  REXP(2200), PFREE(2200), SMC(12)

      COMMON/CLIMATE/ RAIN(4000,200,12), TEMP(4000,200, 12), AAPPT(4000)
                                       
            
      COMMON/CELLINFO/LADUSE(4000,20),HUCNO(4000),
     >                LATUDE(4000),LONGI(4000)
     
      
      COMMON/HUC/ HUCAREA(2200)
       
      COMMON/FLOW/ STRFLOW(2200, 200, 12)
       
      COMMON/CARBON/ GEPM(2200, 200, 12),RECOM(2200, 200, 12), 
     >               NEEM(2200,200,12)
       
       
                    
      COMMON/BYLAND/RUNLAND(500, 50, 12, 31, 19)
C ----------------------------------------------------------------------------     
     
     
      INTEGER DAY, HUCNO(4000), MNDAY
      
      REAL AETTEMP, RUNOFFTEMP, PBFTEMP, SBFTEMP, 
     > IFTEMP, GEPTEMP,RECOTEMP, NEETEMP
          
      REAL LZTWM(2200), LZFPM(2200), LZFSM(2200), LZSK(2200),
     >  LZPK(2200), UZTWM(2200), UZFWM(2200), UZK(2200), ZPERC(2200),
     >  REXP(2200), PFREE(2200)

      REAL UZTWC(20), UZFWC(20), LZTWC(20), LZFSC(20), LZFPC(20)
      
      REAL ETUZTW(200,12,20), RESIDET(200,12,20), ETUZFW(200,12,20)
      
      REAL ETLZTW(200, 12, 20), RATLZT(20), RATLZ(20)
      
      REAL SNOW, SNOWW, SP(12)
      
      REAL LADUSE(4000,20), TASM, TAREA
      
      REAL ET(200,12,20), SURFRO(20), GEP(200,12,20), INFIL(20),
     >  RECO(200, 12, 20), NEE(200, 12,20) 
      
      REAL DPAET, PET(200,12,20)
      
      REAL UZRAT(20), TWX(20), PERCM(20), PERC(20), DEFR(20), LZDEF(20) 
      
      REAL PERCT(20), PERCF(20)
      
      REAL HPL(20), RATLP(20), RATLS(20), FRACP(20), PERCP(20),PERCS(20)
      
      REAL PBF(20), SBF(20), INF(20)
      
      REAL TAUZTWC, TAUZFWC, TALZTWC, TALZFPC, TALZFSC
      
      REAL AUZTWC, AUZFWC, ALZTWC, ALZFPC, ALZFSC
      
      REAL ASM, AET(12), RUNOFF(12), PRIBF(12), SECBF(12), INTF(12), 
     &SMC(12)
      
      REAL AVUZTWC(12), AVUZFWC(12), AVLZTWC(12), AVLZFPC(12), 
     >     AVLZFSC(12)
      
      REAL STRFLOW(2200,200,12),GEPM(2200,200,12),RECOM(2200,200,12),
     >      NEEM(2200,200,12)
      
      INTEGER GEPFLAG
      
      REAL HUCELE(2200)
      
      double precision HUCAREA(2200)
      
      REAL RUNLAND (150, 50, 12, 31, 19)
            
C *****************************************************************************************************
C *****************************************************************************************************
C *****************************************************************************************************
C --- INITIALIZE VARIABLES FOR START OF SIMULATION


             AETTEMP =0.
             RUNOFFTEMP = 0.
             PBFTEMP = 0.0
             SBFTEMP = 0.0
             IFTEMP = 0.0
                          
             GEPTEMP = 0.
             RECOTEMP = 0.
             NEETEMP =0.

        IF (J .EQ. 1 .AND. M .EQ. 1) THEN

        IAM =0     
        
                  
        DO 50 K=1, NLC
                        
           UZTWC(K) = UZTWM(I)
           UZFWC(K) = 0.0
           LZTWC(K) = LZTWM(I)
           LZFSC(K) = 0.75*LZFSM(I)
           LZFPC(K) = 0.75*LZFPM(I)
           
               
50      CONTINUE
        
        ENDIF 
         
C *****************************************************************************************************
C *****************************************************************************************************
C----- SIMULATE SNOWPACK (SEE VAROSMARTY  ET AL., 1989)
      
        IF (TEMP(I,J, M) .LE.  -1.0) THEN
        
           SNOW = RAIN(I,J, M)
    
           SNOWPACK = SNOWPACK + SNOW
           
           IAM = 0
                                
        ELSE 
        
            IAM = IAM +1 
            
            HUCELE(I) = 500.
            
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
                            

C *****************************************************************************************************
C *****************************************************************************************************
C *****************************************************************************************************
C -- LOOP THROUGH DAYS OF CURRENT MONTH AND CALCULATE SOIL WATER STORAGE, BASEFLOW, RUNOFF, AND AET
        
        DO 100 DAY= 1, MNDAY    
        
             TASM = 0.            
             TAREA = 0.             
             TAREA = 0.
             TASM = 0.
             TAUZTWC = 0.
             TAUZFWC = 0.
             TALZTWC = 0.
             TALZFPC = 0.
             TALZFSC = 0.


C *****************************************************************************************************
C *****************************************************************************************************
C -- LOOP THROUGH LAND COVERS IN THE HUC AND PERFORM WATER BALANCE COMPUTATIONS
C -- ASSUMES OPEN WATER LAND COVER IS NEG.
         
             K=0
          
             DO 40 K=1, NLC
                  

C *****************************************************************************************************
C -- SET ET, SURFACE RUNOFF, INTERFLOW, GEP TO ZERO IF TEMPERATURE IS LE -1.0
C -- BASEFLOW STILL OCCURS

             IF (TEMP (I,J, M) .LE. -1.0) THEN
          
                ET(J,M,K) = 0.
                SURFRO(K) = 0.
                INF(K) = 0.
                GEP (J, M, K) = 0.
                
                
C -- COMPUTE PRIMARY BASEFLOW WHEN T <= -1.0

                PBF(K) = LZFPC(K) * LZPK(I)
                LZFPC(K) = LZFPC(K) - PBF(K)
                IF (LZFPC(K) .LE. 0.0001) THEN 
                   PBF(K) = PBF(K) + LZFPC(K)
                   LZFPC(K) = 0.0
                ENDIF
                
C -- COMPUTE SECONDARY BASEFLOW WHEN T <= -1.0

                SBF(K) = LZFSC(K) * LZSK(I)
                LZFSC(K) = LZFSC(K) - SBF(K)
                IF (LZFSC(K) .LE. 0.0001) THEN
                   SBF(K) = SBF(K) + LZFSC(K)
                   LZFSC(K) = 0.0
                ENDIF                

             ELSE
                  
C *****************************************************************************************************
C *****************************************************************************************************
C -- COMPUTE THE DAILY AVERAGE INFILTRATION FOR A GIVEN MONTH FOR EACH LAND USE

                INFIL(K) = RAIN(I,J,M)/MNDAY + SNOWW/MNDAY
          
            
C *****************************************************************************************************
C --- COMPUTE AET GIVEN TOTAL WATER STORED IN UPPER SOIL LAYER STORAGES AND PAET CALCULATED IN PET.FOR
C --- ASSUME ET IS SUPPLIED ONLY FROM UPPER LAYER NO UPWARD FLUX FROM LOWER LAYER TO UPPER LAYER
C --- NOTE THAT SAC-SMA ALLOWS ET TO ALSO BE SUPPLIED UNRESTRICTED BY LZ TENSION WATER STORAGE

                
                   DPAET = PAET(J, M, K)/MNDAY
               
                   ET(J, M, K) = DPAET
                
C --- COMPUTE ET FROM UZ TENSION WATER STORAGE, RECALCULATE UZTWC, CALCULATE RESIDUAL ET DEMAND

                   ETUZTW(J, M, K) = ET(J, M, K) * (UZTWC(K)/UZTWM(I))
                   
                   RESIDET(J, M, K) = ET(J, M, K) - ETUZTW(J, M, K)
                   
                   UZTWC(K) = UZTWC(K) - ETUZTW(J, M, K)
                   
                   ETUZFW(J, M, K) = 0.0
                   
                   IF (UZTWC(K).GE.0.0) GOTO 220
                   
                   ETUZTW(J, M, K) = ETUZTW(J, M, K) + UZTWC(K)
                   
                   UZTWC(K) = 0.0
                   
                   RESIDET(J, M, K) = ET(J, M, K) - ETUZTW(J, M, K)
                   
C --- COMPUTE ET FROM UZ FREE WATER STORAGE, RECALCULATE UZFWC, CALCULATE RESIDUAL ET DEMAND                   
                   
                   IF (UZFWC(K) .GE. RESIDET(J, M, K)) GO TO 221
                   
                   ETUZFW(J, M, K) = UZFWC(K)
                   
                   UZFWC(K) = 0.0
                   
                   RESIDET(J, M, K) = RESIDET(J, M, K) - ETUZFW(J, M, K)
                   
                   GO TO 225
                   
221                ETUZFW(J, M, K) = RESIDET(J, M, K)

                   UZFWC(K) = UZFWC(K) - ETUZFW(J, M, K)
                   
                   RESIDET(J, M, K) = 0.0
                   
C --- REDISTRIBUTE WATER BETWEEN UZ TENSION WATER AND FREE WATER STORAGES

220                IF((UZTWC(K)/UZTWM(I)).GE.(UZFWC(K)/UZFWM(I))) 
     &             GO TO 225

                   UZRAT(K)=(UZTWC(K)+UZFWC(K))/(UZTWM(I)+UZFWM(I))
                      
                   UZTWC(K) = UZTWM(I) * UZRAT(K)
                      
                   UZFWC(K) = UZFWM(I) * UZRAT(K)
                        
225                IF (UZTWC(K) .LT. 0.00001) UZTWC(K) = 0.0

                   IF (UZFWC(K) .LT. 0.00001) UZFWC(K) = 0.0
                   
                   
C --- COMPUTE ET FROM LZ TENSION WATER STORAGE, RECALCULATE LZTWC, CALCULATE RESIDUAL ET DEMAND

                   ETLZTW(J, M, K) = RESIDET(J, M, K) * (LZTWC(K) / 
     &             (UZTWM(I) + LZTWM(I)))
                   
                   LZTWC(K) = LZTWC(K) - ETLZTW(J, M, K)
                   
                   IF(LZTWC(K) .GE. 0.0) GO TO 226
                   
                   ETLZTW(J, M, K) = ETLZTW(J, M, K) + LZTWC(K)
                   
                   LZTWC(K) = 0.0
                   
226                RATLZT(K) = LZTWC(K) / LZTWM(I)

                   RATLZ(K) = (LZTWC(K) + LZFPC(K) + LZFSC(K)) / 
     &             (LZTWM(I) + LZFPM(I) + LZFSM(I))
     
                   IF (RATLZT(K) .GE. RATLZ(K)) GO TO 230
                  
                   LZTWC(K) = LZTWC(K) + (RATLZ(K) - RATLZT(K)) * 
     &             LZTWM(I)

                   
                   LZFSC(K) = LZFSC(K) - (RATLZ(K) - RATLZT(K)) * 
     &             LZTWM(I)
                   
                   IF(LZFSC(K) .GE. 0.0) GO TO 230
                   
                   LZFPC(K) = LZFPC(K) + LZFSC(K)
                   
                   LZFSC(K) = 0.0
                   
230                IF (LZTWC(K) .LT. 0.00001) LZTWC(K) = 0.0

C --- CALCULATE TOTAL ET SUPPLIED BY UPPER AND LOWER LAYERS

                   ET(J, M, K) = ETUZTW(J, M, K) + ETUZFW(J, M, K) + 
     &             ETLZTW(J, M, K)
                   

C *****************************************************************************************************
C *****************************************************************************************************
C --- COMPUTE PERCOLATION INTO SOIL WATER STORAGES AND SURFACE RUNOFF

C     --- COMPUTE WATER IN EXCESS OF UZ TENSION WATER CAPACITY (TWX)

                  TWX(K) = INFIL(K) + UZTWC(K) - UZTWM(I)
           
                  IF (TWX(K).GE.0.0) THEN
             	
C     --- IF INFIL EXCEEDS UZ TENSION WATER CAPACITY, SET UZ TENSION WATER STORAGE TO CAPACITY, 
C         REMAINDER OF INFIL GOES TO UZFWC IF EXCEEDS UZFWC EXCESS GOES TO SURFACE RUNOFF

                     UZTWC(K) = UZTWM(I)     
                
                     UZFWC(K) = UZFWC(K) + TWX(K)
                
                     IF (UZFWC(K) .GT. UZFWM(I)) THEN
                
                        SURFRO(K) = UZFWC(K) - UZFWM(I)
                
                        UZFWC(K) = UZFWM(I)
                
                     ELSE
                
                        SURFRO(K) = 0.0
                
                     ENDIF 

                  ELSE        	
           

C     --- IF INFIL DOES NOT EXCEED UZ TENSION WATER CAPACITY, ALL INFIL GOES TO UZ TENSION WATER STORAGE

                     UZTWC(K) = UZTWC(K) + INFIL(K)
             	
                  ENDIF
           
C --- COMPUTE PERCOLATION TO LZ IF FREE WATER IS AVAILABLE IN UZ

		
	          IF (UZFWC(K) .GT. 0.0) THEN
		

C     --- COMPUTE PERCOLATION DEMAND FROM LZ

                     PERCM(K) = LZFPM(K) * LZPK(I) + LZFSM(K) * LZSK(I)
                
                     PERC(K) = PERCM(K) * (UZFWC(K)/UZFWM(I))
                
                     DEFR(K)=1.0-((LZTWC(K)+LZFPC(K)+LZFSC(K))/
     &(LZTWM(I)+LZFPM(I)+LZFSM(I)))
                
                     PERC(K) = PERC(K) * (1.0 + ZPERC(I) * (DEFR(K)
     &**REXP(I)))
            

C     --- COMPARE LZ PERCOLATION DEMAND TO UZ FREE WATER AVAILABLE AND COMPUTE ACTUAL PERCOLATION

                    IF (PERC(K) .LT. UZFWC(K)) THEN
                
                       UZFWC(K) = UZFWC(K) - PERC(K)
                
                    ELSE
                
                       PERC(K) = UZFWC(K)
                
                       UZFWC(K) = 0.0
                
                    ENDIF
            
C      --- CHECK TO SEE IF PERC EXCEEDS LZ TENSION AND FREE WATER DEFICIENCY, IF SO SET PERC TO LZ DEFICIENCY


                    LZDEF(K) = (LZTWC(K) + LZFPC(K) + LZFSC(K)) - 
     &(LZTWM(I) + LZFPM(I) + LZFSM(I)) + PERC(K)
                    
                    IF (LZDEF(K) .GT. 0.0) THEN
                    
                       PERC(K) = PERC(K) - LZDEF(K)
          
                       UZFWC(K) = UZFWC(K) + LZDEF(K)
                       
                    ENDIF
                
                
C --- DISRIBUTE PERCOLATED WATER INTO THE LZ STORAGES AND COMPUTE THE REMAINDER IN UZ FREE WATER STORAGE AND RESIDUAL AVAIL FOR RUNOFF
   

C     --- COMPUTE PERC WATER GOING INTO LZ TENSION WATER STORAGE AND COMPARE TO AVAILABLE STORAGE

                    PERCT(K) = PERC(K) * (1.0 - PFREE(I))
                
                    IF ((PERCT(K) + LZTWC(K)) .GT. LZTWM(I)) THEN
                
C     --- WHEN PERC IS GREATER THAN AVAILABLE TENSION WATER STORAGE, SET TENSION WATER STORAGE TO MAX, REMAINDER OF PERC GETS EVALUATED AGAINST FREE WATER STORAGE

                       PERCF(K) = PERCT(K) + LZTWC(K) - LZTWM(I)
                
                       LZTWC(K) = LZTWM(I)
                
                    ELSE
                
C     --- WHEN PERC IS LESS THAN AVAILABLE TENSION WATER STORAGE, UPDATE TENSION WATER STORAGE

                       LZTWC(K) = LZTWC(K) + PERCT(K)
                
                       PERCF(K) = 0.0
                
                    ENDIF
                
C     --- COMPUTE TOTAL PERC WATER GOING INTO LZ FREE WATER STORAGE

                    PERCF(K) = PERCF(K) + PERC(K) * PFREE(I)                

                    IF(PERCF(K) .GT. 0.0) THEN
                
C     --- COMPUTE RELATIVE SIZE OF LZ PRIMARY FREE WATER STORAGE COMPARED TO LZ TOTAL FREE WATER STORAGE

                       HPL(K) = LZFPM(I) / (LZFPM(I) + LZFSM(I))
                
C     --- COMPUTE LZ PRIMARY AND SECONDARY FREE WATER CONTENT TO CAPACITY RATIOS

                       RATLP(K) = LZFPC(K) / LZFPM(I)
                
                       RATLS(K) = LZFSC(K) / LZFSM(I)
                
C     --- COMPUTE FRACTIONS AND PERCENTAGES OF FREE WATER PERC TO GO TO LZ PRIMARY STORAGE

                       FRACP(K) = (HPL(K) * 2.0 * (1.0 - RATLP(K))) 
     >                 / ((1.0 - RATLP(K)) + (1.0 - RATLS(K)))
                
                       IF (FRACP(K) .GT. 1.0) FRACP(K) = 1.0

                          PERCP(K) = PERCF(K) * FRACP(K)
                
                          PERCS(K) = PERCF(K) - PERCP(K)
                
C     --- COMPUTE NEW PRIMARY AND SECONDARY STORAGE

C         --- COMPUTE NEW SECONDARY FREE WATER STORAGE

                          LZFSC(K) = LZFSC(K) + PERCS(K)

                          IF(LZFSC(K) .GT. LZFSM(I)) THEN
                
C         --- IF NEW SECONDARY FREE WATER STORAGE EXCEEDS CAPACITY SET SECONDARY STORAGE TO CAPACITY AND EXCESS GOES TO PRIMARY FREE WATER STORAGE

                             PERCS(K) = PERCS(K) - LZFSC(K) + LZFSM(I)
                
                             LZFSC(K) = LZFSM(I)
                          
                          ENDIF
                
            
C         --- IF NEW LZ SECONDARY FREE WATER STORAGE IS LESS THAN CAPACITY MOVE ON TO COMPUTE NEW PRIMARY FREE WATER STORAGE


                       LZFPC(K) = LZFPC(K) + (PERCF(K) - PERCS(K))

                
                       IF (LZFPC(K) .GT. LZFPM(I)) THEN

C             --- IF LZ FREE PRIMARY WATER STORAGE EXCEEDS CAPACITY SET PRIMARY STORAGE TO CAPACITY AND EVALUATE EXCESS AGAINST LZ TENSION WATER STORAGE

                          LZTWC(K) = LZTWC(K) + LZFPC(K) - LZFPM(I)
                
                          LZFPC(K) = LZFPM(I)
                
                          IF (LZTWC(K) .GT. LZTWM(I)) THEN

C            --- IF LZ TENSION WATER EXCEEDS CAPACITY EVALUATE EXCESS AGAINST UZ FREE WATER CAPACITY AND SET LZ TENSION WATER STORAGE TO CAPACITY

                             UZFWC(K) = UZFWC(K) + LZTWC(K) - LZTWM(I)
                
                             LZTWC(K) = LZTWM(I)
                             
                          ENDIF
                          
                       ENDIF
                       
                    ENDIF
                
		 ENDIF
		 
C ***************************************************************************************************** 
C *****************************************************************************************************                
C --- COMPUTE BASEFLOW AND UPDATE LZ PRIMARY AND SECONDARY FREE WATER STORAGES

                
C      --- COMPUTE PRIMARY BASEFLOW AND COMPARE TO AVAILABLE FREE PRIMARY STORAGE

                 PBF(K) = LZFPC(K) * LZPK(I)
                
                 LZFPC(K) = LZFPC(K) - PBF(K)
                
                 IF (LZFPC(K) .LE. 0.0001) THEN 
                
                    PBF(K) = PBF(K) + LZFPC(K)
                
                    LZFPC(K) = 0.0
                
                 ENDIF
                

C      --- COMPUTE SECONDARY BASEFLOW AND COMPARE TO AVAILABLE FREE PRIMARY STORAGE

                 SBF(K) = LZFSC(K) * LZSK(I)
                
                 LZFSC(K) = LZFSC(K) - SBF(K)
                
                 IF (LZFSC(K) .LE. 0.0001) THEN
                
                   SBF(K) = SBF(K) + LZFSC(K)
                
                   LZFSC(K) = 0.0
                
                 ENDIF                
                 

C *****************************************************************************************************
C --- COMPUTE INTERFLOW FROM UZ

                 INF(K) = UZFWC(K) * UZK(I)
                
                 IF (UZFWC(K) .LT. INF(K)) THEN
                 
                    INF(K) = UZFWC(K)
                    
                    UZFWC(K) = 0.0
                 
                 ELSE
                    
                    UZFWC(K) = UZFWC(K) - INF(K)
                
                 ENDIF

             ENDIF
             
 
            GEPFLAG = 2
            
           IF (GEPFLAG .EQ. 1) THEN 

C NOTE  the following is based on Law et al

C *****************************************************************************************************
C *****************************************************************************************************
C -- Caculate GEP based on Law et al (2002) paper, GEP =f(monthly ET, ECOSYSTEMS)
C --- LUMPED CROP, GRASSLAND, SHRUB, SAVANNAH, AND WATER/URBAN/BARREN
C --- MIXED FOREST SHOULD BE AVG OF DECID/EVERGREEN?  EVERGREEN?

C --  SHRUB, SAVANNAH, AND WATER/URBAN/BARREN

               IF (K.EQ. 9 .OR. K.EQ. 10 .OR. K .GE. 12) THEN 
     
               GEP(J, M, K) = (3.2 * ET(J,M,K) * MNDAY - 0.4)/MNDAY

C -- DECIDUOUS FOREST
 
               ELSEIF (K .EQ. 5  .OR. K .EQ. 6) THEN 

               GEP(J, M, K) = (3.2 * ET(J,M,K)*MNDAY - 0.4)/MNDAY


C -- EVERGREEN FOREST

               ELSEIF (K .EQ. 4  .OR. K .EQ. 8) THEN 

               GEP(J, M, K) = (2.4 * ET(J,M,K)*MNDAY + 30.4)/MNDAY
               
C -- MIXED FOREST (SAME AS EVERGREEN)
               
               ELSEIF (K .EQ. 7  .OR. K .EQ. 11) THEN 
               
               GEP(J, M, K) = (2.4 * ET(J,M,K)*MNDAY + 30.4)/MNDAY


C -- crop lands
               ELSEIF (K .EQ. 1  .OR. K .EQ. 2) THEN 
               
               GEP(J, M, K) = (3.06 * ET(J,M,K)*MNDAY - 31.6)/MNDAY
               

C -- Grasslands

               ELSEIF (K .EQ. 3) THEN 
               
               GEP(J, M, K) = (3.39 * ET(J,M,K)*MNDAY - 67.88)/MNDAY

              
               ENDIF
                     
               IF (GEP(J,M,K) .LE. 0.) THEN
               
                GEP(J,M,K) = 0.                  
               
               ENDIF

          ELSE
C *****************************************************************************************************
C *****************************************************************************************************


C IF GEPFLAG= 2
C CALCULATING DAILY GEP G C/M2/DAY
C NOTE  the following is based on New Analysis by Asko (Aug 24, 2010)


C -- CROP

               IF (K.EQ.1  .OR. K .EQ. 2) THEN 

               GEP(J, M, K) = 3.13 * ET(J,M,K)

               RECO(J,M,K)= 11.14 + 1.85 * ET(J,M,K)*MNDAY 


                
C -- DECIDUOUS FOREST
 
               ELSEIF (K .EQ. 5  .OR. K .EQ. 6) THEN 

               GEP(J, M, K) = 3.2 * ET(J,M,K)              
               RECO(J,M,K)= 24.12 + 1.49 * ET(J,M,K)*MNDAY

c MIXED FOREST

               
               ELSEIF (K .EQ. 4 .OR. K .EQ. 7) THEN 
               
               GEP(J, M, K) = 2.74 * ET(J,M,K)
             
               IF (TEMP(I,J,M) .LE. -1.0) THEN 
                
                GEP(J, M, K) = 0.0
                
               ENDIF 

               RECO(J,M,K)= 24.44 + 0.617 * GEP(J,M,K)*MNDAY
               
C -- GRASSLANDS               
        ELSEIF (K .EQ. 3 .OR. K .EQ. 11 .OR. K .EQ. 13) THEN                
               
               GEP(J, M, K) = 2.12 * ET(J,M,K)
               RECO(J,M,K)= 14.2 + 1.42 * ET(J,M,K)*MNDAY
               
C -- SHRUBLANDS                   
               
               ELSEIF (K .EQ. 9 .OR. K .EQ. 10 .OR. K .EQ. 8) THEN                

               GEP(J, M, K) = 1.35 * ET(J,M,K)  

                
               RECO(J,M,K)= 3.93 + 0.85 * ET(J,M,K)*MNDAY

 
                                           
C -- SAVANNAS                   
               
C               ELSEIF (K .EQ. 12) THEN                

c            GEP(J, M, K) = (1.171 * ET(J,M,K)*MNDAY + 10.5)/MNDAY
C               GEP(J, M, K) = 1.26 * ET(J,M,K)
               
c               RECO(J,M,K)= 19.6 + 2.0 * TEMP(I,J,M)
               
C               RECO(J,M,K)= 27.7 + 0.87 * ET(J,M,K)*MNDAY
                              
c               RECO(J,M,K)= 25.24 + 0.53 * GEP(J,M,K)*MNDAY 
               
C          RECO(J, M, K) = 41.6 * EXP((TEMP(I,J,M)-10.0)*LOG(1.34)/10.0)
               
               
C       RECO(J, M, K) = ( 41.60 * EXP((TEMP(I,J,M)-10.0)*LOG(1.336)/10.0))
               
C     >   + 27.23 * EXP((TEMP(I,J,M)-10.0)*LOG(2.41)/10.) )/ 2.0
        

C          RECO(J,M,K) =( (49.94+0.475*RAIN(I,J,M))*
C     >                  EXP((TEMP(I,J,M)-10.0)*LOG(1.08)/10.0)
C     >  + (23.94+0.268*RAIN(I,J,M))*
C     >                  EXP((TEMP(I,J,M)-10.0)*LOG(2.18)/10.0) )/2.

                               
C -- URBAN/BARRENS/WATRE BODY (SAME AS OPEN SHRUB)                  
               
           ELSE
               
              GEP(J, M, K) = 0.
              RECO(J,M,K) =0.
              
              
           ENDIF

                 IF (GEP(J,M,K) .LE. 0.) THEN
                      GEP(J,M,K) = 0.                  
                 ENDIF
                                   
          ENDIF                
                RECO(J,M,K) = RECO(J,M,K)/MNDAY 
                                                            
                NEE(J, M, K) = -GEP(J,M,K) + RECO(J,M,K)


                                    
C --- COMPUTE FRACTION OF EACH WATER BALANCE COMPONENT AND GEP FOR EACH LAND COVER

C *****************************************************************************************************
C -- CALCULATE THE FRACTION OF daily GEP

               GEPTEMP = GEPTEMP + GEP(J,M,K)*LADUSE(I,K)   
               RECOTEMP = RECOTEMP + RECO(J,M,K)*LADUSE(I,K)               
               NEETEMP = NEETEMP + NEE(J,M,K)*LADUSE(I,K)
                            

C *****************************************************************************************************
C --- CALCULATE THE FRACTION OF daily AET
               
               AETTEMP = AETTEMP + ET(J,M,K) * LADUSE(I,K)
               
C *****************************************************************************************************
C--- CALCULATE THE FRACTION OF daily SURFACE RUNOFF
              
               RUNOFFTEMP = RUNOFFTEMP + SURFRO(K) * LADUSE(I,K)
               
C *****************************************************************************************************
C--- CALCULATE THE FRACTION OF DAILY PRIMARY BASEFLOW
              
               PBFTEMP = PBFTEMP + PBF(K) * LADUSE(I,K)               
               
C *****************************************************************************************************
C--- CALCULATE THE FRACTION OF DAILY SECONDARY BASEFLOW
              
               SBFTEMP = SBFTEMP + SBF(K) * LADUSE(I,K)               



C *****************************************************************************************************
C--- CALCULATE THE FRACTION OF DAILY INTERFLOW
              
               IFTEMP = IFTEMP + INF(K) * LADUSE(I,K)                                              
              
C *****************************************************************************************************


C ----CALCUALTE TOTAL RUNOFF FOR EACH LANDUSE, K (GE SUN OCT 19, 2010) 

                                   
            RUNLAND(I,J,M,DAY, K) = SURFRO(K) + PBF(K) + SBF(K) + INF(K)


C--- calculate the fraction of soil moisture 
          
              
              TAUZTWC = TAUZTWC + UZTWC(K) * LADUSE(I,K)
              
              TAUZFWC = TAUZFWC + UZFWC(K) * LADUSE(I,K)
              
              TALZTWC = TALZTWC + LZTWC(K) * LADUSE(I,K)
              
              TALZFPC = TALZFPC + LZFPC(K) * LADUSE(I,K)
              
              TALZFSC = TALZFSC + LZFSC(K) * LADUSE(I,K)
              
              TASM = TASM + (UZTWC(K)+UZFWC(K)+LZTWC(K)+LZFPC(K)
     &        +LZFSC(K)) * LADUSE(I,K)
                   
               TAREA = TAREA + LADUSE(I,K) 

    
40         CONTINUE

           
C -- CALCULATE AVG SMC

              AUZTWC = TAUZTWC / TAREA
              AUZFWC = TAUZFWC / TAREA
              ALZTWC = TALZTWC / TAREA
              ALZFPC = TALZFPC / TAREA
              ALZFSC = TALZFSC / TAREA
              ASM = TASM/TAREA

100        CONTINUE


           AET(M) = AETTEMP        
           RUNOFF(M) = RUNOFFTEMP
           PRIBF(M) = PBFTEMP
           SECBF(M) = SBFTEMP
           INTF(M) = IFTEMP
           SMC (M) = ASM          
           SP(M) = SNOWPACK
           AVUZTWC(M) = AUZTWC
           AVUZFWC(M) = AUZFWC
           AVLZTWC(M) = ALZTWC
           AVLZFPC(M) = ALZFPC
           AVLZFSC(M) = ALZFSC
                   
           IF (RUNOFF(M) .LT. 0.) THEN
           
           RUNOFF(M)=0.
           
           ENDIF            
     

C -- STREAMFLOW IN MILLION M3 FOR EACH HUC FOR MONTH M. HUCAREA IN SQ. METERS



          STRFLOW(I, J, M) = (RUNOFF(M) + PRIBF(M) + SECBF(M) + INTF(M))
     & * HUCAREA(I)/1000./1000000.
           
           GEPM(I,J, M) = GEPTEMP
           RECOM(I,J,M)  = RECOTEMP
           NEEM(I,J,M) = NEETEMP
          
C
C --- Return
      RETURN
      END
