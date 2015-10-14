C**********************************************************************C
C                                                                      C
C     *** SUBROUTINE WATERBAL ***                                      C
C     SIMULATES MONTHLY WATER BALANCE USING 2 LAYER SOIL MOISTURE      C
C     ALGORITHM FROM NOAA NATIONAL WEATHER SERVICE SACRAMENTO SOIL     C
C     MOISTURE ACCOUNTING MODEL (SAC-SMA)                              C
C！！！！！！！！更改591行的碳平衡计算公式                             C
C！！！！！！！817行RUNLAND (流域数，年数，月份，月份，植被类型数）数组大小C
C**********************************************************************C
      
      SUBROUTINE WATERBAL(TUN1,I,J,M,MNDAY)
C      use myvar
      COMMON/BASIC/NGRID, NYEAR, NLC,BYEAR,IYSTART,IYEND
      COMMON/VAL/VAL_1(100), VAL_2(100) , VAL_3 ,VAL_4,VAL_5,VAL_6

      COMMON/OUTPUT1/ PET(200,12,20),APET(12),PAET(200,12,20),APAET(12),
     &AET(12), RUNOFF(12), INTER(12), PRIBF(12), SECBF(12), INTF(12), 
     &AVUZTWC(12), AVUZFWC(12), AVLZTWC(12), AVLZFPC(12)
     >,A_ET(1000,200, 12),P_ET(1000,200, 12),Sun_ET(1000,200, 12)
     >,RUN_HRU(1000,200, 12),BASE_HRU(1000,200, 12)
	 
      COMMON/SNOWPACK/SP(12), SNOWPACK, NSPM(200)
       
      COMMON/SUMMARY1/ANURAIN(200),ANURUN(200),ANUPET(200),ANUAET(200)
     >,ANUPAET(200)
              
      COMMON/SOIL/LZTWM(1000), LZFPM(1000), LZFSM(1000), LZSK(1000),
     >  LZPK(1000), UZTWM(1000), UZFWM(1000), UZK(1000), ZPERC(1000),
     >  REXP(1000), PFREE(1000), SMC(12)

      COMMON/CLIMATE/ RAIN(1000,200,12), TEMP(1000,200, 12), AAPPT(1000)
                                       
            
      COMMON/CELLINFO/LADUSE(1000,20),HUCNO(1000),
     >                LATUDE(1000),LONGI(1000)
     
      
      COMMON/HUC/ HUCAREA(1000)
       
      COMMON/FLOW/ STRFLOW(1000, 200, 12),STRET(1000, 200, 12)
     >,STRGEP(1000, 200, 12)
       
      COMMON/CARBON/ GEPM(1000, 200, 12),RECOM(1000,200,12), 
     >  NEEM(1000,200,12),GEPA(1000,200),NEEA(1000,200)
       
       
C----RUNLAND (流域数，年数，月份，日，植被类型数）
C----由于设计问题编译时会超出数组大小的限制                    
      COMMON/BYLAND/ RUNLAND(600, 30,12, 31,13), 
     >ETLAND(600, 30,12, 31,13), GEPLAND(600, 30,12, 31,13)
C ----------------------------------------------------------------------------     
     
     
      INTEGER DAY, HUCNO, MNDAY
      
      REAL AETTEMP, RUNOFFTEMP, PBFTEMP, SBFTEMP, 
     > IFTEMP, GEPTEMP,RECOTEMP, NEETEMP
          
      REAL LZTWM, LZFPM, LZFSM, LZSK,
     >  LZPK, UZTWM, UZFWM, UZK, ZPERC,
     >  REXP, PFREE

      REAL UZTWC, UZFWC, LZTWC, LZFSC, LZFPC
      
      REAL ETUZTW(200,12,20), RESIDET(200,12,20), ETUZFW(200,12,20)
      
      REAL ETLZTW(200, 12, 20), RATLZT, RATLZ
      
      REAL SNOW, SNOWW, SP
      
      REAL LADUSE, TASM, TAREA
      
      REAL ET(200,12,20), SURFRO, GEP(200,12,20), INFIL,
     >  RECO(200, 12, 20), NEE(200, 12,20) 
      
      REAL DPAET, PET
      
      REAL UZRAT, TWX, PERCM, PERC, DEFR, LZDEF 
      
      REAL PERCT, PERCF
      
      REAL HPL, RATLP, RATLS, FRACP, PERCP,PERCS
      
      REAL PBF, SBF, INF
      
      REAL TAUZTWC, TAUZFWC, TALZTWC, TALZFPC, TALZFSC
      
      REAL AUZTWC, AUZFWC, ALZTWC, ALZFPC, ALZFSC
      
      REAL ASM, AET, RUNOFF, PRIBF, SECBF, INTF, 
     &SMC
      
      REAL AVUZTWC, AVUZFWC, AVLZTWC, AVLZFPC, 
     >     AVLZFSC(12)
      
      REAL STRFLOW,GEPM,RECOM,STRET,STRGEP,
     >      NEEM
      
      INTEGER GEPFLAG
      
      REAL HUCELE(1000)
      
      double precision HUCAREA
      
      REAL RUNLAND,ETLAND,GEPLAND 
            
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
        
                  

                        
           UZTWC = 0.1*UZTWM(I)
           UZFWC = 0.0
           LZTWC = 0.1*LZTWM(I)
           LZFSC = 0.75*LZFSM(I)
           LZFPC = 0.75*LZFPM(I)
           

        
        ENDIF 
         
C *****************************************************************************************************
C *****************************************************************************************************
C----- SIMULATE SNOWPACK (SEE VAROSMARTY  ET AL., 1989)
      
        IF (TEMP(I,J, M) .LE.  -1.0) THEN
        
           SNOW = RAIN(I,J, M)
    
           SNOWPACK = SNOWPACK + SNOW

           SNOWW=SNOWPACK*(0.68+0.18*TEMP(I,J, M))
         !  SNOWW=0
           IF (SNOWW .LE. 0) THEN SNOWW=0
           
           SNOWPACK = SNOWPACK - SNOWW


           
           IAM = 0
                                
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
          
!                ET(J,M,K) = 0.
C                SURFRO = 0.
!               INF = 0.
                GEP (J, M, K) = 0.
C *****************************************************************************************************               
C *****************计算温度小于-1℃的ET**********************************************    
!       P+SOIL > PET      ET=PET   
!       P+SOIL < PET      ET=P
C *****************************************************************************************************       
             DPAET=0.0
             DPAET=PAET(J,M,K)/MNDAY


            IF (UZTWC .GT. DPAET) THEN 
                ET(J,M,K)=DPAET
                UZTWC(K)=UZTWC(K)-ET(J,M,K)
              ELSE
                ET(J,M,K)=0
            ENDIF

       
C *****************************************************************************************************
C -- COMPUTE THE DAILY AVERAGE INFILTRATION FOR A GIVEN MONTH FOR EACH LAND USE
C---流域植被类型日水分输入量=降水+融雪
                INFIL = SNOWW/MNDAY
                 

C     --- COMPUTE WATER IN EXCESS OF UZ TENSION WATER CAPACITY (TWX)

                  TWX = INFIL + UZTWC - UZTWM(I)

C *****************************************************************************************************
C *****************************************************************************************************
C --- COMPUTE PERCOLATION INTO SOIL WATER STORAGES AND SURFACE RUNOFF

                    IF (TWX(K).GE.0.0) THEN           	
C     --- IF INFIL EXCEEDS UZ TENSION WATER CAPACITY, SET UZ TENSION WATER STORAGE TO CAPACITY, 
C         REMAINDER OF INFIL GOES TO UZFWC IF EXCEEDS UZFWC EXCESS GOES TO SURFACE RUNOFF

                     UZTWC = UZTWM(I)     
                
                     UZFWC = UZFWC + TWX
                
                     IF (UZFWC .GT. UZFWM(I)) THEN
                
                        SURFRO = UZFWC - UZFWM(I)
                
                        UZFWC = UZFWM(I)
                
                     ELSE
                
                        SURFRO = 0.0
                
                     ENDIF 

                  ELSE        	
           

C     --- IF INFIL DOES NOT EXCEED UZ TENSION WATER CAPACITY, ALL INFIL GOES TO UZ TENSION WATER STORAGE

                     UZTWC = UZTWC + INFIL
                     SURFRO = 0.0
             	
                  ENDIF
           
C --- COMPUTE PERCOLATION TO LZ IF FREE WATER IS AVAILABLE IN UZ

		
	          IF (UZFWC .GT. 0.0) THEN
		

C     --- COMPUTE PERCOLATION DEMAND FROM LZ

                     PERCM = LZFPM * LZPK(I) + LZFSM * LZSK(I)
                
                     PERC = PERCM * (UZFWC(K)/UZFWM(I))
                
                     DEFR(K)=1.0-((LZTWC(K)+LZFPC(K)+LZFSC(K))/
     &(LZTWM(I)+LZFPM(I)+LZFSM(I)))
                
                     PERC = PERC * (1.0 + ZPERC(I) * (DEFR
     &**REXP(I)))
            

C     --- COMPARE LZ PERCOLATION DEMAND TO UZ FREE WATER AVAILABLE AND COMPUTE ACTUAL PERCOLATION

                    IF (PERC .LT. UZFWC(K)) THEN
                
                       UZFWC = UZFWC - PERC
                
                    ELSE
                
                       PERC = UZFWC
                
                       UZFWC = 0.0
                
                    ENDIF
            
C      --- CHECK TO SEE IF PERC EXCEEDS LZ TENSION AND FREE WATER DEFICIENCY, IF SO SET PERC TO LZ DEFICIENCY


                    LZDEF = (LZTWC + LZFPC + LZFSC(K)) - 
     &(LZTWM(I) + LZFPM(I) + LZFSM(I)) + PERC
                    
                    IF (LZDEF .GT. 0.0) THEN
                    
                       PERC = PERC - LZDEF
          
                       UZFWC = UZFWC + LZDEF
                       
                    ENDIF
                
                
C --- DISRIBUTE PERCOLATED WATER INTO THE LZ STORAGES AND COMPUTE THE REMAINDER IN UZ FREE WATER STORAGE AND RESIDUAL AVAIL FOR RUNOFF
   

C     --- COMPUTE PERC WATER GOING INTO LZ TENSION WATER STORAGE AND COMPARE TO AVAILABLE STORAGE

                    PERCT = PERC * (1.0 - PFREE(I))
                
                    IF ((PERCT + LZTWC(K)) .GT. LZTWM(I)) THEN
                
C     --- WHEN PERC IS GREATER THAN AVAILABLE TENSION WATER STORAGE, SET TENSION WATER STORAGE TO MAX, REMAINDER OF PERC GETS EVALUATED AGAINST FREE WATER STORAGE

                       PERCF = PERCT + LZTWC - LZTWM(I)
                
                       LZTWC = LZTWM(I)
                
                    ELSE
                
C     --- WHEN PERC IS LESS THAN AVAILABLE TENSION WATER STORAGE, UPDATE TENSION WATER STORAGE

                       LZTWC = LZTWC + PERCT
                
                       PERCF = 0.0
                
                    ENDIF
                
C     --- COMPUTE TOTAL PERC WATER GOING INTO LZ FREE WATER STORAGE

                    PERCF = PERCF + PERC * PFREE(I)                

                    IF(PERCF .GT. 0.0) THEN
                
C     --- COMPUTE RELATIVE SIZE OF LZ PRIMARY FREE WATER STORAGE COMPARED TO LZ TOTAL FREE WATER STORAGE

                       HPL = LZFPM(I) / (LZFPM(I) + LZFSM(I))
                
C     --- COMPUTE LZ PRIMARY AND SECONDARY FREE WATER CONTENT TO CAPACITY RATIOS

                       RATLP = LZFPC / LZFPM(I)
                
                       RATLS = LZFSC / LZFSM(I)
                
C     --- COMPUTE FRACTIONS AND PERCENTAGES OF FREE WATER PERC TO GO TO LZ PRIMARY STORAGE

                       FRACP = (HPL * 2.0 * (1.0 - RATLP(K))) 
     >                 / ((1.0 - RATLP(K)) + (1.0 - RATLS(K)))
                
                       IF (FRACP .GT. 1.0) FRACP = 1.0

                          PERCP = PERCF * FRACP
                
                          PERCS = PERCF - PERCP
                
C     --- COMPUTE NEW PRIMARY AND SECONDARY STORAGE

C         --- COMPUTE NEW SECONDARY FREE WATER STORAGE

                          LZFSC = LZFSC + PERCS

                          IF(LZFSC .GT. LZFSM(I)) THEN
                
C         --- IF NEW SECONDARY FREE WATER STORAGE EXCEEDS CAPACITY SET SECONDARY STORAGE TO CAPACITY AND EXCESS GOES TO PRIMARY FREE WATER STORAGE

                             PERCS = PERCS - LZFSC + LZFSM(I)
                
                             LZFSC = LZFSM(I)
                          
                          ENDIF
                
            
C         --- IF NEW LZ SECONDARY FREE WATER STORAGE IS LESS THAN CAPACITY MOVE ON TO COMPUTE NEW PRIMARY FREE WATER STORAGE


                       LZFPC = LZFPC + (PERCF - PERCS(K))

                
                       IF (LZFPC .GT. LZFPM(I)) THEN

C             --- IF LZ FREE PRIMARY WATER STORAGE EXCEEDS CAPACITY SET PRIMARY STORAGE TO CAPACITY AND EVALUATE EXCESS AGAINST LZ TENSION WATER STORAGE

                          LZTWC = LZTWC + LZFPC - LZFPM(I)
                
                          LZFPC = LZFPM(I)
                
                          IF (LZTWC .GT. LZTWM(I)) THEN

C            --- IF LZ TENSION WATER EXCEEDS CAPACITY EVALUATE EXCESS AGAINST UZ FREE WATER CAPACITY AND SET LZ TENSION WATER STORAGE TO CAPACITY

                             UZFWC = UZFWC + LZTWC - LZTWM(I)
                
                             LZTWC = LZTWM(I)
                             
                          ENDIF
                          
                       ENDIF
                       
                    ENDIF
                
		 ENDIF
		 
C ***************************************************************************************************** 
C *****************************************************************************************************                
C --- COMPUTE BASEFLOW AND UPDATE LZ PRIMARY AND SECONDARY FREE WATER STORAGES

                
C      --- COMPUTE PRIMARY BASEFLOW AND COMPARE TO AVAILABLE FREE PRIMARY STORAGE

                 PBF = LZFPC * LZPK(I)
                
                 LZFPC = LZFPC - PBF
                
                 IF (LZFPC .LE. 0.0001) THEN 
                
                    PBF = PBF + LZFPC
                
                    LZFPC = 0.0
                
                 ENDIF
                

C      --- COMPUTE SECONDARY BASEFLOW AND COMPARE TO AVAILABLE FREE PRIMARY STORAGE

                 SBF = LZFSC * LZSK(I)
                
                 LZFSC = LZFSC - SBF
                
                 IF (LZFSC .LE. 0.0001) THEN
                
                   SBF = SBF + LZFSC
                
                   LZFSC = 0.0
                
                 ENDIF                          

             ELSE
                  
C *****************************************************************************************************
C *****************************************************************************************************
C -- COMPUTE THE DAILY AVERAGE INFILTRATION FOR A GIVEN MONTH FOR EACH LAND USE
C---流域植被类型日水分输入量=降水+融雪
                INFIL = RAIN(I,J,M)/MNDAY + SNOWW/MNDAY
          
            
C *****************************************************************************************************
C --- COMPUTE AET GIVEN TOTAL WATER STORED IN UPPER SOIL LAYER STORAGES AND PAET CALCULATED IN PET.FOR
C --- ASSUME ET IS SUPPLIED ONLY FROM UPPER LAYER NO UPWARD FLUX FROM LOWER LAYER TO UPPER LAYER
C --- NOTE THAT SAC-SMA ALLOWS ET TO ALSO BE SUPPLIED UNRESTRICTED BY LZ TENSION WATER STORAGE

                
                   DPAET = PAET(J, M, K)/MNDAY
               
                   ET(J, M, K) = DPAET
                
C		开始计算植被类型K 每天的ET、
C --- COMPUTE ET FROM UZ TENSION WATER STORAGE, RECALCULATE UZTWC, CALCULATE RESIDUAL ET DEMAND

                   ETUZTW(J, M, K) = ET(J, M, K) * (UZTWC(K)/UZTWM(I))
                   
                   RESIDET(J, M, K) = ET(J, M, K) - ETUZTW(J, M, K)
                   
                   UZTWC = UZTWC - ETUZTW(J, M, K)
                   
                   ETUZFW(J, M, K) = 0.0
                   
                   IF (UZTWC(K).GE.0.0) GOTO 220
                   
                   ETUZTW(J, M, K) = ETUZTW(J, M, K) + UZTWC
                   
                   UZTWC = 0.0
                   
                   RESIDET(J, M, K) = ET(J, M, K) - ETUZTW(J, M, K)
                   
C --- COMPUTE ET FROM UZ FREE WATER STORAGE, RECALCULATE UZFWC, CALCULATE RESIDUAL ET DEMAND                   
                   
                   IF (UZFWC .GE. RESIDET(J, M, K)) GO TO 221
                   
                   ETUZFW(J, M, K) = UZFWC
                   
                   UZFWC = 0.0
                   
                   RESIDET(J, M, K) = RESIDET(J, M, K) - ETUZFW(J, M, K)
                   
                   GO TO 225
                   
221                ETUZFW(J, M, K) = RESIDET(J, M, K)

                   UZFWC = UZFWC - ETUZFW(J, M, K)
                   
                   RESIDET(J, M, K) = 0.0
                   
C --- REDISTRIBUTE WATER BETWEEN UZ TENSION WATER AND FREE WATER STORAGES

220                IF((UZTWC(K)/UZTWM(I)).GE.(UZFWC(K)/UZFWM(I))) 
     &             GO TO 225

                   UZRAT(K)=(UZTWC(K)+UZFWC(K))/(UZTWM(I)+UZFWM(I))
                      
                   UZTWC = UZTWM(I) * UZRAT
                      
                   UZFWC = UZFWM(I) * UZRAT
                        
225                IF (UZTWC .LT. 0.00001) UZTWC = 0.0

                   IF (UZFWC .LT. 0.00001) UZFWC = 0.0
                   
                   
C --- COMPUTE ET FROM LZ TENSION WATER STORAGE, RECALCULATE LZTWC, CALCULATE RESIDUAL ET DEMAND

                   ETLZTW(J, M, K) = RESIDET(J, M, K) * (LZTWC / 
     &             (UZTWM(I) + LZTWM(I)))
                   
                   LZTWC = LZTWC - ETLZTW(J, M, K)
                   
                   IF(LZTWC .GE. 0.0) GO TO 226
                   
                   ETLZTW(J, M, K) = ETLZTW(J, M, K) + LZTWC
                   
                   LZTWC = 0.0
                   
226                RATLZT = LZTWC / LZTWM(I)

                   RATLZ = (LZTWC + LZFPC + LZFSC(K)) / 
     &             (LZTWM(I) + LZFPM(I) + LZFSM(I))
     
                   IF (RATLZT .GE. RATLZ(K)) GO TO 230
                  
                   LZTWC = LZTWC + (RATLZ - RATLZT(K)) * 
     &             LZTWM(I)

                   
                   LZFSC = LZFSC - (RATLZ - RATLZT(K)) * 
     &             LZTWM(I)
                   
                   IF(LZFSC .GE. 0.0) GO TO 230
                   
                   LZFPC = LZFPC + LZFSC
                   
                   LZFSC = 0.0
                   
230                IF (LZTWC .LT. 0.00001) LZTWC = 0.0

C --- CALCULATE TOTAL ET SUPPLIED BY UPPER AND LOWER LAYERS

                   ET(J, M, K) = ETUZTW(J, M, K) + ETUZFW(J, M, K) + 
     &             ETLZTW(J, M, K)
                  IF (ET(J, M, K) .LT. 0.00001) ET(J, M, K) = 0.0
C *****************************************************************************************************
C *****************************************************************************************************
C --- COMPUTE PERCOLATION INTO SOIL WATER STORAGES AND SURFACE RUNOFF

C     --- COMPUTE WATER IN EXCESS OF UZ TENSION WATER CAPACITY (TWX)

                  TWX = INFIL + UZTWC - UZTWM(I)
           
                  IF (TWX(K).GE.0.0) THEN
             	
C     --- IF INFIL EXCEEDS UZ TENSION WATER CAPACITY, SET UZ TENSION WATER STORAGE TO CAPACITY, 
C         REMAINDER OF INFIL GOES TO UZFWC IF EXCEEDS UZFWC EXCESS GOES TO SURFACE RUNOFF

                     UZTWC = UZTWM(I)     
                
                     UZFWC = UZFWC + TWX
                
                     IF (UZFWC .GT. UZFWM(I)) THEN
                
                        SURFRO = UZFWC - UZFWM(I)
                
                        UZFWC = UZFWM(I)
                
                     ELSE
                
                        SURFRO = 0.0
                
                     ENDIF 

                  ELSE        	
           

C     --- IF INFIL DOES NOT EXCEED UZ TENSION WATER CAPACITY, ALL INFIL GOES TO UZ TENSION WATER STORAGE

                     UZTWC = UZTWC + INFIL
             	
                  ENDIF
           
C --- COMPUTE PERCOLATION TO LZ IF FREE WATER IS AVAILABLE IN UZ

		
	          IF (UZFWC .GT. 0.0) THEN
		

C     --- COMPUTE PERCOLATION DEMAND FROM LZ

                     PERCM = LZFPM * LZPK(I) + LZFSM * LZSK(I)
                
                     PERC = PERCM * (UZFWC(K)/UZFWM(I))
                
                     DEFR(K)=1.0-((LZTWC(K)+LZFPC(K)+LZFSC(K))/
     &(LZTWM(I)+LZFPM(I)+LZFSM(I)))
                
                     PERC = PERC * (1.0 + ZPERC(I) * (DEFR
     &**REXP(I)))
            

C     --- COMPARE LZ PERCOLATION DEMAND TO UZ FREE WATER AVAILABLE AND COMPUTE ACTUAL PERCOLATION

                    IF (PERC .LT. UZFWC(K)) THEN
                
                       UZFWC = UZFWC - PERC
                
                    ELSE
                
                       PERC = UZFWC
                
                       UZFWC = 0.0
                
                    ENDIF
            
C      --- CHECK TO SEE IF PERC EXCEEDS LZ TENSION AND FREE WATER DEFICIENCY, IF SO SET PERC TO LZ DEFICIENCY


                    LZDEF = (LZTWC + LZFPC + LZFSC(K)) - 
     &(LZTWM(I) + LZFPM(I) + LZFSM(I)) + PERC
                    
                    IF (LZDEF .GT. 0.0) THEN
                    
                       PERC = PERC - LZDEF
          
                       UZFWC = UZFWC + LZDEF
                       
                    ENDIF
                
                
C --- DISRIBUTE PERCOLATED WATER INTO THE LZ STORAGES AND COMPUTE THE REMAINDER IN UZ FREE WATER STORAGE AND RESIDUAL AVAIL FOR RUNOFF
   

C     --- COMPUTE PERC WATER GOING INTO LZ TENSION WATER STORAGE AND COMPARE TO AVAILABLE STORAGE

                    PERCT = PERC * (1.0 - PFREE(I))
                
                    IF ((PERCT + LZTWC(K)) .GT. LZTWM(I)) THEN
                
C     --- WHEN PERC IS GREATER THAN AVAILABLE TENSION WATER STORAGE, SET TENSION WATER STORAGE TO MAX, REMAINDER OF PERC GETS EVALUATED AGAINST FREE WATER STORAGE

                       PERCF = PERCT + LZTWC - LZTWM(I)
                
                       LZTWC = LZTWM(I)
                
                    ELSE
                
C     --- WHEN PERC IS LESS THAN AVAILABLE TENSION WATER STORAGE, UPDATE TENSION WATER STORAGE

                       LZTWC = LZTWC + PERCT
                
                       PERCF = 0.0
                
                    ENDIF
                
C     --- COMPUTE TOTAL PERC WATER GOING INTO LZ FREE WATER STORAGE

                    PERCF = PERCF + PERC * PFREE(I)                

                    IF(PERCF .GT. 0.0) THEN
                
C     --- COMPUTE RELATIVE SIZE OF LZ PRIMARY FREE WATER STORAGE COMPARED TO LZ TOTAL FREE WATER STORAGE

                       HPL = LZFPM(I) / (LZFPM(I) + LZFSM(I))
                
C     --- COMPUTE LZ PRIMARY AND SECONDARY FREE WATER CONTENT TO CAPACITY RATIOS

                       RATLP = LZFPC / LZFPM(I)
                
                       RATLS = LZFSC / LZFSM(I)
                
C     --- COMPUTE FRACTIONS AND PERCENTAGES OF FREE WATER PERC TO GO TO LZ PRIMARY STORAGE

                       FRACP = (HPL * 2.0 * (1.0 - RATLP(K))) 
     >                 / ((1.0 - RATLP(K)) + (1.0 - RATLS(K)))
                
                       IF (FRACP .GT. 1.0) FRACP = 1.0

                          PERCP = PERCF * FRACP
                
                          PERCS = PERCF - PERCP
                
C     --- COMPUTE NEW PRIMARY AND SECONDARY STORAGE

C         --- COMPUTE NEW SECONDARY FREE WATER STORAGE

                          LZFSC = LZFSC + PERCS

                          IF(LZFSC .GT. LZFSM(I)) THEN
                
C         --- IF NEW SECONDARY FREE WATER STORAGE EXCEEDS CAPACITY SET SECONDARY STORAGE TO CAPACITY AND EXCESS GOES TO PRIMARY FREE WATER STORAGE

                             PERCS = PERCS - LZFSC + LZFSM(I)
                
                             LZFSC = LZFSM(I)
                          
                          ENDIF
                
            
C         --- IF NEW LZ SECONDARY FREE WATER STORAGE IS LESS THAN CAPACITY MOVE ON TO COMPUTE NEW PRIMARY FREE WATER STORAGE


                       LZFPC = LZFPC + (PERCF - PERCS(K))

                
                       IF (LZFPC .GT. LZFPM(I)) THEN

C             --- IF LZ FREE PRIMARY WATER STORAGE EXCEEDS CAPACITY SET PRIMARY STORAGE TO CAPACITY AND EVALUATE EXCESS AGAINST LZ TENSION WATER STORAGE

                          LZTWC = LZTWC + LZFPC - LZFPM(I)
                
                          LZFPC = LZFPM(I)
                
                          IF (LZTWC .GT. LZTWM(I)) THEN

C            --- IF LZ TENSION WATER EXCEEDS CAPACITY EVALUATE EXCESS AGAINST UZ FREE WATER CAPACITY AND SET LZ TENSION WATER STORAGE TO CAPACITY

                             UZFWC = UZFWC + LZTWC - LZTWM(I)
                
                             LZTWC = LZTWM(I)
                             
                          ENDIF
                          
                       ENDIF
                       
                    ENDIF
                
		 ENDIF
		 
C ***************************************************************************************************** 
C *****************************************************************************************************                
C --- COMPUTE BASEFLOW AND UPDATE LZ PRIMARY AND SECONDARY FREE WATER STORAGES

                
C      --- COMPUTE PRIMARY BASEFLOW AND COMPARE TO AVAILABLE FREE PRIMARY STORAGE

                 PBF = LZFPC * LZPK(I)
                
                 LZFPC = LZFPC - PBF
                
                 IF (LZFPC .LE. 0.0001) THEN 
                
                    PBF = PBF + LZFPC
                
                    LZFPC = 0.0
                
                 ENDIF
                

C      --- COMPUTE SECONDARY BASEFLOW AND COMPARE TO AVAILABLE FREE PRIMARY STORAGE

                 SBF = LZFSC * LZSK(I)
                
                 LZFSC = LZFSC - SBF
                
                 IF (LZFSC .LE. 0.0001) THEN
                
                   SBF = SBF + LZFSC
                
                   LZFSC = 0.0
                
                 ENDIF                
                 

C *****************************************************************************************************
C --- COMPUTE INTERFLOW FROM UZ

                 INF = UZFWC * UZK(I)
                
                 IF (UZFWC .LT. INF(K)) THEN
                 
                    INF = UZFWC
                    
                    UZFWC = 0.0
                 
                 ELSE
                    
                    UZFWC = UZFWC - INF
                
                 ENDIF

             ENDIF

             

C *****************************************************************************************************
C --- CALCULATE THE FRACTION OF daily AET
               
               AETTEMP = AETTEMP + ET(J,M,K) * LADUSE(I,K)
               
C *****************************************************************************************************
C--- CALCULATE THE FRACTION OF daily SURFACE RUNOFF
              
               RUNOFFTEMP = RUNOFFTEMP + SURFRO * LADUSE(I,K)
               
C *****************************************************************************************************
C--- CALCULATE THE FRACTION OF DAILY PRIMARY BASEFLOW
              
               PBFTEMP = PBFTEMP + PBF * LADUSE(I,K)               
               
C *****************************************************************************************************
C--- CALCULATE THE FRACTION OF DAILY SECONDARY BASEFLOW
              
               SBFTEMP = SBFTEMP + SBF * LADUSE(I,K)               



C *****************************************************************************************************
C--- CALCULATE THE FRACTION OF DAILY INTERFLOW
              
               IFTEMP = IFTEMP + INF * LADUSE(I,K)                                              
              
C *****************************************************************************************************


C ----CALCUALTE TOTAL RUNOFF FOR EACH LANDUSE, K (GE SUN OCT 19, 2010) 

C----!!!!! RUNLAND(I,Y,DAY, K)更改
C			RUNLAND_Y=(J-1)*12+M							
            RUNLAND(I,J,M,DAY, K) = SURFRO + PBF + 
     >     SBF + INF
            ETLAND(I,J,M,DAY, K) = ET(J,M,K)
            GEPLAND(I,J,M,DAY, K) = GEP(J,M,K)

C--- calculate the fraction of soil moisture 
          
              
              TAUZTWC = TAUZTWC + UZTWC * LADUSE(I,K)
              
              TAUZFWC = TAUZFWC + UZFWC * LADUSE(I,K)
              
              TALZTWC = TALZTWC + LZTWC * LADUSE(I,K)
              
              TALZFPC = TALZFPC + LZFPC * LADUSE(I,K)
              
              TALZFSC = TALZFSC + LZFSC * LADUSE(I,K)
              
              TASM = TASM + (UZTWC(K)+UZFWC(K)+LZTWC(K)+LZFPC
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
     


       
           GEPM(I,J, M) = GEPTEMP
           IF (TEMP(I,J,M) .LT. -1)  THEN
             GEPM(I,J, M)=0
             ELSE 
             GEPM(I,J, M) = GEPTEMP
             ENDIF
           RECOM(I,J,M)  = RECOTEMP
           NEEM(I,J,M) = NEETEMP

C -- STREAMFLOW IN MILLION M3 FOR EACH HUC FOR MONTH M. HUCAREA IN SQ. METERS 
          STRFLOW(I, J, M) = (RUNOFF(M) + PRIBF(M) + SECBF(M) + INTF(M))
     & * HUCAREA(I)/1000./1000000.

C
C --- Return
      RETURN
      END
