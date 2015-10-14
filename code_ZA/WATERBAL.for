C**********************************************************************C
C                                                                      C
C     *** SUBROUTINE WATERBAL ***                                      C
C     SIMULATES MONTHLY WATER BALANCE USING 2 LAYER SOIL MOISTURE      C
C     ALGORITHM FROM NOAA NATIONAL WEATHER SERVICE SACRAMENTO SOIL     C
C     MOISTURE ACCOUNTING MODEL (SAC-SMA)                              C
C！！！！！！！！更改591行的碳平衡计算公式                             C
C！！！！！！！817行RUNLAND (流域数，年数，月份，月份，植被类型数）数组大小C
C**********************************************************************C
      
      SUBROUTINE WATERBAL(TUN1,I,J,DAY)
C      use myvar
      COMMON/BASIC/NGRID, NYEAR, NLC,BYEAR,IYSTART,IYEND
      COMMON/VAL/VAL_1(100), VAL_2(100) , VAL_3 ,VAL_4,VAL_5,VAL_6

      COMMON/OUTPUT1/ PET(200,12,20),APET(12),PAET(200,12,20),APAET(12),
     &AET(366), RUNOFF(366), INTER(366), PRIBF(366), SECBF(366), 
     &INTF(366),AVUZTWC(366), AVUZFWC(366), AVLZTWC(366), AVLZFPC(366)
     >,A_ET(1000,200, 12),P_ET(1000,200, 12),Sun_ET(1000,200, 12)
     >,RUN_HRU(100,100,366),BASE_HRU(1000,200, 12)
	 
      COMMON/SNOWPACK/SP(366), SNOWPACK, NSPM(200)
       
      COMMON/SUMMARY1/ANURAIN(200),ANURUN(200),ANUPET(200),ANUAET(200)
     >,ANUPAET(200)
              
      COMMON/SOIL/LZTWM(1000), LZFPM(1000), LZFSM(1000), LZSK(1000),
     >  LZPK(1000), UZTWM(1000), UZFWM(1000), UZK(1000), ZPERC(1000),
     >  REXP(1000), PFREE(1000), SMC(366)

      COMMON/CLIMATE/ RAIN(100,200,366), TEMP(100,200,366), 
     <AAPPT(1000),P_PET(100,200,366)
                                       
            
      COMMON/CELLINFO/LADUSE(1000,20),HUCNO(1000),
     >                LATUDE(1000),LONGI(1000)
     
      
      COMMON/HUC/ HUCAREA(1000)
       
      COMMON/FLOW/ STRFLOW(100,200, 366),STRET(1000, 200, 12)
     >,STRGEP(1000, 200, 12)
       
      COMMON/CARBON/ GEPM(1000, 200, 12),RECOM(1000,200,12), 
     >  NEEM(1000,200,12),GEPA(1000,200),NEEA(1000,200)
       
       
C----RUNLAND (流域数，年数，月份，日，植被类型数）
C----由于设计问题编译时会超出数组大小的限制                    
      COMMON/BYLAND/ RUNLAND(600,100,366) 
C     > ,ETLAND(600, 30,12, 31,13), GEPLAND(600, 30,12, 31,13)
C ----------------------------------------------------------------------------     
     
     
      INTEGER  HUCNO, MNDAY,IDY,DAY
      
      REAL AETTEMP, RUNOFFTEMP, PBFTEMP, SBFTEMP, 
     > IFTEMP, GEPTEMP,RECOTEMP, NEETEMP
          
      REAL LZTWM, LZFPM, LZFSM, LZSK,
     >  LZPK, UZTWM, UZFWM, UZK, ZPERC,
     >  REXP, PFREE

      REAL UZTWC, UZFWC, LZTWC, LZFSC, LZFPC
      
      REAL ETUZTW(200,366), RESIDET(200,366), ETUZFW(200,366)
      
      REAL ETLZTW(200,366), RATLZT, RATLZ
      
      REAL SNOW, SNOWW, SP
      
      REAL LADUSE, TASM, TAREA
      
      REAL ET(200,366), SURFRO, GEP(200,366), INFIL,
     >  RECO(200,366), NEE(200,366) 
      
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
     >     AVLZFSC(366)
      
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
                         
C-----IDY = THE CALANDER YEAR, BYEAR = YEAR TO SATRT
         
      IDY = J + 1979 - 1
      
      
      			 
C		SET default soil water content
		 
        IF (J .EQ. 1 .AND. DAY .EQ. 1) THEN

        IAM =0     
        
                        
           UZTWC = 0.1*UZTWM(I)
           UZFWC = 0.0
           LZTWC = 0.1*LZTWM(I)
           LZFSC = 0.75*LZFSM(I)
           LZFPC = 0.75*LZFPM(I)
           SNOWPACK=0

        
        ENDIF 
         
C *****************************************************************************************************
C *****************************************************************************************************
C----- SIMULATE SNOWPACK (SEE VAROSMARTY  ET AL., 1989)
      
        IF (TEMP(I,J,DAY) .LE.  -1.0) THEN
        
           SNOW = RAIN(I,J,DAY)
    
           SNOWPACK = SNOWPACK + SNOW

!           SNOWW=SNOWPACK*(0.68+0.18*TEMP(I,J,DAY))
           SNOWW=0
!           IF (SNOWW .LE. 0) THEN SNOWW=0
           
!           SNOWPACK = SNOWPACK - SNOWW
  
                                
        ELSE 
        

              SNOWW = 0.7 *(TEMP(I,J,DAY)+0.4)
              
              SNOWPACK = SNOWPACK - SNOWW
			If (SNOWPACK .le. 0) then
				SNOWPACK= 0
				SNOWW=SNOWPACK
			Endif

     
        ENDIF  
                            

C *****************************************************************************************************
C *****************************************************************************************************
C *****************************************************************************************************
C -- LOOP THROUGH DAYS OF CURRENT MONTH AND CALCULATE SOIL WATER STORAGE, BASEFLOW, RUNOFF, AND AET
        
C        DO 100 DAY= 1, MNDAY    
        
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
         
C             K=0
          
C             DO 40 K=1, NLC
                  

C *****************************************************************************************************
C -- SET ET, SURFACE RUNOFF, INTERFLOW, GEP TO ZERO IF TEMPERATURE IS LE -1.0
C -- BASEFLOW STILL OCCURS

             IF (TEMP (I,J,DAY) .LE. -1.0) THEN
          
!                ET(J,DAY) = 0.
C                SURFRO = 0.
!               INF = 0.
!                GEP (J, M, K) = 0.
C *****************************************************************************************************               
C *****************计算温度小于-1℃的ET**********************************************    
!       P+SOIL > PET      ET=PET   
!       P+SOIL < PET      ET=P
C *****************************************************************************************************       
             DPAET=0.0
             DPAET=P_PET(I,J,DAY)

            IF (UZTWC .GT. DPAET) THEN 
                ET(J,DAY)=DPAET
                UZTWC=UZTWC-ET(J,DAY)
              ELSE
                ET(J,DAY)=0
            ENDIF

       
C *****************************************************************************************************
C -- COMPUTE THE DAILY AVERAGE INFILTRATION FOR A GIVEN MONTH FOR EACH LAND USE
C---流域植被类型日水分输入量=降水+融雪
                INFIL = SNOWW
                 

C     --- COMPUTE WATER IN EXCESS OF UZ TENSION WATER CAPACITY (TWX)

                  TWX = INFIL + UZTWC - UZTWM(I)

C *****************************************************************************************************
C *****************************************************************************************************
C --- COMPUTE PERCOLATION INTO SOIL WATER STORAGES AND SURFACE RUNOFF

                    IF (TWX.GE.0.0) THEN           	
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

                     PERCM = LZFPM(I) * LZPK(I) + LZFSM(I) * LZSK(I)
                
                     PERC = PERCM * (UZFWC/UZFWM(I))
                
                     DEFR=1.0-((LZTWC+LZFPC+LZFSC)/
     &(LZTWM(I)+LZFPM(I)+LZFSM(I)))
                
                     PERC = PERC * (1.0 + ZPERC(I) * (DEFR
     &**REXP(I)))
            

C     --- COMPARE LZ PERCOLATION DEMAND TO UZ FREE WATER AVAILABLE AND COMPUTE ACTUAL PERCOLATION

                    IF (PERC .LT. UZFWC) THEN
                
                       UZFWC = UZFWC - PERC
                
                    ELSE
                
                       PERC = UZFWC
                
                       UZFWC = 0.0
                
                    ENDIF
            
C      --- CHECK TO SEE IF PERC EXCEEDS LZ TENSION AND FREE WATER DEFICIENCY, IF SO SET PERC TO LZ DEFICIENCY


                    LZDEF = (LZTWC + LZFPC + LZFSC) - 
     &(LZTWM(I) + LZFPM(I) + LZFSM(I)) + PERC
                    
                    IF (LZDEF .GT. 0.0) THEN
                    
                       PERC = PERC - LZDEF
          
                       UZFWC = UZFWC + LZDEF
                       
                    ENDIF
                
                
C --- DISRIBUTE PERCOLATED WATER INTO THE LZ STORAGES AND COMPUTE THE REMAINDER IN UZ FREE WATER STORAGE AND RESIDUAL AVAIL FOR RUNOFF
   

C     --- COMPUTE PERC WATER GOING INTO LZ TENSION WATER STORAGE AND COMPARE TO AVAILABLE STORAGE

                    PERCT = PERC * (1.0 - PFREE(I))
                
                    IF ((PERCT + LZTWC) .GT. LZTWM(I)) THEN
                
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

                       FRACP = (HPL * 2.0 * (1.0 - RATLP)) 
     >                 / ((1.0 - RATLP) + (1.0 - RATLS))
                
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


                       LZFPC = LZFPC + (PERCF - PERCS)

                
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
				 
C Begin to calculate Soil water when T > -1 ℃				 
             
			 ELSE
                  
C *****************************************************************************************************
C -- COMPUTE THE DAILY AVERAGE INFILTRATION FOR A GIVEN MONTH FOR EACH LAND USE
C---流域植被类型日水分输入量=降水+融雪
                INFIL = RAIN(I,J,DAY) + SNOWW
          
            
C *****************************************************************************************************
C --- COMPUTE AET GIVEN TOTAL WATER STORED IN UPPER SOIL LAYER STORAGES AND PAET CALCULATED IN PET.FOR
C --- ASSUME ET IS SUPPLIED ONLY FROM UPPER LAYER NO UPWARD FLUX FROM LOWER LAYER TO UPPER LAYER
C --- NOTE THAT SAC-SMA ALLOWS ET TO ALSO BE SUPPLIED UNRESTRICTED BY LZ TENSION WATER STORAGE

                
                   DPAET = P_PET(I,J,DAY)
               
                   ET(J,DAY) = DPAET
                
C		开始计算植被类型K 每天的ET、
C --- COMPUTE ET FROM UZ TENSION WATER STORAGE, RECALCULATE UZTWC, CALCULATE RESIDUAL ET DEMAND

                   ETUZTW(J,DAY) = ET(J,DAY) * (UZTWC/UZTWM(I))
                   
                   RESIDET(J,DAY) = ET(J,DAY) - ETUZTW(J,DAY)
                   
                   UZTWC = UZTWC - ETUZTW(J,DAY)
                   
                   ETUZFW(J,DAY) = 0.0
                   
                   IF (UZTWC.GE.0.0) GOTO 220
                   
                   ETUZTW(J,DAY) = ETUZTW(J,DAY) + UZTWC
                   
                   UZTWC = 0.0
                   
                   RESIDET(J,DAY) = ET(J,DAY) - ETUZTW(J,DAY)
                   
C --- COMPUTE ET FROM UZ FREE WATER STORAGE, RECALCULATE UZFWC, CALCULATE RESIDUAL ET DEMAND                   
                   
                   IF (UZFWC .GE. RESIDET(J,DAY)) GO TO 221
                   
                   ETUZFW(J,DAY) = UZFWC
                   
                   UZFWC = 0.0
                   
                   RESIDET(J,DAY) = RESIDET(J,DAY) - ETUZFW(J,DAY)
                   
                   GO TO 225
                   
221                ETUZFW(J,DAY) = RESIDET(J,DAY)

                   UZFWC = UZFWC - ETUZFW(J,DAY)
                   
                   RESIDET(J,DAY) = 0.0
                   
C --- REDISTRIBUTE WATER BETWEEN UZ TENSION WATER AND FREE WATER STORAGES

220                IF((UZTWC/UZTWM(I)).GE.(UZFWC/UZFWM(I))) 
     &             GO TO 225

                   UZRAT=(UZTWC+UZFWC)/(UZTWM(I)+UZFWM(I))
                      
                   UZTWC = UZTWM(I) * UZRAT
                      
                   UZFWC = UZFWM(I) * UZRAT
                        
225                IF (UZTWC .LT. 0.00001) UZTWC = 0.0

                   IF (UZFWC .LT. 0.00001) UZFWC = 0.0
                   
                   
C --- COMPUTE ET FROM LZ TENSION WATER STORAGE, RECALCULATE LZTWC, CALCULATE RESIDUAL ET DEMAND

                   ETLZTW(J,DAY) = RESIDET(J,DAY) * (LZTWC / 
     &             (UZTWM(I) + LZTWM(I)))
                   
                   LZTWC = LZTWC - ETLZTW(J,DAY)
                   
                   IF(LZTWC .GE. 0.0) GO TO 226
                   
                   ETLZTW(J,DAY) = ETLZTW(J,DAY) + LZTWC
                   
                   LZTWC = 0.0
                   
226                RATLZT = LZTWC / LZTWM(I)

                   RATLZ = (LZTWC + LZFPC + LZFSC) / 
     &             (LZTWM(I) + LZFPM(I) + LZFSM(I))
     
                   IF (RATLZT .GE. RATLZ) GO TO 230
                  
                   LZTWC = LZTWC + (RATLZ - RATLZT) * 
     &             LZTWM(I)

                   
                   LZFSC = LZFSC - (RATLZ - RATLZT) * 
     &             LZTWM(I)
                   
                   IF(LZFSC .GE. 0.0) GO TO 230
                   
                   LZFPC = LZFPC + LZFSC
                   
                   LZFSC = 0.0
                   
230                IF (LZTWC .LT. 0.00001) LZTWC = 0.0

C --- CALCULATE TOTAL ET SUPPLIED BY UPPER AND LOWER LAYERS

                   ET(J,DAY) = ETUZTW(J,DAY) + ETUZFW(J,DAY) + 
     &             ETLZTW(J,DAY)
                  IF (ET(J,DAY) .LT. 0.00001) ET(J,DAY) = 0.0
C *****************************************************************************************************
C *****************************************************************************************************
C --- COMPUTE PERCOLATION INTO SOIL WATER STORAGES AND SURFACE RUNOFF

C     --- COMPUTE WATER IN EXCESS OF UZ TENSION WATER CAPACITY (TWX)

                  TWX = INFIL + UZTWC - UZTWM(I)
           
                  IF (TWX.GE.0.0) THEN
             	
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

                     PERCM = LZFPM(I) * LZPK(I) + LZFSM(I) * LZSK(I)
                
                     PERC = PERCM * (UZFWC/UZFWM(I))
                
                     DEFR=1.0-((LZTWC+LZFPC+LZFSC)/
     &(LZTWM(I)+LZFPM(I)+LZFSM(I)))
                
                     PERC = PERC * (1.0 + ZPERC(I) * (DEFR
     &**REXP(I)))
            

C     --- COMPARE LZ PERCOLATION DEMAND TO UZ FREE WATER AVAILABLE AND COMPUTE ACTUAL PERCOLATION

                    IF (PERC .LT. UZFWC) THEN
                
                       UZFWC = UZFWC - PERC
                
                    ELSE
                
                       PERC = UZFWC
                
                       UZFWC = 0.0
                
                    ENDIF
            
C      --- CHECK TO SEE IF PERC EXCEEDS LZ TENSION AND FREE WATER DEFICIENCY, IF SO SET PERC TO LZ DEFICIENCY


                    LZDEF = (LZTWC + LZFPC + LZFSC) - 
     &(LZTWM(I) + LZFPM(I) + LZFSM(I)) + PERC
                    
                    IF (LZDEF .GT. 0.0) THEN
                    
                       PERC = PERC - LZDEF
          
                       UZFWC = UZFWC + LZDEF
                       
                    ENDIF
                
                
C --- DISRIBUTE PERCOLATED WATER INTO THE LZ STORAGES AND COMPUTE THE REMAINDER IN UZ FREE WATER STORAGE AND RESIDUAL AVAIL FOR RUNOFF
   

C     --- COMPUTE PERC WATER GOING INTO LZ TENSION WATER STORAGE AND COMPARE TO AVAILABLE STORAGE

                    PERCT = PERC * (1.0 - PFREE(I))
                
                    IF ((PERCT + LZTWC) .GT. LZTWM(I)) THEN
                
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

                       FRACP = (HPL * 2.0 * (1.0 - RATLP)) 
     >                 / ((1.0 - RATLP) + (1.0 - RATLS))
                
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


                       LZFPC = LZFPC + (PERCF - PERCS)

                
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
                
                 IF (UZFWC .LT. INF) THEN
                 
                    INF = UZFWC
                    
                    UZFWC = 0.0
                 
                 ELSE
                    
                    UZFWC = UZFWC - INF
                
                 ENDIF

             ENDIF

             

C *****************************************************************************************************
C --- CALCULATE THE FRACTION OF daily AET
               
               AETTEMP = AETTEMP + ET(J,DAY)
               
C *****************************************************************************************************
C--- CALCULATE THE FRACTION OF daily SURFACE RUNOFF
              
               RUNOFFTEMP = RUNOFFTEMP + SURFRO
               
C *****************************************************************************************************
C--- CALCULATE THE FRACTION OF DAILY PRIMARY BASEFLOW
              
               PBFTEMP = PBFTEMP + PBF              
               
C *****************************************************************************************************
C--- CALCULATE THE FRACTION OF DAILY SECONDARY BASEFLOW
              
               SBFTEMP = SBFTEMP + SBF               



C *****************************************************************************************************
C--- CALCULATE THE FRACTION OF DAILY INTERFLOW
              
               IFTEMP = IFTEMP + INF                                            
              
C *****************************************************************************************************


C ----CALCUALTE TOTAL RUNOFF FOR EACH LANDUSE, K (GE SUN OCT 19, 2010) 

C----!!!!! RUNLAND(I,Y,DAY, K)更改
C			RUNLAND_Y=(J-1)*12+M							
            RUNLAND(I,J,DAY) = SURFRO + PBF + 
     >     SBF + INF
C            ETLAND(I,J,DAY) = ET(J,DAY)
C            GEPLAND(I,J,DAY, K) = GEP(J,DAY)

C--- calculate the fraction of soil moisture 
          
              
!              TAUZTWC = TAUZTWC + UZTWC
!              
!              TAUZFWC = TAUZFWC + UZFWC
!              
!              TALZTWC = TALZTWC + LZTWC
!              
!              TALZFPC = TALZFPC + LZFPC
!              
!              TALZFSC = TALZFSC + LZFSC
              
              ASM = ASM + (UZTWC+UZFWC+LZTWC+LZFPC
     &        +LZFSC)
                   
!               TAREA = TAREA + LADUSE(I,K)

    
C40         CONTINUE

           
C -- CALCULATE AVG SMC

!              AUZTWC = TAUZTWC / TAREA
!              AUZFWC = TAUZFWC / TAREA
!              ALZTWC = TALZTWC / TAREA
!              ALZFPC = TALZFPC / TAREA
!              ALZFSC = TALZFSC / TAREA
!              ASM = TASM/TAREA

C100        CONTINUE


           AET(DAY) = AETTEMP        
           RUNOFF(DAY) = RUNOFFTEMP
           PRIBF(DAY) = PBFTEMP
           SECBF(DAY) = SBFTEMP
           INTF(DAY) = IFTEMP
           SMC (DAY) = ASM          
           SP(DAY) = SNOWPACK
           AVUZTWC(DAY) = UZTWC
           AVUZFWC(DAY) = UZFWC
           AVLZTWC(DAY) = LZTWC
           AVLZFPC(DAY) = LZFPC
           AVLZFSC(DAY) = LZFSC
                   
           IF (RUNOFF(DAY) .LT. 0.) THEN
           
           RUNOFF(DAY)=0.
           
           ENDIF            
     


C -- STREAMFLOW IN MILLION M3 FOR EACH HUC FOR MONTH M. HUCAREA IN SQ. METERS 
          STRFLOW(I, J, DAY) = (RUNOFF(DAY) + PRIBF(DAY) + SECBF(DAY) + 
     & INTF(DAY))* HUCAREA(I)/1000.0/1000000.0



         WRITE (80,2100) I,IDY,DAY,RAIN(I,J,DAY),TEMP(I,J,DAY),
     >AETTEMP,P_PET(I,J,DAY),RUNOFFTEMP,PBFTEMP,SBFTEMP,RUNLAND(I,J,DAY)
     >,STRFLOW(I, J, DAY),IFTEMP,ASM,SNOWPACK,SNOWW

2100     FORMAT(I4.0, ',', I4.0, ',',I4.0, ',', F8.3, ',',
     >   F8.3, ',', F8.3,',' F8.3, ',',F8.3, ',', F8.3, ',', 
     >   F8.3,',',F8.3, ',', F8.3, ',', F8.3 ',', F8.3 ',', F8.3,
     >   ',', F8.3)
         WRITE (900,2200) HUCNO(I), IDY, DAY, UZTWC,UZFWC,
     >LZTWC,LZFPC,LZFSC

2200     FORMAT(I4.0, ',', I4.0, ',',I4.0, ',', F8.3, ',', 
     >   F8.3,',',F8.3, ',', F8.3, ',', F8.3)

C
C --- Return
      RETURN
      END
