C**********************************************************************C
C                                                                      C
C     *** SUBROUTINE WARMPET ***                                       C
C     INPUT  MONTHLY PRECIPITATION AND TEMPERATURE, AND CALCULATE      C
C     MONTHLY PET AND POTENTIAL AET                                    C
C                                                                      C
C**********************************************************************C
      SUBROUTINE WARMPET(I, J, M, MNDAY)
      
      COMMON/BASIC/NGRID, NYEAR, NLC,BYEAR,IYSTART,IYEND


       
      COMMON/CELLINFO/LADUSE(1000,20),HUCNO(1000),
     >  LATUDE(1000),LONGI(1000)
              
      COMMON/OUTPUT1/ PET(200,12,20),APET(12),PAET(200,12,20),APAET(12),
     &AET(12), RUNOFF(12), INTER(12), PRIBF(12), SECBF(12), INTF(12), 
     &AVUZTWC(12), AVUZFWC(12), AVLZTWC(12), AVLZFPC(12)
     >,A_ET(1000,200, 12),P_ET(1000,200, 12),Sun_ET(1000,200, 12)
     >,RUN_HRU(1000,200, 12),BASE_HRU(1000,200, 12)      
      
      COMMON/CLIMATE/ RAIN(1000,200,12), TEMP(1000,200, 12), AAPPT(1000)
      
      COMMON/LAI/LAI_1(1000,200,12), LAI_2(1000,200,12), 
     &LAI_3(1000,200,12),LAI_4(1000,200,12),LAI_5(1000,200,12), 
     &LAI_6(1000,200,12), LAI_7(1000,200,12),LAI_8(1000,200,12),
     &LAI_9(1000,200,12),LAI_10(1000,200,12),LAI_11(1000,200,12), 
     &LAI_12(1000,200,12), LAI_13(1000,200,12), LAI_14(1000,200,12),
     &LAI_15(1000,200,12),LAI_16(1000,200,12), LAI_17(1000,200,12),
     &LAI_18(1000,200,12), LAI_19(1000,200,12)


      COMMON/LANDCHANGE/FPERD, FPERDLAI
                
      INTEGER NLC, HUCNO, M  
      
      INTEGER BYEAR
      
      INTEGER MONTHD(12),MONTHL(12), MJD(12), MMD
      
      REAL DEGLAT, LATUDE, PET
 
      REAL APET, PE, LADUSE, TEMP
      
      REAL TPET, RAIN, AAPPT
      
      REAL LAI_1
      REAL LAI_2
      REAL LAI_3
      REAL LAI_4
      REAL LAI_5
      REAL LAI_6
      REAL LAI_7
      REAL LAI_8
      
      REAL LAI_9
      REAL LAI_10
      REAL LAI_11
      REAL LAI_12
      REAL LAI_13
      REAL LAI_14
      REAL LAI_15
      REAL LAI_16 
      REAL LAI_17
      REAL LAI_18
      REAL LAI_19      
      
      
      REAL LAI, PAET TPAET, APAET
      
      REAL FPERD, FPERDLAI        
             
C --- Number of days for each month during regular year
      DATA MONTHD/31,28,31,30,31,30,31,31,30,31,30,31/
C 
C --- Number of days for each month during leap year
      DATA MONTHL/31,29,31,30,31,30,31,31,30,31,30,31/
C-----JULIAN DAY FOR MID-DAY OF EACH MONTH
      DATA MJD/15,46,76,107,137,168,198,229,259,290,321,351/      

C --- Calculate Monthly potential evapotranspiration
                 
        
            DTEMP=TEMP(I,J,M)
            
            TPET=0. 
            
            TPAET=0. 
C			   PET=0.
                         
        DO 40 K=1, NLC
              
C -- MMD = JULIAN DATE FOR MONTH M
                               
            MMD=MJD(M)
                     
            DEGLAT = LATUDE(I)
            
C -K= LANDUSE TYPE, DTEMP = AIR TEMP, HPEC = CORRECTION PARAMETER, PE =CALCUALTED PET (MM)--DAY
           
            CALL HAMON(K, DTEMP,M,MMD,DEGLAT,PE)
            
            
C ----PET (YEAR, MONTH, LANDUSE) FOR THE CURRENT CELL)
           
             PET(J,M,K) = PE  
                  
             PET(J,M,K) = PET(J,M,K) * MNDAY 
             
             
C ----ASSIGN LAI FOR EACH LAND USE
            
             IF (K.EQ.1) THEN
             
                LAI = LAI_1(I,BYEAR-IYSTART+J,M)
             
             ELSEIF (K.EQ.2) THEN
             
                LAI = LAI_2(I,BYEAR-IYSTART+J,M)
             
             ELSEIF (K.EQ.3) THEN
             
                LAI = LAI_3(I,BYEAR-IYSTART+J,M)
             
             ELSEIF (K.EQ.4) THEN
             
                LAI = LAI_4(I,BYEAR-IYSTART+J,M)
             
             ELSEIF (K.EQ.5) THEN
             
                LAI = LAI_5(I,BYEAR-IYSTART+J,M)
             
             ELSEIF (K.EQ.6) THEN
             
                LAI = LAI_6(I,BYEAR-IYSTART+J,M)
             
             ELSEIF (K.EQ.7) THEN
             
                LAI = LAI_7(I,BYEAR-IYSTART+J,M)
             
             ELSEIF (K.EQ.8) THEN
             
                LAI = LAI_8(I,BYEAR-IYSTART+J,M)
                                
             ELSEIF (K.EQ.9) THEN
             
                LAI = LAI_9(I,BYEAR-IYSTART+J,M)
             
             ELSEIF (K.EQ.10) THEN
             
                LAI = LAI_10(I,BYEAR-IYSTART+J,M)
             
             ELSEIF (K.EQ.11) THEN
             
                LAI = LAI_11(I,BYEAR-IYSTART+J,M)
             
             ELSEIF (K.EQ.12) THEN
             
                LAI = LAI_12(I,BYEAR-IYSTART+J,M)
             
             ELSEIF (K.EQ.13) THEN
             
                LAI = LAI_13(I,BYEAR-IYSTART+J,M)
             
             ELSEIF (K.EQ. 14) THEN
             
                LAI = LAI_14(I,BYEAR-IYSTART+J,M)
             
             ELSEIF (K.EQ. 15) THEN
             
                LAI = LAI_15(I,BYEAR-IYSTART+J,M)

             ELSEIF (K.EQ. 16) THEN
             
                LAI = LAI_16(I,BYEAR-IYSTART+J,M)
             
             ELSEIF (K.EQ. 17) THEN
             
                LAI = LAI_17(I,BYEAR-IYSTART+J,M)
             
             ELSEIF (K.EQ. 18) THEN
             
                LAI = LAI_18(I,BYEAR-IYSTART+J,M)
             
             ELSEIF (K.EQ. 19) THEN
             
                LAI = LAI_19(I,BYEAR-IYSTART+J,M)
             
             ENDIF
                          

C ----CALCULATE PAET 
C ----PAET (YEAR, MONTH, LANDUSE) FOR THE CURRENT CELL)
C ----PAET IS THE POTENTIAL AET, ASSUMING SOIL MOISTURE NOT LIMITING

c             IF ((LAI.GE.1.0).OR.(AAPPT(I).GE.600.0)) THEN
             
C     CALCULATE PAET USING MODIFIED SUN HAMON EQUATION FOR HIGHER LAI
C     NO COWEETA
C     NO -LAI*RAIN TERM (MAKES PAET INVERSELY PROPORTIONAL TO LAI DURING WINTER
C     R2=0.87, N=90, MODEL ET= 9.37+0.87*MEASURED ET

c             PAET(J,M,K) = 9.95+(PET(J,M,K))*LAI*0.205+0.153*
c     &RAIN(I,J,M)+0.246*(PET(J,M,K))

C  new model with coweeta Hamon ET Sep 10,2010

             PAET(J,M,K) = 0.0222*PET(J,M,K)*LAI+0.174*
     &       RAIN(I,J,M)+0.502*PET(J,M,K) + 5.31*LAI

  
C           PRINT *, 'pet', LAI,RAIN(I,J,M), PET(J,M,K), PAET(J,M,K)
     
     
c             ELSE
             
C     CALCULATE PAET FOR GRASSLAND AND LOW LAI
C     R2=0.696
             
c             PAET(J,M,K) = 1.49 + 0.325*(PET(J,M,K))+0.353*(RAIN(I,J,M))
             
c             ENDIF
          
c             WRITE(910,5039)  HUCNO(I), J, M, K, PAET(J,M,K)
c5039         FORMAT(4I10, F20.1)


C ----CALCULATE TOTAL PET AND PAET FOR THE HUC FOR A GIVEN YEAR AND MONTH
        
             
             TPET = TPET + PET(J,M,K) * LADUSE(I,K)
             
             TPAET = TPAET + PAET(J,M,K) * LADUSE(I,K)
             
             
        
c          WRITE(77, 5040) HUCNO(I), J, M, LADUSE(I,K), LAI          
       
c5040  FORMAT (3I10, 2F10.5)
     
                    
             
   40      CONTINUE                 

C-------流域单元月APET、APAET（各植被类型的加权平均值）
C ------APET =AVERAGE PET FOR CURRENT CELL, FOR ALL YEAR, MONTH


              APET(M) = TPET    
              
C ------APAET =AVERAGE PAET FOR CURRENT CELL, FOR ALL YEAR, MONTH

              APAET(M) = TPAET
              

C ------TEST OUTPUT


c       WRITE(77, 5050) HUCNO(I),J,M, APET(M), APAET(M) 
      
c5050  FORMAT (3I10, 2F10.5)

                              
C --- Return
      RETURN
      END


C**********************************************************************C
C                                                                      C
C     *** SUBROUTINE HAMON ***                                         C
C     Calculate potential evapotranspiration by using Hamon's          C
C     equation                                                         C
C                                                                      C
C**********************************************************************C
      SUBROUTINE HAMON(K, TEMP,MONTH,J,DEGLAT,PE)

      COMMON/VAL/VAL_1(1000), VAL_2, VAL_3,VAL_4,VAL_5,VAL_6

      REAL PE, TEMP, DEGLAT
      
      REAL SOLDEC, SSANG, DAY
      
      REAL ESAT, RHOSAT, PI
      
      INTEGER J, K  
      
      PI = 3.14159265

C --- ESAT = saturated vapor pressure at TEMP
C --- RHOSAT = saturated vapor density at TEMP
      
      ESAT = 6.108*EXP(17.2693882*TEMP/(TEMP+237.3))
      RHOSAT = 216.7*ESAT/(TEMP+273.3)
      
C --- CALCULATE MEAN MONTHLY DAY LENGTH (DAY) BASED ON LATITUDE AND MID-MONTH JULIAN DATE
C --- SHUTTLEWORTH, W.J. 1993. Evaporation, in Handbook of Hydrology, 
C --- MAIDMENT, D.R. ED., MCGRAW HILL, NY, PP. 4, 1-53

C ---    CALCULATE THE ANGLE OF SOLAR DECLINATION IN RADIANS     
 
         SOLDEC = 0.4093*SIN(2*PI*J/365.-1.405)
      
C ---    CALCULATE THE SUNSET HOUR ANGLE IN RADIANS

         SSANG = ACOS(-1*TAN(DEGLAT*.0174529)*TAN(SOLDEC))
      
C ---    CALCULATE THE ADJUSTMENT IN LENGTH OF DAY FOR 12 HR PERIOD
      
         DAY = 2*SSANG/PI
         
         
C --- Calculate Daily PE
     
      PE = 0.1651*DAY*RHOSAT*1.2
      
C      print *, 'PET', MONTH, J, TEMP, PE, K
          
      RETURN

      END















