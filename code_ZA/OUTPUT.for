C**********************************************************************C
C                                                                      C
C     *** SUBROUTINE OUTPUT ***                                        C
C     WRITE MONTHLY WATER BALANCE OUTPUT TO MONTHRUNOFF.TXT            C
C     WRITE MONTHLY SOIL STORAGE OUTPUT TO SOILSTORAGE.TXT             C
C     CALCULATE TOTAL ANNUAL RAIN, PET, AET, DISCHARGE, INT, SNOWP     C
C     PRINT ANNUAL WATER BALANCE COMPONENTS TO ANNUALFLOW.TXT          C
C                                                                      C
C                                                                      C
C**********************************************************************C
      SUBROUTINE OUTPUT (I, J)
      
      COMMON/BASIC/NGRID, NYEAR, NLC,BYEAR,IYSTART,IYEND
      
    
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
     >        LATUDE(1000),LONGI(1000)
                        
     
      COMMON/FLOW/ STRFLOW(100,200, 366),STRET(1000, 200, 12)
     >,STRGEP(1000, 200, 12)


       
      COMMON/HUCPETAET/HUCAET(1000,200), HUCPET(1000,200),
     >HUCPAET(1000,200)
      
      COMMON/BYLAND/ RUNLAND(600,100,366) 
      COMMON/R/ RFACTOR(200)
C -----------------------------------------------------------------------------     
     
     

      REAL LADUSE,RAIN, APET, AET,
     > PAET INTER, SMC,STRFLOW,
     > APAET,A_ET,P_ET,Sun_ET,RUN_HRU,BASE_HRU
     
      REAL RUNOFF, PRIBF, SECBF, INTF
     
      REAL TANURAIN, TANUPET,TANUAET,TANUPAET,TANURUN

      REAL ANURAIN,ANUPET,ANUAET,ANUPAET, ANURUN
      REAL ARUNRT(200), AETRT(200)

c      REAL MWASSI(12),DEMAND(12),SUPPLY(12),
c     >  ANUDM(200), ANUSP(200)

c      REAL TDM, TSP
      
      REAL TSNOWP, SP
      
      REAL HUCAET, HUCPET,HUCPAET  
      
      REAL AVUZTWC, AVUZFWC, AVLZTWC, AVLZFPC

      INTEGER HUCNO, BYEAR, IDY, ISNOWP
      REAL TRUNOFF，BASEFLOW
      
      REAL RAINSQ, F, RFACTOR

      INTEGER YEAR,NDAY,ICOUNT
      
C-----IDY = THE CALANDER YEAR, BYEAR = YEAR TO SATRT
         
      IDY = J + BYEAR - 1
         
      TANURAIN =0.
      TANUPET= 0.
      TANUAET= 0.
	TANUPAET= 0.
      TANURUN= 0.
      TSNOWP = 0.
         
      ISM = 0
      TSP = 0.
      TDM = 0.
         
      ISNOWP = 0
      
      RAINSQ =0. 
       
      ICOUNT = 0
  
      IF (IDY .GE. IYSTART .AND. IDY .LE. IYEND) THEN        

            NDAY = 365
            IF(IDY/4*4.NE.YEAR) GO TO 610
            NDAY=366   

610         CONTINUE 
 
 
 
         
      DO 100 IM = 1, NDAY     


C------PRINT MONTHLY WATER BALANCE DATA TO MONTHFLOW.TXT


C------SUM THE TOTAL RAIN, PET, AET, DISCHARGE, INT, SNOWP FOR YEAR


         TANURUN = TANURUN + RUNLAND(I,J,IM)
     
                                                    
100   CONTINUE
         
C------ASSIGN TOTAL ANNUAL RAIN, PET, AET, DISCHARGE, INT, SNOWP
C -----TANURUN 为流域的总出流量(RUNOFF(IM)+PRIBF(IM)+SECBF(IM)+INTF(IM))
         

      ANURUN(J) = TANURUN
      
  

C------PRINT ANNUAL WATER BALANCE COMPONENTS TO ANNUALFLOW.TXT
                               


         WRITE (79,2100) I, IDY, ANURUN(J)


2100     FORMAT(I10, ',', I10, ',',F10.0 )


        
200   CONTINUE       
       

        ENDIF 
        
              
      RETURN
      END
      

