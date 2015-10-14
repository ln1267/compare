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
     &AET(12), RUNOFF(12), INTER(12), PRIBF(12), SECBF(12), INTF(12), 
     &AVUZTWC(12), AVUZFWC(12), AVLZTWC(12), AVLZFPC(12)
     >,A_ET(1000,200, 12),P_ET(1000,200, 12),Sun_ET(1000,200, 12)
     >,RUN_HRU(1000,200, 12),BASE_HRU(1000,200, 12)
     
                      
      COMMON/SUMMARY1/ANURAIN(200),ANURUN(200),ANUPET(200),ANUAET(200)
     >,ANUPAET(200)
       
      COMMON/SOIL/LZTWM(1000), LZFPM(1000), LZFSM(1000), LZSK(1000),
     >  LZPK(1000), UZTWM(1000), UZFWM(1000), UZK(1000), ZPERC(1000),
     >  REXP(1000), PFREE(1000), SMC(12)

      COMMON/CELLINFO/LADUSE(1000,20),HUCNO(1000),
     >        LATUDE(1000),LONGI(1000)
                        
      COMMON/CLIMATE/ RAIN(1000,200,12), TEMP(1000,200, 12), AAPPT(1000)
      
 
      COMMON/FLOW/ STRFLOW(1000, 200, 12),STRET(1000, 200, 12)
     >,STRGEP(1000, 200, 12)

      COMMON/SNOWPACK/SP(12), SNOWPACK, NSPM(200)
       
      COMMON/HUCPETAET/HUCAET(1000,200), HUCPET(1000,200),
     >HUCPAET(1000,200)
      
      
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
       
  
      IF (IDY .GE. IYSTART .AND. IDY .LE. IYEND) THEN        

         
      DO 100 IM = 1, 12     

        A_ET(I,J,IM)=AET(IM)
        P_ET(I,J,IM)=APET(IM)
		Sun_ET(I,J,IM)=APAET(IM)
        RUN_HRU(I,J,IM)=RUNOFF(IM) + PRIBF(IM) + SECBF(IM) + INTF(IM)
        BASE_HRU(I,J,IM)=PRIBF(IM) + SECBF(IM)
        TRUNOFF = RUNOFF(IM) + PRIBF(IM) + SECBF(IM) + INTF(IM)
		BASEFLOW=PRIBF(IM) + SECBF(IM)
C------PRINT MONTHLY WATER BALANCE DATA TO MONTHFLOW.TXT

                   
         WRITE (78,2025) HUCNO(I), IDY, IM, RAIN(I,J,IM),TEMP(I,J,IM),
     &   SMC(IM), SP(IM), APET(IM), AET(IM),APAET(IM), TRUNOFF, 
     >	    BASEFLOW,STRFLOW(I,J,IM)

     
     
2025      FORMAT (I10, ',',I6, ',', I6, ',', F10.1, ',', F10.1,',',  
     >  F10.1, ',', F10.1,',', F8.1, ',', F8.1, ',',F8.1,',',
     >	    F10.1,',', F10.1,',', F10.1)         
     
       

C------PRINT MONTHLY SOIL STORAGE DATA TO SOILSTORAGE.TXT

c         WRITE(900,2035)  HUCNO(I), IDY, IM, AVUZTWC(IM), AVUZFWC(IM),
c     >   AVLZTWC(IM), AVLZFPC(IM)
     
2035     FORMAT(I10, 2I6, 4F10.1)

C------SUM THE TOTAL RAIN, PET, AET, DISCHARGE, INT, SNOWP FOR YEAR

         TANURAIN = TANURAIN + RAIN(I,J,IM)
         TANUPET = TANUPET + APET(IM)
         TANUAET = TANUAET + AET(IM)
		 TANUPAET= TANUPAET + APAET(IM)
         TANURUN = TANURUN + (RUNOFF(IM)+PRIBF(IM)+SECBF(IM)+INTF(IM))
         TSNOWP    = TSNOWP + SP(IM)
          
         IF (SP(IM) .GT. 0.) THEN 
         
            ISNOWP = ISNOWP + 1    
               
         ENDIF
            
         
         RAINSQ= RAINSQ + RAIN(I,J,IM)**2
         
                                                    
100   CONTINUE
         
C------ASSIGN TOTAL ANNUAL RAIN, PET, AET, DISCHARGE, INT, SNOWP
C -----TANURUN 为流域的总出流量(RUNOFF(IM)+PRIBF(IM)+SECBF(IM)+INTF(IM))
         
      ANURAIN(J) = TANURAIN
      ANUPET(J) = TANUPET
      ANUAET(J) = TANUAET
	ANUPAET(J)=TANUPAET
      ANURUN(J) = TANURUN
      
      IF (TANURAIN .GE. 1.0) THEN        
        ARUNRT(J) = TANURUN/TANURAIN
        AETRT(J)  = TANUAET/TANURAIN
		
      ELSE
      
       ARUNRT (J) = 0.0
       AETRT (J) = 0.0
      
      ENDIF 
      
      
      ETRATIO = AETRT(J) + ARUNRT(J)
      NSPM(J) = ISNOWP 
         
      HUCAET(I,J) = ANUAET(J)
      HUCPET(I,J) = ANUPET(J)     
      HUCPAET(I,J)=ANUPAET(J)
C------PRINT ANNUAL WATER BALANCE COMPONENTS TO ANNUALFLOW.TXT

                               

C ---- CALCULATING R FACTOR

       
       IF (TANURAIN .NE. 0.) THEN
          F = RAINSQ/TANURAIN
       
      
         IF (F .LT. 55) THEN
               
            
         RFACTOR (J) = (0.07397 * F**1.847 )/17.2
       
         ELSE
       
          RFACTOR (J) = (95.77-6.081*F+0.4770*F**2) /17.2
               
        ENDIF
            
       ELSE
       
         RFACTOR(J) =0.
       
       ENDIF  


         WRITE (79,2100) HUCNO(I), IDY, ANURAIN(J),
     >   ANUPET(J), ANUAET(J),ANUPAET(J), ANURUN(J), ARUNRT(J),  
     >   AETRT(J),ETRATIO, NSPM(J), RFACTOR (J)

2100     FORMAT(I10, ',', I10, ',',F10.0, ',', 
     >   F8.1, ',', F8.1,',' F8.1, ',',F8.1, ',', F8.2, ',', 
     >   F8.2,',',F8.2, ',', I8, ',', F8.1)


        
200   CONTINUE       
       

        ENDIF 
        
              
      RETURN
      END
      

