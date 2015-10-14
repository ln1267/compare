!C**********************************************************************C
!C                                                                      C
!C     *** SUBROUTINE OUTPUT ***                                        C
!C     WRITE MONTHLY WATER BALANCE OUTPUT TO MONTHRUNOFF.TXT            C
!C     WRITE MONTHLY SOIL STORAGE OUTPUT TO SOILSTORAGE.TXT             C
!C     CALCULATE TOTAL ANNUAL RAIN, PET, AET, DISCHARGE, INT, SNOWP     C
!C     PRINT ANNUAL WATER BALANCE COMPONENTS TO ANNUALFLOW.TXT          C
!C                                                                      C
!C                                                                      C
!**********************************************************************C
      SUBROUTINE OUTPUT (I, J)
      
      use common_var
      implicit none 
! -----------------------------------------------------------------------------     
     
      REAL TANURAIN, TANUPET,TANUAET,TANUPAET,TANURUN

      REAL ARUNRT(200), AETRT(200)

!c      REAL MWASSI(12),DEMAND(12),SUPPLY(12),
!c     >  ANUDM(200), ANUSP(200)
!
      REAL TDM, TSP, ISM,ETRATIO
      
      REAL TSNOWP
      
      INTEGER  IDY,J,ISNOWP,IM,I
      REAL TRUNOFF,BASEFLOW_L
      
      REAL RAINSQ, F
      
!-----IDY = THE CALANDER YEAR, BYEAR = YEAR TO SATRT
         
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
		BASEFLOW_L=PRIBF(IM) + SECBF(IM)
!------PRINT MONTHLY WATER BALANCE DATA TO MONTHFLOW.TXT

! Print *, 'RUN_HRU', RUN_HRU(I,J,IM),'BASE_HRU', BASE_HRU(I,J,IM)                
         WRITE (78,2025) HUCNO(I), IDY, IM, RAIN(I,J,IM),TEMP(I,J,IM),&
        SMC(IM), SP(IM), APET(IM), AET(IM),APAET(IM), TRUNOFF, &
        BASEFLOW_L,STRFLOW(I,J,IM)

     
     
2025      FORMAT (I10, ',',I6, ',', I6, ',', F10.1, ',', F10.1,',',  &
       F10.1, ',', F10.1,',', F8.1, ',', F8.1, ',',F8.1,',',&
         F10.1,',', F10.1,',', F10.1)         
     
       

!------PRINT MONTHLY SOIL STORAGE DATA TO SOILSTORAGE.TXT

         WRITE(900,2035)  HUCNO(I), IDY, IM, AVUZTWC(IM), AVUZFWC(IM),&
        AVLZTWC(IM), AVLZFPC(IM), AVLZFSC(IM)
     
2035     FORMAT(I10, 2I6, 5F10.1)

!------SUM THE TOTAL RAIN, PET, AET, DISCHARGE, INT, SNOWP FOR YEAR

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
         
!C------ASSIGN TOTAL ANNUAL RAIN, PET, AET, DISCHARGE, INT, SNOWP
!C -----TANURUN 为流域的总出流量(RUNOFF(IM)+PRIBF(IM)+SECBF(IM)+INTF(IM))
         
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
!-----PRINT ANNUAL WATER BALANCE COMPONENTS TO ANNUALFLOW.TXT

                               

! ---- CALCULATING R FACTOR

       
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


         WRITE (79,2100) HUCNO(I), IDY, ANURAIN(J),&
        ANUPET(J), ANUAET(J),ANUPAET(J), ANURUN(J), ARUNRT(J), & 
        AETRT(J),ETRATIO, NSPM(J), RFACTOR (J)

2100     FORMAT(I10, ',', I10, ',',F10.0, ',', &
        F8.1, ',', F8.1,',' F8.1, ',',F8.1, ',', F8.2, ',',& 
         F8.2,',',F8.2, ',', I8, ',', F8.1)


        
200   CONTINUE       
       

        ENDIF 
        
              
      RETURN
      END
      

