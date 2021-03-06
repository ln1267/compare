
C**********************************************************************C
C                                                                      C
C     *** SUBROUTINE SUMMARY ***                                       C
C     CALCULATE AVERAGE WATER BALANCE COMPONENTS FROM IYSTART TO IYEND C
C     WRITE TO SUMMARRUNOFF.TXT by WATERSHED                                       C
C                                                                      C
C                                                                      C
C**********************************************************************C
     
      SUBROUTINE SUMMARY(I)

      COMMON/BASIC/NGRID, NYEAR, NLC,BYEAR,IYSTART,IYEND, POP_FLAG
      
      COMMON/CELLINFO/LADUSE(4000,20), HUCNO(4000), LATUDE(4000),
     &LONGI(4000)
      
      COMMON/SUMMARY1/ANURAIN(200), ANURUN(200), ANUPET(200), 
     & ANUAET(200)
      COMMON/R/ RFACTOR(200)

C-----------------------------------------------------------------------      

      REAL RAINALL, AETALL, PETALL, RUNALL, RUNRATIO, ETRATIO, TRATIO
          
      REAL ANURAIN, ANURUN, ANUPET, ANUAET
            
      INTEGER HUCNO, J, M, IYSTART, IYEND, ISTEP, BYEAR
      
      REAL RFACTOR , RALL
      
      
      RAINALL =0.
      AETALL =0.   
      PETALL=0.
      RUNALL=0.
      RALL = 0.
      
      M=0
      
      ISTEP = IYEND - IYSTART + 1
          
      DO 100 J = 1, ISTEP
       
            RAINALL = RAINALL + ANURAIN(J+IYSTART-BYEAR)    
            AETALL = AETALL + ANUAET(J+IYSTART-BYEAR)  
            PETALL = PETALL + ANUPET(J+IYSTART-BYEAR)
            RUNALL = RUNALL + ANURUN(J+IYSTART-BYEAR)
            
            RALL = RALL + RFACTOR(J+IYSTART-BYEAR)
                 
            M = M + 1

100   CONTINUE
     
      RAINALL = RAINALL /M  
      AETALL = AETALL / M
      PETALL = PETALL / M
      RUNALL = RUNALL / M
      
      RALL =RALL /M


      IF (RAINALL .NE. 0.) THEN  
      RUNRATIO = RUNALL/RAINALL
      ETRATIO = AETALL/RAINALL   
      ELSE
      
      RUNRATIO = 0.0
      ETRATIO = 0.0
      
      
      ENDIF

      TRATIO = RUNRATIO + ETRATIO
   
C--WRITE TO FILE SUMMARRUNOFF.TXT

         WRITE (80,250) HUCNO(I), RAINALL, PETALL, AETALL, RUNALL, 
     >   RUNRATIO, ETRATIO, TRATIO, RALL
     
250      FORMAT (I10, ',', F10.1, ',', F10.1,',',  
     >         F10.1, ',', F10.1,',', F8.3, ',', F8.3,',', F8.3,
     >    ',', F8.1)    
    
         RUNRATIO = RUNRATIO *100.
          
         WRITE  (*,300) HUCNO(I), RAINALL, RUNALL, 
     >   RUNRATIO
    
300    FORMAT ('GRID=',I5,' PRECIP (MM)=',F6.0, '   RUNOFF(MM)=', F5.0, 
     >   '  RUNOFF/PRECIP=', F4.1, '%')    

      RETURN
      END
