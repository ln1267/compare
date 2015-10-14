
!C**********************************************************************C
!C                                                                      C
!C     *** SUBROUTINE SUMMARY ***                                       C
!C     CALCULATE AVERAGE WATER BALANCE COMPONENTS FROM IYSTART TO IYEND C
!C     WRITE TO SUMMARRUNOFF.TXT by WATERSHED                                       C
!C                                                                      C
!C                                                                      C
!C**********************************************************************C
     
      SUBROUTINE SUMMARY(I)

      use common_var
      implicit none 

!----------------------------------------------------------------------      

      REAL RAINALL, AETALL, PETALL, RUNALL, RUNRATIO, ETRATIO, TRATIO
           
      INTEGER  I,J, M,ISTEP
      
      REAL  RALL
      
      
      RAINALL =0.
      AETALL =0.   
      PETALL=0.
      RUNALL=0.
      RALL = 0.
      
      M=0
      
      ISTEP = IYEND - IYSTART + 1
          
     DO 100 J = 1, ISTEP
!------ 排除异常的气候值，异常值，气候和温度设置为-99999
       
       IF  (ANURAIN(I,J+IYSTART-BYEAR) < -50.0 .or. &
      ANUAET(I,J+IYSTART-BYEAR) < -50.0 .or. ANUPET(I,J+IYSTART-BYEAR) &
        < -50.0 .or. ANURUN(I,J+IYSTART-BYEAR) < -50.0 ) then 
      
      ELSE
            RAINALL = RAINALL + ANURAIN(I,J+IYSTART-BYEAR)    
            AETALL = AETALL + ANUAET(I,J+IYSTART-BYEAR)  
            PETALL = PETALL + ANUPET(I,J+IYSTART-BYEAR)
            RUNALL = RUNALL + ANURUN(I,J+IYSTART-BYEAR)
            
            RALL = RALL + RFACTOR(I,J+IYSTART-BYEAR)
                 
            M = M + 1
      ENDIF
!------ ---------------------------------------------------------------------


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
   
!--WRITE TO FILE SUMMARRUNOFF.TXT

         WRITE (80,250) HUCNO(I), RAINALL, PETALL, AETALL, RUNALL, &
         RUNRATIO, ETRATIO, TRATIO, RALL
     
250      FORMAT (I10, ',', F10.1, ',', F10.1,',', & 
              F10.1, ',', F10.1,',', F8.3, ',', F8.3,',', F8.3,&
         ',', F8.1)    
    
         RUNRATIO = RUNRATIO *100.
          
 !        WRITE  (*,300) HUCNO(I), RAINALL, RUNALL, RUNRATIO
   
    
300    FORMAT ('GRID=',I5,' PRECIP (MM)=',F6.0, '   RUNOFF(MM)=', F5.0, &
        '  RUNOFF/PRECIP=', F4.1, '%')    

      RETURN
      END
