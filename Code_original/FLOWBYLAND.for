
C**********************************************************************C
C                                                                      C
C     *** SUBROUTINE Cabon balance ***                                 C
C     Simulate GEP AND NEE for selected HUC                            C
C     WRITE GEP AND NEE TO MONTHCARBON.TXT, ANNUALCARBON.TXT, HUCCARBON.TXT        C
C     SIMULATE BIODIVERSITY FOR SELECTED HUC                           C
C     WRITE BIODIVERSITY TO HUCBIO.TXT                                 C
C                                                                      C
C**********************************************************************C
C        I=HUC; J= YEAR, M =MONTH, MNDAY= NO OF DAYS IN A MONTH

      SUBROUTINE FLOWBYLAND
      
      COMMON/BASIC/NGRID, NYEAR, NLC,BYEAR,IYSTART,IYEND, POP_FLAG

      COMMON/HUC/HUCAREA(2200)
      COMMON/CELLINFO/LADUSE(4000,20),HUCNO(4000),
     >                LATUDE(4000),LONGI(4000)
              
      COMMON/BYLAND/RUNLAND(500, 50, 12, 31, 19)
            

C --------------------------------------------------------------
      INTEGER MONTHD(12),MONTHL(12)
      
      INTEGER I,J, M, K,DAY,YEAR,NGRID
      
      INTEGER HUCNO(4000), BYEAR, IYSTART,IYEND,NYEAR
                   
      REAL LADUSE(4000,20)
      
      double precision HUCAREA(2200)
      
C      REAL RUN_LAND(2200,100,12,31,8)

      REAL RUNYR, FLOWYR
      
      REAL RUNLAND(500, 50, 12, 31, 19)
      
      
                
      REAL CROPFLOW, FORESTFLOW, GRASSFLOW, SHRUBSAVFLOW,
     >     URBANWATERFLOW, TFLOW
      
      REAL  FLOWK(500, 50, 20)
      
      
C --- Number of days for each month during regular year
      DATA MONTHD/31,28,31,30,31,30,31,31,30,31,30,31/

C --- Number of days for each month during leap year
      DATA MONTHL/31,29,31,30,31,30,31,31,30,31,30,31/
C ----------------------------------------------------------------      
            
               
      DO 400 K=1, NLC     
           
           
      DO 300 I=1, NGRID         
             
                                   
         DO 200 J=1, NYEAR
             
             YEAR = BYEAR + J -1
             
C --- DETERMINE WHETHER YEAR IS A LEAP YEAR 
C --- http://www.timeanddate.com/date/leapyear.html

             IF (YEAR/4 .EQ. INT(YEAR/4)) THEN
             
                NDAY = 366
             
                IF (YEAR/100 .EQ. INT(YEAR/100)) THEN
                
                   NDAY = 365
                   
                   IF (YEAR/400 .EQ. INT(YEAR/400)) THEN
                   
                     NDAY = 366
                     
                   ENDIF
                   
                ENDIF
                
             ELSE
             
               NDAY = 365
               
             ENDIF
        
               RUNYR=0.
               
            DO 100 M=1, 12
                
                
               IF (NDAY .EQ. 365) THEN
                 MNDAY=MONTHD(M)
               ELSE
                 MNDAY=MONTHL(M)
               ENDIF

               
               DO 50 DAY=1, MNDAY
         
                 RUNYR = RUNYR+RUNLAND(I,J, M,DAY,K)
                          
50             CONTINUE
                             
100        CONTINUE

C --- FLOW MILLION CUBIC METERS
                          
         FLOWYR = RUNYR*LADUSE(I,K)*HUCAREA(I)/1000./1000000.

         
             IF (YEAR .GE. IYSTART .AND. YEAR .LE. IYEND) THEN 
           
         WRITE (910,4000) HUCNO(I),YEAR,K, RUNYR, FLOWYR, 
     >         LADUSE (I,K), HUCAREA(I) 
                                              
                                              
4000        FORMAT (I10, ',', I5, ',', I5, ',', 
     >           F8.1, ',', F10.3, ',', F10.3,',', F12.1)     
         
             FLOWK(I,J, K) = FLOWYR  
       
            
       
           ENDIF
          
200      CONTINUE
300      CONTINUE
400      CONTINUE





C -- RECLASSIFY LANDCOVER AND WATRE YIELD 


  
      DO 700 I=1, NGRID                               
        DO 600 J=1, NYEAR      


             YEAR = BYEAR + J -1
             
             CROPFLOW =0.
             FORESTFLOW = 0.
             GRASSFLOW = 0.
             SHRUBSAVAFLOW = 0.
             URBANWATERFLOW = 0.
             
             
           DO 500 K=1, NLC    

C -- CROP

               IF (K.EQ.1  .OR. K .EQ. 2) THEN 
               
                  CROPFLOW = CROPFLOW +  FLOWK(I,J, K)
                  
              
C -- FORESTS
 
          ELSEIF (K .EQ. 4  .OR. K .EQ. 5 .OR. K .EQ. 6 
     >            .OR. K .EQ. 7) THEN 
     
               FORESTFLOW = FORESTFLOW +  FLOWK(I,J, K)
 
C -- GRASSLANDS               
       ELSEIF (K .EQ. 3 .OR. K .EQ. 9 .OR. K .EQ. 11 
     >            .OR. K .EQ. 13) THEN                
               
             GRASSFLOW = GRASSFLOW + FLOWK(I, J , K)
              
                               
C -- SHRUBLANDS AND SAVANNAS                   
               
               ELSEIF (K .EQ. 8 .OR. K .EQ. 10 ) THEN                

                SHRUBSAVAFLOW =  SHRUBSAVAFLOW + FLOWK(I, J , K)
                               
C -- URBAN/BARRENS/WATRE BODY (SAME AS OPEN SHRUB)  

                             
               ELSE
              
                              
              URBANWATERFLOW =  URBANWATERFLOW + FLOWK(I, J , K)
                             
              ENDIF                      
                                     
                               
500         CONTINUE

         TFLOW=CROPFLOW+FORESTFLOW+GRASSFLOW+SHRUBSAVAFLOW+URBANWATERFLO
                
             IF (YEAR .GE. IYSTART .AND. YEAR .LE. IYEND) THEN 
           
         WRITE (920,5000) HUCNO(I),YEAR, CROPFLOW,
     >       FORESTFLOW, GRASSFLOW, SHRUBSAVAFLOW, URBANWATERFLO,
     >   TFLOW                                            
                                              
5000     FORMAT (I10, ',', I5, ',', 
     > F10.3, ',', F10.3, ',', F10.3,',', F10.3, ',', F10.3, ',', F10.3)     
                
           ENDIF       
           

600      CONTINUE
700   CONTINUE



      RETURN
      END
