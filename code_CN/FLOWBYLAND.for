
C**********************************************************************C
C                                                                      C
C     *** SUBROUTINE Cabon balance ***                                 C
C     Simulate GEP AND NEE for selected HUC                            C
C     WRITE GEP AND NEE TO MONTHCARBON.TXT, ANNUALCARBON.TXT, HUCCARBON.TXT        C
C     SIMULATE BIODIVERSITY FOR SELECTED HUC                           C
C     WRITE BIODIVERSITY TO HUCBIO.TXT                                 C
C！！！！！！！107行RUNLAND (流域数，年数，月份，月份，植被类型数）数组大小 C
C**********************************************************************C
C        I=HUC; J= YEAR, M =MONTH, MNDAY= NO OF DAYS IN A MONTH


      SUBROUTINE FLOWBYLAND
c      use myvar
      COMMON/BASIC/NGRID, NYEAR, NLC,BYEAR,IYSTART,IYEND, POP_FLAG



      COMMON/HUC/ HUCAREA(1000),HUCELE(1000)
      COMMON/CELLINFO/LADUSE(1000,20),HUCNO(1000),
     >                LATUDE(1000),LONGI(1000)
              
       
      COMMON/BYLAND/ RUNLAND(650, 40,12, 31,13), 
     >ETLAND(650, 40,12, 31,13), GEPLAND(650, 40,12, 31,13)
      

C --------------------------------------------------------------
      INTEGER MONTHD(12),MONTHL(12)
      
      INTEGER I,J, M, K,DAY,YEAR,NGRID
      
      INTEGER HUCNO, BYEAR, IYSTART,IYEND,NYEAR
      INTEGER RUNLAND_Y,ETLAND_Y,GEPLAND_Y
                   
      REAL LADUSE
      
      double precision HUCAREA
      
C      REAL RUN_LAND(4000,100,12,31,8)

      REAL RUNYR, FLOWYR,ETYR,GEPYR,UETYR,UGEPYR
      
      REAL RUNLAND,RUNLAND2,ETLAND,GEPLAND
      
      
                
      REAL CROPFLOW, FORESTFLOW, GRASSFLOW, SHRUBSAVFLOW,
     >     URBANWATERFLOW, TFLOW,URBANWATERFLO
      
      REAL  FLOWK(4000, 70, 20)
      
      
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
               ETYR=0.
               GEPYR=0.
               
            DO 100 M=1, 12
                
                
               IF (NDAY .EQ. 365) THEN
                 MNDAY=MONTHD(M)
               ELSE
                 MNDAY=MONTHL(M)
               ENDIF

C               RUNLAND_Y=(J-1)*12+M
			  
               DO 50 DAY=1, MNDAY

         
                 RUNYR = RUNYR+RUNLAND(I,J,M,DAY,K)
                 ETYR = ETYR+ETLAND(I,J,M,DAY,K)
                 GEPYR = GEPYR+GEPLAND(I,J,M,DAY,K)
                          
50             CONTINUE
                             
100        CONTINUE

C --- FLOW MILLION CUBIC METERS
                          
         FLOWYR = RUNYR*LADUSE(I,K)*HUCAREA(I)/1000./1000000.
         UETYR = ETYR*LADUSE(I,K)*HUCAREA(I)/1000./1000000.
         UGEPYR = GEPYR*LADUSE(I,K)*HUCAREA(I)/1000./1000000.

         
             IF (YEAR .GE. IYSTART .AND. YEAR .LE. IYEND) THEN 
           
         WRITE (910,4000) HUCNO(I),YEAR,K, RUNYR, FLOWYR,UETYR,UGEPYR, 
     >         LADUSE (I,K), HUCAREA(I) 
                                              
                                              
4000        FORMAT (I10, ',', I5, ',', I5, ',',F10.3, ',', F10.3, ',', 
     >           F10.3, ',', F10.3, ',', F10.3,',', F12.1)     
         
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
			   URBANWATERFLO= 0.
             
             
           DO 500 K=1, NLC    

C -- CROP

               IF (K.EQ.1) THEN 
               
                  CROPFLOW = CROPFLOW +  FLOWK(I,J, K)
                  
              
C -- FORESTS
 
          ELSEIF (K .EQ. 4  .OR. K .EQ. 5
     > .OR. K .EQ. 7.OR. K .EQ. 3) THEN 
     
               FORESTFLOW = FORESTFLOW +  FLOWK(I,J, K)
 
C -- GRASSLANDS               
       ELSEIF (K .EQ. 6) THEN                
               
             GRASSFLOW = GRASSFLOW + FLOWK(I, J , K)
              
                               
C -- SHRUBLANDS AND SAVANNAS                   
               
               ELSEIF (K .EQ. 8 .OR. K .EQ. 10 
     >   .OR. K .EQ. 9 ) THEN                

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
