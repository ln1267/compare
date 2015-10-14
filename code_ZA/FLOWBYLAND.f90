!C**********************************************************************C
!C                                                                      C
!C     *** SUBROUTINE Cabon balance ***                                 C
!C     Simulate GEP AND NEE for selected HUC                            C
!C     WRITE GEP AND NEE TO MONTHCARBON.TXT, ANNUALCARBON.TXT, HUCCARBON.TXT        C
!C     SIMULATE BIODIVERSITY FOR SELECTED HUC                           C
!C     WRITE BIODIVERSITY TO HUCBIO.TXT                                 C
!C！！！！！！！107行RUNLAND (流域数，年数，月份，月份，植被类型数）数组大小 C
!C**********************************************************************C
!C        I=HUC; J= YEAR, M =MONTH, MNDAY= NO OF DAYS IN A MONTH


      SUBROUTINE FLOWBYLAND

        Use Common_var
        implicit none      
! --------------------------------------------------------------
      INTEGER MONTHD(12),MONTHL(12)
      
      INTEGER I,J, M, K,DAY,YEAR,NDAY,MNDAY
      

!      REAL RUN_LAND(4000,100,12,31,8)

      REAL RUNYR, FLOWYR,ETYR,GEPYR,NEEYR,UETYR,UGEPYR,UNEEYR
      
      REAL RUNMR, FLOWMR,ETMR,GEPMR,NEEMR,UETMR,UGEPMR,UNEEMR
      
      REAL RUNLAND2
      
      REAL CROPFLOW, FORESTFLOW, GRASSFLOW, SHRUBSAVFLOW,&
          URBANWATERFLOW, TFLOW,URBANWATERFLO
      REAL VEG_1,VEG_2,VEG_3,VEG_4,VEG_5,VEG_6,VEG_7
      REAL  FLOWK(4000, 70, 20)
      REAL  FLOWMK(4000, 70,12, 20)
      
! --- Number of days for each month during regular year
      DATA MONTHD/31,28,31,30,31,30,31,31,30,31,30,31/

! --- Number of days for each month during leap year
      DATA MONTHL/31,29,31,30,31,30,31,31,30,31,30,31/
! ----------------------------------------------------------------      
            
     
	   
      DO 400 K=1, NLC     
           
           
      DO 300 I=1, NGRID         
             
                                   
         DO 200 J=1, NYEAR
             
             YEAR = BYEAR + J -1
             
! --- DETERMINE WHETHER YEAR IS A LEAP YEAR 
! --- http://www.timeanddate.com/date/leapyear.html

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
               NEEYR=0.0
               
            DO 100 M=1, 12
                
                
               IF (NDAY .EQ. 365) THEN
                 MNDAY=MONTHD(M)
               ELSE
                 MNDAY=MONTHL(M)
               ENDIF

!               RUNLAND_Y=(J-1)*12+M
               RUNMR=0.
               ETMR=0.
               GEPMR=0.
               NEEMR=0.
			  
               DO 50 DAY=1, MNDAY

         
                 RUNYR = RUNYR+RUNLAND(I,J,M,DAY,K)
                 ETYR = ETYR+ETLAND(I,J,M,DAY,K)
                 GEPYR = GEPYR+GEPLAND(I,J,M,DAY,K)
                 NEEYR = NEEYR+NEELAND(I,J,M,DAY,K)
                  
				 RUNMR = RUNMR+RUNLAND(I,J,M,DAY,K)
                 ETMR = ETMR+ETLAND(I,J,M,DAY,K)
                 GEPMR = GEPMR+GEPLAND(I,J,M,DAY,K) 
                 NEEMR = NEEMR+NEELAND(I,J,M,DAY,K) 

50             CONTINUE

! --- FLOW MILLION CUBIC METERS
                          
         FLOWMR = RUNMR*LADUSE(I,K)*HUCAREA(I)/1000./1000000.
         UETMR = ETMR*LADUSE(I,K)*HUCAREA(I)/1000./1000000.
         UGEPMR = GEPMR*LADUSE(I,K)*HUCAREA(I)/1000./1000000.
         UNEEMR = NEEMR*LADUSE(I,K)*HUCAREA(I)/1000./1000000.

         
             IF (YEAR .GE. IYSTART .AND. YEAR .LE. IYEND) THEN 
           
         WRITE (930,4001) HUCNO(I),YEAR,M,K, RUNMR, FLOWMR,UETMR,ETMR, &
             UGEPMR,GEPMR,UNEEMR,NEEMR,LADUSE (I,K), HUCAREA(I) 
                                              
                                              
4001        FORMAT (I10, ',', I5, ',', I5, ',', I5,',',F10.3, ',',  &
            F10.3, ',',F10.3, ',',F10.3, ',',F10.3, ',', F10.3, ',', &
     F10.3, ',',F10.3, ',', F10.3,',', F12.1)     
         

            FLOWMK(I,J,M, K) = FLOWMR  
            
       
           ENDIF


            
                             
100        CONTINUE

! --- FLOW MILLION CUBIC METERS
                          
         FLOWYR = RUNYR*LADUSE(I,K)*HUCAREA(I)/1000./1000000.
         UETYR = ETYR*LADUSE(I,K)*HUCAREA(I)/1000./1000000.
         UGEPYR = GEPYR*LADUSE(I,K)*HUCAREA(I)/1000./1000000.
         UNEEYR = NEEYR*LADUSE(I,K)*HUCAREA(I)/1000./1000000.

         
             IF (YEAR .GE. IYSTART .AND. YEAR .LE. IYEND) THEN 
           
         WRITE (910,4000) HUCNO(I),YEAR,K, RUNYR, FLOWYR,UETYR,ETYR, &
              UGEPYR,GEPYR,UNEEYR,NEEYR,LADUSE (I,K), HUCAREA(I) 
                                              
                                              
4000        FORMAT (I10, ',', I5, ',', I5, ',',F10.3, ',', F10.3, ',', &
           F10.3, ',', F10.3, ',',F10.3, ',', F10.3,',',F10.3, ',', &
      F10.3,',', F10.3,',', F12.1)     
         
             FLOWK(I,J, K) = FLOWYR  
       
            
       
           ENDIF
          
200      CONTINUE
300      CONTINUE
      


400      CONTINUE





! -- RECLASSIFY LANDCOVER AND WATRE YIELD 


  
      DO 700 I=1, NGRID                               
        DO 600 J=1, NYEAR      


             YEAR = BYEAR + J -1
              
             VEG_1 =0.
             VEG_2 = 0.
             VEG_3 = 0.
             VEG_4 = 0.
             VEG_5 = 0.
             VEG_6 = 0.
             VEG_7 = 0.
			 URBANWATERFLOW= 0.
             
             
           DO 500 K=1, NLC    

! -- CROP

               IF (K.EQ.1) THEN 
               
                  VEG_1 = VEG_1 +  FLOWK(I,J, K)
                  
              
! -- FORESTS
 
          ELSEIF (K .EQ. 2) THEN 
     
               VEG_2 = VEG_2 +  FLOWK(I,J, K)
 
! -- GRASSLANDS               
       ELSEIF (K .EQ. 3) THEN                
               
             VEG_3 = VEG_3 + FLOWK(I, J , K)

! -- CROP

              
! -- FORESTS
 
          ELSEIF (K .EQ. 4) THEN 
     
               VEG_4 = VEG_4 +  FLOWK(I,J, K)
 
! -- GRASSLANDS               
       ELSEIF (K .EQ. 5) THEN                
               
             VEG_5 = VEG_5 + FLOWK(I, J , K)
! -- GRASSLANDS               
       ELSEIF (K .EQ. 6) THEN                
               
             VEG_6 = VEG_6 + FLOWK(I, J , K)                               
! -- SHRUBLANDS AND SAVANNAS                   
               
               ELSEIF (K .EQ. 7) THEN                

                VEG_7 =  VEG_7 + FLOWK(I, J , K)
                               
! -- URBAN/BARRENS/WATRE BODY (SAME AS OPEN SHRUB)  

                             
               ELSE
              
                              
              URBANWATERFLOW =  URBANWATERFLOW + FLOWK(I, J , K)
                             
              ENDIF                      
                                     
                               
500         CONTINUE

         TFLOW=VEG_1 +VEG_2+VEG_3+VEG_4+VEG_5+VEG_6+VEG_7+URBANWATERFLOW
                
             IF (YEAR .GE. IYSTART .AND. YEAR .LE. IYEND) THEN 
           
         WRITE (920,5000) HUCNO(I),YEAR, VEG_1 ,VEG_2,VEG_3,VEG_4,VEG_5 &
     	 ,VEG_6,VEG_7, URBANWATERFLOW,TFLOW
                                
                                              
5000     FORMAT (I10, ',', I5, ',', F10.3, ',', F10.3, ',', F10.3,',', &
      F10.3, ',', F10.3, ',', F10.3,',', F10.3, ',', F10.3, ',', F10.3)

         ENDIF       
           

600      CONTINUE
700   CONTINUE




! -- MONTH RECLASSIFY LANDCOVER AND WATRE YIELD 


  
      DO 701 I=1, NGRID                               
        DO 601 J=1, NYEAR      
        DO 401 M=1, 12 

             YEAR = BYEAR + J -1
             
             VEG_1 =0.
             VEG_2 = 0.
             VEG_3 = 0.
             VEG_4 = 0.
             VEG_5 = 0.
             VEG_6 = 0.
             VEG_7 = 0.
			 URBANWATERFLOW= 0.
			 TFLOW= 0.
             
             
           DO 501 K=1, NLC    

! -- CROP

               IF (K.EQ.1) THEN 
               
                  VEG_1 = VEG_1 +  FLOWMK(I,J,M, K)
                  
              
! -- FORESTS
 
          ELSEIF (K .EQ. 2) THEN 
     
               VEG_2 = VEG_2 +  FLOWMK(I,J,M, K)
 
! -- GRASSLANDS               
       ELSEIF (K .EQ. 3) THEN                
               
             VEG_3 = VEG_3 + FLOWMK(I,J,M, K)

! -- CROP

              
! -- FORESTS
 
          ELSEIF (K .EQ. 4) THEN 
     
               VEG_4 = VEG_4 +  FLOWMK(I,J,M, K)
 
! -- GRASSLANDS               
       ELSEIF (K .EQ. 5) THEN                
               
             VEG_5 = VEG_5 + FLOWMK(I,J,M, K)
! -- GRASSLANDS               
       ELSEIF (K .EQ. 6) THEN                
               
             VEG_6 = VEG_6 + FLOWMK(I,J,M, K)                           
! -- SHRUBLANDS AND SAVANNAS                   
               
               ELSEIF (K .EQ. 7) THEN                

                VEG_7 =  VEG_7 + FLOWMK(I,J,M, K)
                               
! -- URBAN/BARRENS/WATRE BODY (SAME AS OPEN SHRUB)  

                             
               ELSE
              
                              
              URBANWATERFLOW =  URBANWATERFLOW + FLOWMK(I,J,M, K)
                             
              ENDIF                      
                                     
                               
501         CONTINUE

         TFLOW=VEG_1 +VEG_2+VEG_3+VEG_4+VEG_5+VEG_6+VEG_7+URBANWATERFLOW
                
             IF (YEAR .GE. IYSTART .AND. YEAR .LE. IYEND) THEN 
           
         WRITE (940,5001) HUCNO(I),YEAR,M, VEG_1 ,VEG_2,VEG_3,VEG_4, &
      VEG_5,VEG_6,VEG_7, URBANWATERFLOW,TFLOW 
                                                    
                                              
5001     FORMAT (I10, ',', I5, ',',I5, ',', F10.3, ',', F10.3, &
      ',', F10.3,',',F10.3, ',', F10.3, ',', F10.3,',', F10.3, ',', F10.3, ',', F10.3)
          
                
           ENDIF       
           
401      CONTINUE
601      CONTINUE
701   CONTINUE




      RETURN
      END
