C**********************************************************************C
C                                                                      C
C     *** SUBROUTINE RPSDF ***                                         C
C     Read basic input data from GENERAL.TXT and write to              C
C     BASICOUT.TXT, set up column headings in output files             C
C                                                                      C
C**********************************************************************C
      SUBROUTINE RPSDF
      CHARACTER*4 HEADNG(20)
      
      
      COMMON/BASIC/NGRID, NYEAR, NLC,BYEAR,IYSTART,IYEND
          
c      COMMON/LAND/ SPRING,SUMMER,FALL,WINTER,SI(366)

c      COMMON/GROUNDWATER/ GROUNDWATER(4000,9), ALAFA
      
      COMMON/SNOWPACK/SP(12), SNOWPACK, NSPM(200)
      
c      COMMON/NODE/NONODE
      
      COMMON/LANDCHANGE/FPERD, FPERDLAI
 
      INTEGER NLC
      INTEGER BYEAR
    
c      REAL ALAFA
      REAL FPERD, FPERDLAI


C --- Read in data from GENERAL.TXT and write to BASICOUT.TXT

           
      READ(1,1000) HEADNG
 1000 FORMAT(20A4)
      WRITE(77,2010) HEADNG
 2010 FORMAT(' ',20A4/)

      READ(1,*) ISCENARIOS
      
      WRITE(77,*) ISCENARIOS
2015  FORMAT(' Scenario #', I10)


      READ(1,*) NGRID, NYEAR, BYEAR, NLC

      WRITE(77,2020) NGRID
 2020 FORMAT(I5,' ACTIVE GRIDS'/)
      
      WRITE(77,2030) NYEAR, BYEAR
      
 2030 FORMAT(I10,' YEARS TO BE SIMULATED AND',' FIRST YEAR =', I10)
      
      WRITE(77,2040) NLC
      
 2040 FORMAT('NUMBER OF LAND COVER CATEGORIES: ',I10)      
           
      READ (1,*) IYSTART, IYEND

      WRITE(77,2050) IYSTART, IYEND
2050  FORMAT('FOR SIMULATION SUMMARY, YEAR TO START',I10, ' , END ',I10)


      READ (1,*) FPERD

C--  reduction fraction of Leaf Area index for scenario analysis

      READ (1, *) FPERDLAI
C1033  FORMAT (F10.2) 

      WRITE(77,2058) FPERD, FPERDLAI
2058  FORMAT('DEFOREST RATE%',F10.2, /'LAI REDUCTION%=', F10.2)


       READ (1,*) SNOWPACK
C1046   FORMAT (F10.2) 
              
      WRITE(77,1047) SNOWPACK

1047  FORMAT('INTIAL SNOWPACK (MM) = :', F10.2)


C-----PRINT TITLE FOR MONTHLY OUTPUT FILE MONTHRUNOFF.TXT 
    
      WRITE (78, 1050)
1050  FORMAT(' CELL#,     YEAR, MONTH,    PRECIP,  TEMP,',
     >'         SMC,        SNWPK,    PET, AET,',
     >'  RUNOFF, FLOWMCMMon')
          
c      WRITE (900, 1055)
c1055  FORMAT('     CELL#  YEAR MONTH     UZTWC     UZFWC',
c     >'     LZTWC     LZFPC     LZFSC')
     
C-----PRINT TITLE FOR ANNUAL OUTPUT ANNUALFLOW.TXT    
     
       WRITE (79, 1060)
1060  FORMAT ( ' CELL#,          YEAR,      RAIN,    PET,',
     >'   AET,    RUNOFF, RUN_Pratio, ET_Pratio, RUN_ETRatio,', 
     > ' SNWPCKMON, RFACTOR')
                
C-----PRINT TITLE FOR SUMMARY OUTPUT SUMMARRUNOFF.TXT  

      WRITE (80, 1070)
1070  FORMAT (' CELL#,   RAIN,    PET,',
     > '       AET,    RUNOFF,    RUNOFF/P,   ET/P,  (RUN+ET)/P',
     > ' RFACTOR')

       
         WRITE (910,1080) 
         
1080    FORMAT ('WATERSHEDID,  YEAR, LADUSEID,',
     > 'HUCRUNOFF, FLOWVOL, LAND%, HUCAREA')        
       
       
       
         WRITE (920,1090) 
         
1090    FORMAT ('WATERSHEDID,    YEAR,   CROPFLOW,',
     > 'FORESTFLOW, GRASSFLOW, SHRUBSAVAFLOW, URBANWATERFLOW, TFLOW') 

	     
      RETURN
      END
C**********************************************************************C
C                                                                      C
C     *** SUBROUTINE RPSINT ***                                        C
C     Read in landuse data from CELLINFO.TXT, calcuate change in       C
C     landuse based on percent forest decrease (if desired), write     C
C     to BASICOUT.TXT                                                  C
C     基于设定的采伐值重计算土地利用情况。                             C
C**********************************************************************C
      SUBROUTINE RPSINT 
          
      COMMON/BASIC/NGRID, NYEAR, NLC,BYEAR,IYSTART,IYEND
      
      COMMON/CELLINFO/LADUSE(4000, 20),HUCNO(4000),
     >                LATUDE(4000),LONGI(4000)
      
      COMMON/SOIL/LZTWM(4000), LZFPM(4000), LZFSM(4000), LZSK(4000),
     >  LZPK(4000), UZTWM(4000), UZFWM(4000), UZK(4000), ZPERC(4000),
     >  REXP(4000), PFREE(4000), SMC(12)

      COMMON/LANDCHANGE/FPERD, FPERDLAI
             
      INTEGER HUCNO, NLC

      REAL  LATUDE,LONGI, LADUSE
      
      
      REAL LZTWM, LZFPM, LZFSM, LZSK,
     >  LZPK, UZTWM, UZFWM, UZK, ZPERC,
     >  REXP, PFREE, SMC
     
      REAL FPERD
      
      REAL CROP


      CHARACTER*1000 DUMY(30)
      
C --- Read and print land use data for each active cell IN THE BASIC.OUT FILE
      WRITE(77,2000)
2000  FORMAT(/'LANDUSE INFO FOR EACH SIMULATION CELL'/)
      READ (2,500) DUMY
500   FORMAT (1000A30)
      

C ----LANC = raw Landcover types    
      
      WRITE (77,500) DUMY
             
      DO 10 I=1, NGRID

      READ(2,*) ID, HUCNO(I), LATUDE(I), LONGI(I),  
     >    (LADUSE(I,K),K=1, NLC)
      
             
c      WRITE(77,1100) ID, HUCNO(I),LATUDE(I), LONGI(I), 
c     > (LADUSE(I,K),K=1, NLC)

1100  FORMAT(2I10, 2F10.4, 15F10.4)    
     
10    CONTINUE


C --- Read and print SOIL PARAMETERS for each active cell IN THE BASIC.OUT FILE

      WRITE(77,2051)
2051  FORMAT(/'SOIL PARAMETERS FOR EACH SIMULATION CELL'/)
      READ (7,550) DUMY
550   FORMAT (30A8)
      
      
      WRITE (77,550) DUMY
          
      DO 15 I=1, NGRID

      READ(7,*) ID, HUCNO(I), UZTWM(I), UZFWM(I), UZK(I), ZPERC(I),
     &REXP(I), LZTWM(I), LZFSM(I), LZFPM(I), LZSK(I),
     &LZPK(I), PFREE(I)
            
C      WRITE(77,1150) HUCNO(I), UZTWM(I), UZFWM(I), UZK(I), ZPERC(I),
C     &REXP(I), LZTWM(I), LZFSM(I), LZFPM(I), LZSK(I),
C     &LZPK(I), PFREE(I)
     
1150  FORMAT(I12, 11F10.4)    
     
15    CONTINUE


C ---- Converting forest to two types of croplands in the same watersheds
C------根据设定的采伐率重计算植被类型中的某些变化植被类型
 
!      DO 20 I=1, NGRID
!      
!           
!      CROP = LADUSE(I,1) + LADUSE(I,2)
!     
!      IF (CROP .NE. 0.) THEN
! 
!      LADUSE(I,1)=(LADUSE(I,1)/(LADUSE(I,1) + LADUSE(I,2))) 
!     & * FPERD * (LADUSE(I,4) + LADUSE(I,5)+ LADUSE(I,6)+   
!     &LADUSE(I,7) + LADUSE(I,8)+ LADUSE(I,11) ) 
!     &+LADUSE(I,1)
!             
!      
!      LADUSE(I,2)=(LADUSE(I,2)/(LADUSE(I,1) + LADUSE(I,2)))
!     & * FPERD * (LADUSE(I,4) + LADUSE(I,5)+ LADUSE(I,6) +   
!     & LADUSE(I,7) + LADUSE(I,8)+ LADUSE(I,11) ) 
!     & + LADUSE(I,2)
! 
!      ELSEIF (CROP .EQ. 0.) THEN
!     
!         LADUSE(I,1) = 0.5*FPERD
!     & *FPERD * (LADUSE(I,4) + LADUSE(I,5)+ LADUSE(I,6)   
!     & +LADUSE(I,7)+LADUSE(I,8)+ LADUSE(I,11) ) 
!     & +LADUSE(I,1)
!      
!        LADUSE(I,2) = 0.5
!     & * FPERD * (LADUSE(I,4) + LADUSE(I,5)+ LADUSE(I,6)+   
!     & LADUSE(I,7) + LADUSE(I,8)+ LADUSE(I,11))
!     & + LADUSE(I,2)
!     
!         ENDIF 
!                          
!      LADUSE(I,4) = LADUSE(I,4)*(1-FPERD)
!      LADUSE(I,5) = LADUSE(I,5)*(1-FPERD)
!      LADUSE(I,6) = LADUSE(I,6)*(1-FPERD)
!      LADUSE(I,7) = LADUSE(I,7)*(1-FPERD)
!      LADUSE(I,8) = LADUSE(I,8)*(1-FPERD)
!      LADUSE(I,11) = LADUSE(I,11)*(1-FPERD)
!  20    CONTINUE

      RETURN
      END

      
C**********************************************************************C
C                                                                      C
C     *** SUBROUTINE RPSWATERUSE ***                                   C
C     Read in HUC area, elevation, and slope from HUCAREA.TXT          C
C     Read in return flow rate from RETURNFLOW.TXT                     C
C     Read in water use by water resource region from HUCREGION.TXT    C
C     Read in population info from POPULATION.TXT                      C
C     READ IN GROUNDWATER USE BY SECTOR from GROUNDWATER.TXT           C
C     READ IN SURFACE WATER USE BY SECTOR from WATERUSE.TXT            C
C     calculate change in irrigation water use if desired              C
C     Read in huc node info from NODEHUC.TXT                           C
C     Read in average monthly flows from AVGMONFLOW.TXT                C
C                                                                      C
C**********************************************************************C
      SUBROUTINE RPSWATERUSE

      COMMON/BASIC/NGRID, NYEAR, NLC,BYEAR,IYSTART,IYEND
     
      
c      COMMON/POPULATION/POPULATION(4000, 150)      
c      COMMON/HUCREGION/DMC(18,4),IRRC(18,4), PPC(18,4) 
c      COMMON/RETURNFLOW/ RETURNFLOW(4000,8)
c      COMMON/GROUNDWATER/ GROUNDWATER(4000,9), ALAFA      
c      COMMON/WATERUSE/ WATERUSE(4000,8), HUCWUSE(4000)

      COMMON/HUC/ HUCAREA(4000)      
      
c      COMMON/ELEVATION/HUCELE(4000)
      
c      COMMON/FLOWNETWORK/IDFHUC(4000), FHUC(4000), IDTHUC(4000), 
c     &THUC(4000)
     
c      COMMON/NODE/NONODE
          
c      COMMON/ROUTING/RECHUC(4000,50),NORECHUC(4000)
      
c      COMMON/IRRIGATION/DRIRR
      
c      REAL  POPULATION(4000,150)                   
c      REAL HUCAREA(4000), RETURNFLOW(4000,8), WATERUSE(4000, 8) 
c      REAL GROUNDWATER (4000, 9), HUCWUSE(4000)

       double precision HUCAREA
      
c      REAL DMC(18,4), IRRC(18,4), PPC(18,4)
c      REAL DRIRR     
      
c      INTEGER IDFHUC(4000), FHUC(4000), IDTHUC(4000), THUC(4000)
      
c      INTEGER K,RECHUC(4000, 50)
      
c      INTEGER NORECHUC(4000), POP_FLAG 
      
      
      CHARACTER*10 DUMY7(30)
          
c      INTEGER HUCN
      
       
C---READ IN HUC AREA, ELEV, and slope from HUCAREA.TXT
      
           
       READ (11, 50) DUMY7
50     FORMAT (30A10)
 
       WRITE (77, 50) DUMY7
             
       DO 60 I = 1, NGRID
       
       READ (11, *) ID, IDHUC, HUCAREA(I)
       
C       print *, ID, IDHUC, HUCAREA(I)
      
60     CONTINUE


C---READ IN RETURN FLOW RATE from RETURNFLOW.TXT

c       WRITE (77, 20)
c20    FORMAT (/'RETURNFLOW RATES BY SECTOR IN DECIMAL POINT'/)
 
c       READ (15, 50) DUMY7
c       WRITE (77, 50) DUMY7
               
c       DO 65 I=1, NGRID
       
c       READ (15, *) IDHUC, HUCN,(RETURNFLOW(I,J), J=1, 8)
       
C       WRITE (77,86) IDHUC, HUCN,(RETURNFLOW(I,J), J=1, 8)
       
c86     FORMAT(2I12, 8F10.3)

c65     CONTINUE 
     
C-- READ IN water resource region water use info from HUCREGION.TXT
        
c       READ (12,50) DUMY7
c       WRITE (77, 50) DUMY7
            
c       DO 55 I = 1, 18
       
c       READ (12, *) IREGION, (DMC(I,J), J=1,4), 
c     >        (IRRC(I,J), J=1,4), (PPC(I,J), J=1,4)   
     
C       WRITE (77, 98) IREGION, (DMC(I,J), J=1,4), 
C     >        (IRRC(I,J), J=1,4), (PPC(I,J), J=1,4)       
          
c55    CONTINUE

c98    FORMAT (I10, 12F10.5)

C-- READ IN population info from POPULATION.TXT
    

c      READ (13, 50) DUMY2
     
c      WRITE (77, 800)
       
c800   FORMAT (/'POPULATION DATA 10^3 People'/)

c      WRITE (77,900) DUMY2
       
c900   FORMAT (30A10)
        
        
c      DO 300 I = 1, NGRID
 
C ---J=35 => YEAR = 1994
C ---J=91 => YEAR = 2050

c         DO 400 J=35, 91
      
c            READ (13, *) HUCN, JYEAR, POPULATION(I, J) 
            
c1000        FORMAT(2I10, F12.3)

c400       CONTINUE 

c300   CONTINUE   


C ----POP_FLAG=3, TIME VARIABLE POPULATION 1994-2050

c      IF (POP_FLAG .EQ. 3) THEN

c      DO 211 I=1, NGRID
                
C --- ASSIGN YEAR 1994 POPULATION DATA TO YEARS BEFORE 1994

c         DO 311 J=1, 34

c            POPULATION(I,J) = POPULATION(I,35)
            
c311      CONTINUE

C --- ASSIGN YEAR 2050 POPULATION DATA TO YEARS AFTER 2050 UP TO 2100

c         DO 312 J=92, 141

c            POPULATION(I,J) = POPULATION(I,91)
            
c312      CONTINUE

c211   CONTINUE   


C ---POP_FLAG = 1, CONSTANT BASELINE POPULATION USING YEAR 2000 POPULATION

c      ELSEIF (POP_FLAG .EQ. 1) THEN
      
c      DO 214 I=1,NGRID
      
c         DO 314 J=1,40
      
c            POPULATION(I,J) = POPULATION(I,41)
         
c314      CONTINUE

c         DO 315 J=42,141
      
c            POPULATION(I,J) = POPULATION(I,41) 
           
c315      CONTINUE

c214   CONTINUE

C ---POP_FLAG = 2, CONSTANT FUTURE POPULATION USING YEAR 2050 POPULATION

c      ELSEIF (POP_FLAG .EQ. 2) THEN
      
c      DO 216 I=1,NGRID
      
c         DO 316 J=1,90
      
c            POPULATION(I,J) = POPULATION(I,91)
         
c316      CONTINUE

c         DO 317 J=92,141
      
c            POPULATION(I,J) = POPULATION(I,91) 
            
c317      CONTINUE

c216   CONTINUE     

C ---POP_FLAG = OTHER- POPULATION FLAG ERROR

c      ELSE
      
c      WRITE (*,23)
      
c23    FORMAT(/'POPULATION FLAG ERROR'/) 

c      ENDIF
      
C ----WRITE POPULATION DATA TO BASICOUT.TXT FOR VERIFICATION

c      DO 218 I=1,NGRID
      
c         DO 318 J=1,141
      
C            WRITE(77,1069) I,J+1959,POPULATION(I,J)
            
c1069        FORMAT (2I10, F12.3)

            
c318      CONTINUE

c218   CONTINUE     

        
C--READ IN GROUNDWATER WITHDRAWAL DATA BY SECTOR (8 SECTORS) IN MILLION GALON/day
c
c       WRITE (77, 820)
c820    FORMAT (/'GROUNDWATER WITHDRAWAL DATA'/)
       
c       READ (14, 50) DUMY4
       
c       WRITE (77, 50) DUMY4
       
c       DO 80 I = 1, NGRID
       
c       READ (14, *) IDHUC, HUCN, (GROUNDWATER(I, J), J=1, 9)     

C       WRITE (77, 1100) IDHUC, HUCN, (GROUNDWATER(I, J), J=1, 9)
       
c1100   FORMAT (2I12, 9F12.3)                         

c80    CONTINUE      

C--READ IN SURFACE WATER USE BY SECTOR (8 SECTORS)

c       WRITE (77, 830)
c830    FORMAT (/'WATERUSE DATA'/)
       
c       READ (16, 50) DUMY6
c       WRITE (77,50) DUMY6
      
c       DO 90 I = 1, NGRID
       
c       READ (16, *) IDHUC, HUCN, (WATERUSE(I,J), J=1, 9)
              
C       WRITE (77,1200)  IDHUC,HUCN, (WATERUSE (I,J), J=1,9)       
c1200   FORMAT(2I15, 9F12.3)
             
c90     CONTINUE

C------WATER USE CHANGE IRRIGATION SECTOR REDUCED BY DRIRR
c       WRITE (77, 835)
c835    FORMAT (/'ALTERED IRRIGATION WATERUSE DATA'/)

c       DO 99 I = 1, NGRID
       
c       WATERUSE(I,3) = WATERUSE(I,3) * (1-DRIRR)               
       
C       WRITE (77,1250) I, (WATERUSE (I,J), J=1,8), DRIRR
       
c1250   FORMAT(I10, 8F12.3, F5.2)
             
c99     CONTINUE



C --- READ IN HUC NODE INFO FROM NODEHUC.TXT

c      WRITE (77, 840)
c840   FORMAT (/'Stream Network Info'/)
       
c       READ (5, 50) DUMY7
c       WRITE (77,50) DUMY7
             
c       K=1

C --  SET FIRST NODE = 99

c       IDTHUC(0) = 99
       
c       DO 100 I = 1, NONODE
       
c          READ (5, *) IDFHUC(I), FHUC(I), IDTHUC(I), THUC(I)
       
C       WRITE(77,841) IDFHUC(I), FHUC(I), IDTHUC(I), THUC(I)
       
c841       FORMAT(4I10)
       
C ---  ESTABLISH RELATIONS AMONG NODE AND HUC       
       
c          IDHUCT = IDTHUC(I)
c          IDHUCF = IDFHUC(I)

C -- IF THERE IS A DUPLICATED HUC THEN ESTIMATE NUMBER OF HUC RECEIVING FLOWS
       
c          IF (IDHUCT .EQ. IDTHUC(I-1) ) THEN  
       
c             K = K + 1
       
c             RECHUC(IDHUCT, K) = IDFHUC(I) 
             
c          ELSE
       
c             K=1
       
c             RECHUC(IDHUCT, K) = IDFHUC(I) 
       
c          ENDIF       
     
c          NORECHUC(IDHUCT) = K    
          
       
c100    CONTINUE

C -----------------------------------------------------------

      RETURN
      END
      
C**********************************************************************C
C                                                                      C
C     *** SUBROUTINE RPSLAI ***                                        C
C     Input MONTHLY LAI, FILL IN GAPS FOR PERIODS WITH NO LAI DATA     C
C                                                                      C
C**********************************************************************C
      SUBROUTINE RPSLAI
      
      COMMON/BASIC/NGRID, NYEAR, NLC,BYEAR,IYSTART,IYEND

      COMMON/LAI/LAI_1(4000,200,12), LAI_2(4000,200,12), 
     &LAI_3(4000,200,12),LAI_4(4000,200,12),LAI_5(4000,200,12), 
     &LAI_6(4000,200,12), LAI_7(4000,200,12),LAI_8(4000,200,12),
     &LAI_9(4000,200,12),LAI_10(4000,200,12),LAI_11(4000,200,12), 
     &LAI_12(4000,200,12), LAI_13(4000,200,12), LAI_14(4000,200,12),
     &LAI_15(4000,200,12),LAI_16(4000,200,12), LAI_17(4000,200,12),
     &LAI_18(4000,200,12), LAI_19(4000,200,12)
     
     
      COMMON/LANDCHANGE/FPERD, FPERDLAI
              
      INTEGER NLC, HUCNO(4000), YEAR, NGRID
      
      REAL FPERD, FPERDLAI
      
      INTEGER I, J, M,Mon
     
      CHARACTER*100 TEMPHEAD3 (11)
            
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
 
 
c --- READ IN LAI DATA FROM LANDLAI.TXT

      DO 201 I=1, NGRID
                
         DO 301 J=1,1 
         
C --- J=41 => YEAR 2000 (FIRST YEAR OF LAI DATA)
C --- J=47 => YEAR 2006 (LAST YEAR OF LAI DATA)

            DO 401 M=1, 12

            IF (I .EQ. 1 .AND. J .EQ. 1 .AND. M .EQ. 1) THEN 

               READ (8, 902) TEMPHEAD3
 
 902           FORMAT (100A11)
 
            ENDIF
                      
 
C --- LAI_* IS THE LAI FOR LANDUSE * (8 TOTAL IN LANDLAI.TXT)

            READ(8,*) HUCNO(I),YEAR,Mon,LAI_1(I,J,M),LAI_2(I,J,M),
     &LAI_3(I,J,M), LAI_4(I,J,M), LAI_5(I,J,M), LAI_6(I,J,M), 
     &LAI_7(I,J,M), LAI_8(I,J,M),
     &LAI_9(I,J,M), LAI_10(I,J,M), LAI_11(I,J,M), LAI_12(I,J,M)
C     &,LAI_13(I,J,M)
C	 , LAI_14(I,J,M), LAI_15(I,J,M)                 
    
    
c1011        FORMAT(3I10, 8F10.2)               


401         CONTINUE 

301      CONTINUE

201   CONTINUE

C --- ASSIGN YEAR 2000 LAI DATA TO YEARS BEFORE 2000
C -----将2001年的数据赋给以前的年份

!      DO 202 I=1, NGRID
!                
!         DO 302 J=1, 40
!
!            DO 402 M=1, 12
!
!            LAI_1(I,J,M) = LAI_1(I,41,M)
!            LAI_2(I,J,M) = LAI_2(I,41,M)
!            LAI_3(I,J,M) = LAI_3(I,41,M)
!            LAI_4(I,J,M) = LAI_4(I,41,M)
!            LAI_5(I,J,M) = LAI_5(I,41,M)
!            LAI_6(I,J,M) = LAI_6(I,41,M)
!            LAI_7(I,J,M) = LAI_7(I,41,M)
!            LAI_8(I,J,M) = LAI_8(I,41,M)
!            
!            LAI_9(I,J,M) = LAI_9(I,41,M)
!            LAI_10(I,J,M) = LAI_10(I,41,M)
!            LAI_11(I,J,M) = LAI_11(I,41,M)
!            LAI_12(I,J,M) = LAI_12(I,41,M)
!            LAI_13(I,J,M) = LAI_13(I,41,M)
!            LAI_14(I,J,M) = LAI_14(I,41,M)
!            LAI_15(I,J,M) = LAI_15(I,41,M)
!                      
!     
!402         CONTINUE 
!
!302      CONTINUE
!
!202   CONTINUE


          
C--- ASSIGN YEAR 2002 LAI DATA TO YEARS AFTER 2002
C--- 将2011年以后的数据赋给以后的年份
!      DO 203 I=1, NGRID
!                
!         DO 303 J=52, 70
!
!            DO 403 M=1, 12
!
!            LAI_1(I,J,M) = LAI_1(I,51,M)
!            LAI_2(I,J,M) = LAI_2(I,51,M)
!            LAI_3(I,J,M) = LAI_3(I,51,M)
!            LAI_4(I,J,M) = LAI_4(I,51,M) *(1.0-FPERDLAI)
!            LAI_5(I,J,M) = LAI_5(I,51,M) *(1.0-FPERDLAI)
!            LAI_6(I,J,M) = LAI_6(I,51,M) *(1.0-FPERDLAI)
!            LAI_7(I,J,M) = LAI_7(I,51,M) *(1.0-FPERDLAI)
!            LAI_8(I,J,M) = LAI_8(I,51,M) *(1.0-FPERDLAI) 
!            
!            LAI_9(I,J,M) = LAI_9(I,51,M)
!            LAI_10(I,J,M) = LAI_10(I,51,M)
!            LAI_11(I,J,M) = LAI_11(I,51,M)*(1.0-FPERDLAI)
!            LAI_12(I,J,M) = LAI_12(I,51,M)
!            LAI_13(I,J,M) = LAI_13(I,51,M)
!C            LAI_14(I,J,M) = LAI_14(I,51,M)
!C            LAI_15(I,J,M) = LAI_15(I,51,M)
!           
!           
!
!403         CONTINUE 
!
!303      CONTINUE
!
!203   CONTINUE

C --- WRITE LAI DATA TO BASICOUT.TXT FOR VALIDATION
            

        WRITE (77, 801)
       
801    FORMAT (/'LAI DATA BY LAND USE'/)

 
      WRITE (77, 902) TEMPHEAD3
      

      RETURN
      END

C**********************************************************************C
C                                                                      C
C     *** SUBROUTINE RPSCLIMATE ***                                    C
C     Input MONTHLY CLIMATE DATA, CALCULATE ANNUAL PPT                 C
C                                                                      C
C**********************************************************************C
      SUBROUTINE RPSCLIMATE
      
      COMMON/BASIC/NGRID, NYEAR, NLC,BYEAR,IYSTART,IYEND
      
      COMMON/CLIMATE/ RAIN(4000,200,12), TEMP(4000,200, 12), AAPPT(4000)

      INTEGER HUCNO(4000), NYEAR, NGRID, YEAR
            
      INTEGER I, J, M,Mon
     
      REAL RAIN, TEMP, ANNPPT(4000,200)
      
      REAL SUMANPPT(4000), AAPPT
      
      CHARACTER*10 TEMPHEAD (10)


      ANNPPT =0.
      
      SUMANPPT = 0.

      AAPPT = 0.
      

      DO 5000 I=1,NGRID
      
         DO 5001 J=1,NYEAR
         
            DO 5002 M=1,12
            
               IF (I .EQ. 1 .AND. J .EQ. 1 .AND. M .EQ. 1) THEN 
       
                  READ (4, 900) TEMPHEAD

                 WRITE (77, 900) TEMPHEAD
      
c                 WRITE (77, 905)
c905              FORMAT ('end of input data' )

                             
               ENDIF
        
900            FORMAT (10A10)
C910            FORMAT  (/'CLIMATE DATA', 10A10)
               
              
               READ(4,*) HUCNO(I), YEAR, Mon, RAIN(I,J,M), TEMP(I,J,M)
            
                
c1015        FORMAT(3I10, 2F10.2) 
                       
            
               ANNPPT(I, J) = ANNPPT(I, J) + RAIN(I,J,M)
               

5002        CONTINUE

            SUMANPPT(I) = SUMANPPT(I) + ANNPPT(I, J)

5001     CONTINUE

         AAPPT(I) = SUMANPPT(I)/NYEAR
                
         
C         WRITE(77,5004) HUCNO(I), AAPPT(I)
      
c5004     FORMAT(I10,F10.2)

5000  CONTINUE

      RETURN
      END
