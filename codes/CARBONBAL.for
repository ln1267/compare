
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

      SUBROUTINE CARBONBAL
      
      COMMON/BASIC/NGRID, NYEAR, NLC,BYEAR,IYSTART,IYEND

      COMMON/VAL/VAL_1(1000), VAL_2(100), VAL_3,VAL_4,VAL_5,VAL_6

      COMMON/HUC/ HUCAREA(4000)
	  
	  COMMON/VALID/ GEP_V(1000,200,12), ET_V(1000,200, 12)
     >,GPP_V(1000,200),NPP_V(1000,200), FLOW_V(200,12),FLOW(1,12)
      
      COMMON/CARBON/ GEPM(4000, 200, 12),RECOM(4000,200,12), 
     >  NEEM(4000,200,12),GEPA(1000,200),NEEA(1000,200)
      
      COMMON/CELLINFO/LADUSE(4000,20),HUCNO(4000),
     >                LATUDE(4000),LONGI(4000)
       
      
      COMMON/OUTPUT1/ PET(200,12,20),APET(12),PAET(200,12,20),APAET(12),
     &AET(12), RUNOFF(12), INTER(12), PRIBF(12), SECBF(12), INTF(12), 
     &AVUZTWC(12), AVUZFWC(12), AVLZTWC(12), AVLZFPC(12)
     >,A_ET(1000,200, 12),P_ET(1000,200, 12),Sun_ET(7,1, 12)
      
C---BIODIVERSITY CALCULATION USING ANNUAL AET, PET      
      
      COMMON/HUCPETAET/HUCAET(4000,200), HUCPET(4000,200),
     > HUCPAET(4000,200)
          
C --------------------------------------------------------------
      INTEGER I,J, M

      
C ---------------------------------------------------------------      
                     
      INTEGER HUCNO, BYEAR
                   
      REAL HUCAREA
      
      REAL ANGEP, ANRECO, ANNEE,  AHUCGEP, AHUCNEE,AHUCRE
      REAL HUCGEP, HUCNEE, HUCRE

      REAL HUCAET, HUCPET
      
      REAL HUCTRS,TRS, HUCMAMMALS,MAMMALS,HUCBIRD,BIRD,HUCAMPHIB,
     >  AMPHIB,HUCREPTILES, REPTILES, HUCVERTEB, VERTEB
      REAL PET ,APET ,PAET ,APAET ,
     &AET , RUNOFF , INTER , PRIBF , SECBF , INTF , 
     &AVUZTWC , AVUZFWC , AVLZTWC , AVLZFPC 
      
      REAL GEPM,RECOM,NEEM
C ----------------------------------------------------------------      


        WRITE (400, 500) 
500    FORMAT ('CELL, YEAR, MONTH, GEP(gC/m2/Month), Reco, NEE')


        WRITE (500, 600) 
600    FORMAT ('CELL, YEAR, GEP(gC/m2/yr), Reco, NEE(gC/m2/yr),', 
     >        ' AET(MM), PET(MM),GPP_V(gC/m2/yr),NPP_V(gC/m2/yr)')


        WRITE (600, 650) 
650    FORMAT ('CELL,    NO_YR,    GEP(gC/m2/yr),  Reco,  NEE')


        WRITE (700, 700)
       
700     FORMAT ('CELL, YEAR, TREE, MAMMALS, BIRD, ', 
     > 'AMPHIB, REPTILES, VERTEB, AET, PET')

            WRITE (800, 800) 
            
800      FORMAT ('CELL,NO_YR, TREE, MAMMALS, BIRD,',
     >  'AMPHIB, REPTILES, AHUCVERTEB') 
            
     
           
      DO 300 I=1, NGRID         
                
         HUCGEP =0.
         HUCNEE =0.
         HUCRE  = 0.

         IY=0
         HUCTRS = 0.
         HUCMAMMALS = 0.
         HUCBIRD =0.
         HUCAMPHIB=0.
         HUCREPTILES =0.
         HUCVERTEB=0.
                          
         DO 200 J=1, NYEAR
   
            IDY = J + BYEAR - 1 
                  
            IF (IDY .GE. IYSTART .AND. IDY .LE. IYEND) THEN        
         
            
            ANGEP = 0.
            ANRECO = 0.
            ANNEE = 0.
                    
            DO 100 M=1, 12
                 
       	 
               WRITE (400, 2000) HUCNO(I), IDY, M, GEPM(I,J, M), 
     >          RECOM(I,J,M), NEEM(I,J,M)         
     
2000          FORMAT (I10, ',',I6, ',', I6, ',', F10.2, ',',  
     >   F10.2,',',F10.2)
       
        
C--- ACCUMULATE ANNUAL GEP FORM MONTHLY VALUES (g C/m2/mon)

            ANGEP = ANGEP + GEPM(I,J,M)  
            ANRECO = ANRECO + RECOM(I,J,M) 
            ANNEE = ANNEE + NEEM(I,J,M)              
           
           
100         CONTINUE 
           
c --- convert annual GEP to NEE. ref. Law et al (2002) USING THE AVG OF FORESTS

C            ANNEE = ((285.0 - 0.44 * ANGEP) + (618.0 - 0.67*ANGEP))/2.


C --- CALCULATING Tree Species Richness (TSR) USING CURRIE (1987) NATURE
           
            TRS = 185.8/(1.0 + EXP (3.09-0.00432*HUCAET(I,J)))
           
C --- CALCULATING  (TSR) USING CURRIE (1991) for different
C--The American Society of Naturalists Energy and Large-Scale Patterns of Animal- and Plant-Species Richness 
C --Author(s): David J. Currie Source: The American Naturalist, Vol. 137, No. 1 (Jan., 1991), pp. 27-49 Published by: The University of Chicago Press for The American Society 

            MAMMALS = 1.12*(1.0 -EXP(-0.00348*HUCPET(I,J) )) + 0.653 
          
            MAMMALS = 10**(MAMMALS) - 1.
           
            IF (HUCPET(I,J) .LT. 525) THEN 
            
               BIRD = 1.4 + 0.00159 * HUCPET(I,J)
               
            ELSE 
            
               BIRD = 2.26 - 0.0000256 * HUCPET(I,J)
               
            ENDIF

            BIRD = 10**(BIRD) - 1.
           
            IF (HUCPET(I,J) .LE. 200) THEN 
            
               AMPHIB = 0.
               
            ELSE 
            
               AMPHIB = 3.07*(1.0-Exp(-0.00315*HUCPET(I,J) ))
            
            ENDIF
           
            AMPHIB = exp(AMPHIB) -1.

            IF (HUCPET(I,J) .LT. 400) THEN 
            
               REPTILES = 0.
               
            ELSE 
            
               REPTILES = 5.21*(1.0-Exp(-0.00249*HUCPET(I,J) )) - 3.347
           
            ENDIF
                                
           REPTILES = 10**(REPTILES) -1. 

           
           VERTEB = 1.49*(1.0-Exp(-0.00186*HUCPET(I,J) )) + 0.746
           
           VERTEB = 10**(VERTEB) -1.

C ---  write annual GEP and NEE   
    
               GEPA(I,J)=ANGEP
               NEEA(I,J)=ANNEE
              WRITE (500, 3000) HUCNO(I), IDY, ANGEP,ANRECO,
     >            ANNEE, HUCAET(I,J), HUCPET(I,J), GPP_V(I,J),NPP_V(I,J)
                                              
3000          FORMAT (I12, ',', I12, ',',F16.2, ',', F16.2, ',', F16.2
     >  , ',', F16.2, ',', F16.2, ',', F16.2,',', F16.2)
C ---- write annual biodiversity results
     
              WRITE (700, 3100) HUCNO(I), IDY, TRS,MAMMALS, BIRD, 
     >        AMPHIB, REPTILES, VERTEB, HUCAET(I,J), HUCPET(I,J)
                                              
3100          FORMAT (I12, ',', I12, ',', 
     >           F10.2, ',', F10.2, ',', F10.2, ',', F10.2, ',', F10.2,
     >           ',', F10.2, ',', F8.1, ',', F8.1)
           
    
              HUCGEP = HUCGEP + ANGEP
              
              HUCRE = HUCRE + ANRECO
              
              HUCNEE = HUCNEE + ANNEE
            
              HUCTRS = HUCTRS + TRS
              HUCMAMMALS = HUCMAMMALS + MAMMALS
              HUCBIRD = HUCBIRD + BIRD
              HUCAMPHIB = HUCAMPHIB + AMPHIB
              HUCREPTILES = HUCREPTILES + REPTILES
              HUCVERTEB = HUCVERTEB + VERTEB
                   
              IY = IY + 1
            
           ENDIF

         

200      CONTINUE

         AHUCGEP = HUCGEP/IY
         
         AHUCRE = HUCRE/IY
         
         AHUCNEE = HUCNEE/IY
            
         AHUCTRS = HUCTRS/IY
         AHUCMAMMALS = HUCMAMMALS/IY
         AHUCBIRD = HUCBIRD/IY
         AHUCAMPHIB = HUCAMPHIB/IY
         AHUCREPTILES = HUCREPTILES/IY
         AHUCVERTEB = HUCVERTEB/IY
            

            WRITE (600, 4000) HUCNO(I),IY, AHUCGEP, AHUCRE, AHUCNEE
                                              
4000        FORMAT (I12, ',', I12, ',', 
     >           F14.2, ',', F14.2, ',', F14.2)       



            WRITE (*, 4100) HUCNO(I),AHUCGEP,
     >                    AHUCRE, AHUCNEE
                                              
4100        FORMAT ('CELL=',I12, '  GEP(gC/m2/yr.)=', F10.0, 
     >      '  Reco=', F10.0,' NEE(gC/m2/yr.)=', F10.0)       


            WRITE (800, 4200) HUCNO(I),IY, AHUCTRS, AHUCMAMMALS,
     >        AHUCBIRD, AHUCAMPHIB, AHUCREPTILES, AHUCVERTEB 
                                              
4200        FORMAT (I12, ',', I12, ',', 
     >           F10.2, ',', F10.2, ',', F10.2, ',', F10.2, ',', F10.2,
     >           ',', F10.2)

 

300   CONTINUE


      RETURN
      END
