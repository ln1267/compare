
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

      COMMON/HUC/ HUCAREA(4000)
      
      COMMON/CARBON/ GEPM(4000, 200, 12),RECOM(4000,200,12), 
     >  NEEM(4000,200,12),GEPA(1000,200),NEEA(1000,200)
      
      COMMON/CELLINFO/LADUSE(4000,20),HUCNO(4000),
     >                LATUDE(4000),LONGI(4000)
       
      COMMON/VALID/ GEP_V(1000,200,12), ET_V(1000,200, 12)
     >,GPP_V(1000,200),NPP_V(1000,200), FLOW_V(200,12) 
      
      COMMON/OUTPUT1/ PET(200,12,20),APET(12),PAET(200,12,20),APAET(12),
     &AET(12), RUNOFF(12), INTER(12), PRIBF(12), SECBF(12), INTF(12), 
     &AVUZTWC(12), AVUZFWC(12), AVLZTWC(12), AVLZFPC(12)
     >,A_ET(1000,200, 12),P_ET(1000,200, 12)
      
C---BIODIVERSITY CALCULATION USING ANNUAL AET, PET      
      
      COMMON/HUCPETAET/HUCAET(4000,200), HUCPET(4000,200)  
          
C --------------------------------------------------------------
      INTEGER I,J, M,num_m,num_y
      REAL SUM_GEP,SUM_GEP_V,SUM_AET,SUM_PET,SUM_GPP,SUM_NPP,SUM_ET_V
      REAL AVE_GEP,AVE_GEP_V,AVE_AET,AVE_PET,AVE_GPP,AVE_NPP,AVE_ET_V
      real R_GEP,R_AET,R_PET,R_GPP,R_NPP,P_GEP,P_AET,P_PET,P_GPP,P_NPP,
     > NS_GEP,NS_AET,NS_PET,NS_GPP,NS_NPP
      real  SUM_GEP_U,SUM_AET_U,SUM_PET_U,SUM_GPP_U,SUM_NPP_U
      real  SUM_GEP_L,SUM_AET_L,SUM_PET_L,SUM_GPP_L,SUM_NPP_L
      real  SUM_GEP_R,SUM_AET_R,SUM_PET_R,SUM_GPP_R,SUM_NPP_R
      real  SUM_GEP_N,SUM_AET_N,SUM_PET_N,SUM_GPP_N,SUM_NPP_N
      real  SUM_GEP_X,SUM_GEP_Y,SUM_GEP_XY,SUM_GEP_XX,SUM_GEP_YY
      real  SUM_AET_X,SUM_AET_Y,SUM_AET_XY,SUM_AET_XX,SUM_AET_YY
      real  SUM_PET_X,SUM_PET_Y,SUM_PET_XY,SUM_PET_XX,SUM_PET_YY
      real  SUM_GPP_X,SUM_GPP_Y,SUM_GPP_XY,SUM_GPP_XX,SUM_GPP_YY
      real  SUM_NPP_X,SUM_NPP_Y,SUM_NPP_XY,SUM_NPP_XX,SUM_NPP_YY
      
C ---------------------------------------------------------------      
                     
      INTEGER HUCNO, BYEAR
                   
      REAL HUCAREA
      
      REAL ANGEP, ANRECO, ANNEE,  AHUCGEP, AHUCNEE,AHUCRE
      REAL HUCGEP, HUCNEE, HUCRE
      REAL ET_V, GEP_V,GPP_V,NPP_V,FLOW_V
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
     >        ' AET(MM), PET(MM)')


        WRITE (600, 650) 
650    FORMAT ('CELL,    NO_YR,    GEP(gC/m2/yr),  Reco,  NEE')


        WRITE (700, 700)
       
700     FORMAT ('CELL, YEAR, TREE, MAMMALS, BIRD, ', 
     > 'AMPHIB, REPTILES, VERTEB, AET, PET')

            WRITE (800, 800) 
            
800      FORMAT ('CELL,NO_YR, TREE, MAMMALS, BIRD,',
     >  'AMPHIB, REPTILES, AHUCVERTEB') 
            
            WRITE (2000, 20001) 
            
20001      FORMAT ('R_GEP,R_AET,R_PET,R_GPP,R_NPP,
     > NS_GEP,NS_AET,NS_PET,NS_GPP,NS_NPP,
     > P_GEP,P_AET,P_PET,P_GPP,P_NPP') 
     
           
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
     >                    ANNEE, HUCAET(I,J), HUCPET(I,J)
                                              
3000          FORMAT (I12, ',', I12, ',',F16.2, ',', 
     >           F16.2, ',', F16.2, ',', F16.2,',', F16.2)
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


        num_m=0
        num_y=0
        DO 50001 I=1,NGRID
            DO 50002 J=1,NYEAR
                DO 50003 M=1,12
        IF (GEP_V(I,J,M) .LT. 0 .OR. NEEM(I,J,M) .LT. -400) THEN
            ELSE
                num_m=num_m+1
                SUM_GEP=SUM_GEP+GEPM(I,J,M)
                SUM_GEP_V=SUM_GEP_V+GEP_V(I,J,M)
                                
                SUM_GEP_X=SUM_GEP_X+GEPM(I,J,M)
                SUM_GEP_Y=SUM_GEP_Y+GEP_V(I,J,M)
                SUM_GEP_XY=SUM_GEP_XY+GEPM(I,J,M)*GEP_V(I,J,M)
                SUM_GEP_XX=SUM_GEP_XX+GEPM(I,J,M)**2
                SUM_GEP_YY=SUM_GEP_YY+GEP_V(I,J,M)**2
        ENDIF
        IF (A_ET(I,J,M) .LT. 0 .OR. ET_V(I,J,M) .LT. 0) THEN
            ELSE
                SUM_AET=SUM_AET+A_ET(I,J,M)
                SUM_PET=SUM_PET+P_ET(I,J,M)
                SUM_ET_V=SUM_ET_V+ET_V(I,J,M)
                SUM_AET_X=SUM_AET_X+A_ET(I,J,M)
                SUM_AET_Y=SUM_AET_Y+ET_V(I,J,M)
                SUM_AET_XY=SUM_AET_XY+A_ET(I,J,M)*ET_V(I,J,M)
                SUM_AET_XX=SUM_AET_XX+A_ET(I,J,M)**2
                SUM_AET_YY=SUM_AET_YY+ET_V(I,J,M)**2

                SUM_PET_X=SUM_PET_X+P_ET(I,J,M)
                SUM_PET_Y=SUM_PET_Y+ET_V(I,J,M)
                SUM_PET_XY=SUM_PET_XY+P_ET(I,J,M)*ET_V(I,J,M)
                SUM_PET_XX=SUM_PET_XX+P_ET(I,J,M)**2
                SUM_PET_YY=SUM_PET_YY+ET_V(I,J,M)**2
        ENDIF

50003   Continue

        IF (NEEA(I,J) .LT. -1200) THEN
            ELSE
                num_y=num_y+1
                SUM_GPP=SUM_GPP+GEPA(I,J)
                SUM_NPP=SUM_NPP+NEEA(I,J)
                SUM_GPP_V=SUM_GPP_V+GPP_V(I,J)
                SUM_NPP_V=SUM_NPP_V+NPP_V(I,J)

                SUM_GPP_X=SUM_GPP_X+GEPA(I,J)
                SUM_GPP_Y=SUM_GPP_Y+GPP_V(I,J)
                SUM_GPP_XY=SUM_GPP_XY+GEPA(I,J)*GPP_V(I,J)
                SUM_GPP_XX=SUM_GPP_XX+GEPA(I,J)**2
                SUM_GPP_YY=SUM_GPP_YY+GPP_V(I,J)**2

                SUM_NPP_X=SUM_NPP_X+NEEA(I,J)
                SUM_NPP_Y=SUM_NPP_Y+NPP_V(I,J)
                SUM_NPP_XY=SUM_NPP_XY+NEEA(I,J)*NPP_V(I,J)
                SUM_NPP_XX=SUM_NPP_XX+NEEA(I,J)**2
                SUM_NPP_YY=SUM_NPP_YY+NPP_V(I,J)**2
         ENDIF
50002   Continue
50001   Continue
            AVE_GEP=SUM_GEP/num_m
            AVE_GEP_V=SUM_GEP_V/num_m
            AVE_ET_V=SUM_ET_V/num_m
            AVE_AET=SUM_AET/num_m
            AVE_PET=SUM_PET/num_m
            AVE_GPP=SUM_GPP/num_y
            AVE_GPP_V=SUM_GPP_V/num_y
            AVE_NPP=SUM_NPP/num_y
            AVE_NPP_V=SUM_NPP_V/num_y

          DO 60001 I=1,NGRID
            DO 60002 J=1,NYEAR
                DO 60003 M=1,12
        IF (GEP_V(I,J,M) .LT. 0 .OR. NEEM(I,J,M) .LT. -400) THEN
            ELSE
      SUM_GEP_U=SUM_GEP_U+(GEPM(I,J,M)-AVE_GEP)*(GEP_V(I,J,M)-AVE_GEP_V)
      SUM_GEP_L=SUM_GEP_L+(GEP_V(I,J,M)-AVE_GEP_V)**2
      SUM_GEP_R=SUM_GEP_R+(GEPM(I,J,M)-AVE_GEP)**2
      SUM_GEP_N=SUM_GEP_N+(GEPM(I,J,M)-GEP_V(I,J,M))**2
        ENDIF
        IF (A_ET(I,J,M) .LT. 0 .OR. ET_V(I,J,M) .LT. 0) THEN
            ELSE
      SUM_AET_L=SUM_AET_L+(ET_V(I,J,M)-AVE_ET_V)**2
      SUM_PET_L=SUM_PET_L+(ET_V(I,J,M)-AVE_ET_V)**2

      SUM_AET_U=SUM_AET_U+(A_ET(I,J,M)-AVE_AET)*(ET_V(I,J,M)-AVE_ET_V)
      SUM_PET_U=SUM_PET_U+(P_ET(I,J,M)-AVE_PET)*(ET_V(I,J,M)-AVE_ET_V)
      
      SUM_AET_R=SUM_AET_R+(A_ET(I,J,M)-AVE_AET)**2
      SUM_PET_R=SUM_PET_R+(P_ET(I,J,M)-AVE_PET)**2

      SUM_AET_N=SUM_AET_N+(A_ET(I,J,M)-ET_V(I,J,M))**2
      SUM_PET_N=SUM_PET_N+(P_ET(I,J,M)-ET_V(I,J,M))**2
      ENDIF   
             
60003   Continue
        IF (NEEA(I,J) .LT. -1200) THEN
            ELSE
      SUM_GPP_U=SUM_GPP_U+(GEPA(I,J)-AVE_GPP)*(GPP_V(I,J)-AVE_GPP_V)
      SUM_NPP_U=SUM_NPP_U+(NEEA(I,J)-AVE_NPP)*(NPP_V(I,J)-AVE_NPP_V)
                
      SUM_GPP_L=SUM_GPP_L+(GPP_V(I,J)-AVE_GPP_V)**2
      SUM_NPP_L=SUM_NPP_L+(NPP_V(I,J)-AVE_NPP_V)**2   
      
      SUM_GPP_R=SUM_GPP_R+(GEPA(I,J)-AVE_GPP)**2
      SUM_NPP_R=SUM_NPP_R+(NEEA(I,J)-AVE_NPP)**2     
                

      SUM_GPP_N=SUM_GPP_N+(GEPA(I,J)-GPP_V(I,J))**2
      SUM_NPP_N=SUM_NPP_N+(NEEA(I,J)-NPP_V(I,J))**2  
       ENDIF
60002   Continue
60001   Continue

        R_GEP=(SUM_GEP_U/(SUM_GEP_L*(SUM_GEP_R**0.5)))**2
        R_AET=(SUM_AET_U/(SUM_AET_L*SUM_AET_R**0.5))**2
        R_PET=(SUM_PET_U/(SUM_PET_L*SUM_PET_R**0.5))**2
        R_GPP=(SUM_GPP_U/(SUM_GPP_L*SUM_GPP_R**0.5))**2
        R_NPP=(SUM_NPP_U/(SUM_NPP_L*SUM_NPP_R**0.5))**2


        NS_GEP=1-(SUM_GEP_N/SUM_GEP_L)
        NS_AET=1-(SUM_AET_N/SUM_AET_L)
        NS_PET=1-(SUM_PET_N/SUM_PET_L)
        NS_GPP=1-(SUM_GEP_N/SUM_GPP_L)
        NS_NPP=1-(SUM_GEP_N/SUM_NPP_L)

             P_GEP=(num_m*SUM_GEP_XY-SUM_GEP_X*SUM_GEP_Y)/((num_m*
     >  SUM_GEP_XX-SUM_GEP_X**2)*(num_m*SUM_GEP_YY-SUM_GEP_Y**2))**0.5
             P_AET=(num_m*SUM_AET_XY-SUM_AET_X*SUM_AET_Y)/((num_m*
     >  SUM_AET_XX-SUM_AET_X**2)*(num_m*SUM_AET_YY-SUM_AET_Y**2))**0.5
             P_PET=(num_m*SUM_PET_XY-SUM_PET_X*SUM_PET_Y)/((num_m*
     >  SUM_PET_XX-SUM_PET_X**2)*(num_m*SUM_PET_YY-SUM_PET_Y**2))**0.5
             P_GPP=(num_Y*SUM_GPP_XY-SUM_GPP_X*SUM_GPP_Y)/((num_Y*
     >  SUM_GPP_XX-SUM_GPP_X**2)*(num_Y*SUM_GPP_YY-SUM_GPP_Y**2))**0.5
             P_NPP=(num_Y*SUM_NPP_XY-SUM_NPP_X*SUM_NPP_Y)/((num_Y*
     >  SUM_NPP_XX-SUM_NPP_X**2)*(num_Y*SUM_NPP_YY-SUM_NPP_Y**2))**0.5

        WRITE(2000,20002) R_GEP,R_AET,R_PET,R_GPP,R_NPP,
     > NS_GEP,NS_AET,NS_PET,NS_GPP,NS_NPP,
     > P_GEP,P_AET,P_PET,P_GPP,P_NPP

      WRITE(*,20002) R_GEP,R_AET,R_PET,R_GPP,R_NPP,
     > NS_GEP,NS_AET,NS_PET,NS_GPP,NS_NPP,
     > P_GEP,P_AET,P_PET,P_GPP,P_NPP

20002        FORMAT (E15.6, ',', E15.6, ',', 
     >        E15.6, ',', E15.6, ',', E15.6, ',', E15.6, ',', E15.6, ','
     >        ,E15.6, ',', E15.6, ',', E15.6, ',', E15.6, ',', E15.6,
     >           ',', E15.6, ',', E15.6,
     >           ',', E15.6)

      RETURN
      END
