
C**********************************************************************C
C                                                                      C
C     *** SUBROUTINE VALIDATION ***                                 C
C     Simulate GEP AND NEE for selected HUC                            C
C     WRITE GEP AND NEE TO MONTHCARBON.TXT, ANNUALCARBON.TXT, HUCCARBON.TXT        C
C     SIMULATE BIODIVERSITY FOR SELECTED HUC                           C
C     WRITE BIODIVERSITY TO HUCBIO.TXT                                 C
C                                                                      C
C**********************************************************************C
C        I=HUC; J= YEAR, M =MONTH, MNDAY= NO OF DAYS IN A MONTH

      SUBROUTINE VALIDATION(TUN1,TUN2)
      
      COMMON/BASIC/NGRID, NYEAR, NLC,BYEAR,IYSTART,IYEND
      COMMON/VAL/VAL_1(100), VAL_2(100), VAL_3,VAL_4,VAL_5,VAL_6
      COMMON/HUC/ HUCAREA(4000)
      
      COMMON/CARBON/ GEPM(4000, 200, 12),RECOM(4000,200,12), 
     >  NEEM(4000,200,12),GEPA(1000,200),NEEA(1000,200)
      
      COMMON/VALID/ GEP_V(1000,200,12), ET_V(1000,200, 12)
     >,GPP_V(1000,200),NPP_V(1000,200), FLOW_V(200,12),FLOW(1,12)
      
      COMMON/OUTPUT1/ PET(200,12,20),APET(12),PAET(200,12,20),APAET(12),
     &AET(12), RUNOFF(12), INTER(12), PRIBF(12), SECBF(12), INTF(12), 
     &AVUZTWC(12), AVUZFWC(12), AVLZTWC(12), AVLZFPC(12)
     >,A_ET(7,1, 12),P_ET(7,1, 12),Sun_ET(7,1, 12)
      
      COMMON/HUCPETAET/HUCAET(4000,200), HUCPET(4000,200),
     > HUCPAET(4000,200)
      
      COMMON/CELLINFO/LADUSE(4000,20),HUCNO(4000),
     >                LATUDE(4000),LONGI(4000)
      COMMON/FLOW/ STRFLOW(10, 1, 12)   
C --------------------------------------------------------------
      INTEGER I,J, M,num_m,num_y,num_F

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
      REAL HUCAET, HUCPET,STRFLOW,FLOW,FLOW_L
      

      REAL PET ,APET ,PAET ,APAET ,
     &AET , RUNOFF , INTER , PRIBF , SECBF , INTF , 
     &AVUZTWC , AVUZFWC , AVLZTWC , AVLZFPC 
      
      REAL GEPM,RECOM,NEEM
C ----------------------------------------------------------------      


            WRITE (2000, 201) 
            
201      FORMAT ('cell,YEAR,Month,GEP,NEE,
     > GEP_V,AET, PET,Sun_PET,ET_V') 
            
             WRITE (2001, 202) 
            
202      FORMAT ('cell,year,GEP,NEE,
     > AET,PET, GPP_V,NPP_V')  
        
           
             WRITE (2002, 203) 
            
203      FORMAT ('year,month,flow,flow_V')
C     > NS_GEP,NS_AET,NS_PET,NS_GPP,NS_NPP,
C    > P_GEP,P_AET,P_PET,P_GPP,P_NPP')    
             
     
      
      FLOW_L=0  !FLOW ������ʱ�ļ�
      
      DO 1001 J=1,NYEAR
	  
        DO 1002 M=1,12
		FLOW_L=0  !FLOW ������ʱ�ļ�
            DO 1003 I=1,NGRID
            FLOW_L=FLOW_L+STRFLOW(I,J,M)
1003  CONTINUE 
            FLOW(J,M)=FLOW_L     
1002  CONTINUE
1001  CONTINUE
    
      
C----��ʼ����GEP��AET��PET��GPP��NPP��Ƥ��ɭ���ϵ����P)������ϵ����R����Ч��ϵ����NS��
               
        num_m=0
        num_y=0
                SUM_GEP=0
                SUM_GEP_V=0
                                
                SUM_GEP_X=0
                SUM_GEP_Y=0
                SUM_GEP_XY=0
                SUM_GEP_XX=0
                SUM_GEP_YY=0

                SUM_AET=0
                SUM_AET_V=0
                                
                SUM_AET_X=0
                SUM_AET_Y=0
                SUM_AET_XY=0
                SUM_AET_XX=0
                SUM_AET_YY=0
                
                SUM_PET=0
                SUM_PET_V=0
                                
                SUM_PET_X=0
                SUM_PET_Y=0
                SUM_PET_XY=0
                SUM_PET_XX=0
                SUM_PET_YY=0
               
                SUM_GPP=0
                SUM_NPP=0
                SUM_GPP_V=0
                SUM_NPP_V=0

                SUM_GPP_X=0
                SUM_GPP_Y=0
                SUM_GPP_XY=0
                SUM_GPP_XX=0
                SUM_GPP_YY=0

                SUM_NPP_X=0
                SUM_NPP_Y=0
                SUM_NPP_XY=0
                SUM_NPP_XX=0
                SUM_NPP_YY=0

                  SUM_GEP_U=0
                  SUM_GEP_L=0
                  SUM_GEP_R=0
                  SUM_GEP_N=0

                  SUM_AET_L=0
                  SUM_PET_L=0

                  SUM_AET_U=0
                  SUM_PET_U=0
      
                  SUM_AET_R=0
                  SUM_PET_R=0

                  SUM_AET_N=0
                  SUM_PET_N=0

      SUM_GPP_U=0
      SUM_NPP_U=0
                
      SUM_GPP_L=0
      SUM_NPP_L=0  
      
      SUM_GPP_R=0
      SUM_NPP_R=0     
                

      SUM_GPP_N=0
      SUM_NPP_N=0  

                SUM_FLOW=0
                SUM_FLOW_V=0
                                
                SUM_FLOW_X=0
                SUM_FLOW_Y=0
                SUM_FLOW_XY=0
                SUM_FLOW_XX=0
                SUM_FLOW_YY=0

      SUM_FLOW_U=0
      SUM_FLOW_L=0
      SUM_FLOW_R=0
      SUM_FLOW_N=0

        DO 50001 I=1,NGRID
            DO 50002 J=1,NYEAR
                DO 50003 M=1,12
C	����GEP������֤����
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
C	����ET������֤����
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
C	����NEE������֤����
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

C---------����FLOW��Ƥ��ɭ���ϵ����P)������ϵ����R����Ч��ϵ����NS��
        num_F=0
            DO 70002 J=1,NYEAR
                DO 70003 M=1,12

                num_F=num_F+1
                SUM_FLOW=SUM_FLOW+FLOW(J,M)
                SUM_FLOW_V=SUM_FLOW_V+FLOW_V(J,M)
                                
                SUM_FLOW_X=SUM_FLOW_X+FLOW(J,M)
                SUM_FLOW_Y=SUM_FLOW_Y+FLOW_V(J,M)
                SUM_FLOW_XY=SUM_FLOW_XY+FLOW(J,M)*FLOW_V(J,M)
                SUM_FLOW_XX=SUM_FLOW_XX+FLOW(J,M)**2
                SUM_FLOW_YY=SUM_FLOW_YY+FLOW_V(J,M)**2
       

70003   Continue
70002   Continue

            AVE_FLOW=SUM_FLOW/num_F
            AVE_FLOW_V=SUM_FLOW_V/num_F


            DO 80002 J=1,NYEAR
                DO 80003 M=1,12

      SUM_FLOW_U=SUM_FLOW_U+(FLOW(J,M)-AVE_FLOW)*
     >(FLOW_V(J,M)-AVE_FLOW_V)
      SUM_FLOW_L=SUM_FLOW_L+(FLOW_V(J,M)-AVE_FLOW_V)**2
      SUM_FLOW_R=SUM_FLOW_R+(FLOW(J,M)-AVE_FLOW)**2
      SUM_FLOW_N=SUM_FLOW_N+(FLOW(J,M)-FLOW_V(J,M))**2
                   
80003   Continue
80002   Continue


        R_FLOW=(SUM_FLOW_U/(SUM_FLOW_L*(SUM_FLOW_R**0.5)))**2

        NS_FLOW=1-(SUM_FLOW_N/SUM_FLOW_L)


             P_FLOW=(num_F*SUM_FLOW_XY-SUM_FLOW_X*SUM_FLOW_Y)/((num_F*
     >SUM_FLOW_XX-SUM_FLOW_X**2)*(num_F*SUM_FLOW_YY-SUM_FLOW_Y**2))**0.5



C-----�����֤���ݵ������ļ�������֤DATA_V_A.TXT������֤DATA_V_M.TXT��
      
      DO 7001 I=1,NGRID
        DO 7002 J=1,NYEAR
            DO 7003 M=1,12
        WRITE(2000,20000) HUCNO(I),J+BYEAR-1,M,GEPM(I,J,M),NEEM(I,J,M),
     > GEP_V(I,J,M),A_ET(I,J,M), P_ET(I,J,M),Sun_ET(I,J, M),ET_V(I,J,M)

        WRITE(*,20001) HUCNO(I),J+BYEAR-1,M,GEPM(I,J,M),NEEM(I,J,M),
     > GEP_V(I,J,M),A_ET(I,J,M), P_ET(I,J,M),Sun_ET(I,J, M),ET_V(I,J,M)

20000        FORMAT (I5, ',', I7, ',', 
     >        I5, ',', F12.2, ',', F12.2, ',', F12.2, ',',F12.2, ','
     >        ,F12.2, ',', F12.2, ',', F12.2)
C     , ',', E15.6, ',', E15.6, ',', E15.6,
C    >           ',', E15.6, ',', E15.6,
C    >           ',', E15.6)
      
7003  CONTINUE

      WRITE(2001,20001) HUCNO(I),J+BYEAR-1,GEPA(I,J),NEEA(i,j),
     > HUCAET(I,J), HUCPET(I,J), GPP_V(I,J),NPP_V(I,J)

      WRITE(*,20001) HUCNO(I),J+BYEAR-1,GEPA(I,J),NEEA(i,j),
     > HUCAET(I,J), HUCPET(I,J), GPP_V(I,J),NPP_V(I,J)

20001          FORMAT (I12, ',', I12, ',',F16.2, ',', 
     >           F16.2, ',', F16.2, ',', F16.2,',', F16.2,
     >         ',', F16.2)

7002  CONTINUE
7001  CONTINUE  

C----------������֤����DATA_V_F.TXT
        DO 8002 J=1,NYEAR
            DO 8003 M=1,12
        WRITE(2002,20002) J+BYEAR-1,M,FLOW(J,M),FLOW_V(J,M)

        WRITE(*,20002) J+BYEAR-1,M,FLOW(J,M),FLOW_V(J,M)

20002        FORMAT ( I7, ',', I5,',',
C     >        I5, ',', F12.2, ',', F12.2, ',', F12.2, ',',F12.2, ','
     >        F12.2, ',', F12.2)
C     , ',', E15.6, ',', E15.6, ',', E15.6,
C    >           ',', E15.6, ',', E15.6,
C    >           ',', E15.6)
      
8003  CONTINUE
8002  CONTINUE

      P_GEP=P_GEP**2
      P_AET=P_AET**2
      P_PET=P_PET**2
      P_GPP=P_GPP**2
      P_NPP=P_NPP**2
      P_FLOW=P_FLOW**2


C-------�����֤�����VALIDATION.TXT

        WRITE(2003,20003)VAL_1(TUN1),VAL_2(TUN2), R_GEP,R_AET,
     >R_PET,R_GPP,R_NPP,R_FLOW,
     > NS_GEP,NS_AET,NS_PET,NS_GPP,NS_NPP,NS_FLOW,
     > P_GEP,P_AET,P_PET,P_GPP,P_NPP,P_FLOW

      WRITE(*,20003) VAL_1(TUN1),VAL_2(TUN2),R_GEP,R_AET,
     >R_PET,R_GPP,R_NPP,
     > NS_GEP,NS_AET,NS_PET,NS_GPP,NS_NPP,NS_FLOW,
     > P_GEP,P_AET,P_PET,P_GPP,P_NPP,P_FLOW

20003       FORMAT (f12.3,',',f12.3,',',E15.6, ',', E15.6, ',', 
     >        E15.6, ',', E15.6, ',', E15.6, ',', E15.6, ',', E15.6, ','
     >        ,E15.6, ',', E15.6, ',', E15.6, ',', E15.6, ',', E15.6,
     >        ',', E15.6, ',', E15.6,',', E15.6, ',', E15.6, ',', E15.6,
     >           ',', E15.6)    
      print*,"����س�������һ������"
C     READ(*,*) 
     
      RETURN
      END
