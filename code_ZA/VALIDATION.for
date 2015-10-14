
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

      SUBROUTINE VALIDATION(TUN1)
      
      COMMON/BASIC/NGRID, NYEAR, NLC,BYEAR,IYSTART,IYEND
      COMMON/VAL/VAL_1(100), VAL_2(100), VAL_3,VAL_4,VAL_5,VAL_6
      COMMON/HUC/ HUCAREA(1000)
         
      COMMON/VALID/ RUNOFF_V(100,400),RUN_OFF(100,400) 
      
      COMMON/OUTPUT1/ PET(200,12,20),APET(12),PAET(200,12,20),APAET(12),
     &AET(366), RUNOFF(366), INTER(366), PRIBF(366), SECBF(366), 
     &INTF(366),AVUZTWC(366), AVUZFWC(366), AVLZTWC(366), AVLZFPC(366)
     >,A_ET(1000,200, 12),P_ET(1000,200, 12),Sun_ET(1000,200, 12)
     >,RUN_HRU(100,100,366),BASE_HRU(1000,200, 12)

      
      COMMON/CELLINFO/LADUSE(1000,20),HUCNO(1000),
     >                LATUDE(1000),LONGI(1000)
      COMMON/FLOW/ STRFLOW(1000, 200, 12)   
      COMMON/BYLAND/ RUNLAND(600,100,366) 
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
                   
      double precision HUCAREA
      
      REAL ANGEP, ANRECO, ANNEE,  AHUCGEP, AHUCNEE,AHUCRE
      REAL HUCGEP, HUCNEE, HUCRE
      REAL ET_V, GEP_V,GPP_V,NPP_V,FLOW_V
      REAL HUCAET, HUCPET,STRFLOW,FLOW,FLOW_L

      REAL R_RUNOFF,R_FLOW,R_BASEFLOW
      REAL P_RUNOFF,P_FLOW,P_BASEFLOW
      REAL NS_RUNOFF,NS_FLOW,NS_BASEFLOW

      REAL PET ,APET ,PAET ,APAET ,
     &AET , RUNOFF , INTER , PRIBF , SECBF , INTF , 
     &AVUZTWC , AVUZFWC , AVLZTWC , AVLZFPC 
      
      REAL GEPM,RECOM,NEEM
       INTEGER YEAR,NDAY,ICOUNT
C ----------------------------------------------------------------      

          
            WRITE (2002, 203) 
            
203      FORMAT ('year,DAY,RUNOFF,RUNOFF_V')

             
     
      
      FLOW_L=0  !FLOW 总量临时文件
      RUNOFF_L=0

      UNIONAREA=0


      ICOUNT=0


      DO 1001 J=1,NYEAR

            YEAR = BYEAR + ICOUNT
			ICOUNT=ICOUNT+1 
            NDAY = 365
            IF(YEAR/4*4.NE.YEAR) GO TO 610
C            IF(YEAR/400*400.EQ.YEAR .and. YEAR= 2000) GO TO 110
            NDAY=366   
610         CONTINUE 
	  
        DO 1002 M=1,NDAY
      RUNOFF_L=0

      UNIONAREA=0
            DO 1003 I=1,NGRID
            RUNOFF_L=RUNOFF_L+RUNLAND(I,J,M)*HUCAREA(I)/100000

            UNIONAREA=UNIONAREA+HUCAREA(I)
1003  CONTINUE 
            RUN_OFF(J,M)=RUNOFF_L/UNIONAREA*100000

1002  CONTINUE
1001  CONTINUE
  
      



                SUM_RUNOFF=0
                SUM_RUNOFF_V=0
                                
                SUM_RUNOFF_X=0
                SUM_RUNOFF_Y=0
                SUM_RUNOFF_XY=0
                SUM_RUNOFF_XX=0
                SUM_RUNOFF_YY=0

      SUM_RUNOFF_U=0
      SUM_RUNOFF_L=0
      SUM_RUNOFF_R=0
      SUM_RUNOFF_N=0




C---------计算RUNOFF的皮尔森相关系数（P)，决定系数（R），效率系数（NS）

C    这里的“J=1,15 ” 代表验证的年数
        num_F=0
        ICOUNT=0
            DO 11002 J=10,18
             YEAR = 1988 + ICOUNT
			ICOUNT=ICOUNT+1 
            NDAY = 365
            IF(YEAR/4*4.NE.YEAR) GO TO 6010
C            IF(YEAR/400*400.EQ.YEAR .and. YEAR= 2000) GO TO 110
            NDAY=366   

6010         CONTINUE 

                DO 11003 M=1,NDAY

                num_F=num_F+1
                SUM_RUNOFF=SUM_RUNOFF+RUN_OFF(J,M)
                SUM_RUNOFF_V=SUM_RUNOFF_V+RUNOFF_V(J,M)
                                
                SUM_RUNOFF_X=SUM_RUNOFF_X+RUN_OFF(J,M)
                SUM_RUNOFF_Y=SUM_RUNOFF_Y+RUNOFF_V(J,M)
                SUM_RUNOFF_XY=SUM_RUNOFF_XY+RUN_OFF(J,M)*RUNOFF_V(J,M)
                SUM_RUNOFF_XX=SUM_RUNOFF_XX+RUN_OFF(J,M)**2
                SUM_RUNOFF_YY=SUM_RUNOFF_YY+RUNOFF_V(J,M)**2
       

11003   Continue
11002   Continue

            AVE_RUNOFF=SUM_RUNOFF/num_F
            AVE_RUNOFF_V=SUM_RUNOFF_V/num_F

        ICOUNT=0
            DO 12002 J=10,18

             YEAR = 1988 + ICOUNT
			ICOUNT=ICOUNT+1 
            NDAY = 365
            IF(YEAR/4*4 .NE. YEAR) GO TO 110
C            IF(YEAR/400*400.EQ.YEAR .and. YEAR= 2000) GO TO 110
            NDAY=366   

110         CONTINUE 


                DO 12003 M=1,NDAY

      SUM_RUNOFF_U=SUM_RUNOFF_U+(RUN_OFF(J,M)-AVE_RUNOFF)*
     >(RUNOFF_V(J,M)-AVE_RUNOFF_V)
      SUM_RUNOFF_L=SUM_RUNOFF_L+(RUNOFF_V(J,M)-AVE_RUNOFF_V)**2
      SUM_RUNOFF_R=SUM_RUNOFF_R+(RUN_OFF(J,M)-AVE_RUNOFF)**2
      SUM_RUNOFF_N=SUM_RUNOFF_N+(RUN_OFF(J,M)-RUNOFF_V(J,M))**2
                   
12003   Continue
12002   Continue


!        R_RUNOFF=(SUM_RUNOFF_U/(SUM_RUNOFF_L*(SUM_RUNOFF_R**0.5)))**2
        R_RUNOFF=(SUM_RUNOFF_N/num_F)**0.5
        NS_RUNOFF=1-(SUM_RUNOFF_N/SUM_RUNOFF_L)


             P_RUNOFF=(num_F*SUM_RUNOFF_XY-SUM_RUNOFF_X*SUM_RUNOFF_Y)/
     >((num_F*SUM_RUNOFF_XX
     >-SUM_RUNOFF_X**2)*(num_F*SUM_RUNOFF_YY-SUM_RUNOFF_Y**2))**0.5


     
C-----输出验证数据到各个文件，年验证DATA_V_A.TXT，月验证DATA_V_M.TXT，
      

C----------径流验证数据DATA_V_F.TXT
        ICOUNT=0
        DO 8002 J=10,18
                     YEAR = 1988 + ICOUNT
			ICOUNT=ICOUNT+1 
            NDAY = 365
            IF(YEAR/4*4.NE.YEAR) GO TO 120
C            IF(YEAR/400*400.EQ.YEAR .and. YEAR= 2000) GO TO 110
            NDAY=366   

120         CONTINUE 

            DO 8003 M=1,NDAY
        WRITE(2002,20002) J+BYEAR-1,M,RUN_OFF(J,M),RUNOFF_V(J,M)


C        WRITE(*,20002) J+BYEAR-1,M,RUN_OFF(J,M),RUNOFF_V(J,M)


20002        FORMAT ( I7, ',', I5,',',

     >        F12.2, ',', F12.2)

      
8003  CONTINUE
8002  CONTINUE


      P_RUNOFF=P_RUNOFF**2



C-------输出验证结果到VALIDATION.TXT


        WRITE(2003,20003)VAL_1(TUN1), R_RUNOFF,
     > NS_RUNOFF,P_RUNOFF

        WRITE(*,20003)VAL_1(TUN1), R_RUNOFF,
     > NS_RUNOFF,P_RUNOFF

20003       FORMAT (f12.3,',',f10.3, ',', f10.3, ',', 
     >        f10.3)         
C     print*,"输入回车进入下一步操作"
C     READ(*,*) 
     
      RETURN
      END
