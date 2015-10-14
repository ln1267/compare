
!C**********************************************************************C
!C                                                                      C
!C     *** SUBROUTINE VALIDATION ***                                 C
!C     Simulate GEP AND NEE for selected HUC                            C
!C     WRITE GEP AND NEE TO MONTHCARBON.TXT, ANNUALCARBON.TXT, HUCCARBON.TXT        C
!C     SIMULATE BIODIVERSITY FOR SELECTED HUC                           C
!C     WRITE BIODIVERSITY TO HUCBIO.TXT                                 C
!C                                                                      C
!C**********************************************************************C
!C        I=HUC; J= YEAR, M =MONTH, MNDAY= NO OF DAYS IN A MONTH

      SUBROUTINE VALIDATION(TUN1,TUN2)
      

      use common_var
      implicit none 
! --------------------------------------------------------------
      CHARACTER DUMY
      INTEGER I,ID,J, M,num_m,num_y,num_F,TUN1,TUN2
      INTEGER Valid_year ! start YEAR For validation
      REAL SUM_GEP,SUM_GEP_V,SUM_AET,SUM_PET,SUM_GPP,SUM_NPP,SUM_ET_V
      REAL AVE_GEP,AVE_GEP_V,AVE_AET,AVE_PET,AVE_GPP,AVE_NPP,AVE_ET_V
      real R_GEP,R_AET,R_PET,R_GPP,R_NPP,P_GEP,P_AET,P_PET,P_GPP,P_NPP,&
      NS_GEP,NS_AET,NS_PET,NS_GPP,NS_NPP
      real  SUM_GEP_U,SUM_AET_U,SUM_PET_U,SUM_GPP_U,SUM_NPP_U
      real  SUM_GEP_L,SUM_AET_L,SUM_PET_L,SUM_GPP_L,SUM_NPP_L
      real  SUM_GEP_R,SUM_AET_R,SUM_PET_R,SUM_GPP_R,SUM_NPP_R
      real  SUM_GEP_N,SUM_AET_N,SUM_PET_N,SUM_GPP_N,SUM_NPP_N
      real  SUM_GEP_X,SUM_GEP_Y,SUM_GEP_XY,SUM_GEP_XX,SUM_GEP_YY
      real  SUM_AET_X,SUM_AET_Y,SUM_AET_XY,SUM_AET_XX,SUM_AET_YY
      real  SUM_PET_X,SUM_PET_Y,SUM_PET_XY,SUM_PET_XX,SUM_PET_YY
      real  SUM_GPP_X,SUM_GPP_Y,SUM_GPP_XY,SUM_GPP_XX,SUM_GPP_YY
      real  SUM_NPP_X,SUM_NPP_Y,SUM_NPP_XY,SUM_NPP_XX,SUM_NPP_YY
      
! ---------------------------------------------------------------      
                     
     
      REAL ANGEP, ANRECO, ANNEE,  AHUCGEP, AHUCNEE,AHUCRE
      REAL HUCGEP, HUCNEE, HUCRE

      REAL FLOW_L,RUNOFF_L,BASEFLOW_L, UNIONAREA

      REAL R_RUNOFF,R_FLOW,R_BASEFLOW
      REAL P_RUNOFF,P_FLOW,P_BASEFLOW
      REAL NS_RUNOFF,NS_FLOW,NS_BASEFLOW


      REAL SUM_RUNOFF,SUM_RUNOFF_V,SUM_RUNOFF_X,SUM_RUNOFF_Y,&
           SUM_RUNOFF_XY,SUM_RUNOFF_XX,SUM_RUNOFF_YY,    &
           SUM_RUNOFF_U,SUM_RUNOFF_L,SUM_RUNOFF_R,SUM_RUNOFF_N,&
           SUM_BASEFLOW,SUM_BASEFLOW_V,SUM_BASEFLOW_X,SUM_BASEFLOW_Y,&
           SUM_BASEFLOW_XY,SUM_BASEFLOW_XX,SUM_BASEFLOW_YY,&
           SUM_BASEFLOW_U,SUM_BASEFLOW_L,SUM_BASEFLOW_R,SUM_BASEFLOW_N,&
           AVE_RUNOFF,AVE_RUNOFF_V,AVE_BASEFLOW,AVE_BASEFLOW_V
      

!----------------------------------------------------------------      


!            WRITE (2000, 201) 
!            
!201      FORMAT ('cell,YEAR,Month,GEP,NEE,
!     > GEP_V,AET, PET,S_PET,ET_V') 
!            
!             WRITE (2001, 202) 
!            
!202      FORMAT ('cell,year,GEP,NEE,
!     > AET,PET, GPP_V,NPP_V')  
!        
!           
            WRITE (2002, 203) 
            
203      FORMAT ('year,month,RUNOFF,RUNOFF_V,BASEFLOW,BASEFLOW_V')
!C     > NS_GEP,NS_AET,NS_PET,NS_GPP,NS_NPP,
!C    > P_GEP,P_AET,P_PET,P_GPP,P_NPP')    
             
     
      Valid_year=1952
      FLOW_L=0  !FLOW 总量临时文件
      RUNOFF_L=0
      BASEFLOW_L=0
      UNIONAREA=0


!---Read runoff validation data from valid.txt
READ (22,5001) DUMY
5001   FORMAT (1000A30)
         
      DO  J=1982-IYSTART+1, 2006-IYSTART+1
        
        DO  M=1,12
      READ(22,*) ID,ID, RUNOFF_V(J,M), FLOW_V(J,M),BASEFLOW_V(J,M)  
      print*,J,M,RUNOFF_V(J,M), FLOW_V(J,M),BASEFLOW_V(J,M)
        END DO
        END DO

      DO 1001 J=1,NYEAR
	  
        DO 1002 M=1,12
      RUNOFF_L=0
      BASEFLOW_L=0
      UNIONAREA=0
            DO 1003 I=1,NGRID
            RUNOFF_L=RUNOFF_L+RUN_HRU(I,J,M)*HUCAREA(I)/100000
            BASEFLOW_L=BASEFLOW_L+BASE_HRU(I,J,M)*HUCAREA(I)/100000
            UNIONAREA=UNIONAREA+HUCAREA(I)
!			print *, I,J,M,RUN_HRU(I,J,M),BASE_HRU(I,J,M),HUCAREA(I),RUNOFF_L,BASEFLOW_L,UNIONAREA
1003  CONTINUE 
            RUN_OFF(J,M)=RUNOFF_L/UNIONAREA*100000
            BASE_FLOW(J,M)=BASEFLOW_L/UNIONAREA*100000    
1002  CONTINUE
1001  CONTINUE
  
      
!      DO 1001 J=1,NYEAR
!	  
!        DO 1002 M=1,12
!		FLOW_L=0  !FLOW 总量临时文件
!            DO 1003 I=1,NGRID
!            FLOW_L=FLOW_L+STRFLOW(I,J,M)
!1003  CONTINUE 
!            FLOW(J,M)=FLOW_L     
!1002  CONTINUE
!1001  CONTINUE
    
      
!C----开始计算GEP，AET，PET，GPP，NPP的皮尔森相关系数（P)，决定系数（R），效率系数（NS）
!               
!        num_m=0
!        num_y=0
!                SUM_GEP=0
!                SUM_GEP_V=0
!                                
!                SUM_GEP_X=0
!                SUM_GEP_Y=0
!                SUM_GEP_XY=0
!                SUM_GEP_XX=0
!                SUM_GEP_YY=0
!
!                SUM_AET=0
!                SUM_AET_V=0
!                                
!                SUM_AET_X=0
!                SUM_AET_Y=0
!                SUM_AET_XY=0
!                SUM_AET_XX=0
!                SUM_AET_YY=0
!                
!                SUM_PET=0
!                SUM_PET_V=0
!                                
!                SUM_PET_X=0
!                SUM_PET_Y=0
!                SUM_PET_XY=0
!                SUM_PET_XX=0
!                SUM_PET_YY=0
!               
!                SUM_GPP=0
!                SUM_NPP=0
!                SUM_GPP_V=0
!                SUM_NPP_V=0
!
!                SUM_GPP_X=0
!                SUM_GPP_Y=0
!                SUM_GPP_XY=0
!                SUM_GPP_XX=0
!                SUM_GPP_YY=0
!
!                SUM_NPP_X=0
!                SUM_NPP_Y=0
!                SUM_NPP_XY=0
!                SUM_NPP_XX=0
!                SUM_NPP_YY=0
!
!                  SUM_GEP_U=0
!                  SUM_GEP_L=0
!                  SUM_GEP_R=0
!                  SUM_GEP_N=0
!
!                  SUM_AET_L=0
!                  SUM_PET_L=0
!
!                  SUM_AET_U=0
!                  SUM_PET_U=0
!      
!                  SUM_AET_R=0
!                  SUM_PET_R=0
!
!                  SUM_AET_N=0
!                  SUM_PET_N=0
!
!      SUM_GPP_U=0
!      SUM_NPP_U=0
!                
!      SUM_GPP_L=0
!      SUM_NPP_L=0  
!      
!      SUM_GPP_R=0
!      SUM_NPP_R=0     
!                
!
!      SUM_GPP_N=0
!      SUM_NPP_N=0  
!
!                SUM_FLOW=0
!                SUM_FLOW_V=0
!                                
!                SUM_FLOW_X=0
!                SUM_FLOW_Y=0
!                SUM_FLOW_XY=0
!                SUM_FLOW_XX=0
!                SUM_FLOW_YY=0
!
!      SUM_FLOW_U=0
!      SUM_FLOW_L=0
!      SUM_FLOW_R=0
!      SUM_FLOW_N=0

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


                SUM_BASEFLOW=0
                SUM_BASEFLOW_V=0
                                
                SUM_BASEFLOW_X=0
                SUM_BASEFLOW_Y=0
                SUM_BASEFLOW_XY=0
                SUM_BASEFLOW_XX=0
                SUM_BASEFLOW_YY=0

      SUM_BASEFLOW_U=0
      SUM_BASEFLOW_L=0
      SUM_BASEFLOW_R=0
      SUM_BASEFLOW_N=0

!
!        DO 50001 I=1,NGRID
!            DO 50002 J=1,NYEAR
!                DO 50003 M=1,12
!C	计算GEP的月验证参数
!        IF (GEP_V(I,J,M) .LT. 0 .OR. NEEM(I,J,M) .LT. -400) THEN
!            ELSE
!                num_m=num_m+1
!                SUM_GEP=SUM_GEP+GEPM(I,J,M)
!                SUM_GEP_V=SUM_GEP_V+GEP_V(I,J,M)
!                                
!                SUM_GEP_X=SUM_GEP_X+GEPM(I,J,M)
!                SUM_GEP_Y=SUM_GEP_Y+GEP_V(I,J,M)
!                SUM_GEP_XY=SUM_GEP_XY+GEPM(I,J,M)*GEP_V(I,J,M)
!                SUM_GEP_XX=SUM_GEP_XX+GEPM(I,J,M)**2
!                SUM_GEP_YY=SUM_GEP_YY+GEP_V(I,J,M)**2
!
!        ENDIF
!C	计算ET的月验证参数
!        IF (A_ET(I,J,M) .LT. 0 .OR. ET_V(I,J,M) .LT. 0) THEN
!            ELSE
!                SUM_AET=SUM_AET+A_ET(I,J,M)
!                SUM_PET=SUM_PET+P_ET(I,J,M)
!                SUM_ET_V=SUM_ET_V+ET_V(I,J,M)
!                SUM_AET_X=SUM_AET_X+A_ET(I,J,M)
!                SUM_AET_Y=SUM_AET_Y+ET_V(I,J,M)
!                SUM_AET_XY=SUM_AET_XY+A_ET(I,J,M)*ET_V(I,J,M)
!                SUM_AET_XX=SUM_AET_XX+A_ET(I,J,M)**2
!                SUM_AET_YY=SUM_AET_YY+ET_V(I,J,M)**2
!
!                SUM_PET_X=SUM_PET_X+P_ET(I,J,M)
!                SUM_PET_Y=SUM_PET_Y+ET_V(I,J,M)
!                SUM_PET_XY=SUM_PET_XY+P_ET(I,J,M)*ET_V(I,J,M)
!                SUM_PET_XX=SUM_PET_XX+P_ET(I,J,M)**2
!                SUM_PET_YY=SUM_PET_YY+ET_V(I,J,M)**2
!        ENDIF
!
!50003   Continue
!C	计算NEE的年验证参数
!        IF (NEEA(I,J) .LT. -1200) THEN
!            ELSE
!                num_y=num_y+1
!                SUM_GPP=SUM_GPP+GEPA(I,J)
!                SUM_NPP=SUM_NPP+NEEA(I,J)
!                SUM_GPP_V=SUM_GPP_V+GPP_V(I,J)
!                SUM_NPP_V=SUM_NPP_V+NPP_V(I,J)
!
!                SUM_GPP_X=SUM_GPP_X+GEPA(I,J)
!                SUM_GPP_Y=SUM_GPP_Y+GPP_V(I,J)
!                SUM_GPP_XY=SUM_GPP_XY+GEPA(I,J)*GPP_V(I,J)
!                SUM_GPP_XX=SUM_GPP_XX+GEPA(I,J)**2
!                SUM_GPP_YY=SUM_GPP_YY+GPP_V(I,J)**2
!
!                SUM_NPP_X=SUM_NPP_X+NEEA(I,J)
!                SUM_NPP_Y=SUM_NPP_Y+NPP_V(I,J)
!                SUM_NPP_XY=SUM_NPP_XY+NEEA(I,J)*NPP_V(I,J)
!                SUM_NPP_XX=SUM_NPP_XX+NEEA(I,J)**2
!                SUM_NPP_YY=SUM_NPP_YY+NPP_V(I,J)**2
!         ENDIF
!50002   Continue
!50001   Continue
!            AVE_GEP=SUM_GEP/num_m
!            AVE_GEP_V=SUM_GEP_V/num_m
!            AVE_ET_V=SUM_ET_V/num_m
!            AVE_AET=SUM_AET/num_m
!            AVE_PET=SUM_PET/num_m
!            AVE_GPP=SUM_GPP/num_y
!            AVE_GPP_V=SUM_GPP_V/num_y
!            AVE_NPP=SUM_NPP/num_y
!            AVE_NPP_V=SUM_NPP_V/num_y
!
!          DO 60001 I=1,NGRID
!            DO 60002 J=1,NYEAR
!                DO 60003 M=1,12
!        IF (GEP_V(I,J,M) .LT. 0 .OR. NEEM(I,J,M) .LT. -400) THEN
!            ELSE
!      SUM_GEP_U=SUM_GEP_U+(GEPM(I,J,M)-AVE_GEP)*(GEP_V(I,J,M)-AVE_GEP_V)
!      SUM_GEP_L=SUM_GEP_L+(GEP_V(I,J,M)-AVE_GEP_V)**2
!      SUM_GEP_R=SUM_GEP_R+(GEPM(I,J,M)-AVE_GEP)**2
!      SUM_GEP_N=SUM_GEP_N+(GEPM(I,J,M)-GEP_V(I,J,M))**2
!
!        ENDIF
!        IF (A_ET(I,J,M) .LT. 0 .OR. ET_V(I,J,M) .LT. 0) THEN
!            ELSE
!      SUM_AET_L=SUM_AET_L+(ET_V(I,J,M)-AVE_ET_V)**2
!      SUM_PET_L=SUM_PET_L+(ET_V(I,J,M)-AVE_ET_V)**2
!
!      SUM_AET_U=SUM_AET_U+(A_ET(I,J,M)-AVE_AET)*(ET_V(I,J,M)-AVE_ET_V)
!      SUM_PET_U=SUM_PET_U+(P_ET(I,J,M)-AVE_PET)*(ET_V(I,J,M)-AVE_ET_V)
!      
!      SUM_AET_R=SUM_AET_R+(A_ET(I,J,M)-AVE_AET)**2
!      SUM_PET_R=SUM_PET_R+(P_ET(I,J,M)-AVE_PET)**2
!
!      SUM_AET_N=SUM_AET_N+(A_ET(I,J,M)-ET_V(I,J,M))**2
!      SUM_PET_N=SUM_PET_N+(P_ET(I,J,M)-ET_V(I,J,M))**2
!      ENDIF   
!             
!60003   Continue
!        IF (NEEA(I,J) .LT. -1200) THEN
!            ELSE
!      SUM_GPP_U=SUM_GPP_U+(GEPA(I,J)-AVE_GPP)*(GPP_V(I,J)-AVE_GPP_V)
!      SUM_NPP_U=SUM_NPP_U+(NEEA(I,J)-AVE_NPP)*(NPP_V(I,J)-AVE_NPP_V)
!                
!      SUM_GPP_L=SUM_GPP_L+(GPP_V(I,J)-AVE_GPP_V)**2
!      SUM_NPP_L=SUM_NPP_L+(NPP_V(I,J)-AVE_NPP_V)**2   
!      
!      SUM_GPP_R=SUM_GPP_R+(GEPA(I,J)-AVE_GPP)**2
!      SUM_NPP_R=SUM_NPP_R+(NEEA(I,J)-AVE_NPP)**2     
!                
!
!      SUM_GPP_N=SUM_GPP_N+(GEPA(I,J)-GPP_V(I,J))**2
!      SUM_NPP_N=SUM_NPP_N+(NEEA(I,J)-NPP_V(I,J))**2  
!       ENDIF
!60002   Continue
!60001   Continue
!
!        R_GEP=(SUM_GEP_U/(SUM_GEP_L*(SUM_GEP_R**0.5)))**2
!        R_AET=(SUM_AET_U/(SUM_AET_L*SUM_AET_R**0.5))**2
!        R_PET=(SUM_PET_U/(SUM_PET_L*SUM_PET_R**0.5))**2
!        R_GPP=(SUM_GPP_U/(SUM_GPP_L*SUM_GPP_R**0.5))**2
!        R_NPP=(SUM_NPP_U/(SUM_NPP_L*SUM_NPP_R**0.5))**2
!
!
!        NS_GEP=1-(SUM_GEP_N/SUM_GEP_L)
!        NS_AET=1-(SUM_AET_N/SUM_AET_L)
!        NS_PET=1-(SUM_PET_N/SUM_PET_L)
!        NS_GPP=1-(SUM_GEP_N/SUM_GPP_L)
!        NS_NPP=1-(SUM_GEP_N/SUM_NPP_L)
!
!             P_GEP=(num_m*SUM_GEP_XY-SUM_GEP_X*SUM_GEP_Y)/((num_m*
!     >  SUM_GEP_XX-SUM_GEP_X**2)*(num_m*SUM_GEP_YY-SUM_GEP_Y**2))**0.5
!             P_AET=(num_m*SUM_AET_XY-SUM_AET_X*SUM_AET_Y)/((num_m*
!     >  SUM_AET_XX-SUM_AET_X**2)*(num_m*SUM_AET_YY-SUM_AET_Y**2))**0.5
!             P_PET=(num_m*SUM_PET_XY-SUM_PET_X*SUM_PET_Y)/((num_m*
!     >  SUM_PET_XX-SUM_PET_X**2)*(num_m*SUM_PET_YY-SUM_PET_Y**2))**0.5
!             P_GPP=(num_Y*SUM_GPP_XY-SUM_GPP_X*SUM_GPP_Y)/((num_Y*
!     >  SUM_GPP_XX-SUM_GPP_X**2)*(num_Y*SUM_GPP_YY-SUM_GPP_Y**2))**0.5
!             P_NPP=(num_Y*SUM_NPP_XY-SUM_NPP_X*SUM_NPP_Y)/((num_Y*
!     >  SUM_NPP_XX-SUM_NPP_X**2)*(num_Y*SUM_NPP_YY-SUM_NPP_Y**2))**0.5

!C---------计算FLOW的皮尔森相关系数（P)，决定系数（R），效率系数（NS）
!        num_F=0
!            DO 70002 J=1,NYEAR
!                DO 70003 M=1,12
!
!                num_F=num_F+1
!                SUM_FLOW=SUM_FLOW+FLOW(J,M)
!                SUM_FLOW_V=SUM_FLOW_V+FLOW_V(J,M)
!                                
!                SUM_FLOW_X=SUM_FLOW_X+FLOW(J,M)
!                SUM_FLOW_Y=SUM_FLOW_Y+FLOW_V(J,M)
!                SUM_FLOW_XY=SUM_FLOW_XY+FLOW(J,M)*FLOW_V(J,M)
!                SUM_FLOW_XX=SUM_FLOW_XX+FLOW(J,M)**2
!                SUM_FLOW_YY=SUM_FLOW_YY+FLOW_V(J,M)**2
!       
!
!70003   Continue
!70002   Continue
!
!            AVE_FLOW=SUM_FLOW/num_F
!            AVE_FLOW_V=SUM_FLOW_V/num_F
!
!
!            DO 80002 J=1,NYEAR
!                DO 80003 M=1,12
!
!      SUM_FLOW_U=SUM_FLOW_U+(FLOW(J,M)-AVE_FLOW)*
!     >(FLOW_V(J,M)-AVE_FLOW_V)
!      SUM_FLOW_L=SUM_FLOW_L+(FLOW_V(J,M)-AVE_FLOW_V)**2
!      SUM_FLOW_R=SUM_FLOW_R+(FLOW(J,M)-AVE_FLOW)**2
!      SUM_FLOW_N=SUM_FLOW_N+(FLOW(J,M)-FLOW_V(J,M))**2
!                   
!80003   Continue
!80002   Continue
!
!
!        R_FLOW=(SUM_FLOW_U/(SUM_FLOW_L*(SUM_FLOW_R**0.5)))**2
!
!        NS_FLOW=1-(SUM_FLOW_N/SUM_FLOW_L)
!
!
!             P_FLOW=(num_F*SUM_FLOW_XY-SUM_FLOW_X*SUM_FLOW_Y)/((num_F*
!     >SUM_FLOW_XX-SUM_FLOW_X**2)*(num_F*SUM_FLOW_YY-SUM_FLOW_Y**2))**0.5




!---------计算RUNOFF的皮尔森相关系数（P)，决定系数（R），效率系数（NS）

!    这里的“J=1,15 ” 代表验证的年数
        num_F=0
            DO 11002 J=Valid_year-IYSTART+1,2006-IYSTART+1
                DO 11003 M=1,12

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


            DO 12002 J=Valid_year-IYSTART+1,2006-IYSTART+1
                DO 12003 M=1,12

      SUM_RUNOFF_U=SUM_RUNOFF_U+(RUN_OFF(J,M)-AVE_RUNOFF)* &
      (RUNOFF_V(J,M)-AVE_RUNOFF_V)
      SUM_RUNOFF_L=SUM_RUNOFF_L+(RUNOFF_V(J,M)-AVE_RUNOFF_V)**2
      SUM_RUNOFF_R=SUM_RUNOFF_R+(RUN_OFF(J,M)-AVE_RUNOFF)**2
      SUM_RUNOFF_N=SUM_RUNOFF_N+(RUN_OFF(J,M)-RUNOFF_V(J,M))**2
                   
12003   Continue
12002   Continue


!        R_RUNOFF=(SUM_RUNOFF_U/(SUM_RUNOFF_L*(SUM_RUNOFF_R**0.5)))**2
        R_RUNOFF=(SUM_RUNOFF_N/num_F)**0.5
        NS_RUNOFF=1-(SUM_RUNOFF_N/SUM_RUNOFF_L)


             P_RUNOFF=(num_F*SUM_RUNOFF_XY-SUM_RUNOFF_X*SUM_RUNOFF_Y)/&
      ((num_F*SUM_RUNOFF_XX - &
      SUM_RUNOFF_X**2)*(num_F*SUM_RUNOFF_YY-SUM_RUNOFF_Y**2))**0.5


!---------计算BASEFLOW的皮尔森相关系数（P)，决定系数（R），效率系数（NS）
        num_F=0
            DO 13002 J=Valid_year-IYSTART+1,2006-IYSTART+1
                DO 13003 M=1,12

                num_F=num_F+1
                SUM_BASEFLOW=SUM_BASEFLOW+BASE_FLOW(J,M)
                SUM_BASEFLOW_V=SUM_BASEFLOW_V+BASEFLOW_V(J,M)
                                
                SUM_BASEFLOW_X=SUM_BASEFLOW_X+BASE_FLOW(J,M)
                SUM_BASEFLOW_Y=SUM_BASEFLOW_Y+BASEFLOW_V(J,M)
                SUM_BASEFLOW_XY=SUM_BASEFLOW_XY+BASE_FLOW(J,M)*BASEFLOW_V(J,M)
    
                SUM_BASEFLOW_XX=SUM_BASEFLOW_XX+BASE_FLOW(J,M)**2
                SUM_BASEFLOW_YY=SUM_BASEFLOW_YY+BASEFLOW_V(J,M)**2
       

13003   Continue
13002   Continue

            AVE_BASEFLOW=SUM_BASEFLOW/num_F
            AVE_BASEFLOW_V=SUM_BASEFLOW_V/num_F


            DO 14002 J=Valid_year-IYSTART+1,2006-IYSTART+1
                DO 14003 M=1,12

      SUM_BASEFLOW_U=SUM_BASEFLOW_U+(BASE_FLOW(J,M)-AVE_BASEFLOW)* &
      (BASEFLOW_V(J,M)-AVE_BASEFLOW_V)
      SUM_BASEFLOW_L=SUM_BASEFLOW_L+(BASEFLOW_V(J,M)-AVE_BASEFLOW_V)**2
      SUM_BASEFLOW_R=SUM_BASEFLOW_R+(BASE_FLOW(J,M)-AVE_BASEFLOW)**2
      SUM_BASEFLOW_N=SUM_BASEFLOW_N+(BASE_FLOW(J,M)-BASEFLOW_V(J,M))**2
                   
14003   Continue
14002   Continue


!        R_BASEFLOW=(SUM_BASEFLOW_U/(SUM_BASEFLOW_L*(SUM_BASEFLOW_R**0.5)
!     >))**2
        R_BASEFLOW=(SUM_BASEFLOW_N/num_F)**0.5
        NS_BASEFLOW=1-(SUM_BASEFLOW_N/SUM_BASEFLOW_L)


         P_BASEFLOW=(num_F*SUM_BASEFLOW_XY-SUM_BASEFLOW_X*SUM_BASEFLOW_Y &
      )/((num_F*SUM_BASEFLOW_XX-SUM_BASEFLOW_X**2)&
      *(num_F*SUM_BASEFLOW_YY-SUM_BASEFLOW_Y**2))**0.5

!-----输出验证数据到各个文件，年验证DATA_V_A.TXT，月验证DATA_V_M.TXT，
      
!      DO 7001 I=1,NGRID
!        DO 7002 J=1,NYEAR
!            DO 7003 M=1,12
!        WRITE(2000,20000) HUCNO(I),J+BYEAR-1,M,GEPM(I,J,M),NEEM(I,J,M),
!     > GEP_V(I,J,M),A_ET(I,J,M), P_ET(I,J,M),SP_ET(I,J, M),ET_V(I,J,M)
!
!        WRITE(*,20001) HUCNO(I),J+BYEAR-1,M,GEPM(I,J,M),NEEM(I,J,M),
!     > GEP_V(I,J,M),A_ET(I,J,M), P_ET(I,J,M),SP_ET(I,J, M),ET_V(I,J,M)
!
!20000        FORMAT (I5, ',', I7, ',', 
!     >        I5, ',', F12.2, ',', F12.2, ',', F12.2, ',',F12.2, ','
!     >        ,F12.2, ',', F12.2, ',', F12.2)
!C     , ',', E15.6, ',', E15.6, ',', E15.6,
!C    >           ',', E15.6, ',', E15.6,
!C    >           ',', E15.6)
!      
!7003  CONTINUE
!
!      WRITE(2001,20001) HUCNO(I),J+BYEAR-1,GEPA(I,J),NEEA(i,j),
!     > HUCAET(I,J), HUCPET(I,J), GPP_V(I,J),NPP_V(I,J)
!
!      WRITE(*,20001) HUCNO(I),J+BYEAR-1,GEPA(I,J),NEEA(i,j),
!     > HUCAET(I,J), HUCPET(I,J), GPP_V(I,J),NPP_V(I,J)
!
!20001          FORMAT (I12, ',', I12, ',',F16.2, ',', 
!     >           F16.2, ',', F16.2, ',', F16.2,',', F16.2,
!     >         ',', F16.2)
!
!7002  CONTINUE
!7001  CONTINUE  


!203      FORMAT ('year,month,RUNOFF,RUNOFF_V,BASEFLOW,BASEFLOW_V')
!---------径流验证数据DATA_V_F.TXT
        DO 8002 J= Valid_year-IYSTART+1,2006-IYSTART+1 !2,2006-IYSTART+1
            DO 8003 M=1,12
        WRITE(2002,20002) J+BYEAR-1,M,RUN_OFF(J,M),RUNOFF_V(J,M)&
      ,BASE_FLOW(J,M),BASEFLOW_V(J,M)

!        WRITE(*,20002) J+BYEAR-1,M,RUN_OFF(J,M),RUNOFF_V(J,M)&
!      ,BASEFLOW(J,M),BASEFLOW_V(J,M)

20002        FORMAT ( I7, ',', I5,',',&
!     >        I5, ',', F12.2, ',', F12.2, ',', F12.2, ',',F12.2, ','
             F12.2, ',', F12.2,',',F12.2, ',', F12.2)
!C     , ',', E15.6, ',', E15.6, ',', E15.6,
!C    >           ',', E15.6, ',', E15.6,
!C    >           ',', E15.6)
      
8003  CONTINUE
8002  CONTINUE

!      P_GEP=P_GEP**2
!      P_AET=P_AET**2
!      P_PET=P_PET**2
!      P_GPP=P_GPP**2
!      P_NPP=P_NPP**2
!      P_FLOW=P_FLOW**2
      P_RUNOFF=P_RUNOFF**2
      P_BASEFLOW=P_BASEFLOW**2


!------输出验证结果到VALIDATION.TXT

!        WRITE(2003,20003)VAL_1(TUN1),VAL_2(TUN2), R_GEP,R_AET,
!     >R_PET,R_GPP,R_NPP,R_FLOW,
!     > NS_GEP,NS_AET,NS_PET,NS_GPP,NS_NPP,NS_FLOW,
!     > P_GEP,P_AET,P_PET,P_GPP,P_NPP,P_FLOW
!
!      WRITE(*,20003) VAL_1(TUN1),VAL_2(TUN2),R_GEP,R_AET,
!     >R_PET,R_GPP,R_NPP,
!     > NS_GEP,NS_AET,NS_PET,NS_GPP,NS_NPP,NS_FLOW,
!     > P_GEP,P_AET,P_PET,P_GPP,P_NPP,P_FLOW
!
!20003       FORMAT (f12.3,',',f12.3,',',E15.6, ',', E15.6, ',', 
!     >        E15.6, ',', E15.6, ',', E15.6, ',', E15.6, ',', E15.6, ','
!     >        ,E15.6, ',', E15.6, ',', E15.6, ',', E15.6, ',', E15.6,
!     >        ',', E15.6, ',', E15.6,',', E15.6, ',', E15.6, ',', E15.6,
!     >           ',', E15.6)   


!        WRITE(2003,20003)VAL_1(TUN1), VAL_2(TUN2),R_RUNOFF,R_BASEFLOW,&
!       NS_RUNOFF,NS_BASEFLOW,P_RUNOFF,P_BASEFLOW

        WRITE(*,20003)VAL_1(TUN1), VAL_2(TUN2),R_RUNOFF,R_BASEFLOW,&
      NS_RUNOFF,NS_BASEFLOW,P_RUNOFF,P_BASEFLOW

20003       FORMAT (2f12.3,',',f10.3, ',', f10.3, ',', &
              f10.3, ',', f10.3, ',', f10.3, ',', f10.3)         
!C     print*,"输入回车进入下一步操作"
!C     READ(*,*) 
     
      RETURN
      END
