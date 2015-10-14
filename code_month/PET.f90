!C**********************************************************************C
!C                                                                      C
!C     *** SUBROUTINE WARMPET ***                                       C
!C     INPUT  MONTHLY PRECIPITATION AND TEMPERATURE, AND CALCULATE      C
!C     MONTHLY PET AND POTENTIAL AET                                    C
!C                                                                      C
!C**********************************************************************C
      SUBROUTINE WARMPET(I, J, M, MNDAY)
      
      USE common_var
      implicit none
               
      INTEGER I,J,M,MNDAY,K  

      INTEGER MONTHD(12),MONTHL(12), MJD(12), MMD
      
      REAL TPET,TPAET,PE,DEGLAT,LAI,DTEMP

 ! --- Number of days for each month during regular year
      DATA MONTHD/31,28,31,30,31,30,31,31,30,31,30,31/
! 
! --- Number of days for each month during leap year
      DATA MONTHL/31,29,31,30,31,30,31,31,30,31,30,31/
!-----JULIAN DAY FOR MID-DAY OF EACH MONTH
      DATA MJD/15,46,76,107,137,168,198,229,259,290,321,351/      

! --- Calculate Monthly potential evapotranspiration 
    
            DTEMP=TEMP(I,J,M)
            
            TPET=0. 
            
            TPAET=0. 
!			   PET=0.
      DO K=1, NLC                   
              
! -- MMD = JULIAN DATE FOR MONTH M
                               
            MMD=MJD(M)
                     
            DEGLAT = LATUDE(I)
            
! - DTEMP = AIR TEMP, HPEC = CORRECTION PARAMETER, PE =CALCUALTED PET (MM)--DAY
           
            CALL HAMON(K,DTEMP,M,MMD,DEGLAT,PE)
            
            
! ----PET (YEAR, MONTH, LANDUSE) FOR THE CURRENT CELL)
           
             PET(J,M,K) = PE  
                  
             PET(J,M,K) = PET(J,M,K) * MNDAY 
                        
!
!C ----CALCULATE PAET 
!C ----PAET (YEAR, MONTH, LANDUSE) FOR THE CURRENT CELL)
!C ----PAET IS THE POTENTIAL AET, ASSUMING SOIL MOISTURE NOT LIMITING
!
!c             IF ((LAI.GE.1.0).OR.(AAPPT(I).GE.600.0)) THEN
!             
!C     CALCULATE PAET USING MODIFIED SUN HAMON EQUATION FOR HIGHER LAI
!C     NO COWEETA
!C     NO -LAI*RAIN TERM (MAKES PAET INVERSELY PROPORTIONAL TO LAI DURING WINTER
!C     R2=0.87, N=90, MODEL ET= 9.37+0.87*MEASURED ET
!
    !        PAET(J,M,K) = 9.95+(PET(J,M,K))*LAI*0.205+0.153* &
  !   RAIN(I,J,M)+0.246*(PET(J,M,K))
!
!C  new model with coweeta Hamon ET Sep 10,2010
!
     !        PAET(J,M,K) = 0.0222*PET(J,M,K)*LAI+0.174* &
     !       RAIN(I,J,M)+0.502*PET(J,M,K) + 5.31*LAI


! Latest model By Yuan Fang Sep 10,2015
!          R2=0.68, p<0.0001,RMSE=18.1 mm

            PAET(J,M,K) = -12.59 + 0.75*PET(J,M,K) + 3.92*LAI_VEG(I,J,M,K)


  
!C           PRINT *, 'pet', LAI,RAIN(I,J,M), PET(J,M,K), PAET(J,M,K)
!     
!     
!c             ELSE
!             
!C     CALCULATE PAET FOR GRASSLAND AND LOW LAI
!C     R2=0.696
!             
!c             PAET(J,M,K) = 1.49 + 0.325*(PET(J,M,K))+0.353*(RAIN(I,J,M))
!             
!c             ENDIF
!          
!c             WRITE(910,5039)  HUCNO(I), J, M, K, PAET(J,M,K)
!c5039         FORMAT(4I10, F20.1)
!
!
!C ----CALCULATE TOTAL PET AND PAET FOR THE HUC FOR A GIVEN YEAR AND MONTH
        
             
             TPET = TPET + PET(J,M,K) * LADUSE(I,K)
             
             TPAET = TPAET + PAET(J,M,K) * LADUSE(I,K)
             
             
        
 !         WRITE(77, 5040) HUCNO(I), J, M,K,LADUSE(I,K), LAI_VEG(I,J,M,K)          
!       
5040  FORMAT (4I10, 2F10.5)
     
                    
             
	END DO			

!C-------流域单元月APET、APAET（各植被类型的加权平均值）
!C ------APET =AVERAGE PET FOR CURRENT CELL, FOR ALL YEAR, MONTH


              APET(I,J,M) = TPET    
              
! ------APAET =AVERAGE PAET FOR CURRENT CELL, FOR ALL YEAR, MONTH

              APAET(I,J,M) = TPAET
              

! ------TEST OUTPUT


!      WRITE(*,5050) HUCNO(I),J,M, APET(I,J,M), APAET(I,J,M)
      
5050  FORMAT (3I10, 2F10.5)

                              
! --- Return
      RETURN
      END


!**********************************************************************C
!                                                                      C
!     *** SUBROUTINE HAMON ***                                         C
!     Calculate potential evapotranspiration by using Hamon's          C
!     equation                                                         C
!                                                                      C
!**********************************************************************C
      SUBROUTINE HAMON(K,TEMP,MONTH,DNUM,DEGLAT,PE)
      
      REAL PE, TEMP, DEGLAT
      
      REAL SOLDEC, SSANG, DAY
      
      REAL ESAT, RHOSAT, PI
      
      INTEGER DNUM,K  

      PI = 3.14159265

! --- ESAT = saturated vapor pressure at TEMP
! --- RHOSAT = saturated vapor density at TEMP
      
      ESAT = 6.108*EXP(17.2693882*TEMP/(TEMP+237.3))
      RHOSAT = 216.7*ESAT/(TEMP+273.3)
      
! --- CALCULATE MEAN MONTHLY DAY LENGTH (DAY) BASED ON LATITUDE AND MID-MONTH JULIAN DATE
! --- SHUTTLEWORTH, W.J. 1993. Evaporation, in Handbook of Hydrology, 
! --- MAIDMENT, D.R. ED., MCGRAW HILL, NY, PP. 4, 1-53

! ---    CALCULATE THE ANGLE OF SOLAR DECLINATION IN RADIANS     
 
         SOLDEC = 0.4093*SIN(2*PI*DNUM/365.0-1.405)
      
! ---    CALCULATE THE SUNSET HOUR ANGLE IN RADIANS

         SSANG = ACOS(-1*TAN(DEGLAT*0.0174529)*TAN(SOLDEC))
      
! ---    CALCULATE THE ADJUSTMENT IN LENGTH OF DAY FOR 12 HR PERIOD
      
         DAY = 2*SSANG/PI
         
         
! --- Calculate Daily PE
     
      PE = 0.1651*DAY*RHOSAT*1.2
      
!     print *, 'PET', MONTH, DNUM, TEMP, PE, K
          
      RETURN

      END















