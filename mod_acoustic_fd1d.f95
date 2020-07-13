MODULE MOD_ACOUSTIC_FD1D
    IMPLICIT NONE
   !-------------------------------------------------
    TYPE MODEL
        REAL                ::  DH, H    
        INTEGER             ::  NH, NLAYER   
        REAL,ALLOCATABLE,DIMENSION(:)   ::  DEPTH,VPL,VP    !,RHO
    END TYPE MODEL
   !------------------------------------------------- 
    TYPE SOURCE
        REAL                ::  T       !MAX TIME               
        REAL                ::  DT      !TIME STEP
        REAL                ::  F0      !FREQUENCY      
        REAL                ::  T0      !TIME OFFSET    
        INTEGER             ::  SNH     !SOURCE POSITION   
        INTEGER             ::  NT       !NUMBER OF TIME STEPS

        REAL,ALLOCATABLE,DIMENSION(:)   ::  SIG     !SIGNATURE
        REAL,ALLOCATABLE,DIMENSION(:)   ::  TIME    !TIME        
    END TYPE SOURCE
   !-------------------------------------------------    
   TYPE SS
        REAL,ALLOCATABLE    :: SS(:)
        INTEGER             :: NT,RNH     
    END TYPE SS

    
    !##############################################    
    CONTAINS

    !######################################################
    SUBROUTINE ERR_STOP(VAR,EQVAL,STR1)
        INTEGER, INTENT(IN) :: VAR, EQVAL
        CHARACTER(LEN=*),INTENT(IN):: STR1 
        IF (VAR/=EQVAL) THEN  
            PRINT*, STR1
            STOP
        END IF
    END SUBROUTINE ERR_STOP

    SUBROUTINE FD_PRINT_SS(S)
        TYPE(SS),INTENT(IN) :: S
        INTEGER :: I
        PRINT*, ' '
        PRINT*, 'SS: -----------------------'
        PRINT*, 'NT   : ', S%NT
        PRINT*, 'RNH  : ', S%RNH
        !PRINT*, 'SS   : ', S%SS
        
        IF (ALLOCATED(S%SS)) THEN
              WRITE(*,'(A12)',ADVANCE='NO') ' SS       : '
            DO I=1,SIZE(S%SS)
                WRITE(*,'(F10.5)',ADVANCE='NO') S%SS(I)
            END DO
        END IF
        WRITE(*,*) ''
    END SUBROUTINE FD_PRINT_SS


    SUBROUTINE FD_PRINT_SOURCE(S)
        TYPE(SOURCE),INTENT(IN) :: S
        INTEGER :: I
        PRINT*, ' '
        PRINT*, 'SOURCE: -----------------------'
        PRINT*, 'T    : ', S%T
        PRINT*, 'DT   : ', S%DT
        PRINT*, 'F0   : ', S%F0
        PRINT*, 'T0   : ', S%T0
        PRINT*, 'SNH  : ', S%SNH
        PRINT*, 'NT   : ', S%NT
        IF (ALLOCATED(S%SIG)) THEN
              WRITE(*,'(A12)',aDVANCE='NO') ' SIG      : '
            DO I=1,SIZE(S%SIG)
                WRITE(*,'(F10.5)',ADVANCE='NO') S%SIG(I)
            END DO
        END IF
        WRITE(*,*) ''        

        !PRINT*, 'TIME : ', S%TIME
    END SUBROUTINE FD_PRINT_SOURCE

    SUBROUTINE FD_PRINT_MODEL(M)
        TYPE(MODEL),INTENT(IN) :: M
        INTEGER :: I
        PRINT*, ' '
        PRINT*, 'MODEL: -----------------------'        
        PRINT*, 'DH       : ', M%DH
        PRINT*, 'H        : ', M%H 
        PRINT*, 'NH       : ', M%NH
        PRINT*, 'NLAYERS  : ', M%NLAYER
        PRINT*, 'DEPTH    : ', M%DEPTH
        PRINT*, 'VPL      : ', M%VPL
        IF (ALLOCATED(M%VP)) THEN
              WRITE(*,'(A12)',aDVANCE='NO') ' VP       : '
            DO I=1,SIZE(M%VP)
                WRITE(*,'(F8.0)',ADVANCE='NO') M%VP(I)
            END DO
        END IF
        WRITE(*,*) ''        
    END SUBROUTINE FD_PRINT_MODEL
    
    !##############################################    
    SUBROUTINE FD_ACOUSTIC(M,SRC,SS0,VERB)
        IMPLICIT NONE  
        TYPE(MODEL),INTENT(IN)   :: M
        TYPE(SOURCE),INTENT(IN)  :: SRC
        TYPE(SS),INTENT(INOUT)      :: SS0 
        CHARACTER,INTENT(IN), OPTIONAL            :: VERB
        REAL, ALLOCATABLE           :: P0(:),P1(:),P2(:),DER(:),CN(:) 
        INTEGER         :: I,OK
        CHARACTER(50)   :: STR1,STR2 
        

        ! CHECK CFL CONDITION 
        IF((MAXVAL(M%VP)*SRC%DT/M%DH)< .75) THEN
            IF ((VERB=='y').or.(VERB=='Y')) WRITE(*,*)'CFL CONDITION: SATISFIED (CN= ',(MAXVAL(M%VP)*SRC%DT/M%DH),')'
        ELSE    
            WRITE(*,*) 'INSTABILITY DETECTED, CHANGE PARAMETERS'
            PRINT*,'VPMAX=', MAXVAL(M%VP),'DT=',SRC%DT,'DH=',M%DH
            STOP
        END IF
        
        ALLOCATE(P0(M%NH),P1(M%NH),P2(M%NH),DER(M%NH),CN(M%NH),STAT=OK) 
        CALL ERR_STOP(OK,0,'ALLOCATION ERROR')  
            
        P0=0.0 
        P1=0.0 
        P2=0.0 
        DER=0.0
        CN= (M%VP*SRC%DT/M%DH)**2
        SS0%SS=0.
        
        WRITE(STR1,'(I5.5)') M%NH   
        STR2='('//TRIM(STR1)//'F10.5'//')'
        
        OPEN(1,FILE='SRC.txt',STATUS='REPLACE') 
            DO I=1,SRC%NT
                WRITE(1,*) SRC%SIG(I)
            END DO
        CLOSE(1)

        OPEN(1,FILE='OP.txt',STATUS='REPLACE') 
        DO I=1,SRC%NT
            P1(SRC%SNH) = P1(SRC%SNH) + SRC%SIG(I) 
            DER(2:M%NH-1)= P1(1:M%NH-2) - 2*P1(2:M%NH-1) + P1(3:M%NH) 
            P2=2*P1 - P0 + CN*DER
            WRITE(1,STR2) P2  !(P2(J),J=1,M%NH)
 
            P2(1)=-P2(2)
            SS0%SS(I)=P2(SS0%RNH)       
            P0=P1
            P1=P2            
        END DO 
        
        CLOSE(1)
        DEALLOCATE(P0,P1,P2,DER,CN,STAT=OK)
        CALL ERR_STOP(OK,0,'ALLOCATION ERROR')
        
        IF ((VERB=='y').or.(VERB=='Y')) write(*,*) 'Modelling done' 
    END SUBROUTINE FD_ACOUSTIC

    
    !############################################## 
    SUBROUTINE FD_BUILD_MODEL(M)
        IMPLICIT NONE 
        TYPE(MODEL),INTENT(INOUT):: M 
        INTEGER :: I,OK
        INTEGER, ALLOCATABLE:: ZN(:)
        !---------------------------------
        ALLOCATE(ZN(M%NLAYER),STAT=OK)
        CALL ERR_STOP(OK,0,'ALLOCATION ERROR')     
        
        ZN = ceiling(M%DEPTH/M%DH)
        ZN= ZN+1;       !ADJUST FOR THE SURFACE LAYER
        M%VP(1:ZN(1)) = M%VPL(1)
        DO I=1,M%NLAYER-1
             M%VP(ZN(I):ZN(I+1)) = M%VPL(I+1)
        END DO
             
        DEALLOCATE(ZN,STAT=OK)
        CALL ERR_STOP(OK,0,'DEALLOCATION ERROR')
    END SUBROUTINE FD_BUILD_MODEL

    
    !##############################################
    SUBROUTINE FD_BUILD_SOURCE(SRC)
        IMPLICIT NONE 
        TYPE(SOURCE),INTENT(INOUT):: SRC
        INTEGER :: I,OK
        REAL:: PI
        REAL,ALLOCATABLE:: PFT(:)
        
        PI= 4*ATAN(1.0)        
        DO I=1,SRC%NT
            SRC%TIME(I)=(I-1)*SRC%DT
        END DO
        
        ALLOCATE(PFT(SRC%NT),STAT=OK)
        CALL ERR_STOP(OK,0,'ALLOCATION ERROR IN SOURCE')
        
        PFT= PI*SRC%F0*(SRC%TIME-SRC%T0)
        SRC%SIG= (1-2*PFT)*EXP(-PFT**2) 

        DEALLOCATE(PFT,STAT=OK)              
        CALL ERR_STOP(OK,0,'DEALLOCATION ERROR IN SOURCE')
    END SUBROUTINE FD_BUILD_SOURCE


END MODULE MOD_ACOUSTIC_FD1D