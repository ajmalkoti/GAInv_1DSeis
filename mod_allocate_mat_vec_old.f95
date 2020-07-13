MODULE MOD_ALLOCATE_MAT_VEC

INTERFACE ALLOCATE_VEC
    MODULE PROCEDURE ALLOCATE_VEC_I
    MODULE PROCEDURE ALLOCATE_VEC_R
END INTERFACE ALLOCATE_VEC

INTERFACE ALLOCATE_MAT
    MODULE PROCEDURE ALLOCATE_MAT_I
    MODULE PROCEDURE ALLOCATE_MAT_R
END INTERFACE ALLOCATE_MAT

CONTAINS
    !######################################################    
    SUBROUTINE ALLOCATE_VEC_REAL5(STR1,VEC1,STR2,VEC2,STR3,VEC3,STR4,VEC4,STR5,VEC5,N)
        IMPLICIT NONE
        REAL, DIMENSION(:),INTENT(INOUT),ALLOCATABLE,OPTIONAL     :: VEC1,VEC2,VEC3,VEC4,VEC5
        CHARACTER(LEN=*), OPTIONAL,INTENT(IN)     :: STR1,STR2,STR3,STR4,STR5 
        INTEGER,INTENT(IN)                 :: N

        if (present(VEC1).and.present(STR1)) CALL ALLOCATE_VEC_R(STR=STR1,VEC=VEC1,N=N)
        if (present(VEC2).and.present(STR2)) CALL ALLOCATE_VEC(STR=STR2,VEC=VEC2,N=N)            
        if (present(VEC3).and.present(STR3)) CALL ALLOCATE_VEC(STR=STR3,VEC=VEC3,N=N)
        if (present(VEC4).and.present(STR4)) CALL ALLOCATE_VEC(STR=STR4,VEC=VEC4,N=N)
        if (present(VEC5).and.present(STR5)) CALL ALLOCATE_VEC(STR=STR5,VEC=VEC5,N=N)
               
    END SUBROUTINE ALLOCATE_VEC_REAL5
    
   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    SUBROUTINE ALLOCATE_VEC_I(STR,VEC,N)           
        IMPLICIT NONE
        INTEGER,INTENT(IN)            :: N
        CHARACTER(len=*),INTENT(IN)   ::  STR
        INTEGER, DIMENSION(:), ALLOCATABLE,INTENT(INOUT)     :: VEC
        INTEGER                       :: OK
                            
        ALLOCATE(VEC(N),STAT=OK)
        CALL ERR_STOP(OK,0,'ALLOCATION ERROR FOR "'// TRIM(STR)// '"')
        VEC=0
    END SUBROUTINE ALLOCATE_VEC_I


   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    SUBROUTINE ALLOCATE_VEC_R(STR,VEC,N)           
        IMPLICIT NONE
        INTEGER,INTENT(IN)            :: N
        CHARACTER(len=*),INTENT(IN)   ::  STR
        REAL, DIMENSION(:), ALLOCATABLE,INTENT(INOUT)     :: VEC
        INTEGER                       :: OK
                            
        ALLOCATE(VEC(N),STAT=OK)
        CALL ERR_STOP(OK,0,'ALLOCATION ERROR FOR "'// TRIM(STR)// '"')
        VEC=0.0
    END SUBROUTINE ALLOCATE_VEC_R

    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    SUBROUTINE ALLOCATE_MAT_I(STR,MAT,NR,NC)
        IMPLICIT NONE
        INTEGER,INTENT(IN)            ::  NR,NC
        CHARACTER(len=*),INTENT(IN)   ::  STR
        INTEGER, DIMENSION(:,:), ALLOCATABLE,INTENT(INOUT)   ::  MAT            
        INTEGER                       ::  OK
        
        ALLOCATE(MAT(NR,NC),STAT=OK)
        CALL ERR_STOP(OK,0,'ALLOCATION ERROR FOR "'// TRIM(STR)// '"')
        MAT=0
   END SUBROUTINE ALLOCATE_MAT_I
   
    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    SUBROUTINE ALLOCATE_MAT_R(STR,MAT,NR,NC)
        IMPLICIT NONE
        INTEGER,INTENT(IN)            ::  NR,NC
        CHARACTER(len=*),INTENT(IN)   ::  STR
        REAL, DIMENSION(:,:), ALLOCATABLE,INTENT(INOUT)   ::  MAT            
        INTEGER                       ::  OK
        
        ALLOCATE(MAT(NR,NC),STAT=OK)
        CALL ERR_STOP(OK,0,'ALLOCATION ERROR FOR "'// TRIM(STR)// '"')
        MAT=0.0
   END SUBROUTINE ALLOCATE_MAT_R
   
   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   SUBROUTINE ERR_STOP(VAR,EQVAL,STR1)
        INTEGER, INTENT(IN) :: VAR, EQVAL
        CHARACTER(LEN=*),INTENT(IN):: STR1 
        IF (VAR/=EQVAL) THEN  
            PRINT*, STR1
            STOP
        END IF
    END SUBROUTINE ERR_STOP
END MODULE MOD_ALLOCATE_MAT_VEC