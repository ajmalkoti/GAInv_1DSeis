MODULE MOD_PLOTTING_GNU
CONTAINS
    !######################################################
    SUBROUTINE PLT_VEC(VEC,TITLE,XLABEL,YLABEL,WTTIME)
        IMPLICIT NONE
        
        REAL, INTENT(IN)               :: VEC(:)
        CHARACTER(LEN=*), INTENT(IN),OPTIONAL   :: TITLE,XLABEL,YLABEL
        INTEGER, INTENT(IN), OPTIONAL           :: WTTIME
  
        OPEN(UNIT=20,FILE='TEMP.DAT',ACTION='WRITE')
        CALL WRITE_VEC(VEC,'TEMP.DAT')
        CLOSE(20)
        CALL PLT_VEC_F('TEMP.DAT',TITLE,XLABEL,YLABEL,WTTIME)
    END SUBROUTINE PLT_VEC
    
    !######################################################
    SUBROUTINE PLT_MAT(MAT,TITLE,XLABEL,YLABEL,WTTIME)
        IMPLICIT NONE
        REAL, INTENT(IN)                :: MAT(:,:)
        CHARACTER(LEN=*), INTENT(IN),OPTIONAL   :: TITLE,XLABEL,YLABEL
        INTEGER, INTENT(IN), OPTIONAL           :: WTTIME

        
        OPEN(UNIT=20,FILE='TEMP.DAT',ACTION='WRITE')
        CALL WRITE_MAT(MAT,'TEMP.DAT')
        CLOSE(20)
        CALL PLT_MAT_F('TEMP.DAT',TITLE,XLABEL,YLABEL,WTTIME)
    END SUBROUTINE PLT_MAT   
    
    !######################################################    
    SUBROUTINE PLT_VEC_F(FNAME,TITLE,XLABEL,YLABEL,WTTIME)
        IMPLICIT NONE
        CHARACTER(LEN=*), INTENT(IN)        :: FNAME
        CHARACTER(LEN=*), INTENT(IN),OPTIONAL   :: TITLE,XLABEL,YLABEL
        INTEGER, INTENT(IN), OPTIONAL           :: WTTIME
        
        OPEN(UNIT=20,FILE='TEMP.GNU',ACTION='WRITE')
        IF (PRESENT(TITLE)) WRITE(20,*) 'set title "',TITLE,'"'
        IF (PRESENT(XLABEL)) WRITE(20,*) 'set xlabel "',XLABEL,'"'          
        IF (PRESENT(YLABEL)) WRITE(20,*) 'set ylabel "',YLABEL,'"'
        !WRITE(20,*) 'set yrange [:] reverse '  
        WRITE(20,*) 'plot "',FNAME,'" using 1 with lines'
        IF (PRESENT(WTTIME)) WRITE(20,*) 'pause(',WTTIME,')'
        CLOSE(20)
        CALL SYSTEM('gnuplot TEMP.GNU')    
    END SUBROUTINE PLT_VEC_F
    
    !#####################################################
    SUBROUTINE PLT_MAT_F(FNAME,TITLE,XLABEL,YLABEL,WTTIME)
        IMPLICIT NONE
        CHARACTER(LEN=*)            :: FNAME
        CHARACTER(LEN=*),OPTIONAL   :: TITLE,XLABEL,YLABEL
        INTEGER, OPTIONAL           :: WTTIME

        
        OPEN(UNIT=20,FILE='TEMP.GNU',ACTION='WRITE')
        IF (PRESENT(TITLE)) WRITE(20,*) 'set title "',TITLE,'"'
        IF (PRESENT(XLABEL)) WRITE(20,*) 'set xlabel "',XLABEL,'"'          
        IF (PRESENT(YLABEL)) WRITE(20,*) 'set ylabel "',YLABEL,'"'
        WRITE(20,*) 'set yrange [:] reverse '  
        WRITE(20,*) 'plot "',FNAME,'" matrix with image'
        IF (PRESENT(WTTIME)) WRITE(20,*) 'pause(',WTTIME,')'
        CLOSE(20)
        CALL SYSTEM('gnuplot TEMP.GNU')    
    END SUBROUTINE PLT_MAT_F    


    !######################################################
    SUBROUTINE WRITE_VEC(VEC,FILENAME)
        IMPLICIT NONE
        REAL,DIMENSION(:), INTENT(IN) :: VEC
        CHARACTER(LEN=*), INTENT(IN) :: FILENAME
        INTEGER         :: I
 
        OPEN(UNIT=20,FILE=FILENAME,ACTION='WRITE')
        DO I=1,SIZE(VEC,1)
            WRITE(20,'(F22.15)', ADVANCE='NO') VEC(I)
            WRITE(20,*) ''
        END DO
        CLOSE(20)
    END SUBROUTINE WRITE_VEC
    

    !######################################################
    SUBROUTINE WRITE_MAT(MAT,FILENAME)
        IMPLICIT NONE
        REAL,DIMENSION(:,:), INTENT(IN) :: MAT
        CHARACTER(LEN=*), INTENT(IN) :: FILENAME
        INTEGER         :: I,J
 
        OPEN(UNIT=20,FILE=FILENAME,ACTION='WRITE')
        DO I=1,SIZE(MAT,1)
            DO J=1,SIZE(MAT,2)
                WRITE(20,'(F22.15)', ADVANCE='NO') MAT(I,J)
            END DO
            WRITE(20,*) ''
        END DO
        CLOSE(20)
    END SUBROUTINE WRITE_MAT


END MODULE MOD_PLOTTING_GNU