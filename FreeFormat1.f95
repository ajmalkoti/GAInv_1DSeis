INCLUDE 'MOD_READ_WRITE.F95'
INCLUDE 'MOD_ACOUSTIC_FD1D.F95' 
INCLUDE 'MOD_ALLOCATE_MAT_VEC.F95' 
INCLUDE 'MOD_PLOTTING_GNU.F95' 
INCLUDE 'MOD_GA.F95'
INCLUDE 'DISPMODULE.F90'

PROGRAM GA_FWI
    USE MOD_READ_WRITE
    USE MOD_ACOUSTIC_FD1D
    USE MOD_ALLOCATE_MAT_VEC
    USE MOD_PLOTTING_GNU
    USE MOD_GA
    USE DISPMODULE
    
    IMPLICIT NONE
    TYPE(MODEL) :: M0,M1
    TYPE(SOURCE):: SRC
    TYPE(SS):: SS0,SS1

    TYPE(GA_PARAM) :: P
    TYPE(CHROMOSOME),ALLOCATABLE :: GEN1(:) , GEN2(:)
    TYPE(CHROMOSOME) :: MATE1,MATE2,CHILD(2), FITTEST,ELITE
    
    REAL,ALLOCATABLE  :: TEMP1(:)
    INTEGER :: I,OK, NoInEQ
    
    
    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    !%%%%%%%%%%%%%%%%%%%%%%%%%%% FD_SYNTHETIC 

    ! READ MODEL PARAMETERS
    CALL READ_VAR(FNAME='IP_DATA.txt',VARNAME='NLAYERS',VAL=M0%NLAYER,VERB='Y')    !CALL READ_VAR(FNAME='IP_DATA.txt',LINENO=4,VAL=I1,VERB='Y')
	CALL READ_VAR_REAL5(FNAME='IP_DATA.txt',NAME1='H',VAL1=M0%H,&
                                            NAME2='DH',VAL2=M0%DH,VERB='Y')   

    M0%NH= CEILING(M0%H/M0%DH) +1 
    CALL ALLOCATE_VEC_REAL5(STR1='DEPTH',VEC1=M0%DEPTH,&
    							  STR2='VPL'  ,VEC2=M0%VPL,&
                                  STR3='VP'  , VEC3=M0%VP,  N=M0%NLAYER)           ! CALL ALLOCATE_VEC('DEPTH',VEC=M0%DEPTH,N=M0%NLAYER)
        
    CALL READ_VAR(FNAME='IP_DATA.txt',VARNAME='DEPTH',VAL=M0%DEPTH,VERB='Y')      
    CALL READ_VAR(FNAME='IP_DATA.txt',VARNAME='VPL',VAL=M0%VPL,VERB='Y')
    
    CALL FD_BUILD_MODEL(M0)
    
!$$$$$$     !%%%%%%%%%%%%%%% READ SOURCE PARAMETERS 
    CALL READ_VAR(FNAME='IP_DATA.txt',VARNAME='SNH',VAL=SRC%SNH,VERB='Y')
	CALL READ_VAR_REAL5(FNAME='IP_DATA.txt',NAME1='T',VAL1=SRC%T,&
                                            NAME2='DT',VAL2=SRC%DT,&
                                            NAME3='F0',VAL3=SRC%F0,&
                                            NAME4='T0',VAL4=SRC%T0,VERB='Y') 

    SRC%NT=CEILING(SRC%T/SRC%DT)
    CALL ALLOCATE_VEC('SRC_SIG',VEC=SRC%SIG,N=SRC%NT)         
    CALL ALLOCATE_VEC('SRC_TIME',VEC=SRC%TIME,N=SRC%NT)         
    CALL FD_BUILD_SOURCE(SRC)
    
    CALL PLT_VEC(SRC%SIG,'SOURCE','T','AMPLITUDE',5)
!$$$$$$ 
!$$$$$$     !%%%%%%%%%%%%%%% READ SS PARAMETERS 
!$$$$$$     CALL READ_VAR(FNAME='IP_DATA.txt',VARNAME='RNH',VAL=SS0%RNH,VERB='Y')
!$$$$$$     
!$$$$$$     SS0%NT=SRC%NT
!$$$$$$     CALL ALLOCATE_VEC('SS0%SS',VEC=SS0%SS,N=SRC%NT)    
!$$$$$$     
!$$$$$$     CALL FD_ACOUSTIC(M0,SRC,SS0,'Y')
!$$$$$$     
!$$$$$$     !CALL PLT_VEC(SS0%SS,'SYNTHETIC SEISMOGRAM','T','AMPLITUDE',5)
!$$$$$$ 
!$$$$$$ 
!$$$$$$     !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!$$$$$$     !%%%%%%%%%%%%%%%%%%%%%%%%%%% GA 
!$$$$$$ 
!$$$$$$     PRINT*, '################################'
!$$$$$$     !WE NEED A NEW MODEL FOR GA SO LET US CREATE ONE 
!$$$$$$     CALL ALLOCATE_VEC('DEPTH',VEC=M1%DEPTH,N=M0%NLAYER)         
!$$$$$$     CALL ALLOCATE_VEC('VPL',VEC=M1%VPL,N=M0%NLAYER)
!$$$$$$     M1=M0
!$$$$$$     
!$$$$$$     CALL ALLOCATE_VEC('SS0%SS',VEC=SS1%SS,N=SRC%NT)
!$$$$$$     SS1=SS0
!$$$$$$ 
!$$$$$$     CALL RANDOM_SEED()
!$$$$$$ 
!$$$$$$     !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!$$$$$$     !%%%%%%%%%%%%%%%%%%%%%%%%%%% GA 
!$$$$$$ 
!$$$$$$     ! READ PARAMETERS
!$$$$$$     CALL READ_VAR(FNAME='IP_DATA.txt',VARNAME='NPOP',VAL=P%NPOP,VERB='Y')
!$$$$$$     CALL READ_VAR(FNAME='IP_DATA.txt',VARNAME='NGEN',VAL=P%NGEN,VERB='Y')  
!$$$$$$     CALL READ_VAR(FNAME='IP_DATA.txt',VARNAME='PMUTE',VAL=P%PMUTE,VERB='Y')
!$$$$$$     CALL READ_VAR(FNAME='IP_DATA.txt',VARNAME='PCROSS',VAL=P%PCROSS,VERB='Y') 
!$$$$$$     CALL READ_VAR(FNAME='IP_DATA.txt',VARNAME='UNK',VAL=P%UNK,VERB='Y')
!$$$$$$     CALL READ_VAR(FNAME='IP_DATA.txt',VARNAME='PARAMLIST',VAL=P%PARAMLIST,VERB='Y')
!$$$$$$ 
!$$$$$$     P%UNK= M0%NLAYER*SUM(P%PARAMLIST)-1   ! ONE LESS, SINCE TOTAL DEPTH = CONSTANT     
!$$$$$$ 
!$$$$$$     CALL ALLOCATE_VEC('GA_ACCVEC',VEC=P%ACC,N=P%UNK) 
!$$$$$$     CALL ALLOCATE_VEC('GA_MINVEC',VEC=P%MIN,N=P%UNK) 
!$$$$$$     CALL ALLOCATE_VEC('GA_MAXVEC',VEC=P%MAX,N=P%UNK)
!$$$$$$ 
!$$$$$$     CALL READ_VAR(FNAME='IP_DATA.txt',VARNAME='VACC',VAL=P%ACC(1:5),VERB='Y')
!$$$$$$     CALL READ_VAR(FNAME='IP_DATA.txt',VARNAME='VMIN',VAL=P%MIN(1:5),VERB='Y')
!$$$$$$     CALL READ_VAR(FNAME='IP_DATA.txt',VARNAME='VMAX',VAL=P%MAX(1:5),VERB='Y')
!$$$$$$     
!$$$$$$     ! NOTE WE ARE LODAING PARAMETERS FOR THICKNESS .NOT. DEPTH
!$$$$$$     CALL READ_VAR(FNAME='IP_DATA.txt',VARNAME='ZACC',VAL=P%ACC(6:9),VERB='Y')
!$$$$$$     CALL READ_VAR(FNAME='IP_DATA.txt',VARNAME='ZMIN',VAL=P%MIN(6:9),VERB='Y')
!$$$$$$     CALL READ_VAR(FNAME='IP_DATA.txt',VARNAME='ZMAX',VAL=P%MAX(6:9),VERB='Y')    
!$$$$$$ 
!$$$$$$     CALL ALLOCATE_VEC('NSAMPLES',VEC=P%NSAMPLES,N=P%UNK)
!$$$$$$     CALL ALLOCATE_VEC('CHROMLEN',VEC=P%CHROMLEN,N=P%UNK)
!$$$$$$     CALL ALLOCATE_VEC('POSITION',VEC=P%POS,N=P%UNK)
!$$$$$$     CALL GA_INIT_PARAM(P, VERB='Y')  
!$$$$$$     
!$$$$$$     CALL ALLOCATE_VEC('B2I_RULE',VEC=P%B2I_RULE,N=P%TCHROMLEN)
!$$$$$$     CALL GA_BINARY2INT_RULER(P,'Y')
!$$$$$$ 
!$$$$$$     ALLOCATE(GEN1(P%NPOP),GEN2(P%NPOP),STAT=OK)    
!$$$$$$     !NO OF INEQUILITIES    LB < P*X < UB
!$$$$$$     NoInEQ=5
!$$$$$$     CALL ALLOCATE_MAT('INEQ_MAT',MAT=P%INEQMV,NR=NoInEQ,NC=P%UNK) 
!$$$$$$     CALL ALLOCATE_VEC('UPPER_BOUND',VEC=P%INEQMV_UB, N=NoInEQ)  
!$$$$$$     
!$$$$$$     P%INEQMV(1,:)=(/0,0,0,0,0,1,1,1,1/)  ! THE SUM OF THICKNESS SHOULD BE LESS THAN TOTAL
!$$$$$$     P%INEQMV_UB(1)=M0%H-MINVAL(P%MIN(6:9))
!$$$$$$ 
!$$$$$$     P%INEQMV(2,:)=(/0,0,0,0,0,1,0,0,0/)  ! THICKNESS SHOULD BE LESS THAN 1000
!$$$$$$     P%INEQMV(3,:)=(/0,0,0,0,0,0,1,0,0/)  ! THICKNESS SHOULD BE LESS THAN 1000    
!$$$$$$     P%INEQMV(4,:)=(/0,0,0,0,0,0,0,1,0/)  ! THICKNESS SHOULD BE LESS THAN 1000    
!$$$$$$     P%INEQMV(5,:)=(/0,0,0,0,0,0,0,0,1/)  ! THICKNESS SHOULD BE LESS THAN 1000    
!$$$$$$     P%INEQMV_UB(2:5)=(/800,800,800,800/)
!$$$$$$ 
!$$$$$$     !CALL DISP(P%INEQMV,'F5.2',ZEROAS='.')
!$$$$$$ 
!$$$$$$     ! INITIALIZE FIRST GENERATION
!$$$$$$     CALL GA_POP_INIT(GEN1,GEN2,P) 
!$$$$$$     
!$$$$$$     DO I=1,P%NPOP
!$$$$$$         CALL THICK2DEPTH(GEN1(I),M1)    
!$$$$$$         CALL FD_BUILD_MODEL(M1)
!$$$$$$         CALL FD_ACOUSTIC(M1,SRC,SS1,'N')
!$$$$$$         !CALL PLT_VEC(SS0%SS-SS1%SS,'SYNTHETIC SEISMOGRAM','T','AMPLITUDE',2)     
!$$$$$$         GEN1(I)%FITNESS = SUM((SS0%SS-SS1%SS)**2)          
!$$$$$$     END DO
!$$$$$$     CALL GA_PRINT_GENERATION(GEN1,P)
!$$$$$$     
!$$$$$$ 
!$$$$$$     ! EVOLVE GENERATION 
!$$$$$$     DO I=1,P%NGEN    
!$$$$$$         !SELECT TWO MATE        
!$$$$$$         !PERFORM CROSSOVER IF P>PCROSS        
!$$$$$$     END DO
!$$$$$$ 
!$$$$$$     !PERFORM MUTATION
!$$$$$$     !FIND FITNESS OF NEW MEMBERS 

    !#################################################-
    !#################################################-
    CONTAINS
    SUBROUTINE THICK2DEPTH(IND,M)      
        IMPLICIT NONE
        TYPE(CHROMOSOME), INTENT(IN) :: IND
        TYPE(MODEL), INTENT(INOUT)     :: M
        
        INTEGER :: J
        M%DEPTH(1)= IND%PHENOTYPE(6)
        DO J=2,4
            M%DEPTH(J)= IND%PHENOTYPE(J+5)  +  M%DEPTH(J-1)
        END DO
    END SUBROUTINE THICK2DEPTH    

    !#################################################-
    
END PROGRAM GA_FWI