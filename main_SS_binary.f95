include 'mod_read_write.f95'
include 'mod_allocate_mat_vec.f95'
include 'mod_acoustic_fd1d.f95'
include 'mod_ga.f95'

program main_new
    use mod_read_write
    use mod_acoustic_fd1d
    use mod_allocate_mat_vec
    use mod_ga
    implicit none
    type(model) :: m0
    type(source):: src 
    type(SS)    :: ss0
    !----------------
    type(model) :: m1
    type(SS)    :: ss1
    type(ga_param):: p
    type(chromosome),allocatable:: gen1(:),gen2(:),tempgen(:)
    type(chromosome):: mate1, mate2, child1, child2
    integer :: i,j,ok,flag1,flag2
    character(len=15)::temp
    real :: x

    !%%%%%%%%%%%%%%%%%%% Forward modelling %%%%%%%%%%%%%%%%%%%
    !------------------- model creation -------------------
    call read_var_real5('ip_data.txt','H     ',m0%h,'DH    ',m0%dh,verb='Y')
    call read_var(fname='ip_data.txt',varname='NLAYER',val=m0%nlayer,verb='y')!5
    m0%nh= ceiling(m0%h/m0%dh) +1 
    call allocate_vec(vec=m0%depth,n=m0%nlayer,str='DEPTH')
    call allocate_vec(vec=m0%vpl,n=m0%nlayer,str='VPL')
    call allocate_vec(vec=m0%vp,n=m0%nh,str='VP VEC')          
    call read_var(fname='ip_data.txt',varname='DEPTH ',val=m0%depth,verb='y')      
    call read_var(fname='ip_data.txt',varname='VPL   ',val=m0%vpl,verb='y')   
    call fd_build_model(m0)        !call fd_print_model(m0)
    !------------------- source creation -------------------
    call read_var_real5('ip_data.txt','T     ',src%t,'DT    ',src%dt,'F0    ',src%f0,'T0    ',src%t0,verb='y')
    call read_var(fname='ip_data.txt',varname='SNH   ',val=src%snh,verb='y')
    src%nt=ceiling(src%t/src%dt)
    call allocate_vec(vec=src%sig,n=src%nt,STR='SOURCE SIGNATURE')         
    call allocate_vec(vec=src%time,n=src%nt,STR='SOURCE TIME')         
    call fd_build_source(src)       !call fd_print_source(src);        
    !------------------- SS parameters -------------------
    call read_var(fname='ip_data.txt',varname='RNH   ',val=ss0%rnh,verb='y')
    ss0%nt=src%nt  
    call allocate_vec(vec=ss0%ss,n=src%nt,str='ss0%ss')    
    !------------------- modelling -------------------
    call fd_acoustic(m0,src,ss0,'y')     !call fd_print_ss(ss0);            

    !minimum resolution corroespoinding to fastest velocity 
    !maximum resolution corroespoinding to slowest velocity  

    !%%%%%%%%%%%%%%%%%%% GA inversion %%%%%%%%%%%%%%%%%%%
    call random_seed() 
    m1=m0;    ! call fd_print_model(m0)   ! call fd_print_model(m1)    !works on 2003 or later version compilers
    ss1=ss0;  ! call fd_print_ss(ss1)         

    call read_var_int5('ip_data.txt','NPOP  ',p%npop,'NGEN  ',p%ngen,'UNK   ',p%unk,verb='n')
    call read_var_real5('ip_data.txt','PMUTE ',p%pmute,'PCROSS',p%pcross,verb='n')
    !call read_var(fname='ip_data.txt',varname='PARAMLIST',val=p%paramlist,verb='y')
    !p%unk= m0%nlayer*sum(p%paramlist)-1     ! n: velocity layers,  n-1: layer thickness, since total depth = constant     
    p%unk=9
    call allocate_vec(vec=p%acc,n=p%unk,str='ga_accvec') 
    call allocate_vec(vec=p%min,n=p%unk,str='ga_minvec') 
    call allocate_vec(vec=p%max,n=p%unk,str='ga_maxvec')
    ! note we are lodaing parameters for thickness .not. depth
    call read_var(fname='ip_data.txt',varname='VACC  ',val=p%acc(1:5),verb='n')
    call read_var(fname='ip_data.txt',varname='VMIN  ',val=p%min(1:5),verb='n')
    call read_var(fname='ip_data.txt',varname='VMAX  ',val=p%max(1:5),verb='n')
    call read_var(fname='ip_data.txt',varname='ZACC  ',val=p%acc(6:9),verb='n')
    call read_var(fname='ip_data.txt',varname='ZMIN  ',val=p%min(6:9),verb='n')
    call read_var(fname='ip_data.txt',varname='ZMAX  ',val=p%max(6:9),verb='n')     
    !constraints on parameters, inequilities    lb < p*x < ub
    call allocate_mat(mat=p%ineqmv,nr=2,nc=p%unk,str='ineq_mat') 
    call allocate_vec(vec=p%ineqmv_ub, n=2,str='upper_bound')   
    p%ineqmv(1,:)=(/0.,0.,0.,0.,0.,1.,1.,1.,1./)  ! the sum of thickness should be less than total
    p%ineqmv(2,:)=(/0.,0.,0.,0.,0.,1.,1.,1.,1./)
    p%ineqmv_ub(1)=m0%h-50.    !call disp(p%ineqmv,'f5.2',zeroas='.')
    p%ineqmv_ub(2)=m0%h-50.
    
    call ga_init_param(p, verb='n')    
    call ga_print_param(p)
  
    !initialize first generation
    allocate(gen1(p%npop),gen2(p%npop),tempgen(p%npop),stat=ok) 
    call ga_pop_init(gen1,p,verb='n',str='~~~~~~~~ gen1 init ~~~~~~~~');     !call ga_print_generation(gen1,p)
    call ga_pop_init(gen2,p,verb='n',str='~~~~~~~~ gen2 init ~~~~~~~~')
    call ga_pop_init(tempgen,p,verb='n',str='~~~~~~~~ tempgen init ~~~~~~~~')

    !For Testing only : gen1(1)%phenotype=(/1800.,1900.,2000.,2100.,2200.,400.,200.,250.,450./)
    do i=1,p%npop
        call fitness_calc(gen1(i),m1,src,ss0,ss1,p,'y','')    !call ga_print_individual(gen1(i),p)
    end do

    call ga_total_fitness(gen1,p,'n')
    call ga_print_generation(gen1,p, '################ first gen #############')                
    
    
    !initialize the other mates,  childrens & generation
    mate1=gen1(1)      
    mate2=gen1(1)
    child1=gen1(1)
    child2=gen1(1)             !     gen2=gen1

    do j=1,p%ngen
        print*, '~~~~~~~~~~~~~~ Generation :', j,'~~~~~~~~~~~~~~'
        do i=1,p%npop,2  
            !print*, '---------------------------'
            !print*, 'i= ', i, i+1
            do 
                call ga_selection(mate1,gen1,p,verb='n',str=' ******mate1 ******')  !call ga_print_individual(mate1,p)
                call ga_selection(mate2,gen1,p,verb='n',str=' ******mate2 ******')  !call ga_print_individual(mate1,p)
                !check if mate1 and mate2 are identical 
                        
                !perform crossover if p<pcross
                call ga_crossover_multi(mate1,mate2,p,child1,child2,verb='n',str='****** in crossover ******')  
                
                !perform mutation if p<pmute            
                call ga_mutation(child1,p,verb='n',str=' ******mutation 1 ******')
                call ga_mutation(child2,p,verb='n',str=' ******mutation 2 ******')                 
                  ! check for total thickness first
                call ga_check_param(child1,p,flag1,'n')  
                call ga_check_param(child2,p,flag2,'n')  
                if ((flag1==0).and.(flag2==0)) then 
                    exit
                end if
            end do  

            !calculate fitness        
            call fitness_calc(child1,m1,src,ss0,ss1,p,'n','in fitness 1')
            call fitness_calc(child2,m1,src,ss0,ss1,p,'n','in fitness 2')    

            call ga_copy_chrom(child1,gen2(i))
            call ga_copy_chrom(child2,gen2(i+1))
        end do
        call ga_elitism(gen2,gen1,p,1)
        call ga_copy_gen(gen2,gen1)

        call ga_sorting(tempgen,gen2,p)
        !call ga_print_generation(tempgen,p)
        write(temp,*) j
        call ga_print_individual(tempgen(1),p,' gen fittest'//temp)
    end do



  !***************************************************
    contains  
        
    !###################################################
    subroutine fitness_calc(ind,m,src,ss0,ss1,p,verb,str)
        implicit none
        type(source),intent(in)     :: src
        type(ga_param),intent(in)   :: p
        character(len=*), intent(in):: verb,str
        
        type(model),intent(inout)     :: m        
        type(ss),intent(inout)        :: ss0,ss1 
        type(chromosome),intent(inout):: ind
        integer :: j
        real :: x
        
        !print*, str

        m%vpl = ind%phenotype(1:5)
        do j=1,4            
            m%depth(j)= sum(ind%phenotype(6:j+5))  
        end do

  !     if ((verb=='Y').or.(verb=='y')) then
  !           write(*,'(//a27,9f10.2)') 'in fitness, ind%phenotype: ',ind%phenotype
  !           write(*,'(a27,5f10.2)') 'm%vpl: ', m%vpl
  !           write(*,'(a27,5f10.2)') 'm%depth: ', m%depth
  !      end if
        
        call fd_build_model(m);           !call fd_print_model(m1)
        call fd_acoustic(m,src,ss1,'n');  !call plt_vec(ss0%ss-ss1%ss,'the difference','t','amplitude',2)
        ind%fitness = 1./(sqrt(sum((ss0%ss-ss1%ss)**2))+10*tiny(x))
        !ind%fitness = -sqrt(sum((ss0%ss-ss1%ss)**2)/ss0%nt)

        !ind%fitness = sum(ss0%ss * ss1%ss)/sum(ss0%ss*ss0%ss+ ss1%ss*ss1%ss)   ! range of fitness .3- .5 only
        !ind%fitness = sum(ss0%ss * ss1%ss)/sum(ss0%ss*ss0%ss)                  ! wrong ind may get more fitness i.e. fittest fitness=1, wrong fitness > 1

        !if ((verb=='Y').or.(verb=='y')) call ga_print_individual(ind,p,'new fitness ')
    end subroutine fitness_calc    
end program main_new