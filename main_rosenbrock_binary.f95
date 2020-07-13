include 'mod_allocate_mat_vec.f95'
include 'mod_ga.f95'

program main_new
    use mod_allocate_mat_vec
    use mod_ga
    implicit none
    type(ga_param):: p
    type(chromosome),allocatable:: gen1(:),gen2(:),tempgen(:)
    type(chromosome):: mate1, mate2, child1, child2,fittest
    integer :: i,j,ok,flag1,flag2
    character(len=15):: temp

    !%%%%%%%%%%%%%%%%%%% GA inversion %%%%%%%%%%%%%%%%%%%
    call random_seed() 
    p%npop = 20
    p%ngen=200
    p%unk=2
    p%pmute=.05
    p%pcross=.8 
    p%acc=(/.0001,.0001/)
    p%min=(/-3.,-3./)
    p%max=(/3.,3./)
    !constraints on parameters, inequilities    lb < p*x < ub        
    call ga_init_param(p, verb='n')    
    call ga_print_param(p)
  
    ! initialize first generation
    allocate(gen1(p%npop),gen2(p%npop),tempgen(p%npop),stat=ok) 
    call ga_pop_init(gen1,p,verb='n',str='~~~~~~~~ gen1 init ~~~~~~~~');     !call ga_print_generation(gen1,p)
    call ga_pop_init(gen2,p,verb='n',str='~~~~~~~~ gen2 init ~~~~~~~~')
    call ga_pop_init(tempgen,p,verb='n',str='~~~~~~~~ tempgen init ~~~~~~~~')
    do i=1,p%npop
         call fitness_calc(gen1(i),p,'y','')    !call ga_print_individual(gen1(i),p)
    end do

    call ga_total_fitness(gen1,p,'n')
    !call ga_print_generation(gen1,p, '################ first gen #############')                
    
    !initialize the other mates,  childrens & generation
    mate1=gen1(1)      
    mate2=gen1(1)
    child1=gen1(1)
    child2=gen1(1)             !     gen2=gen1
    fittest=gen1(1)

    do j=2,p%ngen
        print*,''
        print*, '~~~~~~~~~~~~~~ Generation :', j,'~~~~~~~~~~~~~~'
        do i=1,p%npop,2  
            ! i=1
            call ga_selection(mate1,gen1,p,verb='n',str=' ******mate1 ******')  
            call ga_selection(mate2,gen1,p,verb='n',str=' ******mate2 ******')  
            ! !check if mate1 and mate2 are identical 
                    
            ! ! !perform crossover if p<pcross
            ! !call ga_crossover(mate1,mate2,p,child1,child2,verb='n',str='****** in crossover ******')  
            call ga_crossover_multi(mate1,mate2,p,child1,child2,verb='n',str='****** in crossover ******')  
            
            !perform mutation if p<pmute            
            call ga_mutation(child1,p,verb='n',str=' ******mutation 1 ******')
            call ga_mutation(child2,p,verb='n',str=' ******mutation 2 ******')                 
            
                
            !calculate fitness        
            call fitness_calc(child1,p,'n','in fitness 1')
            call fitness_calc(child2,p,'n','in fitness 2')    

            call ga_copy_chrom(child1,gen2(i))
            call ga_copy_chrom(child2,gen2(i+1))
        end do
        call ga_elitism(gen2,gen1,p,2)
        call ga_copy_gen(gen2,gen1)

        call ga_sorting(tempgen,gen2,p)
        write(temp,*) j
        call ga_print_individual(tempgen(1),p,' gen fittest'//temp)
        !call ga_print_generation(tempgen,p)
    end do


  !***************************************************
    contains  
        
    ! !###################################################
    subroutine fitness_calc(ind,p,verb,str)
        implicit none
        type(chromosome),intent(inout)  :: ind
        type(ga_param),intent(in)       :: p
        character(len=*), intent(in)    :: verb,str
      
        integer :: j
        real :: x,y,a,b,f

        a= 1.
        b= 100.   
        x=ind%phenotype(1)
        y=ind%phenotype(2)
             
        !f=x**2 +y**2
        f=(a-x)**2.  +  b*(y-x**2.)**2.     
        ind%fitness= 1./(f+tiny(x))
    end subroutine fitness_calc    


    

end program main_new