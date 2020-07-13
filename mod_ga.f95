!constrain relaxed ! pramlist and nparam should not be a part of this module
!constrain relaxed ! parameter checking should not be a part of this module 

module mod_ga
    implicit none
    !#################################################    
    type chromosome
        integer,allocatable ::  genotype(:)
        integer,allocatable ::  geno2int(:)
        real,allocatable    ::  phenotype(:)         
        real    ::  fitness      
    end type chromosome
    
   !-------------------------------------------------
    type ga_param
        integer :: npop, ngen
        !integer :: paramlist(3), nparam  ! which parameters vel, density, depth, 
        integer :: unk                   ! total number of unknowns
        integer :: tchromlen             ! 
        integer :: nelite=2   
        real    :: pmute=1./20.,  pcross=0.8, total_fitness
        integer,allocatable,dimension(:) ::  b2i_rule
        real,allocatable,dimension(:)    ::  acc,min,max  ! accuracy, min, max-->(nparam,unk)
        integer,allocatable,dimension(:) ::  nsamples, chromlen,pos
        real,allocatable  :: ineqmv(:,:), ineqmv_ub(:)                 
        real,allocatable  :: eqmv(:,:), eqmv_r(:)        
    end type ga_param
    
    private:: err_stop, print_a_vec_int,print_a_vec_real

    !#################################################-
    !#################################################-
    contains
    !#################################################-    
    !#################################################-

    subroutine err_stop(var,eqval,str1)
        integer, intent(in) :: var, eqval
        !character(len=100),intent(in):: str1 
        character(len=*),intent(in):: str1 
        if (var/=eqval) then  
            print*, str1
            !stop        !enabling it may signal ieee_underflow_flag ieee_denormal  exception
        end if
    end subroutine err_stop

    !#################################################-
    subroutine ga_print_param(p)
        implicit none
        type(ga_param) :: p
        integer :: i
        print*, ' '
        print*, 'GA parameters: -----------------------'
        write(*,'(A15,I16)') 'NPOP         :', p%npop
        write(*,'(A15,I16)') 'NGEN         :', p%NGEN
        write(*,'(A15,I16)') 'NELITE       :', p%nelite
        write(*,'(A15,F16.5)') ' PMUTE        :', p%pmute
        write(*,'(A15,F16.5)') ' PCROSS       :', p%pcross
        write(*,'(A15,I16)') 'UNK          :', p%unk
        write(*,'(A15,I16)') 'TCHROMLEN    :', p%tchromlen
        call print_a_vec_int('CHROMLEN     :',p%chromlen,'(i16)')
        call print_a_vec_int('POS          :',p%pos,'(i16)')
           
        write(*,'(A15,F16.5)') ' TOTAL FITNESS:', p%total_fitness
        !call print_a_vec_int(' B2I_RULE     :',p%b2i_rule,'(i4)')
        
        call print_a_vec_real('ACC          :', p%acc,'(f16.5)')        
        call print_a_vec_real('MIN          :', p%min,'(f16.5)')
        call print_a_vec_real('MAX          :', p%max,'(f16.5)')
        !print*, 'ACC          :', p%acc
        !print*, 'MIN          :', p%min
        !print*, 'MAX          :', p%max
        call print_a_vec_int('NSAMPLES     :', p%nsamples,'(i16)')
        !print*, 'NSAMPLES     :', p%nsamples
        print*,''
        print*, 'INEQMV       :', p%ineqmv
        print*, 'INEQMV_UB    :', p%ineqmv_ub
        print*, 'EQMV         :', p%eqmv
        print*, 'EQMV_R       :', p%eqmv_r
        WRITE(*,*) ''

        ! write(*,'(a15)',advance='no' )  ' CHROMLEN     :     '
        ! if (allocated(p%chromlen)) then
        !     do i=1,size(p%chromlen)
        !         write(*,'(i4)',advance='no') p%chromlen(i)    
        !     end do
        !     write(*,*) ''
        ! else  
        !     print*, 'Not allocated'
        ! end if
    end subroutine ga_print_param

    !#################################################-
    subroutine print_a_vec_int(str,vec,fmt)
        integer, intent(in),allocatable:: vec(:)
        character(len=*),intent(in):: str,fmt
        integer:: i

        write(*,'(a15)',advance='no' )  str
        if (allocated(vec)) then
            do i=1,size(vec)
                write(*,fmt,advance='no') vec(i)   
            end do
            write(*,*) ''
        else  
            print*, 'Not allocated...!!!!'
        end if
    end subroutine print_a_vec_int

    !#################################################-
    subroutine print_a_vec_real(str,vec,fmt)
        real, intent(in),allocatable:: vec(:)
        character(len=*),intent(in):: str,fmt
        integer:: i

        write(*,'(a15)',advance='no' )  str
        if (allocated(vec)) then
            do i=1,size(vec)
                write(*,fmt,advance='no') vec(i)   
            end do
            write(*,*) ''
        else  
            print*, 'Not allocated'
        end if
    end subroutine print_a_vec_real

    !#################################################-
    subroutine ga_init_param(p, verb)
        implicit none 
        type(ga_param), intent(inout) :: p
        character(len=*), optional :: verb
        integer :: i,k, ok
        
        if ((verb=='y').or.(verb=='Y')) write(*,'(/a10)') 'in ga_init'

        allocate(p%nsamples(p%unk),p%chromlen(p%unk),p%pos(p%unk),stat=ok)
        call err_stop(ok,0,'error in allocation in ga_init_param: 0')
        ! find chromosome length for each variable 
        p%nsamples= ceiling((p%max-p%min)/p%acc)             
        do i=1,p%unk
            k=0
            do 
                if ((2**k)>p%nsamples(i)) then 
                    p%chromlen(i)=k
                    exit
                else 
                k=k+1
                end if
            end do               
        end do      
        
        if ((verb=='y').or.(verb=='Y')) then
          write(*,'(a25)',advance='no') 'lenght of each chrom:    '
          do i=1,size(p%chromlen,1)
              write(*,'(i5)',advance='no') p%chromlen(i)  
          end do      
          write(*,*)
        end if
        
        !find position of each var in chromosome 
        p%pos(1)=1
        do i=2,p%unk
            p%pos(i)= p%pos(i-1) + p%chromlen(i-1)          
        end do

        if ((verb=='y').or.(verb=='Y')) then
          write(*,'(a25)',advance='no') 'position of each chrom:    '
          do i=1,size(p%chromlen,1)
              write(*,'(i5)',advance='no') p%pos(i)  
          end do      
          write(*,*)
        end if

        p%tchromlen=sum(p%chromlen) 

        if ((verb=='y').or.(verb=='Y')) write(*,'(a25,i5)') 'total lenght of chrom:  ', p%tchromlen

        allocate(p%b2i_rule(p%tchromlen),stat=ok)  
        call err_stop(ok,0,'error in allocation in ga_init_param: 1') 
        
        call ga_binary2int_ruler(p,verb)        
    end subroutine ga_init_param


    !#################################################
    subroutine ga_binary2int_ruler(p,verb)
        implicit none
        type(ga_param), intent(inout)    :: p
        character(len=*),optional :: verb
        integer :: i,j      
        !find the ruler to convert binary into integer      
        !part:1
        p%b2i_rule=1
        if ((verb=='y').or.(verb=='Y')) print*, 'b2a rule'!,p%b2i_rule
        do i=1,p%unk-1
            !print*, p%pos(i),p%pos(i+1)-1
            do j=p%pos(i),(p%pos(i+1)-1)
                 p%b2i_rule(j)= 2**(p%pos(i+1)-j-1)
            end do                      
            if ((verb=='y').or.(verb=='Y')) print*, p%b2i_rule(p%pos(i):(p%pos(i+1)-1)) 
        end do
  
        !part:2 (end)
        do j=p%pos(p%unk),p%tchromlen
            p%b2i_rule(j)= 2**(p%tchromlen-j)
        end do                      
        if ((verb=='y').or.(verb=='Y'))  print*, p%b2i_rule(p%pos(p%unk):p%tchromlen)        
    end subroutine ga_binary2int_ruler



    !#################################################
     subroutine ga_pop_init(gen,p,verb,str)
        implicit none
        type(chromosome), intent(inout) ::  gen(:)
        type(ga_param), intent(inout)   ::  p
        character(len=*),optional :: verb,str

        integer             :: i,j=0,ok 
        
        do i=1,p%npop
            allocate(gen(i)%genotype(p%tchromlen), gen(i)%geno2int(p%unk),& 
                     gen(i)%phenotype(p%unk),      stat=ok)        
            call err_stop(ok,0,'error in allocation')   
        end do

        do i=1,p%npop
            !print*,'i = ', i
            do  
                call ga_generate_genotype(gen(i),p)
                call ga_generate_geno2int(gen(i),p)
                call ga_generate_phenotype(gen(i),p) 
                call ga_check_param(gen(i),p,ok,verb)
                if (ok==0) exit
                !j=j+1
                !if (j>50) exit

             end do 
             gen(i)%fitness=0.
             !call ga_print_individual(gen(i),p)
             !print*, 'total depth: ', sum(gen(i)%phenotype(6:9))
        end do        
        

        if (present(str)) print*, str
        if ((verb=='y').or.(verb=='Y')) call ga_print_generation(gen,p,str='----------- initializing the population -----------')
    end subroutine ga_pop_init
   

    !#################################################
    subroutine ga_check_param(ind,p,flag,verb)
        implicit none
        type(chromosome),intent(in):: ind
        type(ga_param) ,intent(in):: p 
        character(len=*),intent(in):: verb
        integer, intent(out):: flag
        integer :: test(3)=1
        real:: var(P%unk,1)
        !0=ok,          1=error

        ! if ((verb=='y').or.(verb=='Y')) then
        !     print*, 'ineqmv allocated: ',allocated(p%ineqmv)
        !     print*, 'ineqmv_ub allocated: ',allocated(p%ineqmv_ub)
        !     print*, 'eqmv allocated: ', allocated(p%eqmv)
        !     print*, 'eqmv_r allocated: ', allocated(p%eqmv_r)
        ! end if
        !----------------------------------
        flag=1

        if (all(ind%phenotype > p%min).or. all(ind%phenotype <p%max))  test(1)=0

        if (allocated(p%ineqmv).eqv..true.) then   
            !print*,'in check fun, Total thickness: ', matmul(p%ineqmv,ind%phenotype)
            if (all(matmul(p%ineqmv,ind%phenotype)<=p%ineqmv_ub).eqv..true.) then 
                test(2)=0
            else 
                test(2)=1
            end if
        else
            test(2)=0
        end if
        !------------------------------------------------
        if (allocated(p%eqmv).eqv..true.) then   

            if (all(matmul(p%eqmv,ind%phenotype)== p%eqmv_r).eqv..true.) then
               test(3)=0
           else
                test(3)=1
            end if
        else
            test(3)=0
        end if
        !------------------------------------------------
        if (all(test==0).eqv. .true.) then 
            flag=0
        else
            flag =1
        end if
        !if ((verb=='y').or.(verb=='Y')) print*, 'totla depth: ', matmul(p%ineqmv,ind%phenotype)
        if ((verb=='y').or.(verb=='Y')) print*, 'parameter check status flag=',flag, test
    end subroutine ga_check_param

    !#################################################-
    subroutine ga_generate_genotype(ind,p)
        implicit none
        type(chromosome),intent(inout) ::  ind
        type(ga_param),intent(in)      ::  p
        integer     :: bit
        real        :: rnd 

        do bit=1,p%tchromlen
            call random_number(rnd)
            if (rnd>.5) then
                ind%genotype(bit)=1
            else
                ind%genotype(bit)=0
            end if
        end do              
    end subroutine ga_generate_genotype

    !#################################################-
    subroutine ga_generate_geno2int(ind,p)
        implicit none
        type(chromosome),intent(inout) ::  ind
        type(ga_param),intent(in)      ::  p
        integer :: i
        real,allocatable :: temp(:)

        allocate(temp(p%tchromlen))

        temp =ind%genotype* p%b2i_rule
        do i=1,p%unk-1
            ind%geno2int(i)= sum(temp(p%pos(i):(p%pos(i+1)-1))) 
        end do

        i=p%unk   
        ind%geno2int(i)= sum(temp(p%pos(i):p%tchromlen)) 
        deallocate(temp)
        !print*, ind%geno2int
    end subroutine ga_generate_geno2int
    

    !#################################################-
    subroutine ga_generate_phenotype(ind,p)
        implicit none
        type(chromosome),intent(inout) ::  ind
        type(ga_param),intent(in)      ::  p

        ind%phenotype= p%min  +  ind%geno2int*(p%max-p%min)/(2**(p%chromlen)-1)                 
    end subroutine ga_generate_phenotype


    !#################################################-
    subroutine ga_print_generation(gen,p,str)  
        implicit none
        type(chromosome),intent(inout) ::  gen(:)
        type(ga_param),intent(in)      ::  p
        character(len=*),optional, intent(in)::str
        
        integer :: i,n
        character(len=20) :: str1,frmt1,frmt2,frmt3
        
          
        n=size(gen)
        write(str1,*) p%tchromlen
        frmt1='(a15,'//trim(adjustl(str1))//'i2)' 
        
        write(str1,*) p%unk
        frmt2='(a15,'//trim(adjustl(str1))//'i15)' 
        frmt3='(a15,'//trim(adjustl(str1))//'f15.7)' 

        if (present(str)) print*,str    
        do i=1,n  
            write(*,*) ''
            write(*,'(a15,i3,10x,a9,E20.8)') 'individual   :  ', i, 'fitness: ' , gen(i)%fitness
            write(*,frmt1) 'genotype :  ', gen(i)%genotype
            write(*,frmt2) 'geno2int :  ', gen(i)%geno2int
            write(*,frmt3) 'phenotype : ', gen(i)%phenotype  
        end do
        write(*,*) ''                  
    end subroutine ga_print_generation  

    !#################################################-
    subroutine ga_print_individual(individual,p,str)  
        implicit none
        type(chromosome),intent(inout) ::  individual
        type(ga_param),intent(in)      ::  p
        character(len=*),optional, intent(in)::str

        character(len=20) :: str1,frmt1,frmt2,frmt3
        
        write(str1,*) p%tchromlen
        frmt1='(a15,'//trim(adjustl(str1))//'i2)' 
        
        write(str1,*) p%unk
        frmt2='(a15,'//trim(adjustl(str1))//'i15)' 
        frmt3='(a15,'//trim(adjustl(str1))//'f15.7)' 
        
        !print*, 'format : ', str1,frmt1
        write(*,*) ''  
        if (present(str)) print*,str
        print*, 'individual fitness: ' , individual%fitness
        write(*,frmt1) 'genotype :  ', individual%genotype
        write(*,frmt2) 'geno2int :  ', individual%geno2int
        write(*,frmt3) 'phenotype : ', individual%phenotype  
                
    end subroutine ga_print_individual

    
    !#################################################-

    subroutine ga_total_fitness(gen,p,verb)
        implicit none 
        type(chromosome), intent(in) :: gen(:)
        type(ga_param), intent(inout):: p
        character(len=*), intent(in),optional:: verb
        
        integer :: i
        p%total_fitness=0.
        do i=1, p%npop
            p%total_fitness=p%total_fitness + gen(i)%fitness
            !print*,'current',i,gen(i)%fitness,'total=',p%total_fitness
        end do

        if (present(verb))then 
            if ((verb=='y').or.(verb=='Y')) then
                write(*,'(/a15,f15.7)') 'total fitness= ',p%total_fitness
            end if
        end if
    end subroutine ga_total_fitness


    !#################################################-
    subroutine ga_selection(mate,gen,p,verb,str)
        implicit none
        type(chromosome),intent(in)    :: gen(:)
        type(ga_param), intent(in)     :: p 
        type(chromosome),intent(out)   :: mate
        character(len=*), intent(in),optional :: verb,str

        integer    :: i
        real       :: part_sum=0.,rnd,  roulette_wheel 
        character  :: verb_i='n'       
        
        
        call random_number(rnd)             
        roulette_wheel=rnd*p%total_fitness
        
        if (present(verb)) verb_i=verb
        if ((verb_i=='y').or.(verb_i=='Y')) &
        write(*,'(a37,f10.5,a,f10.5)') 'roulette wheel select/total fitness= ',roulette_wheel,' / ',p%total_fitness  
      
        i=0
        part_sum=0.
        do 
            i=i+1
            if (i==p%npop)then
               mate = gen(i) 
            exit
            end if   
            
            part_sum = part_sum + gen(i)%fitness
            if ((verb_i=='y').or.(verb_i=='Y'))  print*, 'part_sum: ',i,part_sum, gen(i)%fitness        !tbc
          
            if(part_sum>=roulette_wheel) then
              call ga_copy_chrom(gen(i),mate)
              exit
            end if
        end do
        !******************************** 

        if ((verb_i=='y').or.(verb_i=='Y')) then
            if (present(str)) then 
                call ga_print_individual(mate,p,str)
            else
                call ga_print_individual(mate,p)
            end if
        end if
    end subroutine ga_selection
    

    !#################################################-
    subroutine ga_crossover(mate1,mate2,p,child1,child2, verb,str)
        implicit none
        type(ga_param), intent(in)  :: p
        type(chromosome),intent(in) :: mate1, mate2
        type(chromosome),intent(inout) :: child1,child2
        character(len=*), optional, intent(in) :: verb,str 
        
        
        integer    ::  cross_site,bit
        real       ::  rnd  
        character  ::  verb_i='n' 
          
        !------------------------------------
        if (present(verb)) verb_i=verb                
        call random_number(rnd) 
                
        if (rnd<=p%pcross) then    !perform crossover
            call random_number(rnd)
            cross_site= int((p%tchromlen-2)*rnd)+1
            !if (verb=='y') write(*,'(/a12,i5)',advance='no') 'cross site= ' , cross_site
           
            do bit=1,cross_site
              child1%genotype(bit) = mate1%genotype(bit)
              child2%genotype(bit) = mate2%genotype(bit)
            end do
           
            do bit=cross_site+1, p%tchromlen
              child1%genotype(bit)=mate2%genotype(bit)
              child2%genotype(bit)=mate1%genotype(bit)
            end do

            call ga_generate_geno2int(child1,p)  
            call ga_generate_phenotype(child1,p)
            call ga_generate_geno2int(child2,p)  
            call ga_generate_phenotype(child2,p)

        else                        !do not perform crossover 
            if ((verb_i=='y').or.(verb_i=='Y'))  print*, 'no crossover'
            child1=mate1
            child2=mate2
        end if
        
        if ((verb_i=='y').or.(verb_i=='Y')) then
              print*,''
              if (present(str)) print*,str
            call ga_print_individual(child1,p,'child 1')
            call ga_print_individual(child2,p,'child 2')            
        end if
        
    end subroutine ga_crossover
     
    !#################################################-
    subroutine ga_crossover_multi(mate1,mate2,p,child1,child2, verb,str)
        implicit none
        type(ga_param), intent(in)  :: p
        type(chromosome),intent(inout) :: mate1, mate2
        type(chromosome),intent(inout) :: child1,child2
        character(len=*), optional, intent(in) :: verb,str 
                        
        real :: pcross_in  
        real,allocatable       ::  rnd(:) ,cross_site(:) 
        
        integer    ::  bit,ok,i,j,   idx1,idx2
        character  ::  verb_i='n' 

        ! print*, 'In subroutine: ga_crossover_multi '          
        ! call ga_print_individual(mate1,p,'mate1')        
        ! call ga_print_individual(mate2,p,'mate2')        
        !------------------------------------
        if (present(verb)) verb_i=verb                     

        allocate(rnd(p%unk),cross_site(p%unk),stat=ok)
        !call err_stop(ok,0,'error in ga_crossover_multi')        

        call random_number(pcross_in)
        if (pcross_in<=p%pcross) then    
            !perform crossover        
            call random_number(rnd)
            ! print*,'random no ',rnd 
            cross_site = int(p%pos + p%chromlen*rnd-1)
            ! print*, 'cross_site',cross_site
            
            idx1=1
            idx2=cross_site(1)    
            do i=1,p%unk
                if (cross_site(i)>p%pos(i)) then
                    ! print*,'idx1,idx2: ',idx1,idx2
                    child1%genotype(idx1:idx2) = mate1%genotype(idx1:idx2)
                    child2%genotype(idx1:idx2) = mate2%genotype(idx1:idx2)
                    
                    idx1=cross_site(i)
                    idx2=p%pos(i)+p%chromlen(i)-1
                    ! print*,'idx1,idx2: ',idx1,idx2
                    child1%genotype(idx1:idx2) = mate2%genotype(idx1:idx2)
                    child2%genotype(idx1:idx2) = mate1%genotype(idx1:idx2)

                    idx1=p%pos(i+1)
                    idx2=cross_site(i+1)   
                else
                    if ((verb_i=='y').or.(verb_i=='Y')) &
                        print*, 'as cross site <=1 , so no crossover performed'
                end if
                
            end do
        else 
            !No crossover performed if prob <=pcross
            if ((verb_i=='y').or.(verb_i=='Y')) &
                        print*, 'prob <=pcross , so no crossover performed'
        end if
                
        if ((verb_i=='y').or.(verb_i=='Y')) then
              print*,''
              if (present(str)) print*,str
            call ga_print_individual(child1,p,'child 1')
            call ga_print_individual(child2,p,'child 2')            
        end if
        
    end subroutine ga_crossover_multi 

  
    !**************************************************************
    subroutine ga_mutation(individual,p,verb,str)     
        implicit none
        type(ga_param),intent(in):: p
        type(chromosome),intent(inout):: individual  
        character(len=*), optional, intent(in) :: verb,str
           
        integer    :: bit
        real       :: rnd
        character  ::  verb_i='n' 
              
        !------------------------------------
        if (present(verb)) verb_i=verb
        
        if (verb_i.eq.'y') then
            write(*,'(/a15)', advance='no') 'mutation bits: '
        end if 
        
        do bit=1,p%tchromlen
            call random_number(rnd)
            if (rnd<=p%pmute) then                   !mutation occurs
                  if ((verb_i=='y').or.(verb_i=='Y')) then
                    write(*,'(i2,",")', advance='no') bit
                   end if
                
                if(individual%genotype(bit)==1)then
                  individual%genotype(bit)=0
                  else
                  individual%genotype(bit)=1 
                  end if
               end if
        end do
        call ga_generate_geno2int(individual,p)  
        call ga_generate_phenotype(individual,p) 

        if ((verb_i=='y').or.(verb_i=='Y')) then
            print*,''
            if (present(str)) print*,str
            call ga_print_individual(individual,p,'after mutation')
        end if 
    end subroutine ga_mutation 
    
    !***************************************************************
    subroutine ga_copy_chrom(org,copy)
        implicit none
        type(chromosome), intent(in) :: org
        type(chromosome), intent(out):: copy   
    
        copy%genotype=org%genotype
        copy%phenotype=org%phenotype
        copy%geno2int=org%geno2int
        copy%fitness=org%fitness            
    end subroutine ga_copy_chrom


    !***************************************************************
    subroutine ga_copy_gen(org,copy)
        implicit none
        type(chromosome), intent(in) :: org(:)
        type(chromosome), intent(out):: copy(:)   
        integer :: i,n
    
        n=size(org)
        do i=1,n
        copy(i)%genotype=org(i)%genotype
        copy(i)%phenotype=org(i)%phenotype
        copy(i)%geno2int=org(i)%geno2int
        copy(i)%fitness=org(i)%fitness            
        end do
    end subroutine ga_copy_gen
    



    !***************************************************************
    subroutine ga_elitism(new,old,p,nel,verb,str)
        implicit none
        type(ga_param), intent(in)  :: p
        type(chromosome),intent(in) :: old(:)   
        type(chromosome),intent(inout):: new(:) 
        character(len=*),intent(in), optional :: verb,str 
        integer, intent(in) :: nel                   ! no of elite elements
        
        type(chromosome), allocatable:: sorto(:), sortn(:)
        integer :: i,loc, dim, ok
        !print*,'in ga_elitism function'
        !--------------------
        dim=size(old)
        allocate(sorto(dim),sortn(dim),stat=ok)                
        call ga_sorting(sorto,old,p,verb,'********* sorting old ********')            !sort according to fitness      
        call ga_sorting(sortn,new,p,verb,'********* sorting new ********')            !sort according to fitness      
                    
        do i=1,nel
           !check if this individual is present in gen2 
            call ga_find_loc(loc,new,sorto(i))
            !print*,'location check:  ',loc                
            !if not then replace with minimum fitness individual 
            if (loc==0)then
                call ga_find_loc(loc,new,sortn(dim-i-1))  ! find the location of min fit
                !print*,'location check for replace:  ',loc                
                call ga_copy_chrom(sorto(i),new(loc))
            end if 
         end do     
         call ga_sorting(sortn,new,p,verb,'********* sorting after elite replacement ********')            !sort according to fitness                
        ! call ga_print_generation(sortn,p,'***** elite sorted generation *****')
    end subroutine ga_elitism



    !***************************************************
    subroutine ga_find_loc(loc,gen,ind)
        ! Finds the location of chrom(ind) in the given set of individual (gen)
        implicit none
        integer,intent(out):: loc
        type(chromosome),intent(in):: gen(:), ind
        integer:: i,n

        n=size(gen)
        loc=0
        do i=1,n
            if(all(ind%genotype == gen(i)%genotype).eqv. .true.) loc=i
        end do
    end subroutine ga_find_loc


    
    !***************************************************
    subroutine ga_sorting(sgen,gen,p,verb,str)
        implicit none
        type(chromosome), intent(in) :: gen(:)
        type(chromosome), intent(out):: sgen(:)
        type(ga_param), intent(in) :: p
        character(len=*),intent(in),optional :: str,verb
        type(chromosome) :: temp
        integer :: i,j,n

        n=size(gen)
        call ga_copy_gen(gen,sgen)

        !sort according to fitness      
        do j=1,n
            do i=1,n-1
                if (sgen(i)%fitness<sgen(i+1)%fitness) then
                    call ga_copy_chrom(sgen(i),temp)
                    call ga_copy_chrom(sgen(i+1),sgen(i))
                    call ga_copy_chrom(temp,sgen(i+1))
                end if
            end do          
        end do
        
        if ((verb=='y').or.(verb=='Y')) then 
            if (present(str)) then
                call ga_print_generation(sgen,p,str)
            else
                call ga_print_generation(sgen,p,'~~~~~~~ Sorted population ~~~~~~~~~')
            end if
        end if
    end subroutine ga_sorting


    !***************************************************
    subroutine ga_find_fittest(nfittest,gen,p,n)
        ! returns nth fittest individual 
        ! i.e., 1= highest fit, 2=lesser fit,  n=least fit
        implicit none
        type(chromosome),intent(in) :: gen(:)      !
        type(ga_param), intent(in)  :: p            !
        integer, intent(in)         :: n            ! best n chromosomes
        type(chromosome), intent(out) :: nfittest

        type(chromosome), allocatable:: sgen(:)
        integer :: dim, ok

        dim=size(gen)
        allocate(sgen(dim),stat=ok)
        print*,'Is SGEN allocated: ', ok

        call ga_sorting(sgen,gen,p)         
        call ga_copy_chrom(sgen(n),nfittest)  
    end subroutine ga_find_fittest


!$$$$$$     subroutine ga_allocate_individual(ind,p)
!$$$$$$         implicit none
!$$$$$$         type(ga_param), intent(in)  :: p
!$$$$$$         type(chromosome),intent(inout) :: ind
!$$$$$$ 
!$$$$$$         integer :: ok
!$$$$$$                 
!$$$$$$         allocate(ind%genotype(p%tchromlen),& 
!$$$$$$                  ind%geno2int(p%unk),& 
!$$$$$$                  ind%phenotype(p%unk),&  !gen2(i)%genotype(p%tchromlen),gen2(i)%geno2int(p%unk), gen2(i)%phenotype(p%unk),& 
!$$$$$$                  stat=ok)
!$$$$$$                  
!$$$$$$         call err_stop(ok,0,'error in allocation')   
!$$$$$$ 
!$$$$$$     end subroutine ga_allocate_individual
    
end module mod_ga