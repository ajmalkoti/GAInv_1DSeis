module mod_allocate_mat_vec

    interface allocate_vec
        module procedure allocate_vec_i
        module procedure allocate_vec_r
    end interface allocate_vec
    
    interface allocate_mat
        module procedure allocate_mat_i
        module procedure allocate_mat_r
    end interface allocate_mat

    private:: err_stop
        
contains
   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   subroutine err_stop(var,eqval,str)
        integer, intent(in) :: var, eqval
        character(len=*),intent(in):: str 
        if (var/=eqval) then  
            print*, str
            stop
        end if
    end subroutine err_stop
        
   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    subroutine allocate_vec_i(vec,n,str)           
        implicit none
        integer,intent(in)            		:: n
        integer, allocatable,intent(inout)	:: vec(:)
        character(len=*),optional, intent(in)::  str
        integer   :: ok
                            
        allocate(vec(n),stat=ok)
		if (present(str)) then
            call err_stop(ok,0,'Allocation error for "'// trim(str)// '"')
        else 
            call err_stop(ok,0,'Allocation error')
        end if        
        vec=0
    end subroutine allocate_vec_i


   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    subroutine allocate_vec_r(vec,n,str)           
        implicit none
        integer,intent(in)            :: n
        real, allocatable,intent(inout):: vec(:)
        character(len=*),optional,intent(in)   ::  str        
        integer    :: ok
                            
        allocate(vec(n),stat=ok)
        if (present(str)) then
            call err_stop(ok,0,'Allocation error for "'// trim(str)// '"')
        else 
            call err_stop(ok,0,'Allocation error')
        end if 
        vec=0.0
    end subroutine allocate_vec_r

    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    subroutine allocate_mat_i(mat,nr,nc,str)
        implicit none
        integer,intent(in)            ::  nr,nc
        integer, allocatable,intent(inout)   ::  mat(:,:)            
        character(len=*),optional, intent(in)::  str
        integer    ::  ok
        
        allocate(mat(nr,nc),stat=ok)
         if (present(str)) then
            call err_stop(ok,0,'Allocation error for "'// trim(str)// '"')
        else 
            call err_stop(ok,0,'Allocation error')
        end if 
        mat=0
   end subroutine allocate_mat_i
   
    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    subroutine allocate_mat_r(mat,nr,nc,str)
        implicit none
        integer,intent(in)            ::  nr,nc
        real,allocatable,intent(inout)      ::  mat(:,:)            
        character(len=*),optional,intent(in)::  str        
        integer  ::  ok
        
        allocate(mat(nr,nc),stat=ok)
         if (present(str)) then
            call err_stop(ok,0,'Allocation error for "'// trim(str)// '"')
        else 
            call err_stop(ok,0,'Allocation error')
        end if 
        mat=0.0
   end subroutine allocate_mat_r
   

end module mod_allocate_mat_vec