module mod_read_write
    integer, parameter :: sp = selected_real_kind(6, 37)
    integer, parameter :: dp = selected_real_kind(15, 307)
    integer, parameter :: qp = selected_real_kind(33, 4931)

    interface read_var
        module procedure read_var_i    
        module procedure read_var_r
        module procedure read_vec_i
        module procedure read_vec_r        
    end interface read_var
    
contains
    !######################################################
    subroutine err_stop(var,eqval,str1)
        integer, intent(in) :: var, eqval
        character(len=*),intent(in):: str1 
        if (var/=eqval) then  
            print*, str1
            stop
        end if
    end subroutine err_stop
    
    !######################################################    
    subroutine read_var_real5(fname,name1,val1,name2,val2,name3,val3,name4,val4,name5,val5,verb)
        implicit none
        character(len=*)     :: fname 
        real, intent(inout),optional      :: val1,val2,val3,val4,val5
        character(len=*),intent(in),optional :: name1,name2,name3,name4,name5,verb

        if (present(val1).and.present(name1)) &
        call read_var(fname=fname,varname=name1,val=val1,verb=verb)

        if (present(val2).and.present(name2)) &
        call read_var(fname=fname,varname=name2,val=val2,verb=verb)

        if (present(val3).and.present(name3)) &        
        call read_var(fname=fname,varname=name3,val=val3,verb=verb)    !call read_var(fname='ip_data.txt',lineno=4,val=i1,verb='y')

        if (present(val4).and.present(name4)) &        
        call read_var(fname=fname,varname=name4,val=val4,verb=verb)    !call read_var(fname='ip_data.txt',lineno=4,val=i1,verb='y')

        if (present(val5).and.present(name5)) &        
        call read_var(fname=fname,varname=name5,val=val5,verb=verb)    !call read_var(fname='ip_data.txt',lineno=4,val=i1,verb='y')       
    end subroutine read_var_real5

    !######################################################    
    subroutine read_var_int5(fname,name1,val1,name2,val2,name3,val3,name4,val4,name5,val5,verb)
        implicit none
        character(len=*)     :: fname 
        integer, intent(inout),optional      :: val1,val2,val3,val4,val5
        character(len=*),intent(in),optional :: name1,name2,name3,name4,name5,verb

        if (present(val1).and.present(name1)) &
        call read_var(fname=fname,varname=name1,val=val1,verb=verb)

        if (present(val2).and.present(name2)) &
        call read_var(fname=fname,varname=name2,val=val2,verb=verb)

        if (present(val3).and.present(name3)) &        
        call read_var(fname=fname,varname=name3,val=val3,verb=verb)    !call read_var(fname='ip_data.txt',lineno=4,val=i1,verb='y')

        if (present(val4).and.present(name4)) &        
        call read_var(fname=fname,varname=name4,val=val4,verb=verb)    !call read_var(fname='ip_data.txt',lineno=4,val=i1,verb='y')

        if (present(val5).and.present(name5)) &        
        call read_var(fname=fname,varname=name5,val=val5,verb=verb)    !call read_var(fname='ip_data.txt',lineno=4,val=i1,verb='y')       
    end subroutine read_var_int5



    !######################################################    
    subroutine read_var_i(fname,val,varname,lineno,verb)
        implicit none
        character(len=*)     :: fname
        integer, intent(inout)    :: val
        integer, intent(in),optional         :: lineno
        character(len=*),intent(in),optional:: varname,verb        
        
        integer        :: i,ipos,ok,lno
        character(200) :: buffer
        character(len=:),allocatable :: line_trim  
        !------------------------------------------
        if ((present(varname).eqv. .true.).and.(present(lineno).eqv. .false.)) then
            open(unit=10,file=fname,action="read",iostat=ok)
              call err_stop(ok,0, 'error in opening file..!')
              do i=1,10000
                  read(10,*,iostat=ok) buffer            ! print*, buffer 
                  call err_stop(ok,0, 'error in reading file: eof detected..! '//trim(varname)//' not found')
                  ipos = scan(buffer," ")                ! print*, ipos 
                  line_trim= trim(buffer(1:ipos-1)) 
                  if(trim(buffer).eq.varname) exit               
              end do   
              lno= i          !   print*, lno         
            close(10)
        elseif ((present(varname).eqv..false.).and.(present(lineno).eqv..true.)) then           
            lno=lineno        !    print*, lno
        else
            call err_stop(1,0, "error_int: provide either 'lineno', or 'varname'..!" )    
        end if  


        open(unit=10,file=fname,action="read")
          do i=1,lno
              read(10,'(a)') buffer 
          end do        
          ipos = scan(buffer,"=",back=.true.)
          line_trim= trim(buffer(ipos+1:))
          read( line_trim, * ) val            
        close(10)
        if ((verb.eq.'y').or.(verb.eq.'Y')) print*, varname,': ',val        
    end subroutine read_var_i

    !######################################################    
    subroutine read_var_r(fname,val,varname,lineno,verb)
        implicit none
        character(len=*)     :: fname
        real, intent(inout)    :: val
        integer, intent(in),optional         :: lineno
        character(len=*),intent(in),optional:: varname,verb        
        
        integer        :: i,ipos,ok,lno
        character(200) :: buffer
        character(len=:),allocatable :: line_trim  
        !------------------------------------------
        if ((present(varname).eqv. .true.).and.(present(lineno).eqv. .false.)) then
            open(unit=10,file=fname,action="read",iostat=ok)
              call err_stop(ok,0, 'error in opening file..!')
              do i=1,10000
                  read(10,*,iostat=ok) buffer            ! print*, buffer 
                  call err_stop(ok,0, 'error in reading file: eof detected..!')
                  ipos = scan(buffer," ")                ! print*, ipos 
                  line_trim= trim(buffer(1:ipos-1)) 
                  if(trim(buffer).eq.varname) exit               
              end do   
              lno= i          !   print*, lno         
            close(10)
        elseif ((present(varname).eqv..false.).and.(present(lineno).eqv..true.)) then           
            lno=lineno        !    print*, lno
        else
            call err_stop(1,0, "error_real: provide either 'lineno', or 'varname'..!" )    
        end if  

        
        open(unit=10,file=fname,action="read")
          do i=1,lno
              read(10,'(a)') buffer 
          end do        
          ipos = scan(buffer,"=",back=.true.)
          line_trim= trim(buffer(ipos+1:))
          read( line_trim, * ) val            
        close(10)
        if ((verb.eq.'y').or.(verb.eq.'Y')) print*, varname,': ',val
    end subroutine read_var_r 


    !######################################################    
    subroutine read_vec_r(fname,val,varname,lineno,verb)
        implicit none
        character(len=*)     :: fname
        real, intent(inout)    :: val(:)
        integer, intent(in),optional         :: lineno
        character(len=*),intent(in),optional:: varname,verb        
        
        integer        :: i,ipos,ok,lno
        character(200) :: buffer
        character(len=:),allocatable :: line_trim  
        !------------------------------------------
        if ((present(varname).eqv. .true.).and.(present(lineno).eqv. .false.)) then
            open(unit=10,file=fname,action="read",iostat=ok)
              call err_stop(ok,0, 'error in opening file..!')
              do i=1,10000
                  read(10,*,iostat=ok) buffer            ! print*, buffer 
                  call err_stop(ok,0, 'error in reading file: eof detected..!')
                  ipos = scan(buffer," ")                ! print*, ipos 
                  line_trim= trim(buffer(1:ipos-1)) 
                  if(trim(buffer).eq.varname) exit               
              end do   
              lno= i          !   print*, lno         
            close(10)
        elseif ((present(varname).eqv..false.).and.(present(lineno).eqv..true.)) then           
            lno=lineno        !    print*, lno
        else
            call err_stop(1,0, "error_real: provide either 'lineno', or 'varname'..!" )    
        end if  

        !print*, 'line no', lno
        !print*, 'var name', varname
        
        open(unit=10,file=fname,action="read")
          do i=1,lno
              read(10,'(a)') buffer 
          end do        
          ipos = scan(buffer,"=",back=.true.)
          line_trim= trim(buffer(ipos+1:))
          read( line_trim, * ) val            
        close(10)
        if ((verb.eq.'y').or.(verb.eq.'Y')) print*, varname,': ',val
    end subroutine read_vec_r 


    !######################################################    
    subroutine read_vec_i(fname,val,varname,lineno,verb)
        implicit none
        character(len=*)     :: fname
        integer, intent(inout)    :: val(:)
        integer, intent(in),optional         :: lineno
        character(len=*),intent(in),optional:: varname,verb        
        
        integer        :: i,ipos,ok,lno
        character(200) :: buffer
        character(len=:),allocatable :: line_trim  
        !------------------------------------------

        if ((present(varname).eqv. .true.).and.(present(lineno).eqv. .false.)) then
            open(unit=10,file=fname,action="read",iostat=ok)
              call err_stop(ok,0, 'error in opening file..!')
              do i=1,10000
                  read(10,*,iostat=ok) buffer            ! print*, buffer 
                  call err_stop(ok,0, 'error in reading file: eof detected..!')
                  ipos = scan(buffer," ")                ! print*, ipos 
                  line_trim= trim(buffer(1:ipos-1)) 
                  if(trim(buffer).eq.varname) exit               
              end do   
              lno= i          !   print*, lno         
            close(10)
        elseif ((present(varname).eqv..false.).and.(present(lineno).eqv..true.)) then           
            lno=lineno        !    print*, lno
        else
            call err_stop(1,0, "error_real: provide either 'lineno', or 'varname'..!" )    
        end if  

        !print*, 'line no', lno
        open(unit=10,file=fname,action="read")
          do i=1,lno
              read(10,'(a)') buffer 
          end do        
          ipos = scan(buffer,"=",back=.true.)
          line_trim= trim(buffer(ipos+1:))
          read( line_trim, * ) val            
        close(10)
        if ((verb.eq.'y').or.(verb.eq.'Y')) print*, varname,': ',val
    end subroutine read_vec_i 

!$$$$$$     !######################################################
!$$$$$$     subroutine read_ip_var_int(lineno,num)
!$$$$$$         implicit none
!$$$$$$         integer, intent(in) ::  lineno
!$$$$$$         integer, intent(out)::  num
!$$$$$$         character(100) :: line
!$$$$$$         character(len=:),allocatable :: line_trim
!$$$$$$         integer        :: i,ipos
!$$$$$$           
!$$$$$$         open(unit=10,file="ip.dat",action="read")
!$$$$$$         do i=1,lineno
!$$$$$$             read(10,'(a)') line
!$$$$$$         end do 
!$$$$$$         print*, line
!$$$$$$         ipos = scan(line,"=",back=.true.)
!$$$$$$         line_trim = trim(line(ipos+1:))
!$$$$$$         read(line_trim, '(i20)' ) num            
!$$$$$$         close(10)
!$$$$$$     end subroutine
!$$$$$$     
!$$$$$$     
!$$$$$$     !######################################################
!$$$$$$     subroutine read_ip_var_real(lineno,num)
!$$$$$$         use mod_param, only: dp
!$$$$$$         implicit none
!$$$$$$         integer, intent(in) ::  lineno
!$$$$$$         real(kind=dp), intent(out)::  num
!$$$$$$         
!$$$$$$         character(100) :: line
!$$$$$$         character(len=:),allocatable :: line_trim
!$$$$$$         integer        :: i,ipos
!$$$$$$           
!$$$$$$         open(unit=10,file="ip.dat",action="read")
!$$$$$$         do i=1,lineno
!$$$$$$             read(10,'(a)') line
!$$$$$$         end do
!$$$$$$         
!$$$$$$         print*, line
!$$$$$$         ipos = scan(line,"=",back=.true.)
!$$$$$$         line_trim= trim(line(ipos+1:))
!$$$$$$         !print*,line_trim
!$$$$$$         read( line_trim, * ) num            
!$$$$$$         close(10)
!$$$$$$     end subroutine 
!$$$$$$ 
!$$$$$$     
!$$$$$$     !######################################################
!$$$$$$     function read_ip_var_str(lineno)
!$$$$$$         implicit none
!$$$$$$         integer, intent(in) ::  lineno
!$$$$$$         character(len=:),allocatable ::  read_ip_var_str
!$$$$$$         
!$$$$$$         character(100) :: line
!$$$$$$         integer        :: i,ipos
!$$$$$$           
!$$$$$$         open(unit=10,file="ip.dat",action="read")
!$$$$$$         do i=1,lineno
!$$$$$$             read(10,'(a)') line
!$$$$$$         end do 
!$$$$$$         print*, line
!$$$$$$         ipos = scan(line,"=",back=.true.)
!$$$$$$         read_ip_var_str= trim(line(ipos+2:))    ! trim doesn't removes only trailing spaces
!$$$$$$         !print*, 'in subroutine', read_ip_var_str
!$$$$$$         close(10)
!$$$$$$     end function read_ip_var_str
!$$$$$$     
!$$$$$$     !######################################################
!$$$$$$     subroutine read_mat_file(mat,filename)
!$$$$$$         use mod_param, only:dp
!$$$$$$         implicit none
!$$$$$$         real(kind=dp),dimension(:,:), intent(inout) :: mat
!$$$$$$         character(len=*), intent(in) :: filename
!$$$$$$         integer         :: i,j
!$$$$$$      
!$$$$$$         open(unit=20,file=filename,action='read')
!$$$$$$         do i=1,size(mat,1)
!$$$$$$             read(20,*) mat(i,:)
!$$$$$$         end do
!$$$$$$         close(20)
!$$$$$$     end subroutine read_mat_file    
!$$$$$$     
!$$$$$$     !######################################################
!$$$$$$     subroutine write_mat(mat,filename)
!$$$$$$         use mod_variables, only: nx,nz,dp
!$$$$$$         implicit none
!$$$$$$         real(kind=dp),dimension(:,:), intent(in) :: mat
!$$$$$$         character(len=*), intent(in) :: filename
!$$$$$$         integer         :: i,j
!$$$$$$  
!$$$$$$         open(unit=20,file=filename,action='write')
!$$$$$$         do i=1,size(mat,1)
!$$$$$$             do j=1,size(mat,2)
!$$$$$$                 write(20,'(f22.15)', advance='no') mat(i,j)
!$$$$$$             end do
!$$$$$$             write(20,*) ''
!$$$$$$         end do
!$$$$$$         close(20)
!$$$$$$     end subroutine write_mat
!$$$$$$ 
!$$$$$$     !######################################################
!$$$$$$     subroutine write_vec(vec,filename)
!$$$$$$         use mod_variables, only: nx,nz,dp
!$$$$$$         implicit none
!$$$$$$         real(kind=dp),dimension(:), intent(in) :: vec
!$$$$$$         character(len=*), intent(in) :: filename
!$$$$$$         integer         :: i,j
!$$$$$$  
!$$$$$$         open(unit=20,file=filename,action='write')
!$$$$$$         do i=1,size(vec,1)
!$$$$$$             write(20,'(f22.15)', advance='no') vec(i)
!$$$$$$             write(20,*) ''
!$$$$$$         end do
!$$$$$$         close(20)
!$$$$$$     end subroutine write_vec
!$$$$$$ 
!$$$$$$     !######################################################
!$$$$$$     subroutine print_mat(str, mat)
!$$$$$$         ! mat: matrix to be printed
!$$$$$$         ! str: message to be displayed on screen before printing matrix
!$$$$$$         use mod_param, only:dp
!$$$$$$         implicit none
!$$$$$$         real(kind=dp), intent(in)   :: mat(:,:)
!$$$$$$         character(len=*),intent(in) :: str
!$$$$$$         integer:: i,j
!$$$$$$         
!$$$$$$         print*,'----------------------------------------'
!$$$$$$         print*, str
!$$$$$$         do i=1,size(mat,1)
!$$$$$$             do j=1,size(mat,2)
!$$$$$$                 write(*,'(f10.3)',advance='no'), mat(i,j)
!$$$$$$             end do
!$$$$$$             write(*,*),''
!$$$$$$         end do
!$$$$$$     end subroutine print_mat
!$$$$$$ 
!$$$$$$     !######################################################
!$$$$$$     subroutine print_vec(str, vec)
!$$$$$$         ! vec: vector to be printed
!$$$$$$         ! str: message to be displayed on screen before printing matrix
!$$$$$$         use mod_param, only:dp
!$$$$$$         implicit none
!$$$$$$         real(kind=dp), intent(in)   :: vec(:)
!$$$$$$         character(len=*),intent(in) :: str
!$$$$$$         integer:: i,j
!$$$$$$         
!$$$$$$         print*,'----------------------------------------'
!$$$$$$         print*, str
!$$$$$$         do i=1,size(vec,1)
!$$$$$$             write(*,'(f10.3)',advance='no') vec(i)
!$$$$$$         end do
!$$$$$$         write(*,*) ''
!$$$$$$     end subroutine print_vec
    
end module mod_read_write







!$$$$$$ Funciton to convert the uppercase string to lowercase
!$$$$$$ 
!$$$$$$ 
!$$$$$$ function string_tolower( string ) result (new) 
!$$$$$$     character(len=*)           :: string 
!$$$$$$ 
!$$$$$$     character(len=len(string)) :: new 
!$$$$$$ 
!$$$$$$     integer                    :: i 
!$$$$$$     integer                    :: k 
!$$$$$$ 
!$$$$$$     length = len(string) 
!$$$$$$     new    = string 
!$$$$$$     do i = 1,len(string) 
!$$$$$$         k = iachar(string(i:i)) 
!$$$$$$         if ( k >= iachar('A') .and. k <= iachar('Z') ) then 
!$$$$$$             k = k + iachar('a') - iachar('A') 
!$$$$$$             new(i:i) = achar(k) 
!$$$$$$         endif 
!$$$$$$     enddo 
!$$$$$$ end function string_tolower 