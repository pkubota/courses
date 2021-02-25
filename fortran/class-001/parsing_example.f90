program parsing_example
  implicit none
!
! illustrates the use of intrinsics for parsing the command line
! build with
!  gfortran -o parsing_example.exe parsing_example.f90
! run with
! ./parsing_example.exe -n 5 -p 33
! or omit arguments
!
  integer, parameter :: fm_len = 80
  integer :: i, ios, sw_len
  character(len=fm_len) :: fmtstring, err
  character(len=:), allocatable :: sw, sw_val
  integer :: n_val = 0, p_val = 0

  call get_command(length=sw_len)
!   each individual argument will have a smaller length than this
  allocate(character(len=sw_len) :: sw, sw_val)

  if (command_argument_count() > 0) then
     write(*,*) 'Processing command line.'
     i = 1
     do
        if (i > command_argument_count()) exit
        call get_command_argument(i, value=sw)
        select case (trim(sw))
        case ('-p')
           call get_command_argument(i+1, value=sw_val)
           write(fmtstring, fmt='(a,i0,a)') '(i',len_trim(sw_val),')'
           read(sw_val, iostat=ios, iomsg=err, fmt=fmtstring) p_val
        case ('-n')
           call get_command_argument(i+1, value=sw_val)
           write(fmtstring, fmt='(a,i0,a)') '(i',len_trim(sw_val),')'
           read(sw_val, iostat=ios, iomsg=err, fmt=fmtstring) n_val
        end select
        i = i + 2
        if (ios /= 0) then
           write(*,*) 'failed with message: ',err
           stop 'Aborting program.'
        end if
     end do
  end if
  write(*, '(''Result: n_val = '',i0,'' p_val = '',i0)') n_val, p_val

end program parsing_example
