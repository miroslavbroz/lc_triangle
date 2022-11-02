! write2.f90
! Write 2-dimensional array.
! Miroslav Broz (miroslav.broz@email.cz), Oct 28th 2022

module write2_module

contains

subroutine write2(filename, vals)

implicit none
character(len=*) :: filename
double precision, dimension(:,:) :: vals
integer :: i, j

open(unit=10, file=filename, status='unknown')
write(10,*) size(vals,1), size(vals,2)
do i = 1,size(vals,1)
  do j = 1,size(vals,2)
!    if (i.ne.j) then
!      if (vals(i,j).lt.1.d0) then
        write(10,*) i, j, vals(i,j)
!      endif
!    endif
  enddo
enddo
close(10)

end subroutine write2

end module write2_module


