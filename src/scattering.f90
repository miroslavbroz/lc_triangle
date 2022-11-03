! scattering.f90
! Scattering, non-convex version.
! Miroslav Broz (miroslav.broz@email.cz), Oct 26th 2022

! Note: Inlining instead of normalize() is faster!

module scattering_module

contains

! Mutual scattering.

subroutine tau(normals, centres, tau_i)

use input_module, only: use_scattering

implicit none
double precision, dimension(:,:), pointer, intent(in) :: normals, centres
double precision, dimension(:,:), intent(inout) :: tau_i

integer :: i, j
double precision, dimension(3) :: r
double precision :: tmp

if (.not.use_scattering) return

!$omp parallel do private(i,j,tmp,r) shared(normals,centres,tau_i)
do i = 1, size(normals,1)
  do j = 1, size(normals,1)
    if (i.eq.j) then
      tau_i(i,j) = 0.d0
      cycle
    endif

    r = centres(j,:)-centres(i,:)
    r = r/sqrt(dot_product(r,r))

    tmp = dot_product(normals(i,:), r)
    if (tmp.lt.0.d0) then
      tau_i(i,j) = 0.d0
      cycle
    endif

    tmp = tmp*dot_product(normals(j,:), -r)
    if (tmp.lt.0.d0) then
      tau_i(i,j) = 0.d0
      cycle
    endif

    tau_i(i,j) = tmp
  enddo
enddo
!$omp end parallel do

return
end subroutine tau

end module scattering_module


