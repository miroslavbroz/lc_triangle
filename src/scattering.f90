! scattering.f90
! Scattering, non-convex version.
! Miroslav Broz (miroslav.broz@email.cz), Oct 26th 2022

module scattering_module

contains

! Mutual scattering.

subroutine tau(normals, centres, tau_i)

implicit none
double precision, dimension(:,:), pointer, intent(in) :: normals, centres
double precision, dimension(:,:), intent(inout) :: tau_i

integer :: i, j
double precision :: tmp

!$omp parallel do private(i,j,tmp) shared(normals,centres,tau_i)
do i = 1, size(normals,1)
  do j = 1, size(normals,1)
    if (i.ne.j) then
      tmp = Xi(dot_product(normals(i,:), centres(j,:)-centres(i,:)))
      tmp = tmp*Xi(dot_product(normals(j,:), centres(i,:)-centres(j,:)))
      tau_i(i,j) = tmp
    else
      tau_i(i,j) = 0.d0
    endif
  enddo
enddo
!$omp end parallel do

return
end subroutine tau

double precision function Heaviside(x)

implicit none
double precision :: x

if (x.gt.0.d0) then
  Heaviside = 1.d0
else
  Heaviside = 0.d0
endif

return
end function Heaviside

double precision function Xi(x)

implicit none
double precision :: x

Xi = x*Heaviside(x)

return
end function Xi

end module scattering_module


