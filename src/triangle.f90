! triangle.f90
! Triangles module.
! Miroslav Broz (miroslav.broz@email.cz), Oct 26th 2022

module triangle_module

contains

! Intersecton of line AB with triangle t.
! https://www.iue.tuwien.ac.at/phd/ertl/node114.html

! Note: p = A + B*x, not (B-A)*x

subroutine intersect_AB_t(A, B, t, C, has_solution)

use vector_product_module

implicit none
double precision, dimension(3), intent(in) :: A, B
double precision, dimension(3,3), intent(in) :: t
double precision, dimension(3), intent(out) :: C
logical, intent(out) :: has_solution

integer :: i
double precision :: totarea
double precision, dimension(3) :: area

do i = 1, 3
  area(i) = 0.5d0*dot_product(vector_product(t(mod(i,3)+1,:)-A, t(mod(i+1,3)+1,:)-A), B)
enddo

if ((area(1).gt.0.d0).and.(area(2).gt.0.d0).and.(area(3).gt.0.d0)) then
  has_solution = .true.
else if ((area(1).lt.0.d0).and.(area(2).lt.0.d0).and.(area(3).lt.0.d0)) then
  has_solution = .true.
else
  has_solution = .false.
endif

if (has_solution) then
  C = (/0.d0, 0.d0, 0.d0/)
  totarea = sum(area)
  do i = 1, 3
    C = C + area(i)/totarea*t(i,:)
  enddo
endif

!write(*,*) 'area = ', area
!write(*,*) 'totarea = ', totarea

!if (dot_product(C-A, B).gt.0.d0) then
!  has_solution = .false.
!endif

return
end subroutine intersect_AB_t

! Dicrectional cosine.

subroutine mu(norms, s, mu_i)

implicit none
double precision, dimension(:,:), pointer :: norms
double precision, dimension(3) :: s
double precision, dimension(:), pointer :: mu_i

integer :: i

do i = 1, size(norms,1)
  mu_i(i) = max(dot_product(norms(i,:), s), 0.d0)
enddo

end subroutine mu

! Mutual shadowing.

subroutine nu(faces, nodes, centres, s, nu_i, nu2_i)

implicit none
integer, dimension(:,:), pointer, intent(in) :: faces
double precision, dimension(:,:), pointer, intent(in) :: nodes, centres
double precision, dimension(3), intent(in) :: s
double precision, dimension(:), intent(out) :: nu_i
double precision, dimension(:,:), intent(out) :: nu2_i

integer :: i, j, k
double precision, dimension(3,3) :: triangle
double precision, dimension(3) :: A, B, C
double precision :: tmp
logical :: has_solution

nu_i = 1.d0

do i = 1, size(faces,1)
  do k = 1, 3
    triangle(k,:) = nodes(faces(i,k),:)
  enddo
  do j = 1, size(faces,1)
    if (i.ne.j) then

      A = centres(j,:)
      B = s
      call intersect_AB_t(A, B, triangle, C, has_solution)

      if (has_solution) then
        tmp = dot_product(C-A, B)
        if (tmp.lt.0.d0) then
          nu_i(i) = 0.d0
        endif
      endif
    
!      write(*,*) 'i = ', i 
!      write(*,*) 'j = ', j 
!      write(*,*) 'has_solution = ', has_solution
!      write(*,*) 'triangle(1,:) = ', triangle(1,:)
!      write(*,*) 'triangle(2,:) = ', triangle(2,:)
!      write(*,*) 'triangle(3,:) = ', triangle(3,:)
!      write(*,*) 'A = ', A
!      write(*,*) 'B = ', B
!      write(*,*) 'C = ', C
!      write(*,*) 'C-A = ', C-A
!      write(*,*) 'tmp = ', tmp
!      write(*,*) 'nu_i(', i, ') = ', nu_i(i)
!      if ((i.eq.2).and.(j.eq.1)) stop

!    if (has_solution) then
!      nu_i(i) = 0.d0
!      nu2_i(i,j) = 0.d0
!    else
!      nu2_i(i,j) = 1.d0
!    endif

    endif
  enddo
enddo

return
end subroutine nu

end module triangle_module


