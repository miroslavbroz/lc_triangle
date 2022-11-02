! shadowing.f90
! Shadowing, non-convex version.
! Miroslav Broz (miroslav.broz@email.cz), Oct 26th 2022

module shadowing_module

contains

! Dicrectional cosine.

subroutine mu(norms, s, mu_i)

implicit none
double precision, dimension(:,:), pointer, intent(in) :: norms
double precision, dimension(3), intent(in) :: s
double precision, dimension(:), pointer, intent(out) :: mu_i

integer :: i

!$omp parallel do private(i) shared(norms,mu_i,s)
do i = 1, size(norms,1)
  mu_i(i) = max(dot_product(norms(i,:), s), 0.d0)
enddo
!$omp end parallel do

end subroutine mu

! Mutual shadowing.

subroutine nu(faces, nodes, normals, centres, s, nu_i)

use intersect_AB_t_module

implicit none
integer, dimension(:,:), pointer, intent(in) :: faces
double precision, dimension(:,:), pointer, intent(in) :: nodes, normals, centres
double precision, dimension(3), intent(in) :: s
double precision, dimension(:), intent(inout) :: nu_i

integer :: i, j, k
double precision, dimension(3,3) :: t
double precision, dimension(3) :: A, B, C
double precision :: tmp
logical :: has_solution

!$omp parallel do private(i,j,k,A,B,C,t,has_solution,tmp) shared(nodes,faces,centres,s,nu_i)
do i = 1, size(faces,1)
  if (nu_i(i).gt.0.d0) then

    A = centres(i,:)
    B = s

    j = 1
    do while ((j.le.size(faces,1)).and.(nu_i(i).gt.0.d0))

      if (i.ne.j) then
        if (nu_i(j).gt.0.d0) then
          C = centres(j,:)-centres(i,:)
          tmp = dot_product(C, normals(i,:))
          if (tmp.gt.0.d0) then

            do k = 1, 3
              t(k,:) = nodes(faces(j,k),:)
            enddo

            call intersect_AB_t(A, B, t, C, has_solution)

            if (has_solution) then
              nu_i(i) = 0.d0
            endif
          endif
        endif
      endif

      j = j+1
    enddo  ! j

  endif
enddo ! i
!$omp end parallel do

return
end subroutine nu

end module shadowing_module


