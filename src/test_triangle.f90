! test_triangle.f90
! Miroslav Broz (miroslav.broz@email.cz), Oct 28th 2022

program test_triangle

use const_module
use normalize_module
use normal_module
use centre_module
use surface_module
use volume_module
use planck_module
use read_face_module
use read_node_module
use read_elem_module
use write1_module
use write2_module
use write_face_module
use write_node_module
use shadowing_module
use scattering_module

implicit none

integer, dimension(:,:), pointer :: faces, faces1, faces2
integer, dimension(:,:), pointer :: elems, elems1, elems2
double precision, dimension(:,:), pointer :: nodes, nodes1, nodes2
double precision, dimension(:,:), pointer :: normals, centres
double precision, dimension(:), pointer :: surf, vols
double precision, dimension(:), pointer :: mu_i, mu_e, nu_i, nu_e, Phi_i, f, I_lambda, I2_lambda
double precision, dimension(:,:), pointer :: nutmp_i, nutmp_e
double precision, dimension(:,:), pointer :: tau_i

integer :: i, j, k, n, m
double precision, dimension(3) :: r, s, o
double precision :: capR, capS, capV, unit
double precision :: A_w, A_hL, A_gL, A_BL, A_gLS, A_BLS
double precision :: f_L, f_g, f_LS
double precision :: d1, d2, omega, T
double precision :: lambda_eff, Delta_eff
double precision :: Phi_nu_cal, Phi_lambda_cal, Phi_V_cal
double precision :: B_lambda, J_lambda, P_lambda, Phi_lambda, P_V, Phi_V, V0
double precision :: tmp, tmp2
double precision :: t1, t2
character(len=80) :: str

! read 2 spheres...
call read_node("sphere.1.node", nodes1)
call read_face("sphere.1.face", faces1)
call read_elem("sphere.1.ele", elems1)

call read_node("sphere.1.node", nodes2)
call read_face("sphere.1.face", faces2)
call read_elem("sphere.1.ele", elems2)

! ... and merge them
allocate(nodes(size(nodes1,1)+size(nodes2,1), size(nodes1,2))) 
allocate(faces(size(faces1,1)+size(faces2,1), size(faces1,2))) 
allocate(elems(size(elems1,1)+size(elems2,1), size(elems1,2))) 

do j = 1, size(nodes1,1)
  nodes(j,:) = nodes1(j,:)
enddo
do j = 1, size(nodes2,1)
  nodes(j+size(nodes1,1),:) = nodes2(j,:)
enddo
do j = 1, size(faces1,1)
  faces(j,:) = faces1(j,:)
enddo
do j = 1, size(faces2,1)
  faces(j+size(faces1,1),:) = faces2(j,:) + size(nodes1,1)
enddo
do j = 1, size(elems1,1)
  elems(j,:) = elems1(j,:)
enddo
do j = 1, size(elems2,1)
  elems(j+size(elems1,1),:) = elems2(j,:) + size(nodes1,1)
enddo

write(*,*) 'nnodes = ', size(nodes,1)
write(*,*) 'nfaces = ', size(faces,1)
write(*,*) 'nelems = ', size(elems,1)
write(*,*)

! allocation
allocate(normals(size(faces,1),3))
allocate(centres(size(faces,1),3))
allocate(surf(size(faces,1)))
allocate(vols(size(elems,1)))
allocate(mu_i(size(faces,1)))
allocate(mu_e(size(faces,1)))
allocate(nu_i(size(faces,1)))
allocate(nu_e(size(faces,1)))
allocate(nutmp_i(size(faces,1),size(faces,1)))
allocate(nutmp_e(size(faces,1),size(faces,1)))
allocate(tau_i(size(faces,1),size(faces,1)))
allocate(Phi_i(size(faces,1)))
allocate(I_lambda(size(faces,1)))
allocate(I2_lambda(size(faces,1)))
allocate(f(size(faces,1)))

! geometry
s = normalize((/0.d0, 0.d0, 1.d0/))  ! target->sun
o = normalize((/0.d0, 0.d0, 1.d0/))  ! target->observer

! pre-computed
call normal(faces, nodes, normals)
call centre(faces, nodes, centres)
capS = surface(faces, nodes, surf)

call mu(normals, s, mu_i)
call mu(normals, o, mu_e)

! stellar surface
lambda_eff = 545.d-9  ! m
Delta_eff = 85.d-9  ! m
T = 5770.d0  ! K

B_lambda = planck(T, lambda_eff)
Phi_lambda = pi*B_lambda                ! over omega, half-space, cosine
J_lambda = pi*R_S**2 * B_lambda         ! over S, visible, cosine
P_lambda = 4.d0*pi * J_lambda           ! over omega, full-space
P_lambda = 4.d0*pi*R_S**2 * Phi_lambda  ! over S
P_V = Delta_eff*P_lambda                ! over lambda

! asteroid surface
d1 = 1.d0*au
A_w = 1.d0

Phi_lambda = P_lambda/(4.d0*pi*d1**2)
Phi_V = Phi_lambda*Delta_eff  ! W m^-2

f_L = A_w/(4.d0*pi)  ! sr^-1
A_hL = pi*f_L
A_gL = 2.d0/3.d0*pi*f_L
A_BL = pi*f_L

! observer location
d2 = 1.d0*au
omega = 1.d0/(d2**2)  ! sr

! calibration
Phi_nu_cal = 3.636d-23  ! W m^-2 Hz^-1; Bessel (2000)
Phi_lambda_cal = Phi_nu_cal*clight/lambda_eff**2
Phi_V_cal = Delta_eff*Phi_lambda_cal

! gnuplotting
open(unit=10, file='output.gnu', status='unknown')
write(10,*) 's1 = ', s(1)
write(10,*) 's2 = ', s(2)
write(10,*) 's3 = ', s(3)
write(10,*) 'o1 = ', o(1)
write(10,*) 'o2 = ', o(2)
write(10,*) 'o3 = ', o(3)
close(10)

m = 100
do k = 1, m
  call cpu_time(t1)

  ! orbital motion (simplified)
  r = (/2.5d0-5.d0*dble(k)/m, 0.d0, 2.5d0/)

  do i = 1, size(nodes2,1)
    nodes(i+size(nodes1,1),:) = nodes2(i,:) + r
  enddo
  call centre(faces, nodes, centres)

  ! non-illuminated || non-visible won't be computed
  nu_i = 1.d0
  nu_e = 1.d0
  !$omp parallel do private(i) shared(mu_i,mu_e,nu_i,nu_e)
  do i = 1, size(faces,1)
    if ((mu_i(i).eq.0.d0).or.(mu_e(i).eq.0.d0)) then
      nu_i(i) = 0.d0
      nu_e(i) = 0.d0
    endif
  enddo
  !$omp end parallel do

  ! shadowing
  call nu(faces, nodes, normals, centres, s, nu_i)
  call nu(faces, nodes, normals, centres, o, nu_e)

  ! integration
  tmp = 0.d0
  !$omp parallel do reduction(+:tmp) private(i) shared(faces,Phi_i,I_lambda,mu_i,mu_e,nu_i,nu_e,surf,f,f_L)
  do i = 1, size(faces,1)
    Phi_i(i) = Phi_lambda*mu_i(i)*nu_i(i)
    f(i) = f_L
    I_lambda(i) = f(i)*Phi_i(i)
    tmp = tmp + I_lambda(i)*surf(i)*mu_e(i)*nu_e(i)
  enddo
  !$omp end parallel do

  ! scattering
  call tau(normals, centres, tau_i)

  do i = 1, size(faces,1)
    tmp2 = 0.d0
    do j = 1, size(faces,1)
      if (i.ne.j) then
        r = centres(i,:) - centres(j,:)
        tmp2 = tmp2 + f(j)*Phi_i(j)*surf(j)*tau_i(i,j) / (pi*dot_product(r,r)) 
      endif
    enddo
    I2_lambda(i) = tmp2
    tmp = tmp + I2_lambda(i)*surf(i)*mu_e(i)*nu_e(i)
  enddo

  ! 2nd scattering

  ! lightcurve
  Phi_V = Delta_eff*omega*tmp
  V0 = 0.d0 - 2.5d0*log10(Phi_V/Phi_V_cal)

  call cpu_time(t2)
  write(*,*) 'k = ', k, ' cpu_time = ', t2-t1, ' s'

  open(unit=20, file='lc.dat', access='append')
  write(20,*) k, V0
  close(20)

  ! debugging
  if ((k.eq.1).or.(k.eq.49).or.(k.eq.50)) then
    write(str,'(i0.2)') k
    call write1("output.I_lambda." // trim(str), I_lambda)
    call write1("output.I2_lambda." // trim(str), I2_lambda)
    call write1("output.nu_i." // trim(str), nu_i)
    call write_node("output.node." // trim(str), nodes)
    call write_face("output.face." // trim(str), faces)
    call write_node("output.normal." // trim(str), normals)
    call write_node("output.centre." // trim(str), centres)
    if (size(faces,1).lt.200) call write2("output.tau_i." // trim(str), tau_i)
  endif

enddo

end program test_triangle


