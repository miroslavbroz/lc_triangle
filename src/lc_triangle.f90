! lc_triangle.f90
! Test of lightcurve computation for a general triangular mesh.
! Miroslav Broz (miroslav.broz@email.cz), Nov 1st 2022

! Notation:
!
! I_lambda       .. monochromatic intensity (ces. intenzita), W m^-2 sr^-1 m^-1
! I2_lambda      .. monochromatic intensity, scattered
! B_lambda       .. Planck monochromatic intensity
! Phi_lambda     .. monochromatic flux, W m^-2 m^-1
! Phi_lambda_cal .. monochromatic flux, calibration
! Phi_nu_cal     .. monochromatic flux, calibration, W m^-2 Hz^-1
! Phi_i          .. monochromatic flux, incoming
! Phi_V          .. passband flux, total, outgoing, W m^-2
! Phi_V_cal      .. passband flux, calibration
! J_lambda       .. monochromatic luminosity (ces. zarivost), W sr^-1 m^-1
! P_lambda       .. monochromatic power, W m^-1
! P_V            .. passband power, W
! mu_i           .. directional cosine, incoming, cos(alpha)
! mu_e           .. directional cosine, outgoing
! alpha          .. phase angle, sun-target-observer
! nu_i           .. shadowing, incoming, 0 or 1
! nu_e           .. shadowing, outgoing
! tau_i          .. visibility (mutual), cos(alpha)*cos(alpha')
! omega          .. solid angle, sr
! lambda_eff     .. effective wavelength, m
! Delta_eff      .. effective pasband, m
! f              .. bi-directional distribution function, 1
! f_L            .. Lambert law
! f_g            .. geometric law
! f_LS           .. Lommel-Seeliger law
! A_w            .. single-scattering albedo, 1
! A_hL           .. hemispherical albedo, Lambert
! A_gL           .. geometric albedo, Lambert
! A_BL           .. Bond albedo, Lambert

! nodes          .. nodes, m
! faces          .. triangular faces, 1
! elems          .. tetrahedral elements, 1
! normals        .. normals of triangles, 1
! centres        .. centres of triangles, m
! surf           .. surface of triangles, m^2
! vols           .. volumes of tetrahedra, m^3
! capR           .. radius, m
! capS           .. surface area, m^2
! capV           .. volume, m^3
! s              .. target->sun unitvector
! o              .. target->observer unitvector
! d1             .. target-sun distance, m
! d2             .. target-observer distance, m


program lc_triangle

use const_module
use input_module
use normalize_module
use normal_module
use centre_module
use surface_module
use planck_module
use read_input_module
use read_face_module
use read_node_module
use read_elem_module
use write1_module
use write2_module
use write_face_module
use write_node_module
use shadowing_module
use scattering_module
use hapke_module

implicit none

integer, dimension(:,:), pointer :: faces, faces1, faces2
integer, dimension(:,:), pointer :: elems, elems1, elems2
double precision, dimension(:,:), pointer :: nodes, nodes1, nodes2
double precision, dimension(:,:), pointer :: normals, centres
double precision, dimension(:), pointer :: surf, vols
double precision, dimension(:), pointer :: mu_i, mu_e, nu_i, nu_e, f, Phi_i
double precision, dimension(:), pointer :: I_lambda, I2_lambda, I3_lambda
double precision, dimension(:,:), pointer :: nutmp_i, nutmp_e
double precision, dimension(:,:), pointer :: tau_i

integer :: i, j, k, n, m
double precision, dimension(3) :: r
double precision :: capR, capS, capV
double precision :: A_hL, A_gL, A_BL
double precision :: alpha, omega
double precision :: B_lambda, J_lambda, P_lambda, P_V, Phi_V, V0
double precision :: B_thermal, Phi_thermal
double precision :: tot, tmp
double precision :: varphi_i, varphi_e
double precision :: t1, t2
character(len=80) :: str

! input parameters
call read_input()

! read 2 objects...
call read_node(f_node1, nodes1)
call read_face(f_face1, faces1)

call read_node(f_node2, nodes2)
call read_face(f_face2, faces2)

! units
nodes1 = nodes1*unit1
nodes2 = nodes2*unit2

! ... and merge them
allocate(nodes(size(nodes1,1)+size(nodes2,1), size(nodes1,2))) 
allocate(faces(size(faces1,1)+size(faces2,1), size(faces1,2))) 

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

write(*,*) 'nnodes = ', size(nodes,1)
write(*,*) 'nfaces = ', size(faces,1)
write(*,*) ''

! allocation
allocate(normals(size(faces,1),3))
allocate(centres(size(faces,1),3))
allocate(surf(size(faces,1)))
allocate(mu_i(size(faces,1)))
allocate(mu_e(size(faces,1)))
allocate(nu_i(size(faces,1)))
allocate(nu_e(size(faces,1)))
allocate(nutmp_i(size(faces,1),size(faces,1)))
allocate(nutmp_e(size(faces,1),size(faces,1)))
allocate(tau_i(size(faces,1),size(faces,1)))
allocate(f(size(faces,1)))
allocate(Phi_i(size(faces,1)))
allocate(I_lambda(size(faces,1)))
allocate(I2_lambda(size(faces,1)))
allocate(I3_lambda(size(faces,1)))

! geometry
call normal(faces, nodes, normals)
call centre(faces, nodes, centres)
capS = surface(faces, nodes, surf)

call mu(normals, s, mu_i)
call mu(normals, o, mu_e)
alpha = acos(dot_product(s,o))

write(*,*) 'capS = ', capS, ' m^2'
write(*,*) 'capR = ', sqrt(capS/(4.d0*pi)), ' m  (surface-equivalent)'
write(*,*) 'alpha = ', alpha, ' rad = ', alpha/deg, ' deg'
write(*,*) ''

! stellar surface
B_lambda = planck(T_star, lambda_eff)
Phi_lambda = pi*B_lambda                ! over omega, half-space, cosine
J_lambda = pi*R_S**2 * B_lambda         ! over S, visible, cosine
P_lambda = 4.d0*pi * J_lambda           ! over omega, full-space
P_lambda = 4.d0*pi*R_S**2 * Phi_lambda  ! over S
P_V = Delta_eff*P_lambda                ! over lambda

write(*,*) '# at stellar surface:'
write(*,*) 'T_star = ', T_star, ' K'
write(*,*) 'lambda_eff = ', lambda_eff, ' m'
write(*,*) 'Delta_eff = ', Delta_eff, ' m'
write(*,*) 'B_lambda = ', B_lambda, ' W m^-2 sr^-1 m^-1'
write(*,*) 'Phi_lambda = ', Phi_lambda, ' W m^-2 m^-1'
write(*,*) 'J_lambda = ', J_lambda, ' W sr^-1 m^-1'
write(*,*) 'P_lambda = ', P_lambda, ' W m^-1'
write(*,*) 'P_V = ', P_V, ' W'
write(*,*) ''

! asteroid surface
Phi_lambda = P_lambda/(4.d0*pi*d1**2)
Phi_V = Phi_lambda*Delta_eff

f_L = A_w/(4.d0*pi)  ! sr^-1
A_hL = pi*f_L
A_gL = 2.d0/3.d0*pi*f_L
A_BL = pi*f_L

call init_hapke(alpha)

write(*,*) '# at asteroid surface:'
write(*,*) 'd1 = ', d1/au, ' au'
write(*,*) 'Phi_lambda = ', Phi_lambda, ' W m^-2 m^-1'
write(*,*) 'Phi_V = ', Phi_V, ' W m^-2'
write(*,*) 'f_L = ', f_L, ' sr^-1'
write(*,*) 'A_w = ', A_w
write(*,*) 'A_hL = ', A_hL
write(*,*) 'A_gL = ', A_gL
write(*,*) 'A_BL = ', A_BL
write(*,*) ''

B_thermal = planck(T_eq, lambda_eff)
Phi_thermal = pi*B_thermal

write(*,*) 'T_eq = ', T_eq, ' K'
write(*,*) 'B_thermal = ', B_thermal, ' W m^-2 sr^-1 m^-1'
write(*,*) 'Phi_thermal = ', Phi_thermal, ' W m^-2 m^-1'
write(*,*) ''

! observer location
omega = 1.d0/(d2**2)  ! sr

write(*,*) '# at observer location:'
write(*,*) 'd2 = ', d2/au, ' au'
write(*,*) 'omega = ', omega, ' sr'

! calibration
Phi_lambda_cal = Phi_nu_cal*clight/lambda_eff**2
Phi_V_cal = Delta_eff*Phi_lambda_cal

write(*,*) 'Phi_nu_cal = ', Phi_nu_cal, ' W m^-2 Hz^-1'
write(*,*) 'Phi_lambda_cal = ', Phi_lambda_cal, ' W m^-2 m^-1'
write(*,*) 'Phi_V_cal = ', Phi_V_cal, ' W m^-2'
write(*,*) ''

! gnuplotting
open(unit=10, file='output.gnu', status='unknown')
write(10,*) 's1 = ', s(1)
write(10,*) 's2 = ', s(2)
write(10,*) 's3 = ', s(3)
write(10,*) 'o1 = ', o(1)
write(10,*) 'o2 = ', o(2)
write(10,*) 'o3 = ', o(3)
close(10)

do k = 1, nsteps
  call cpu_time(t1)

  ! orbital motion (simplified)
  r = r_ + v_*dt*k

  do i = 1, size(nodes2,1)
    nodes(i+size(nodes1,1),:) = nodes2(i,:) + r
  enddo
  call centre(faces, nodes, centres)

  ! non-illuminated || non-visible won't be computed
  call non(mu_i, mu_e, nu_i, nu_e)

  ! shadowing
  call nu(faces, nodes, normals, centres, s, nu_i)
  call nu(faces, nodes, normals, centres, o, nu_e)

  ! integration
  include 'integrate_over_S.inc'

  ! visibility
  call tau(normals, centres, tau_i)

  ! scattering
  include 'integrate_scattered.inc'

  ! thermal
  include 'integrate_thermal.inc'

  ! lightcurve
  Phi_V = Delta_eff*omega*tot
  V0 = 0.d0 - 2.5d0*log10(Phi_V/Phi_V_cal)

  write(*,*) 'Phi_V = ', Phi_V, ' W m^-2'
  write(*,*) 'V0 = ', V0, ' mag'

  call cpu_time(t2)
  write(*,*) 'k = ', k, ' cpu_time = ', t2-t1, ' s'

  open(unit=20, file='lc.dat', access='append')
  write(20,*) k, V0
  close(20)

  ! debugging
  if (debug) then
    if ((k.eq.1).or.(k.eq.49).or.(k.eq.50)) then
      write(str,'(i0.2)') k
      call write1("output.I_lambda." // trim(str), I_lambda)
      call write1("output.I2_lambda." // trim(str), I2_lambda)
      call write1("output.I3_lambda." // trim(str), I3_lambda)
      call write1("output.nu_i." // trim(str), nu_i)
      call write1("output.mu_i." // trim(str), mu_i)
      call write1("output.f." // trim(str), f)
      call write_node("output.node." // trim(str), nodes)
      call write_face("output.face." // trim(str), faces)
      call write_node("output.normal." // trim(str), normals)
      call write_node("output.centre." // trim(str), centres)
      if (size(faces,1).lt.200) call write2("output.tau_i." // trim(str), tau_i)
    endif
  endif

enddo

end program lc_triangle


