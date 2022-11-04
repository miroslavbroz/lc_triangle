! input.f90
! Input parameters.
! Miroslav Broz (miroslav.broz@email.cz), Nov 3rd 2022

module input_module

use const_module

character(len=255) :: f_node1, f_face1, f_node2, f_face2

double precision :: unit1 = 1.d0  ! m
double precision :: unit2 = 1.d0  ! m
double precision :: lambda_eff = 545.d-9  ! m
double precision :: Delta_eff = 85.d-9  ! m
double precision :: T_star = 5770.d0  ! K
double precision :: T_eq = 300.d0  ! K
double precision :: Phi_nu_cal = 3.636d-23  ! W m^-2 Hz^-1; Bessel (2000)
double precision :: A_w = 1.d0
character(len=16) :: law = 'Lambert'

double precision, dimension(3) :: s = (/0.d0, 0.d0, 1.d0/)
double precision, dimension(3) :: o = (/0.d0, 0.d0, 1.d0/)
double precision, dimension(3) :: r_ = (/0.d0, 0.d0, 1.d0/)  ! m
double precision, dimension(3) :: v_ = (/0.d0, 0.d0, 0.d0/)  ! m s^-1
double precision :: d1 = 1.d0  ! au
double precision :: d2 = 1.d0  ! au
double precision :: dt = 0.d0  ! s
integer :: nsteps = 1

! cf. hapke.f90
double precision :: B0 = 0.1d0       ! opposition effect amplitude; 1
double precision :: minh = 0.2d0     ! opposition effect width; 1
double precision :: ming = 0.d0      ! asymmetry of scattering; 1
double precision :: bartheta = 0.d0  ! average slope, macroscopic roughness; rad <- NOT USED!

logical :: use_shadowing = .true.
logical :: use_scattering = .true.
logical :: use_thermal = .true.
logical :: debug = .true.

! common
double precision :: Phi_lambda, Phi_lambda_cal, Phi_V_cal, f_L

namelist /input/ &
  f_node1, f_node2, f_face1, f_face2, &
  unit1, &
  unit2, &
  lambda_eff, &
  Delta_eff, &
  T_star, &
  T_eq, &
  Phi_nu_cal, &
  A_w, &
  law, &
  s, &
  o, &
  d1, &
  d2, &
  r_, &
  v_, &
  dt, &
  nsteps, &
  B0, &
  minh, &
  ming, &
  bartheta, &
  use_shadowing, &
  use_scattering, &
  use_thermal, &
  debug

abstract interface
  double precision function f_func(f_L, mu_i, mu_e, alpha)
    double precision, intent(in) :: f_L, mu_i, mu_e, alpha
  end function f_func
end interface

procedure(f_func), pointer :: f_ptr => null()

contains

end module input_module


