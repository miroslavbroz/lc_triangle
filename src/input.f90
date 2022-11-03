! input.f90
! Input parameters.
! Miroslav Broz (miroslav.broz@email.cz), Nov 3rd 2022

module input_module

character(len=255) :: f_node1, f_node2, f_face1, f_face2
double precision :: unit1, unit2
double precision :: lambda_eff, Delta_eff
double precision :: Phi_nu_cal, Phi_lambda_cal, Phi_V_cal
double precision :: A_w
character(len=16) :: law
double precision, dimension(3) :: s, o
double precision :: d1, d2
double precision, dimension(3) :: r_, v_
double precision :: dt
integer :: nsteps

double precision :: B0, minh, ming, bartheta  ! see hapke.f90

double precision :: Phi_lambda, f_L

namelist /input/ &
  f_node1, f_node2, f_face1, f_face2, &
  unit1, unit2, &
  lambda_eff, Delta_eff, &
  Phi_nu_cal, Phi_lambda_cal, Phi_V_cal, &
  A_w, &
  law, &
  s, o, &
  d1, d2, &
  r_, v_, &
  dt, &
  nsteps, &
  B0, minh, ming, bartheta

abstract interface
  double precision function f_func(f_L, mu_i, mu_e, alpha)
    double precision, intent(in) :: f_L, mu_i, mu_e, alpha
  end function f_func
end interface

procedure(f_func), pointer :: f_ptr => null()

contains

end module input_module


