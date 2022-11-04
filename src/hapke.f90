! hapke.f90
! Hapke law.
! Miroslav Broz (miroslav.broz@email.cz), Nov 3rd 2022

module hapke_module

double precision :: B, P, r0

contains

double precision function f_hapke(f_L, mu_i, mu_e, alpha)

implicit none
double precision, intent(in) :: f_L, mu_i, mu_e, alpha
double precision :: tmp

tmp = mu_i+mu_e
if (tmp.ne.0.d0) then
  f_hapke = f_L/tmp * ((1.d0+B)*P + H(mu_i)*H(mu_e) - 1.d0)
else
  f_hapke = 0.d0
endif

return
end function f_hapke

double precision function H(mu)

use input_module, only : A_w

implicit none
double precision, intent(in) :: mu

H = 1.d0 - A_w*mu*(r0 + (1.d0-2.d0*r0*mu)/2.d0 * log((1.d0+mu)/mu))**(-1.d0)

return
end function H

subroutine init_hapke(alpha)

use input_module

implicit none
double precision, intent(in) :: alpha

B = B0/(1.d0+1.d0/minh*tan(alpha/2.d0))
P = (1.d0-ming**2)/(1.d0 + 2.d0*ming*cos(alpha) + ming**2)**(3.d0/2.d0)
r0 = (1.d0-sqrt(1.d0-A_w))/(1.d0+sqrt(1.d0-A_w))

write(*,*) 'B = ', B 
write(*,*) 'P = ', P 
write(*,*) 'r0 = ', r0 
write(*,*) ''

end subroutine init_hapke

end module hapke_module


