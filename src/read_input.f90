! read_input.f90
! Read input.
! Miroslav Broz (miroslav.broz@email.cz), Nov 3rd 2022

module read_input_module

contains

subroutine read_input()

use const_module
use input_module
use normalize_module
use lambert_module
use lommel_module
use hapke_module

implicit none
character(len=255), parameter :: nml='input.nml'

open(unit=10, file=nml, status='old')
read(10, nml=input)
close(10)

! units
s = normalize(s)
o = normalize(o)
d1 = d1*au
d2 = d2*au

! output
write(*,*) '# input parameters:'
write(*,*) 'f_node1 = ', trim(f_node1)
write(*,*) 'f_face1 = ', trim(f_face1)
write(*,*) 'f_node2 = ', trim(f_node2)
write(*,*) 'f_face2 = ', trim(f_face2)
write(*,*) 'unit1 = ', unit1, ' m'
write(*,*) 'unit2 = ', unit2, ' m'
write(*,*) 'T_star = ', T_star, ' K'
write(*,*) 'T_eq = ', T_eq, ' K'
write(*,*) 'Phi_nu_cal = ', Phi_nu_cal, ' W m^-2 sr^-1 Hz^-1'
write(*,*) 'A_w = ', A_w
write(*,*) 's = ', s
write(*,*) 'o = ', o
write(*,*) 'd1 = ', d1, ' m'
write(*,*) 'd2 = ', d2, ' m'
write(*,*) 'r_ = ', r_, ' m'
write(*,*) 'v_ = ', v_, ' m s^-1'
write(*,*) 'dt = ', dt, ' s'
write(*,*) 'nsteps = ', nsteps
write(*,*) 'B0 = ', B0
write(*,*) 'minh = ', minh
write(*,*) 'ming = ', ming
write(*,*) 'bartheta = ', bartheta
write(*,*) 'use_shadowing = ', use_shadowing
write(*,*) 'use_scattering = ', use_scattering
write(*,*) 'use_thermal = ', use_thermal

if (law(1:2).eq.'La') then
  f_ptr => f_lambert
  write(*,*) 'scattering is Lambert'
elseif (law(1:2).eq.'Lo') then
  f_ptr => f_lommel
  write(*,*) 'scattering is Lommel'
elseif (law(1:2).eq.'Ha') then
  f_ptr => f_hapke
  write(*,*) 'scattering is Hapke'
else
  write(*,*) 'Error: scattering is undefined!'
  stop
endif

write(*,*) ''

end subroutine read_input

end module read_input_module



