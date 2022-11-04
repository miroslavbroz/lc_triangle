! integrate_thermal.inc
! Integrate thermal radiation.
! Miroslav Broz (miroslav.broz@email.cz), Nov 3rd 2022

! Note: This is suitable for include '...', which is faster!

! Note: Constant thermal flux, i.e., VERY simplified.

if (use_thermal) then

  !$omp parallel do reduction(+:tot) private(i) shared(mu_e,nu_e,surf,f_L,I3_lambda)
  do i = 1, size(surf,1)
    if (nu_e(i).ne.0.d0) then
      I3_lambda(i) = f_L*Phi_thermal
      tot = tot + I3_lambda(i)*surf(i)*mu_e(i)*nu_e(i)
    else
      I3_lambda(i) = 0.d0
    endif
  enddo
  !$omp end parallel do

endif


