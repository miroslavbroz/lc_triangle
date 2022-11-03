! integrate_over_S.inc
! Integrate over the surface.
! Miroslav Broz (miroslav.broz@email.cz), Nov 3rd 2022

! Note: This is suitable for include '...', which is faster...

tot = 0.d0
!$omp parallel do reduction(+:tot) private(i) shared(mu_i,mu_e,nu_i,nu_e,surf,f_L,f,Phi_i,I_lambda)
do i = 1, size(I_lambda,1)
!  if (nu_i(i).ne.0.d0) then
    f(i) = f_ptr(f_L, mu_i(i), mu_e(i), alpha)
    Phi_i(i) = Phi_lambda*mu_i(i)*nu_i(i)
    I_lambda(i) = f(i)*Phi_i(i)
    tot = tot + I_lambda(i)*surf(i)*mu_e(i)*nu_e(i)
!  endif
enddo
!$omp end parallel do


