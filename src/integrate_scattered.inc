! integrate_scattered.inc
! Integrate scattered radiation.
! Miroslav Broz (miroslav.broz@email.cz), Nov 3rd 2022

! Note: This is suitable for include '...', which is faster!

if (use_scattering) then

  !$omp parallel do reduction(+:tot) private(i,j,r,tmp) shared(centres,mu_e,nu_e,tau_i,surf,f,Phi_i,I2_lambda,f_L)
  do i = 1, size(centres,1)
    if (nu_e(i).ne.0.d0) then
      tmp = 0.d0
      do j = 1, size(centres,1)
        if (i.ne.j) then
          r = centres(i,:) - centres(j,:)
          tmp = tmp + f_L*Phi_i(j)*surf(j)*tau_i(i,j) / dot_product(r,r)
        endif
      enddo
      I2_lambda(i) = f(i)*tmp
      tot = tot + I2_lambda(i)*surf(i)*mu_e(i)*nu_e(i)
    else
      I2_lambda(i) = 0.d0
    endif
  enddo
  !$omp end parallel do

endif

