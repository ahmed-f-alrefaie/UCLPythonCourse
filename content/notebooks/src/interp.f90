subroutine fort_linear (y0, y1, x0, x1, x, n, c)
  integer, intent(in) :: n
  real(8), intent(in) :: y0(n), y1(n)
  real(8), intent(in) :: x0, x1

  real(8), intent(out) :: c(n)
  integer :: i,j
    
  do i = 1, n
    c(i) = (exp(y0(i)**2) * (x1 - x) + y1(i) * (x - x0))  / (x1 - x0)
  end do
end subroutine fort_linear