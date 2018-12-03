#'@description matrix inversion using cholesky decomp. with backward substitution
#'@export
#'@title solve_chol
#'@name solve_chol
#'@param M symmetric matrix to invert
# 
solve_chol <- function (M) {
  U=chol(M)
  Uinv=solve(U)
  Minv=Uinv%*%t(Uinv)
  Minv
}
