\name{CallCVX}
\alias{CallCVX}
\title{Simple R interface to CVX.}
\description{
This function takes a string containing what you'd normally put between
cvx_begin and cvx_end.  Returns CVX output.
}
\usage{
CallCVX(cvx.code, const.vars, opt.var.names, setup.dir = NULL, norun = FALSE,
        matlab.call = "matlab", cvx.modifiers = NULL)
}
\arguments{
  \item{cvx.code}{
String containing call to CVX, i.e. what's inside cvx_begin and cvx_end
}
  \item{const.vars}{
List of non-optimization variables used in CVX expression.  Labels of
list elements should match what the corresponding variable name in
cvx.code.  E.g. for the Lasso, this would be list(y=y, x=x, lam=lam, p=p)
}
  \item{opt.var.names}{Array of names of optimization
    variables. E.g. for the Lasso, this would be "b"
}
  \item{setup.dir}{
Directory containing the file cvx_setup.  If not needed, leave NULL.
}
  \item{norun}{
Default FALSE.  Mostly for debugging purposes.  Doesn't call Matlab.  Returns the command that would be run in Matlab.
}
  \item{matlab.call}{How Matlab can be invoked through the "system" command.  Default: "matlab" but even if this is the alias in your default shell, "system" might use a different shell in which "matlab" is not recognized.
}
  \item{cvx.modifiers}{Optional string of modifiers passed to CVX on same line as cvx_begin. E.g. "quiet" or "sdp".}
}
\details{
This function is based on \code{CallMatlab}.
}
\value{
  A list of the optimization variables specified in opt.var.names.  Also,
  \item{cvx_optval}{The optimal value as returned by CVX.}
  \item{time}{Elapsed time specifically for CVX call (i.e. excludes
    opening Matlab, loading data, etc.)}
}
\references{
M. Grant and S. Boyd. CVX: Matlab software for disciplined convex programming, version 1.21. http://cvxr.com/cvx, April 2011.

M. Grant and S. Boyd. Graph implementations for nonsmooth convex
programs, Recent Advances in Learning and Control (a tribute to
M. Vidyasagar), V. Blondel, S. Boyd, and H. Kimura, editors, pages
95-110, Lecture Notes in Control and Information Sciences, Springer,
2008. http://stanford.edu/~boyd/graph_dcp.html.
}
\author{
Jacob Bien
}

\seealso{
\code{\link{CallMatlab}}
}
\examples{
# 1) solve both forms of the lasso using CVX
n <- 50
p <- 10
x <- matrix(rnorm(n * p), n, p)
beta <- rnorm(p)
y <- x \%*\% beta + 0.1 * rnorm(n)
lam <- 2
# to call CVX, set setup.dir to be the directory containing "cvx_setup.m"
\dontrun{
setup.dir <- "change/this/to/your/cvx/directory"
lasso <- CallCVX("variables b(p);minimize(square_pos(norm(y-x*b,2))/2+lam*norm(b,1))",
                 const.vars=list(p=p, y=y, x=x, lam=lam),
                 opt.var.names="b", setup.dir=setup.dir)

s <- .1
lasso.boundform <- CallCVX("variables b(p);minimize(norm(y-x*b, 2));subject to;norm(b,1)<=s",
                           const.vars=list(p=p, y=y, x=x, s=s),
                           opt.var.names="b", setup.dir=setup.dir)
}
# 2) solve the graphical lasso using CVX
n <- 50
p <- 10
x <- matrix(rnorm(n * p), n, p)
S <- cov(x)
rho <- .1
\dontrun{
glasso <- CallCVX("variables Th(p,p);minimize(-log_det(Th)+trace(S*Th)+rho*norm(vec(Th),1));Th==semidefinite(p)",
                  const.vars=list(p=p, S=S, rho=rho),
                  opt.var.names="Th", setup.dir=setup.dir)
}
}
