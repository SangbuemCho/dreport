\name{dreport.single}
\alias{dreport.single}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dreport.single(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, y) 
{
    model <- aov(y ~ x)
    com <- duncan.test(model, "x", alpha = 0.05)
    a <- arrange(com$groups, by = row.names(com$groups))
    b <- paste0(round(a[[1]], digits = 2), a[[2]])
    c <- round(sqrt(com$statistics[[1]]), digits = 2)
    d <- format(summary(model)[[1]][5]$`Pr(>F)`[1], format = "e", 
        digits = 2)
    out <- as.data.frame(matrix(c(b, c, d), 1, 5))
    colnames(out) <- c(levels(x), "SEM", "P value")
    rownames(out) <- paste(deparse(substitute(y)))
    env <- c(var_x = paste(deparse(substitute(x))), var_y = paste(deparse(substitute(y))))
    output <- list(env = env, summary = summary(model), multcom = com, 
        abstract = out)
    output
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
