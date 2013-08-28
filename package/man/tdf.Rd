\name{tdf}
\alias{tdf}
\title{Compute degrees of freedom for a 2-sample t-test}
\usage{
  tdf(sd1, sd2, n1, n2)
}
\arguments{
  \item{sd1}{standard deviation of the sample 1}

  \item{sd2}{standard deviation of the sample 2}

  \item{n1}{size of sample 1}

  \item{n2}{size of sample 2}
}
\value{
  estimated degrees of freedom for 2-sample t-test
}
\description{
  This function computes degrees of freedom for a 2-sample
  t-test from the standard deviations and sample sizes of
  the two samples.
}
\examples{
data(KidsFeet)
fs <- favstats( length ~ sex, data=KidsFeet ); fs
t.test( length ~ sex, data=KidsFeet )
tdf( fs[1,'sd'], fs[2,'sd'], fs[1,'n'], fs[2,'n'])
}

