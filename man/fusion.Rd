\name{pheno}
\alias{pheno}
\alias{fusion1}
\alias{fusion2}
\docType{data}
\title{ FUSION type 2 diabetes study }
\description{
Phenotype and genotype data from the Finland United States Investigation of
NIDDM (type 2) Diabetes (FUSION) study.
}
\usage{data(pheno); data(fusion1); data(fusion2)}
\format{
Data frames with the following variables.
  \describe{
    \item{\code{id}}{subject ID number for matching between data sets}
    \item{\code{t2d}}{a factor with levels \code{case} \code{control}}
    \item{\code{bmi}}{body mass index}
    \item{\code{sex}}{a factor with levels \code{F} \code{M}}
    \item{\code{age}}{age of subject at time phenotypes were colelcted}
    \item{\code{smoker}}{a factor with levels \code{former} \code{never} \code{occasional} \code{regular}}
    \item{\code{chol}}{total cholesterol}
    \item{\code{waist}}{waist circumference (cm)}
    \item{\code{weight}}{weight (kg) }
    \item{\code{height}}{height (cm) }
    \item{\code{whr}}{waist hip ratio }
    \item{\code{sbp}}{systolic blood pressure}
    \item{\code{dbp}}{diastolic blood pressure}
	\item{\code{marker}}{RS name of SNP}
    \item{\code{markerID}}{numeric ID for SNP}
    \item{\code{allele1}}{first allele coded as 1=A, 2=C, 3=G, 4=T}
    \item{\code{allele2}}{second allele coded as 1=A, 2=C, 3=G, 4=T}
    \item{\code{genotype}}{both alleles coded as a factor}
    \item{\code{Adose}}{number of A alleles}
    \item{\code{Cdose}}{number of C alleles}
    \item{\code{Gdose}}{number of G alleles}
    \item{\code{Tdose}}{number of T alleles}
  }
}
\source{Similar to the data presented in

Laura J. Scott, Karen L. Mohlke, Lori L. Bonnycastle, Cristen J. Willer, 
Yun Li, William L. Duren, Michael R. Erdos, Heather M. Stringham, Pe- 
ter S. Chines, Anne U. Jackson, Ludmila Prokunina-Olsson, Chia-Jen J. Ding, 
Amy J. Swift, Narisu Narisu, Tianle Hu, Randall Pruim, Rui Xiao, Xiao- 
Yi Y. Li, Karen N. Conneely, Nancy L. Riebow, Andrew G. Sprau, Maurine 
Tong, Peggy P. White, Kurt N. Hetrick, Michael W. Barnhart, Craig W. Bark, 
Janet L. Goldstein, Lee Watkins, Fang Xiang, Jouko Saramies, Thomas A. 
Buchanan, Richard M. Watanabe, Timo T. Valle, Leena Kinnunen, Goncalo R. 
Abecasis, Elizabeth W. Pugh, Kimberly F. Doheny, Richard N. Bergman, 
Jaakko Tuomilehto, Francis S. Collins, and Michael Boehnke, A genome-wide 
association study of type 2 diabetes in Finns detects multiple susceptibility vari- 
ants, \emph{Science} (2007). 
}
\examples{
data(pheno); data(fusion1); data(fusion2)
fusion1m <- merge(fusion1, pheno, by="id", all.x=FALSE, all.y=FALSE) 
xtabs(~t2d + genotype, fusion1m) 
xtabs(~t2d + Gdose, fusion1m) 
chisq.test( xtabs( ~t2d + genotype, fusion1m ) )
f1.glm <- glm( factor(t2d) ~ Gdose, fusion1m, family=binomial) 
summary(f1.glm)
}
\keyword{datasets}
