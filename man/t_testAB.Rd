\name{t_testAB}
\alias{t_testAB}

\title{Testul t al lui Student cu testarea preliminara a omogenitatii variantelor}

\description{
  Executa testul t pentru un singur esantion sau pentru doua esantioane. In cazul a doua
  esantioane independente, functia verifica daca variatiile celor doua esantioane sunt
  egale prin intermediul testului Ansari-Bradley (in SPSS se utilizeaza testul Levene)
}

\usage{
t_testAB(x, y=NULL,
         alternative = c("two.sided", "less", "greater"), var.equal = FALSE,
         mu = 0, paired = FALSE, conf.level = 0.95, data)
}

\arguments{
  \item{x}{Un vector de valori}
  \item{y}{Un vector optional de valori, corespunzator celui de al doilea grup}
  \item{alternative}{Un sir de caractere reprezentand ipoteza alternativa; accepta
        '"two.sided"' ori '"!="' pentru ipoteza bidirectionala, '"less"' ori '"<"'
        pentru ipoteza unidirectionala pe coada din stanga si '"greater"' ori '">"'
        pentru ipoteza unidirectionala pe coada din dreapta}
  \item{var.equal}{Argument logic care specifica daca grupurile sunt omogene sau nu}
  \item{mu}{Valoarea mediei din populatie cu care se compara media din esantion}
  \item{paired}{Argument logic indicand daca se doreste un test t pentru esantioane pereche}
  \item{conf.level}{Nivelul de incredere}
  \item{data}{O matrice sau un set de date optional care contine variabilele din formula}
}


\author{Adrian Dusa}

\examples{

grup1 <- c(13, 14,  9, 12,  8, 10,  5, 10,  9, 12, 16)
grup2 <- c(16, 18, 11, 19, 14, 17, 13, 16, 17, 18, 22, 12)

t_testAB(grup1, grup2)


# daca datele sunt aranjate sub forma de set de date
setdate <- data.frame(valori=c(grup1, grup2), grup=c(rep(1,11), rep(2,12)))

t_testAB(valori ~ grup, data=setdate)

}

\keyword{misc}

