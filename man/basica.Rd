\name{basica}
\alias{basica}
\title{Estadística Descriptiva Simple}
\usage{
basica(x)
}
\description{
Calcula la estadística básica de una variable. Por defecto aproxima 2 decimales.
}
\examples{
x<-rnorm(50,0,1)
basica(x,aprox=F) Estadística sin aproximación
basica(x) # Estadística con aproximación de 2 decimales
basica(x,n=3) # Estadística con aproximación de 3 decimales
}

