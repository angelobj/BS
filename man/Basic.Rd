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

\name{cargar}
\alias{cargar}
\title{Cargar Archivos de datos}
\arguments{
  \item{header}{Si el archivo tiene nombre de columnas, usar header=T}
  \item{dec}{Si el separador decimal es la coma (","), usar dec=","}
  \item{sep}{Separador de columnas, por defecto sep=";"}
}
\usage{
x<-cargar()
}
\description{
Permite cargar datos sólo seleccionando el archivo con la extensión apropiada (.csv,.txt,.xls,.xlsx)
}
\examples{
x<-cargar()
}
