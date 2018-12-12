# Estadística Básica y Simple
#
# Función basica entrega estadística descriptiva
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

skewness<-function (x, na.rm = FALSE)
{
  if (is.matrix(x))
    apply(x, 2, skewness, na.rm = na.rm)
  else if (is.vector(x)) {
    if (na.rm)
      x <- x[!is.na(x)]
    n <- length(x)
    (sum((x - mean(x))^3)/n)/(sum((x - mean(x))^2)/n)^(3/2)
  }
  else if (is.data.frame(x))
    sapply(x, skewness, na.rm = na.rm)
  else skewness(as.vector(x), na.rm = na.rm)
}

kurtosis<-function (x, na.rm = FALSE)
{
  if (is.matrix(x))
    apply(x, 2, kurtosis, na.rm = na.rm)
  else if (is.vector(x)) {
    if (na.rm)
      x <- x[!is.na(x)]
    n <- length(x)
    n * sum((x - mean(x))^4)/(sum((x - mean(x))^2)^2)
  }
  else if (is.data.frame(x))
    sapply(x, kurtosis, na.rm = na.rm)
  else kurtosis(as.vector(x), na.rm = na.rm)
}

getExt <- function(file){
  ex <- strsplit(basename(file), split="\\.")[[1]]
  return(ex[-1])
}

basica<-function(x,aprox=T,n=3){
  if(aprox){warning(paste0("Tabla aproximada con ",n," decimales. n=número de decimales"))}
  res<-data.frame("m"=mean(x),"sd"=sd(x),"var"=var(x),
                  "min"=min(x),"max"=max(x),"skew"=skewness(x),"kurtosis"=kurtosis(x),
                  "shapiro"=shapiro.test(x)$p.value)
  if(aprox){res<-round(res,n)}
  return(res)
}
'%!in%' <- function(x,y)!('%in%'(x,y))
cargar<-function(dec=".",sep=";",...){
  fn<-file.choose()
  ext<-getExt(fn)
  if(ext %!in% c("","csv","txt","xls","xlsx")){stop("Debes seleccionar un archivo con extensión apropiada (csv,txt,xls,xlsx)")}
  if(ext=="xlsx"){
    dat<-readxl::read_excel(fn)}else{
    dat<-read.table(fn,dec=dec,sep=sep,...)
    }
  warning(paste("Archivo seleccionado: ",fn))
  View(dat)
  return(dat)
}

tabla<-function(x,k=NULL){
  if(!is.numeric(k)|k<=1){stop("k debe ser número mayor a 1")}
  n<-length(x)
  if(is.null(k)){
    K<-log2(n)+1
    K<-round(K,digits=1)}else{K=k}
  levels(cut(x,breaks=K))
  ni=table(cut(x,breaks=K))
  fi=prop.table(table(cut(x,breaks=K)))
  Ni=cumsum(table(cut(x,breaks=K)))
  Fi=cumsum(prop.table(table(cut(x,breaks=K))))
  return(data.frame(cbind(ni,fi,Ni,Fi)))}


gr<-function(x){
type<-dlg_list(c("plot","hist","qqnorm"), preselect = "plot", multiple = FALSE, title = "Selecciona el tipo de gráfico:\n 1.- plot: Gráfico de puntos \n 2.- hist: Histograma de frecuencias \n 3.- qqnorm: Grafico cuantil cuantil\n",
         gui = .GUI)$res
main <- svDialogs::dlgInput("Título del gráfico:\n")$res
xlab<-svDialogs::dlgInput("Nombre del eje X:\n")$res
ylab<-svDialogs::dlgInput("Nombre del eje Y:\n")$res
if(type=="hist"){args=", freq=F"}else{args=""}

eval(parse(text=paste0(type,"(",deparse(substitute(x)),
                       ", main= '",main,
                       "' ,xlab= '",xlab,
                       "', ylab= '",ylab,"'",args,")")))


if(type=="qqnorm"){
  line<-paste0("qqline(",deparse(substitute(x)),",col='red')")
  qqline(x,col="red")
}
else if(type=="hist"){
  line<-paste0("lines(density(",deparse(substitute(x)),"),col='red')")
  lines(density(x),col="red")
}
else{line<-""}

cat("Puedes obtener el mismo gráfico con la siguiente función:\n",type,"(",deparse(substitute(x)),
    ", \n main= '",main,
    "', \n xlab= '",xlab,
    "', \n ylab= '",ylab,"' ",args,")\n",line,sep="")

#plot(x,main=main,xlab=xlab,ylab=ylab)
}
