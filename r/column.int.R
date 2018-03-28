#'
#' @param int.fun	function to be integrated, entered as a character string or as class function, default ‘dnorm(x)’
#' @param num.poly	numeric vector indicating number of columns to use for integration, default 20
#' @param from.x	numeric vector indicating starting x value for integration, default -1.96
#' @param to.x	numeric vector indicating ending x value for integration, default 1.96
#' @param plot.cum	logical value indicating if a second plot is included that shows cumulative estimated integration with increasing number of columns up to num.poly, default FALSE
#' 
#' @author fawda123
#' @source https://gist.github.com/fawda123/5568685
#' 
column.int <- function(int.fun='dnorm(x)', num.poly=20, from.x=-1.96, to.x=1.96, plot.cum=F){
  
  if(from.x > to.x) stop('Starting x must be less than ending x')
  
  if(is.character(int.fun)) int.fun <- eval(parse(text=paste('function(x)', int.fun)))
  
  int.base <- 0	
  poly.x <- seq(from.x, to.x, length=num.poly + 1)
  
  polys <- sapply(
    1:(length(poly.x) - 1),
    function(i){
      
      x.strt <- poly.x[i]
      x.stop <- poly.x[i+1]
      
      cord.x <- rep(c(x.strt, x.stop), each=2) 
      cord.y <- c(int.base, rep(int.fun(mean(c(x.strt, x.stop))), 2), int.base) 
      data.frame(cord.x, cord.y)
      
    },
    simplify = FALSE
  )
  
  area <- sum(unlist(lapply(
    polys,
    function(x) diff(unique(x[, 1]))*diff(unique(x[, 2]))
  )))
  return(area)
}
#'
#' @param int.fun	function to be integrated, entered as a character string or as class function, default ‘dnorm(x)’
#' @param num.poly	numeric vector indicating number of columns to use for integration, default 20
#' @param from.x	numeric vector indicating starting x value for integration, default -1.96
#' @param to.x	numeric vector indicating ending x value for integration, default 1.96
#' @param plot.cum	logical value indicating if a second plot is included that shows cumulative estimated integration with increasing number of columns up to num.poly, default FALSE
#' 
#' @author fawda123
#' @source https://gist.github.com/fawda123/5568685
#' 
plot.column.int <- function(int.fun='dnorm(x)', num.poly=20, from.x=-1.96, to.x=1.96, plot.cum=F){
  
  if (!require(ggplot2) | !require(scales) | !require(gridExtra)) {
    stop("This app requires the ggplot2, scales, and gridExtra packages. To install, run 'install.packages(\"name\")'.\n")
  }
  
  if(from.x>to.x) stop('Starting x must be less than ending x')
  
  if(is.character(int.fun)) int.fun<-eval(parse(text=paste('function(x)',int.fun)))
  
  int.base<-0	
  poly.x<-seq(from.x,to.x,length=num.poly+1)
  
  polys<-sapply(
    1:(length(poly.x)-1),
    function(i){
      
      x.strt<-poly.x[i]
      x.stop<-poly.x[i+1]
      
      cord.x<-rep(c(x.strt,x.stop),each=2) 
      cord.y<-c(int.base,rep(int.fun(mean(c(x.strt,x.stop))),2),int.base) 
      data.frame(cord.x,cord.y)
      
    },
    simplify=F
  )
  
  area<-sum(unlist(lapply(
    polys,
    function(x) diff(unique(x[,1]))*diff(unique(x[,2]))
  )))
  txt.val<-paste('Area from',from.x,'to',to.x,'=',round(area,4),collapse=' ')
  
  y.col<-rep(unlist(lapply(polys,function(x) max(abs(x[,2])))),each=4)
  plot.polys<-data.frame(do.call('rbind',polys),y.col)
  
  p1<-ggplot(data.frame(x=c(from.x,to.x)), aes(x)) + stat_function(fun=int.fun)
  if(num.poly==1){ 
    p1<-p1 + geom_polygon(data=plot.polys,mapping=aes(x=cord.x,y=cord.y),
                          alpha=0.7,color=alpha('black',0.6))
  }
  else{
    p1<-p1 + geom_polygon(data=plot.polys,mapping=aes(x=cord.x,y=cord.y,fill=y.col,
                                                      group=y.col),alpha=0.6,color=alpha('black',0.6))
  }
  p1<-p1 + ggtitle(txt.val) + theme(legend.position="none") 
  
  if(!plot.cum) print(p1)
  
  else{
    
    area.cum<-unlist(
      sapply(
        1:num.poly,
        function(val){
          poly.x<-seq(from.x,to.x,length=val+1)
          
          polys<-sapply(
            1:(length(poly.x)-1),
            function(i){
              
              x.strt<-poly.x[i]
              x.stop<-poly.x[i+1]
              
              cord.x<-rep(c(x.strt,x.stop),each=2) 
              cord.y<-c(int.base,rep(int.fun(mean(c(x.strt,x.stop))),2),int.base) 
              data.frame(cord.x,cord.y)
              
            },
            simplify=F
          )
          
          sum(unlist(lapply(
            polys,
            function(x) diff(unique(x[,1]))*diff(unique(x[,2]))
          )))
          
        }
      )
    )
    
    dat.cum<-data.frame(Columns=1:num.poly,Area=area.cum)
    actual<-integrate(int.fun,from.x,to.x)
    
    p2<-ggplot(dat.cum, aes(x=Columns,y=Area)) + geom_point()
    p2<-p2 + geom_hline(yintercept=actual$value,lty=2) 
    p2<-p2 + ggtitle(paste('Actual integration',round(actual$value,4),'with absolute error',prettyNum(actual$abs.error)))
    
    grid.arrange(p1,p2)
    
  }
  
}