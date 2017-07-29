# Internal function that does the actually plotting inside qq.gam or zoo.qqGam
# INPUTS
# - P a list, the output of .discretize.qq.gam().
# - CI if TRUE we want to compute also conf interv
# - xlimit ylimit are self explanatory
# - show.reps, rl.col, rep.col, rep.alpha, ci.col, shape see qq.gam documentation
# OUTPUT
# - pl a ggplot object
.plot.qq.gam <- function(P, CI, show.reps, rl.col, rep.col, rep.alpha, ci.col, shape, 
                         xlimit=NULL, ylimit=NULL, ...)
{
  p1 <- ggplot()
  
  if( CI && !is.null(P$conf) ){ # Add confidence intervals
    p1 <- p1 + geom_polygon(data = P$conf, aes(x = x, y = y), fill = ci.col)
  }
  
  if( show.reps && !is.null(P$dm) ){ # Add a line for each simulation
    p1 <- p1 + geom_line(data=P$dm, aes(x = x, y = y, group = id), 
                         colour = rep.col, alpha = rep.alpha)
  }
  
  p1 <- p1 +
    geom_abline(colour = rl.col) +
    geom_point(data = data.frame("sx"=P$Dq, "sy"=P$D), aes(x = sx, y = sy), shape=shape) +
    labs(x = "theoretical quantiles", y = P$ylab, title = paste("Q-Q Plot, method =", P$method)) +
    coord_cartesian(xlim=xlimit, ylim=ylimit, expand=F) + theme_bw()
  
  return( p1 )
}