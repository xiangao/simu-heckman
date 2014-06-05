library(shiny)
library(ggplot2)
library(reshape)

results <- read.csv('results.csv')

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
output$plot1 <-  renderPlot({
    nobs = ceiling(exp(input$n)/100)*100
    cr1w=input$cr1w
    cr1u=input$cr1u
    sel.cri <- input$sel.cri
    match <- results[(nobs==results$nobs &  (cr1w-results$cr1w < 1e-5) & (cr1u-results$cr1u < 1e-5) & (sel.cri-results$sel.cri < 1e-5)  &  !is.na(results$lm)),c('nobs','nsim','lm','heck.noinst','heck.inst')]
    match.melt <- melt(match, id.vars=c('nobs','nsim'))
    names(match.melt) <- c('nobs','nsim','type','bias')
    ave.bias <- ddply(match.melt, c('type'),
                       function(x) data.frame(
                         mean.bias=mean(x$bias)
                         ))
    p <- ggplot(match.melt,aes(x=bias, fill=type)) + geom_histogram()  + labs(title="bias comparison") +  geom_vline(data=ave.bias, aes(xintercept=mean.bias,  colour=type),
               linetype="dashed", size=1)
    print(p)
#    plot(lm ~ heckit, data=match)
})

})
