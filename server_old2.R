library(shiny)
library(MASS)
library(sampleSelection)

nobs=1000
nsim = 50;    nDim = 3 ; sd11 = 1 ;  sdww=1; crwu=0; sduu=1;
corr.grid = seq(0, .9, .1)
    covarMat = matrix( c(sd11^2, cr1w^2, cr1u^2, cr1w^2, sdww^2, crwu^2, cr1u^2, crwu^2, sduu^2 ) , nrow=nDim , ncol=nDim )
    heckit.x1 <- array(NA, dim=c(nsim,nobs/100, 10, 10))
    lm.x1 <- array(NA, dim=c(nsim,nobs/100, 10, 10))
    for (i in 1:nsim){
        for (j in 1:(nobs/100)){
            for (k in 1:10){
                for (l in 1:10){
                    cr1w <- corr.grid[k]
                    cr1u <- corr.grid[l]
                    covarMat = matrix( c(sd11^2, cr1w^2, cr1u^2, cr1w^2, sdww^2, crwu^2, cr1u^2, crwu^2, sduu^2 ) , nrow=nDim , ncol=nDim )
                    data  = as.data.frame(mvrnorm(n=j*100 , mu=rep(0,nDim), Sigma=covarMat ))
                    names(data) <- c('x1','w','u')
                    data$y <- data$x1 + data$w +  data$u
                    data$rn <- rnorm(dim(data)[1], 0,1)
                    data$select <- (data$w + data$rn > 1)
                    heckit <- heckit(selection = select ~ w, outcome = y ~ x1 + w, method = "ml", data=data)
                    heckit.x1[i,j,k,l] <- summary(heckit)$estimate[[5]]
                    lm1 <- lm(y ~ x1 + w + select, data=data)
                    lm.x1[i,j,k,l] <- summary(lm1)$coefficients[[2]]
                }}}}

# Set up data
# 1. expand grid to get data frame with bunch of parameters
# 2. use apply and function from interior of loop to calculate bias
# for each row, and save that to the table.
# 3. write results to csv file.
# write.csv(simulations, 'simulations.csv')

# Server
# 1. simulations <- read.csv('simulations.csv')
# 2. each call to the server subsets the data frame
#    keep <- (simulations$cr1w == input$cr1w) & ...
#    plot(simulations[keep, 'heckman.bias'], simulations[keep, 'lm.bias'])

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
output$plot1 <-  renderPlot({
    nobs = input$n

    cr1w=input$cr1w;  cr1u=input$cr1u;
#        lmx1 <- as.matrix(as.numeric(lm.x1))
        lm.data <- as.data.frame(lm.x1)-1
#        heckitx1 <- as.matrix(as.numeric(heckit.x1))
        heckit.data <- as.data.frame(heckit.x1)
        heckit.data$diff.heckit <- heckit.data$V1-1
        plot(heckit.data[,nobs/100] ~ lm.data[,nobs/100])
})

})
