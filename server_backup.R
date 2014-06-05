library(shiny)
library(MASS)
library(sampleSelection)


# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
output$plot1 <-  renderPlot({
nsim = input$n
nDim = 3 ; sd11 = 1 ;  cr1w=input$cr1w;  cr1u=input$cr1u;
sdww=1; crwu=0;
sduu=1;
covarMat = matrix( c(sd11^2, cr1w^2, cr1u^2, cr1w^2, sdww^2, crwu^2, cr1u^2, crwu^2, sduu^2 ) , nrow=nDim , ncol=nDim )

for (i in 100:nsim){
data  = as.data.frame(mvrnorm( n=nsim , mu=rep(0,nDim), Sigma=covarMat ))
names(data) <- c('x1','w','u')
data$y <- data$x1 + data$w +  data$u
data$rn <- rnorm(dim(data)[1], 0,1)
data$select <- (data$w + data$rn > 1)
heckit <- heckit(selection = select ~ w + x1, outcome = y ~ x1 , method = "ml", data=data)
heckit.x1[i] <- summary(heckit)$estimates[[5]]
lm1 <- lm(y ~ x1 + w + select, data=data)
lm.x1[i] <- summary(lm1)$coefficients[[2]]
}
plot(lm.x1~heckit.x1)
#plot(data$y,data$x1)
})

})
