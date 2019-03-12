install.packages("ggplot2") #install necessary packages
install.packages("gridExtra")

library(ggplot2) #load necessary packages
library(gridExtra)

values = data.frame(theta = c(-20, 30)) #define theta range and store it in one column data frame named values


#define loss functions
lossMain = function(x){(x - 10)^2}
lossAux = function(x){x^2}


#plot loss function and save under variable p1
p1 = ggplot(values, aes(theta)) + #load values data frame 
  stat_function(fun=lossMain, geom="line", aes(colour="Main")) + #plot loss function for main task
  stat_function(fun=lossAux, geom="line", aes(colour="Aux")) + #plot loss function for aux task
  scale_colour_manual("Loss function", values=c("red","blue"), breaks=c("Main","Aux"), labels = c(expression(paste(L[Main]) == (theta - 10)^2), 
                                                                                                  expression(paste(L[Aux] == theta^2    )))) + #define colors for loss functions
  theme(text = element_text(size=12), axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), legend.text.align = 0,
        legend.position="bottom") + #define properties of plot and legend position
  scale_x_continuous(name = expression(theta), breaks = seq(-20, 30, 10)) + #define x scale
  scale_y_continuous(name = "Loss value", breaks = seq(-100, 1000, 100)) + #define y scale
  geom_vline(xintercept = -20, linetype="dashed", color = "green", size=1.5) + #define vertical dashed line in theta = -20
  geom_vline(xintercept = 5, linetype="dashed", color = "yellowgreen", size=1.5) + #define vertical dashed line in theta = 5
  geom_segment(aes(x=-20, y=lossMain(-20), xend=-15, yend=lossMain(-15) ), arrow=arrow(length=unit(0.3,"cm")), color = "blue", size = 2) + #define arrow for main loss function in theta = -20
  geom_segment(aes(x=5, y=lossMain(5), xend=10, yend=lossMain(10) ), arrow=arrow(length=unit(0.3,"cm")), color = "blue", size = 2) + #define arrow for loss function in theta = 5
  geom_segment(aes(x=-20, y=lossAux(-20), xend=-15, yend=lossAux(-15) ), arrow=arrow(length=unit(0.3,"cm")), color = "red", size = 2) +
  geom_segment(aes(x=5, y=lossAux(5), xend=0, yend=lossAux(0)), arrow=arrow(length=unit(0.3,"cm")), color = "red", size = 2) +
  ggtitle("Loss Functions")

#################

#define gradient functions
gradMain = function(x){2*(x-10)}
gradAux = function(x){2*x}

#define cosine similarity for two values
cosSim = function(x, y)
{
  (x*y)/(sqrt(x^2)* sqrt(y^2))
}

#define cosine similarity between two gradient functions
gradCos = function(x)
{
  cosSim(gradMain(x), gradAux(x))
}

#plot gradient cosine sim function and save under variable p2
p2 = ggplot(values, aes(theta)) + #load values data frame 
  stat_function(fun=gradCos, geom="line", aes(colour= "black"), size = 1) + #plot cosine gradient function
  scale_colour_manual("", values = ("black"), breaks=c("black"), labels = c(expression(paste(a)))) +  #set color of drawing 
  theme(text = element_text(size=12), axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), legend.position = "none") + + #define font sizes of plot, remove legend
  scale_x_continuous(name = expression(theta), breaks = seq(-20, 30, 10)) + #set scale x
  scale_y_continuous(name = "Cosine similarity", breaks = seq(-2, 2, 0.5)) + #set scale y
  geom_vline(xintercept = -20, linetype="dashed", color = "green", size=1.5) + #define vertical line in theta -20
  geom_vline(xintercept = 5, linetype="dashed", color = "yellowgreen", size=1.5) + #define vertical line in theta = 5
  geom_segment(aes(x=0, y=1, xend=0, yend=-1 ), color = "black", size = 1, linetype = "dotted") +  #define vertical segments when cosine sim changes from 1 to -1
  geom_segment(aes(x=10, y=1, xend=10, yend=-1 ), color = "black", size = 1, linetype = "dotted") + 
  ggtitle("Gradient Cosine Similarity")



grid.arrange(p1, p2, ncol = 2) #plot both drawings using grid with two columns




