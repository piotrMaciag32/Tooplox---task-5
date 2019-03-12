

library(ggplot2)#Load necessary libraries
library(gridExtra)

theme_set(theme_bw()) #set blank background in plots

##For the near pair (egyptian cat vs persian cat)
data = data.frame(Classification.type = rep(c("Multi task [285, 283]", "Single task [285]"), each = 30),
                  Epoch = rep(c(1:30), 2),
                  Accuracy = c(resultsEgyptianCatVsPersianCat$`Multi Task`, resultsEgyptianCatVsPersianCat$`Single Task`)) #prepare classification data for plots

#generate plot with ggplot2 
p1<-ggplot(data, aes(x=Epoch, y=Accuracy, group=Classification.type, order = as.numeric(Classification.type))) + 
  geom_line(aes(linetype=Classification.type, color = Classification.type))+
  geom_point(aes(shape = Classification.type, color = Classification.type)) + xlab("Epoch") + ylab("Accuracy") + ggtitle("Classification results - near pair") +
  scale_x_continuous(breaks = seq(1, 30 ,2)) +
  scale_y_continuous(breaks = seq(0.9, 1, 0.001)) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), legend.text.align = 0,
        legend.position="bottom")

p1




##For the far pair (elephant vs Teapot)

#prepare classification data for plots
data = data.frame(Classification.type = rep(c("Multi task [386, 849]", "Single task [386]"), each = 30),
                  Epoch = rep(c(1:30), 2),
                  Accuracy = c(resultsElephantVsTeapot$`Multi Task`, resultsElephantVsTeapot$`Single Task`))

#generate plot with ggplot2 
p2<-ggplot(data, aes(x=Epoch, y=Accuracy, group=Classification.type, order = as.numeric(Classification.type))) +
  geom_line(aes(linetype=Classification.type, color = Classification.type))+
  geom_point(aes(shape = Classification.type, color = Classification.type)) + xlab("Epoch") + ylab("Accuracy") + ggtitle("Classification results - far pair") +
  scale_x_continuous(breaks = seq(1, 30 ,2)) +
  scale_y_continuous(breaks = seq(0.9, 1, 0.001)) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), legend.text.align = 0,
        legend.position="bottom")

p2

#combine two plot in a grid and visualise
grid.arrange(p1, p2, ncol = 2)



