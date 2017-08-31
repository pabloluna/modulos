library(gWidgets)
library(ggplot2)
options("guiToolkit"="RGtk2")



win <- gwindow("Density Graphic Module")

big <- ggroup(container=win)
g <- ggroup(horizontal=FALSE, container=big, expand=TRUE)

lbl1 <- glabel("Density Graphic Module", cont = g)
font(lbl1) <- list(weight = "bold", family = "normal", size   = 12)
add(g, lbl1, anchor = c(0, 0))
addSpace(g, 20)

lv1 <- glabel("Attribute of Interest:", cont = g)
dplist <- gcombobox(c("gender", "weight"), cont = g)
lv2 <- gcheckbox("Graphic fill", cont = g)
lv3 <- glabel("Transparency:", cont = g)
transp <- gslider(0, 1, by = 0.1, cont = g)

plotData = function(h,...)
{
df <- data.frame(
  gender=factor(rep(c("F", "M"), each=200)),
  weight=round(c(rnorm(200, mean=55, sd=5),
                 rnorm(200, mean=65, sd=5)))
  )
head(df)

	if (svalue(lv2, index = TRUE) == TRUE){
		print(ggplot(data = df, aes(x=weight, color=gender)) + ggtitle("Weight by gender") + geom_density(fill = svalue(lv2, index = TRUE), alpha = svalue(transp)))
	} else
		print(ggplot(data = df, aes(x=weight, color=gender)) + ggtitle("Weight by gender") + geom_density())
	}
}

plot_button <- gbutton(
  text = "Plot data",
  container = g,
  handler = plotData
)

parg <- ggroup(horizontal=FALSE, cont=big, label="plot args")
qpg <- ggraphics(container=big, label="plot")

