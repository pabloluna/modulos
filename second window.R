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
dplist <- gcombobox(names(mydata), cont = g)
lv2 <- gcheckbox("Graphic fill", cont = g)
lv3 <- glabel("Transparency:", cont = g)
transp <- gslider(0, 1, by = 0.1, cont = g)

mydata <- read.csv(file="D:/iNZight/NZIncomes03_11000.csv")

newPlot = function(h,...)
{
	if (svalue(dplist) = "age_midpt") {
		p1 <- ggplot(mydata, aes(x = weekly_income, colour = age_midpt))
	}
	else if (svalue(dplist) = "age_cat") {
		p1 <- ggplot(mydata, aes(x = weekly_income, colour = age_cat))
	}
	else if (svalue(dplist) = "sex") {
		p1 <- ggplot(mydata, aes(x = weekly_income, colour = sex))
	}
	else if (svalue(dplist) = "ethnicity") {
		p1 <- ggplot(mydata, aes(x = weekly_income, colour = ethnicity))
	}
	else if (svalue(dplist) = "highest_qualification") {
		p1 <- ggplot(mydata, aes(x = weekly_income, colour = highest_qualification))
	}
	else if (svalue(dplist) = "weekly_hrs") {
		p1 <- ggplot(mydata, aes(x = weekly_income, colour = weekly_hrs))
	}
	else (svalue(dplist) = "weekly_income") {
		p1 <- ggplot(mydata, aes(x = weekly_income, colour = weekly_income))
	}

		if (svalue(lv2, index = TRUE) == TRUE){
			print(p1 + geom_density(fill = svalue(lv2, index = TRUE), alpha = svalue(transp)))
		}
		else {
			print(p1 + geom_density())
		}	
}

plot_button <- gbutton(
  text = "Generate Plot",
  container = g,
  handler = newPlot
)



parg <- ggroup(horizontal=FALSE, cont=big, label="plot args")
qpg <- ggraphics(container=big, label="plot")