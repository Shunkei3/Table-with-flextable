# /*=================================================*/
#' #  Preparation
# /*=================================================*/
library(data.table)
library(flextable)
library(tidyverse)

# /*=================================================*/
#' # Example Data (let's say you have this kind of data)
# /*=================================================*/
# Ex) Calculate mean and sd for all the columns by Species
data <- data.table(iris) %>%
	melt(id.vars = "Species") %>%
	.[,.(
	   mean = mean(value),
	   sd = sd(value)
	   ), by = .(Species, variable)]



# /*=================================================*/
#' # Create a Table 
# /*=================================================*/

#/*----------------------------------*/
#' ## Preparation 
#/*----------------------------------*/
table_prep <- 
	data %>%
	.[,`:=`(
	    mean = format(round(mean,1), nsmall=1),
	    sd = format(round(sd,2), nsmall=2)
        )] %>%
	dcast(variable ~ Species, value.var = c("mean", "sd")) %>%
	# /*===== (1) set NA columns  =====*/
	.[,`:=`(blank1 = NA, blank2 = NA, blank3 = NA)] %>%
	# /*===== (2) reoder columns  =====*/
	# in a desirable columns order
	.[,.(
	variable, 
	blank1, mean_setosa, sd_setosa,
	blank2, mean_versicolor, sd_versicolor,
	blank3, mean_virginica, sd_virginica
	)]


#/*----------------------------------*/
#' ## Table creation
#/*----------------------------------*/
report_table <- 
	table_prep%>%
    	mutate(
    	across(
      	   everything(),
           as.character
        )
    	)%>%
    	flextable(.) %>%
    	border_remove() %>%
        delete_part(part = "header") %>%
    	add_header(
    	   variable="Type", 
	   blank1 = "", mean_setosa = "setosa", sd_setosa = "setosa",
	   blank2 = "", mean_versicolor = "versicolor", sd_versicolor = "versicolor",
	   blank3 = "", mean_virginica = "virginica", sd_virginica = "virginica",
           top = TRUE
	) %>%
    	merge_h(part = "header") %>%
    	hline_bottom(j=c(3:4, 6:7, 9:10), part = "header") %>%
    	add_header(
           variable="", 
           blank1 = "", mean_setosa = "Mean", sd_setosa = "SD",
           blank2 = "", mean_versicolor = "Mean", sd_versicolor = "SD",
           blank3 = "", mean_virginica = "Mean", sd_virginica = "SD",
           top = FALSE
	) %>%
    	hline_bottom(part="all") %>%
    	hline_top(part="header") %>%
    	align(align = "center", part = "all") %>%
    	align(j=1,  align = "left", part = "all") %>%
    	fontsize(i = NULL, j = NULL, size = 9, part = "footer") %>%
    	autofit() %>%
    	width(j = c(3,4,6,7,9,10), width=0.6) %>%
    	width(j = c(2,5,8), width=0.1)









