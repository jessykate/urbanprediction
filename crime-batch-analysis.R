
# package reshape has cast() and melt(); ggplot2 is the graphing package. 
library(reshape)
library(ggplot2)

# read in the csv data
crime <- read.csv("~/Dropbox/pdffiles/umd-jessy-jon/data/mturk/batch_777344_april18-crime.csv")
# clean the data
crime <- crime[crime$AssignmentStatus == "Approved", c('Input.img_src', 'Answer.Q1')]

# rename the levels of the Input.img_src (our facets) to something more
# meaningful
levels(crime$Input.img_src) <- c("austin-burbs-wealthy", "longisland-wealthy-urban", "anacostia-poor", "longisland-middle-urban", "harlempark-poor", "longisland-middle-urban-2", "cupertino-wealthy", "harlempark-poor-2", "austin-burbs-wealthy-2", "kenilworth-top-wealthy", "anacostia-poor-2", "kenilworth-top-wealthy-2")


# get the mean, std, var for each image. 
q1.means <- cast(crime, formula= ~Input.img_src, fun.aggregate = mean)
q1.sd <- cast(crime, formula= ~Input.img_src, fun.aggregate = sd)
q1.var <- cast(crime, formula= ~Input.img_src, fun.aggregate = var)

# get rid of the 'all' value
q1.means$value <- NULL
q1.sd$value <- NULL
q1.var$value <- NULL

# transpose them
q1.means <- t(q1.means)
q1.var <- t(q1.var)
q1.sd <- t(q1.sd)

# each of q1.sd, .var and .means is now a matrix. displaying it shows two
# columns, but the left is actually the "index" names, not a column. using
# cbind we can create a matrix of all the values. 
q1.anova <- cbind(q1.means, q1.sd, q1.var)

# get the cumulative means for each 
# define a new function
fn_cummeans <- function(x) { cumsum(x)/seq_along(x) }
# append the cumulative means as a column to the end of the computed data. 
# ave() groups a numeric by a factor (in this case, img_src), and applies the
# specified function to each group. 
crime$cummeans <-ave(crime$Answer.Q1, crime$Input.img_src, FUN=fn_cummeans)

# create normal distributions reflecting the mean and sd of the underlying data
# for each HITId. 
# first, generate a sequence along the set of possible answers to sample from. 
grid <- with(crime, seq(min(Answer.Q1), max(Answer.Q1), length = 50))

# compute normal distributions. i THINK what ddply is doing is faceting the
# data set on the specified column, and then applying the anonymous
# function to each facet. the anomymous function computes the density
# distribution using the mean and sd over the set of answers (observations) for
# that facet (HITId). 
normals <- ddply(crime, "Input.img_src", function(df) { data.frame( predicted=grid,
	density= dnorm(grid, mean(df$Answer.Q1), sd(df$Answer.Q1)) ) })

# graph the histograms with the gaussian distributions on top, and save it. 
p1 <- ggplot(crime, aes(Answer.Q1, ..density..)) + geom_histogram(binwidth = 1.0) + facet_wrap(~ Input.img_src, ncol=3)
p1 + geom_line(data = normals, aes(x = predicted, y = density), colour = "red")+ opts(title="Crime Estimation Histogram for n=50 HITs")
p1_out <- p1 + geom_line(data = normals, aes(x = predicted, y = density), colour = "red")
ggsave("crime-histograms.png", plot= p1_out)

# graph the cumulative means
p2 <- ggplot(crime, aes(y=cummeans, x=seq_along(crime$cummeans))) + geom_line()
p2 + facet_wrap(~Input.img_src, ncol=3, scales="free_x") + opts(title="Crime Estimation Convergence over n=50 HITs")

# add the means as a horizontal line to each plot. note you HAVE to name the
# 'levels' column to match the facet!! 
themeans <- data.frame(Input.img_src=levels(crime$Input.img_src), yval=c(q1.means))
p2_out <- p2 + facet_wrap(~Input.img_src, ncol=3, scales="free_x") + geom_hline(aes(yintercept=yval), data=themeans, color="#cc0066")
ggsave("crime-cumulative_means.png", plot=p2_out)




