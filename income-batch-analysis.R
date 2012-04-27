
# package reshape has cast() and melt(); ggplot2 is the graphing package. 
library(reshape)
library(ggplot2)

# read in the csv data
income <- read.csv("~/Dropbox/pdffiles/umd-jessy-jon/data/mturk/batch-764161-results-all.csv")
# grab what we care about for now, get rid of NA values 
income <- income[!is.na(income$Answer.Q1), c('Input.img_src', 'Answer.Q1')]

# rename the levels of the Input.img_src (our facets) to something more
# meaningful
levels(income$Input.img_src) <- c("austin-burbs-wealthy", "longisland-wealthy-urban", "anacostia-poor", "longisland-middle-urban", "harlempark-poor", "longisland-middle-urban-2", "cupertino-wealthy", "harlempark-poor-2", "austin-burbs-wealthy-2", "kenilworth-top-wealthy", "anacostia-poor-2", "kenilworth-top-wealthy-2")

# get the mean, std, var for each image. 
q1.means <- cast(income, formula= ~Input.img_src, fun.aggregate = mean)
q1.sd <- cast(income, formula= ~Input.img_src, fun.aggregate = sd)
q1.var <- cast(income, formula= ~Input.img_src, fun.aggregate = var)

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
cummeans <- function(x) { cumsum(x)/seq_along(x) }
# ave() groups a numeric by a factor (in this case, hitid), and applies the
# specified function to each group. 
data.cummeans <- ave(income$Answer.Q1, income$Input.img_src, FUN=cummeans)
computed <- income
# append the cumulative means as a column to the end of the computed data. 
computed$cummeans_income <- data.cummeans

# create normal distributions reflecting the mean and sd of the underlying data
# for each Input.img_src. 

# first, generate a sequence along the set of possible answers to sample from. 
grid <- with(computed, seq(min(Answer.Q1), max(Answer.Q1), length = 50))

# compute normal distributions. i THINK what ddply is doing is faceting the
# "computed" data set on the Input.img_src column, and then applying the anonymous
# function to each facet. the anomymous function computes the density
# distribution using the mean and sd over the set of answers (observations) for
# that facet (Input.img_src). 
normals <- ddply(computed, "Input.img_src", function(df) { data.frame( predicted=grid,
	density= dnorm(grid, mean(df$Answer.Q1), sd(df$Answer.Q1)) ) })

m + geom_line(data = normals, aes(x = predicted, y = density), colour = "red")

# graph the histograms with the gaussian distributions on top, and save it. 
mm <- ggplot(computed, aes(Answer.Q1, ..density..)) + geom_histogram(binwidth = 1.0) + facet_wrap(~ Input.img_src, ncol=3)
hist_out <- mm + geom_line(data = normals, aes(x = predicted, y = density), colour = "red")
ggsave("income-histograms.png", plot= hist_out)

# plot cumulative means
p <- ggplot(computed, aes(y=cummeans_income, x=seq_along(computed$cummeans_income))) + geom_line()
p + facet_wrap(~Input.img_src, ncol=3, scales="free_x")

# add the means as a horizontal line to each plot. note you HAVE to name the
# 'levels' column 'Input.img_src'!!! (to match the facet i guess). 
themeans <- data.frame(Input.img_src=levels(computed$Input.img_src), yval=c(q1.means))
cumulative_means_graphs <- p + facet_wrap(~Input.img_src, ncol=3, scales="free_x") + geom_hline(aes(yintercept=yval), data=themeans, color="#cc0066")
ggsave("income-cumulative_means.png", plot=cumulative_means_graphs)




