
# package reshape has cast() and melt(); ggplot2 is the graphing package. 
library(reshape)
library(ggplot2)

# read in the csv data
grocery <- read.csv("~/Dropbox/pdffiles/umd-jessy-jon/data/mturk/batch_777316and780910_grocery.csv")
# grab what we care about for now, and clean the data: get rid of NA values
# (only keep the approved hits)
g <- grocery[grocery$AssignmentStatus == "Approved", c('Input.img_src', 'Answer.Q1')]

# codify the nominal (categorical) data numerically for analysis manually (ie,
# we know the ordering from inspection) (1 is closest, 6 is farthest)
levels(g$Answer.Q1) <- c(0,5,1,2,3,4,6)
# Answer.Q1 was imported as a factor, so we need to convert it to numeric (see
# http://cran.r-project.org/doc/FAQ/R-FAQ.html#How-do-I-convert-factors-to-numeric_003f)
g$Answer.Q1 <- as.numeric(levels(g$Answer.Q1))[as.integer(g$Answer.Q1)]

# rename the levels of the Input.img_src (our facets) to something more
# meaningful (note we first rename the levels according to what they *are*, and
# only then can we re-order them)
levels(g$Input.img_src) <- c("3-austin-burbs-wealthy", 
							 "6-longisland-wealthy-urban", 
							 "9-anacostia-poor", 
							 "7-longisland-middle-urban", 
							 "11-harlempark-poor", 
							 "8-longisland-middle-urban-2", 
							 "5-cupertino-wealthy", 
							 "12-harlempark-poor-2", 
							 "4-austin-burbs-wealthy-2", 
							 "1-kenilworth-top-wealthy", 
							 "10-anacostia-poor-2", 
							 "2-kenilworth-top-wealthy-2")

# and now re-order the factors roughly according to decreasing wealthiness
g$Input.img_src <- factor(g$Input.img_src, levels=c("1-kenilworth-top-wealthy", 
							 "2-kenilworth-top-wealthy-2",
							 "3-austin-burbs-wealthy", 
							 "4-austin-burbs-wealthy-2", 
							 "5-cupertino-wealthy", 
							 "6-longisland-wealthy-urban", 
							 "7-longisland-middle-urban", 
							 "8-longisland-middle-urban-2", 
							 "9-anacostia-poor", 
							 "10-anacostia-poor-2", 
							 "11-harlempark-poor", 
							 "12-harlempark-poor-2"))

q1.means <- cast(g, formula= ~Input.img_src, fun.aggregate = mean)
# get rid of the 'all' value
q1.means$value <- NULL
# transpose them
q1.means <- t(q1.means)

# get the cumulative means for each 
# define a new function
fn_cummeans <- function(x) { cumsum(x)/seq_along(x) }
# ave() groups a numeric by a factor (in this case, hitid), and applies the
# specified function to each group. 
g$cummeans <- ave(g$Answer.Q1, g$Input.img_src, FUN=fn_cummeans)

# create normal distributions reflecting the mean and sd of the underlying data
# for each HITId. 
# first, generate a sequence along the set of possible answers to sample from. 
grid <- with(g, seq(min(Answer.Q1), max(Answer.Q1), length = 50))

# compute normal distributions. i THINK what ddply is doing is faceting the
# "computed" data set on the HITId column, and then applying the anonymous
# function to each facet. the anomymous function computes the density
# distribution using the mean and sd over the set of answers (observations) for
# that facet (HITId). 
normals <- ddply(g, "Input.img_src", function(df) { data.frame( predicted=grid,
	density= dnorm(grid, mean(df$Answer.Q1), sd(df$Answer.Q1)) ) })

# graph the histograms with the gaussian distributions on top, and save it. 
g1 <- ggplot(g, aes(Answer.Q1, ..density..)) + geom_histogram(binwidth = 1.0) + facet_wrap(~ Input.img_src, ncol=3)
g1_out <- g1 + geom_line(data = normals, aes(x = predicted, y = density), colour = "red") + 
	opts(title="Grocery Store Distance Estimate Histogram for n=50 HITs") + ylab("Density") + xlab("Estimate")
ggsave("grocery-histograms.png", plot= g1_out)

# graph the cumulative means, with the final mean as a horizontal line on each
# plot. note you HAVE to name the 'levels' column to match the facet!! 
g_means <- data.frame(Input.img_src=levels(g$Input.img_src), yval=c(q1.means))
g2 <- ggplot(g, aes(y=cummeans, x=seq_along(g$cummeans))) + 
	geom_line() + 
	facet_wrap(~Input.img_src, ncol=3, scales="free_x") +
	geom_hline(aes(yintercept=yval), data=g_means, color="#cc0066") 
g2_out <- g2 + opts(title="Grocery Distance Estimation Convergence over n=50 HITs", 
	axis.ticks = theme_blank(), axis.text.x = theme_blank()) + 
	xlab("Num Estimates") + 
	ylab("Cumulative Mean") 
ggsave("grocery-cumulative_means.png", plot=g2_out)

# box plots
g3 <- ggplot(g, aes(factor(Input.img_src), Answer.Q1))
g3_out <- g3 + stat_boxplot(outlier.colour = "red", outlier.size = 3, aes(fill = Input.img_src)) + 
	scale_fill_hue(l=40, name="Image") + opts(title ="Box Plots: Grocery, All Images", 
	axis.ticks = theme_blank(), axis.text.x = theme_blank(), legend.position="right") + 
	stat_summary(fun.y=median,shape=95, size=10, col='white',geom='point', aes(shape="median")) + 
	# not working, not sure why :/
	scale_shape_manual("", values=c("mean")) +
	stat_summary(fun.y=mean,shape=95, size=10, col='yellow',geom='point') + 
	scale_x_discrete(breaks=NULL) + 
	scale_y_discrete(labels=c("1 block", "2-3 blocks", "3-6 blocks", "6 blks -1 mile","1-5 miles", "> 5 miles")) +
	ylab("Estimate (white=median; yellow=mean; red=outliers)") + 
	xlab("Image") + 
	geom_point(color="grey50") 
ggsave("grocery-boxplots.png", plot=g3_out)

