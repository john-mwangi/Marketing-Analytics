data = read.delim(file = 'Videos/purchases.txt', sep = '\t', dec = '.', header = 0)

head(data)

# Checking variable types
str(data)

colnames(data) = c('customer_id','purchase_amount','date_of_purchase')

# Format as a date
data$date_of_purchase = as.Date(data$date_of_purchase,'%Y-%m-%d')

data$year_of_purchase = as.numeric(format(data$date_of_purchase,'%Y'))

head(data)

summary(data)
# This is now a proper summary

install.packages('sqldf', repos='http://cran.us.r-project.org')

library(sqldf)

version

x = sqldf("SELECT year_of_purchase, COUNT(year_of_purchase) AS 'counter' from data GROUP BY 1 ORDER BY 1")

head(x)

barplot(x$counter, names.arg = x$year_of_purchase)
# Plot counter and it's label to the year of purchase

# Combine several SQL queries
x = sqldf("SELECT year_of_purchase, COUNT(year_of_purchase) AS 'counter', AVG(purchase_amount) AS 'avg_amount', SUM(purchase_amount) AS 'sum_amount' FROM data GROUP BY 1 ORDER BY 1")

head(x)

library(ggplot2)

# ggplot is the best for plotting many lines in one graph
#https://stackoverflow.com/questions/2564258/plot-two-graphs-in-same-plot-in-r
g <- ggplot(x, aes(x=x$year_of_purchase))
g <- g + geom_line(aes(y=x$counter), colour="red")
g <- g + geom_line(aes(y=x$avg_amount), colour="green")
g <- g + geom_line(aes(y=x$sum_amount), col="blue")
g <- g + ylab('Value') + xlab('Year')
g

data$days_since = as.numeric(difftime(time1 = '2016-01-01',time2 = data$date_of_purchase, units = 'days'))

str(data)

summary(data)

# Now that we've added for each transaction the days_since today, we can compute RFM
library(sqldf)

customers = sqldf("SELECT customer_id, MIN(days_since) AS 'recency', COUNT(*) AS 'frequency', AVG(purchase_amount) AS 'amount' FROM data GROUP BY 1")
# count(*) counts anything for that customer
# We'd rather have an average instead of a median to INCLUDE outliers

str(customers)

hist(customers$amount)

# A more finer histogram
hist(customers$amount, breaks = 100)

# Set customer_id as index instead
new_data = customers
row.names(new_data) = new_data$customer_id
new_data$customer_id = NULL

str(new_data)

head(new_data)

# Normally distribute the amount data
new_data$amount = log(new_data$amount)

hist(new_data$amount)

# Now we can scale all the data to make them comparable
new_data = scale(new_data)

summary(new_data)

# Randomly sample 10% of the data
set.seed(0)
sample_rows = sample(x = nrow(new_data), size = 1842)
new_data_sample = new_data[sample_rows,]

head(new_data_sample)

str(new_data_sample)

# Calculate distances
d = dist(new_data_sample)

# Cluster those distances using ward.D2
c = hclust(d = d, method = 'ward.D2')

# Dendogram
plot(c)
# 7 segments are good

# Cut the dendogram tree into 7 segments
members = cutree(tree = c, k = 7)
#members are the indices of the table which are our customer_ids

#Show first 10 members and their clusters
members[1:10]

# Tabulate the members information
table(members)

# Profile the clusters using the actual/original data
customers_sample = customers[sample_rows,]

cluster_avgs = aggregate(x = customers_sample[,2:4], by = list(members), FUN = mean)
#in R column indices start at 1 instead of 0

cluster_avgs

# Plotting the 7 clusters as a 3D scatter plot
install.packages('plotly', repos='http://cran.us.r-project.org')

# Trying a new package because plotly is failing
install.packages('plot3D', repos='http://cran.us.r-project.org')

library(plot3D)

#http://www.sthda.com/english/wiki/impressive-package-for-3d-and-4d-graph-r-software-and-data-visualization
scatter3D(x = cluster_avgs$recency,
          y = cluster_avgs$frequency,
          z = cluster_avgs$amount,
          clab = 'Amount',
          bty = 'b2', #Adds markers
          ticktype = "detailed", #Adds tick
         pch=20, #Both make the bubbles solid coloured
         cex=2,
         xlab='Recency(days)',
         ylab='Frequency(count)',
         zlab='Amount($)',
         main='3D Scatter Chart')

head(new_data_sample)

set.seed(0)
clusters_new = kmeans(x = new_data_sample, centers = 6) #cluster on transformed data

cluster_avgs_new = aggregate(x = customers_sample[,2:4], by = list(clusters_new$cluster), FUN = mean) #aggregate on actual data

cluster_avgs_new

# Revised clustering
scatter3D(x = cluster_avgs_new$recency,
          y = cluster_avgs_new$frequency,
          z = cluster_avgs_new$amount,
          clab = 'Amount',
          bty = 'b2', #Adds markers
          ticktype = "detailed", #Adds tick
         pch=20, #Both make the bubbles solid coloured
         cex=2,
         xlab='Recency(days)',
         ylab='Frequency(count)',
         zlab='Amount($)',
         main='3D Scatter Chart')

str(data)

# Compute RFM & oldest purchase
library(sqldf)

customers_2015 = sqldf("SELECT customer_id, MIN(days_since) AS 'recency', COUNT(*) AS 'frequency', AVG(purchase_amount) AS 'amount', MAX(days_since) AS 'first_purchase' from data GROUP BY 1")

str(customers_2015)

customers_2015$segment = 'NA' #start here

customers_2015$segment[which(x = customers_2015$recency > 365*3)] = 'inactive'
#start by defining which() then wrap it with $segment
#you use $segment[] because you want to select all rows that qualify

table(customers_2015$segment)

# Initial segments
customers_2015$segment[which(x = customers_2015$recency > 365*2 & customers_2015$recency <= 365*3)] = 'cold'
customers_2015$segment[which(x = customers_2015$recency > 365*1 & customers_2015$recency <= 365*2)] = 'warm'
customers_2015$segment[which(x = customers_2015$recency <= 365*1)] = 'active'

table(customers_2015$segment)

# Additional segments
customers_2015$segment[which(customers_2015$segment == 'active' & customers_2015$first_purchase < 365)] = 'new active'
customers_2015$segment[which(customers_2015$segment == 'warm' & customers_2015$first_purchase < 365*2)] = 'new warm'
customers_2015$segment[which(customers_2015$segment == 'active' & customers_2015$amount < 100)] = 'active low value'
customers_2015$segment[which(customers_2015$segment == 'active' & customers_2015$amount >= 100)] = 'active high value'
customers_2015$segment[which(customers_2015$segment == 'warm' & customers_2015$amount < 100)] = 'warm low value'
customers_2015$segment[which(customers_2015$segment == 'warm' & customers_2015$amount >= 100)] = 'warm high value'

# Apply clustering to determine high and low monetary values per pillar
active_cust = customers_2015[customers_2015$segment=='active',]
warm_cust = customers_2015[customers_2015$segment=='warm',]

head(warm_cust)

set.seed(0)
active_clusters = kmeans(x = active_cust$amount, centers = 2)
warm_clusters = kmeans(x = warm_cust$amount, centers = 2)

# Add cluster to df
active_cust$cluster = as.factor(active_clusters$cluster)
warm_cust$cluster = as.factor(warm_clusters$cluster)

# Average amounts for each cluster
aggregate(x = active_cust$amount, by = list(active_cust$cluster), FUN = mean)

aggregate(x = warm_cust$amount, by = list(warm_cust$cluster), FUN = mean)

# We see that the thresholds for low/high amount is not $100 but $52/58 for warm/active customers

head(customers_2015)

# Include additional segments
# Additional segments
customers_2015$segment[which(customers_2015$segment == 'active' & customers_2015$first_purchase < 365)] = 'new active'
customers_2015$segment[which(customers_2015$segment == 'warm' & customers_2015$first_purchase < 365*2)] = 'new warm'
customers_2015$segment[which(customers_2015$segment == 'active' & customers_2015$amount < 58)] = 'active low value'
customers_2015$segment[which(customers_2015$segment == 'active' & customers_2015$amount >= 58)] = 'active high value'
customers_2015$segment[which(customers_2015$segment == 'warm' & customers_2015$amount < 52)] = 'warm low value'
customers_2015$segment[which(customers_2015$segment == 'warm' & customers_2015$amount >= 52)] = 'warm high value'

head(customers_2015)

set.seed(0)
recency_clusters = kmeans(x = customers_2015$recency, centers = 4)

customers_2015$recency_cluster = as.factor(recency_clusters$cluster)

head(customers_2015)

aggregate(x = customers_2015$recency, by = list(customers_2015$recency_cluster), FUN=mean)

table(customers_2015$segment)

aggregate(x = customers_2015[,2:5], by = list(customers_2015$segment), FUN = mean)

# Custom ordering of segment column
customers_2015$segment = factor(x = customers_2015$segment, levels = c('inactive','cold', 'warm high value','warm low value','new warm','active high value','active low value','new active'))

aggregate(x = customers_2015[,2:5], by = list(customers_2015$segment), FUN = mean)

str(data)

customers_2014 = sqldf("SELECT customer_id, MIN(days_since)-365 AS 'recency', COUNT(*) AS 'frequency', AVG(purchase_amount) AS 'amount', MAX(days_since)-365 AS 'first_purchase' FROM data WHERE days_since > 365 GROUP BY 1")

customers_2014$segment = 'NA'
customers_2014$segment[which(x = customers_2014$recency > 365*3)] = 'inactive'
customers_2014$segment[which(x = customers_2014$recency > 365*2 & customers_2014$recency <= 365*3)] = 'cold'
customers_2014$segment[which(x = customers_2014$recency > 365*1 & customers_2014$recency <= 365*2)] = 'warm'
customers_2014$segment[which(x = customers_2014$recency <= 365*1)] = 'active'
customers_2014$segment[which(customers_2014$segment == 'active' & customers_2014$first_purchase < 365)] = 'new active'
customers_2014$segment[which(customers_2014$segment == 'warm' & customers_2014$first_purchase < 365*2)] = 'new warm'
customers_2014$segment[which(customers_2014$segment == 'active' & customers_2014$amount < 100)] = 'active low value'
customers_2014$segment[which(customers_2014$segment == 'active' & customers_2014$amount >= 100)] = 'active high value'
customers_2014$segment[which(customers_2014$segment == 'warm' & customers_2014$amount < 100)] = 'warm low value'
customers_2014$segment[which(customers_2014$segment == 'warm' & customers_2014$amount >= 100)] = 'warm high value'

table(customers_2014$segment)

str(customers_2014)

# Validation
#2014 customers + new active 2015 customers = 2015 customers
16905+1512

revenue_2015 = sqldf("SELECT customer_id, SUM(purchase_amount) AS 'revenue_2015' FROM data WHERE year_of_purchase = 2015 GROUP BY 1")

str(revenue_2015)

summary(revenue_2015)
# Min=5 meaning it has ommited non-buyers

# Total revenue per customer in 2015
actual = merge(x = customers_2015, y = revenue_2015, all.x = TRUE)
#all.x ensures that all items in x are included

summary(actual)
#revenue_2015 min=5 due to na

head(actual)

actual$revenue_2015[is.na(actual$revenue_2015)]=0

head(actual)

# Summarise average revenue per segment in 2015
aggregate(x = actual$revenue_2015, by = list(actual$segment), FUN = mean)

# Those customers who were inactive in 2014, how much did they bring in 2015?
forward = merge(x = customers_2014, y = revenue_2015, all.x = TRUE)

forward$revenue_2015[is.na(forward$revenue_2015)]=0

aggregate(x = forward$revenue_2015, by = list(forward$segment), FUN = mean)
# These segments from 2014 made these amounts in 2015

r = aggregate(x = forward$revenue_2015, by = list(forward$segment), FUN = mean)

r[order(r$x, decreasing = TRUE),]
# One of the insights from this table is that an active high value customer is worth 8x a new active customer a year later

# CREATE TRAINING DATA
# Predictors
customers_2014 = sqldf("SELECT customer_id, MIN(days_since)-365 AS 'recency', COUNT(*) AS 'frequency', AVG(purchase_amount) AS 'avg_amount', MAX(purchase_amount) AS 'max_amount', MAX(days_since)-365 AS 'first_purchase' FROM data WHERE days_since > 365 GROUP BY 1")

# Targets
revenue_2015 = sqldf("SELECT customer_id, SUM(purchase_amount) AS 'revenue_2015' FROM data WHERE year_of_purchase = 2015 GROUP BY 1")

# Merge 2014 customers and their revenue in 2015
in_sample = merge(x = customers_2014, y = revenue_2015, all.x = TRUE)

# Fill na with 0
in_sample$revenue_2015[is.na(x = in_sample$revenue_2015)]=0

# Identify customers that spent in 2015
in_sample$active_2015 = as.numeric(in_sample$revenue_2015 > 0)
#without as.numeric it would give you true/false

head(in_sample)

library(nnet)

prob.model = multinom(formula = active_2015 ~ recency + frequency + avg_amount + max_amount + first_purchase, data = in_sample)

summary(prob.model)

coef = summary(prob.model)$coefficients

ste = summary(prob.model)$standard.errors

print(coef)

print(ste)

print(coef/ste)

# p-values
z = coef/ste
p = (1 - pnorm(abs(z), 0, 1)) * 2
print(p)

# First, extract only customers that spent in 2015
spenders = in_sample[which(x = in_sample$active_2015==1),]

head(spenders)

amount.model = lm(formula = revenue_2015 ~ avg_amount + max_amount, data = spenders)
# The reason why we pick only these variables is because we want the predicted value in USD only.

summary(amount.model)

# Plotting actual vs fitted values (predicted on training sample)
plot(x = spenders$revenue_2015, y = amount.model$fitted.values)

# Re-calibrated model
amount.model = lm(formula = log(revenue_2015) ~ log(avg_amount) + log(max_amount), data = spenders)

summary(amount.model)

plot(x = log(spenders$revenue_2015), y = amount.model$fitted.values)

# DATA TO PREDICT
# Features
customers_2015 = sqldf("SELECT customer_id, MIN(days_since) AS 'recency', COUNT(*) AS 'frequency', AVG(purchase_amount) AS 'avg_amount', MAX(purchase_amount) AS 'max_amount', MAX(days_since) AS 'first_purchase' FROM data GROUP BY 1")

str(customers_2015)

# Predict probability - add as column next to customer_id
customers_2015$prob_predicted = predict(object = prob.model, newdata = customers_2015, type = 'probs')

# Predict revenue - remember it will return a log
customers_2015$revenue_predicted = exp(predict(object = amount.model, newdata = customers_2015))

# Score a customer
customers_2015$score_predicted = customers_2015$revenue_predicted * customers_2015$prob_predicted

head(customers_2015)

summary(customers_2015)

# Which customers will spend >50 USD?
length(which(customers_2015$score_predicted > 50))
which(customers_2015$score_predicted > 50)[1:10]

head(customers_2015[order(customers_2015$score_predicted, decreasing = TRUE),])

str(customers_2015)

str(customers_2014)

# We need the segments in a very specific order
customers_2015$segment = factor(x = customers_2015$segment, levels = c('inactive','cold','warm high value','warm low value','new warm','active high value','active low value','new active'))
customers_2014$segment = factor(x = customers_2014$segment, levels = c('inactive','cold','warm high value','warm low value','new warm','active high value','active low value','new active'))

# For each 2014 customer, determine its 2015 segment
new_data = merge(x = customers_2014, y = customers_2015, by = 'customer_id', all.x = TRUE)

head(new_data)

# Create a count based transition matrix
transition = table(new_data$segment.x, new_data$segment.y)

transition
# rows=x(2014), columns=y(2015)

# Convert to percentages (probabilities)
transition = transition / rowSums(transition)

transition
# This represents probabilities of switching segments in one year

# Initiate the matrix with rows being the segments and columns being the number of years
# We want to predict 10 years ahead + present = 11 columns
segments = matrix(nrow = 8, ncol = 11)

segments

# Since we want to know the number of customers after 2015, this will be our first column
segments[,1] = table(customers_2015$segment)

# Assign column names to the matrix
colnames(segments) = 2015:2025

# Assign row indices as segment names instead
row.names(segments) = levels(customers_2015$segment)

segments

# Fill in the number of customers in the remaining years
for (i in 2:11) {
    segments[,i] = segments[,i-1] %*% transition
}

segments

barplot(segments[6,])

round(segments)
# This is why we're working with a one year transition matrix - because we're looking at year-on-year change

# Average revenue per customer in each segment in 2015
yearly_revenue = c(0,0,0,0,0,323.56894, 52.30604, 79.16614)
# We are assuming that this customer will continue to make the same amount each year

revenue_per_segment = yearly_revenue * segments

revenue_per_segment

yearly_revenue = colSums(revenue_per_segment)

yearly_revenue

cumulated_revenue = cumsum(yearly_revenue)

cumulated_revenue

# Discounting the revenue
discount_rate = 0.10
discount = 1/((1+discount_rate)^((1:11)-1))

print(discount)

disc_yearly_revenue = yearly_revenue * discount

disc_yearly_revenue

barplot(disc_yearly_revenue)
lines(yearly_revenue)

# What is the future value of my current customers?
disc_cumulated_revenue = cumsum(disc_yearly_revenue)
print(round(disc_cumulated_revenue[11]-disc_yearly_revenue[1]))
