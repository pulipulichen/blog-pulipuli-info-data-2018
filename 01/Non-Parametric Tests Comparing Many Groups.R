if(!require(dplyr)){install.packages("dplyr")}
if(!require(FSA)){install.packages("FSA")}
if(!require(lattice)){install.packages("lattice")}
if(!require(DescTools)){install.packages("DescTools")}
if(!require(car)){install.packages("car")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(userfriendlyscience)){install.packages("userfriendlyscience")}

csv_file_path = file.choose()
csv_dir_path = dirname(csv_file_path)
csv_file_name = sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(csv_file_path))

timestamp = format(Sys.time(), "%m%d-%H%M")

output_file_path = file.path(csv_dir_path, paste(csv_file_name, '_', timestamp, '_kw-test_output.txt', sep=""))


Data = read.csv(csv_file_path, header=TRUE, sep=",")

cat("### File: ", csv_file_path, "\n", file=output_file_path, sep="\n", append=FALSE)

### Specify the order of factor levels

library(dplyr)   
Data = mutate(Data, group = factor(group, levels=unique(group)))

# Medians and descriptive statistics

library(FSA)

out <- capture.output(Summarize(value ~ group, data = Data))
cat("### Medians and descriptive statistics\n", out, file=output_file_path, sep="\n", append=TRUE)

### Graphing the results ###
  
# Stacked histograms of values across groups

library(lattice)


png(file.path(csv_dir_path, paste(csv_file_name, '_', timestamp, '_histogram.png', sep="")))

histogram(~ value | group,  data=Data, layout=c(1,length(levels(Data$group))))

dev.off()

# Simple boxplots of values across groups

png(file.path(csv_dir_path, paste(csv_file_name, '_', timestamp, '_boxplot.png', sep="")))

boxplot(value ~ group,
        data = Data,
        ylab="Value",
        xlab="Group")

dev.off()

### Heteroscedasticity test

library(car)
leveneTest.result = leveneTest(value~group, Data)
out <- capture.output(leveneTest.result)
cat("\n### Heteroscedasticity test\n", out, file=output_file_path, sep="\n", append=TRUE)

is.heteroscedastic = (leveneTest.result$`Pr(>F)` < 0.05)

if (is.heteroscedastic) {
    cat("\nData are heteroscedastic. Excute Welch's anova.", file=output_file_path, sep="\n", append=TRUE)
} else {
    cat("\nData are homoscedasticity. Excute Kruskal–Wallis test.", file=output_file_path, sep="\n", append=TRUE)
}

# ----------------

if (is.heteroscedastic) {

### Welch’s anova for unequal variances

out <- capture.output(oneway.test(value ~ group,
            data=Data,
            var.equal=FALSE))
cat("\n### Welch’s anova for unequal variances", out, file=output_file_path, sep="\n", append=TRUE)

### Performing the Games-Howell Test

library(userfriendlyscience)
out <- capture.output(oneway(Data$group, y = Data$value, posthoc = 'games-howell'))
cat("\n### Games-Howell Post-Hoc Test\n", out, file=output_file_path, sep="\n", append=TRUE)

} else {

### Kruskal–Wallis test

out <- capture.output(kruskal.test(value ~ group, data = Data))
cat("\n### Kruskal–Wallis test for equal variances", out, file=output_file_path, sep="\n", append=TRUE)

### Dunn test for multiple comparisons

### Order groups by median

Data$group = factor(Data$group, levels=levels(Data$group))

### Dunn test

library(FSA)

PT = dunnTest(value ~ group,
              data=Data,
              method="bh")    # Can adjust p-values; 
                              # See ?p.adjust for options 
out <- capture.output(PT)
cat("### Dunn test\n", out, file=output_file_path, sep="\n", append=TRUE)
}