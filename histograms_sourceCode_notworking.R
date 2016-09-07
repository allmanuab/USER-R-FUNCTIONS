# plot_list = list()
# for (i in cont.vars) {
#   l = get(i)
#   x <- subset(l, !is.na(l))
#   breaks <- pretty(range(x), n = nclass.FD(x), min.n = 1)
#   bwidth <- breaks[2]-breaks[1]
#   p <- (ggplot(data=xroads2, aes(l)) +
#           geom_histogram(fill="darkblue", binwidth=bwidth) + theme_tufte() +
#           labs(title=paste(i, " Histogram", sep=""), x=paste(i, " value", sep=""), y="Count"))
#   plot_list[[i]] = p
# }
# # now loop thru and print each plot to a seperate pdf
# for (i in cont.vars) {
#   pdf(file=paste("C:/Users/Hunter/Desktop/Grad_School/Research/Mehta/Analysis/crossroads/results/preliminary-analyses/histograms/", i, ".pdf", sep=""))
#   print(plot_list[[i]])
#   dev.off()
# }