##presentation figs
m3_out <- read.csv("presentation/figures_nina/m3_output.csv")
str(m3_out)

plot(m3_out$harvest, m3_out$surv_harvest, type="l", bty="l", xlab="Harvest intensity", ylab = "Probability of surviving a 65-year time series", cex.lab=1.5, cex.axis=1.5, lwd=2, ylim=c(0.7, 0.9))

plot(m3_out$chloroA, m3_out$surv_chloroA, type="l", bty="l", xlab="Mean chlorophyll content (mg per cubic meter)", ylab = "Probability of surviving a 65-year time series", cex.lab=1.5, cex.axis=1.5, lwd=2,ylim=c(0.7, 0.9))

plot(m3_out$year, m3_out$surv_year, type="l", bty="l", xlab="", ylab = "Probability of surviving 65 years", cex.lab=1.5, cex.axis=1.5, lwd=2,ylim=c(0.7, 0.9))

