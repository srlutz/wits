
setwd(figures_dir)

# Define text parameters
text_params <- list(cex_main = 1.5, cex_title = 1.8, cex_lab = 1.5, font_main = 1) # text size

if (tracer=="ST" | tracer=="H3+ST") {

# Best-fit plot for stable isotope
    png("best-fit_stable.png", height = 2000, width = 2000, res = 200)
    par(mar = c(5, 9, 5, 1))  
    plot(stable_output_date, stable_output, type = "p", pch = 19, col = "grey",
         xlim = c(x_inf_stable, x_sup_stable), ylim = c(y_inf_stable, y_sup_stable),
         xaxt = "n", yaxt = "n", las = 1, xlab = "Time", ylab ="", 
         bty = "l", cex.lab = text_params$cex_lab,mgp=c(3,6,0)) 
    if (time_step != "R") points(median_stable_output_date, median_stable_output, pch = 19)
    lines(inputdates1, y_stable, lwd = 2)
    lines(lowess(stable_output_date,stable_output,f=0.1),lwd=2,col="black",lty=3) # smooth helping to visualise mid-term variations
    if (tracer == "ST") {
        legend("bottomright", 
        legend = c("Observations", "Mean values used for fitting", "Best fit","Smooth"),
        pch = c(19, 19, NA, NA),  # Symbol: filled circle for observations, NA for lines
        lty = c(NA,NA,  1, 3),  # Line types
        lwd = c(NA,NA,  3, 2),  # Line widths
        col = c("grey", "black", "black","black"),  # Colors for the legend
        cex = text_params$cex_lab,  # Text size for the legend
        inset = c(0.02, 0.02),  # Slight offset from the top-right corner
        bty = "n") 
    }  

    if (tracer == "H3+ST") {
        lines(inputdates1, y_combined_stable, lwd = 2, lty = 2)
        legend("bottomright", 
        legend = c("Observations", "Mean values used for fitting", "Best fit", "Best fit constrained by tritium","Smooth"),
        pch = c(19, 19, NA, NA,NA),  # Symbol: filled circle for observations, NA for lines
        lty = c(NA, NA,  1, 2, 3),  # Line types
        lwd = c(NA, NA, 3, 2, 2),  # Line widths
        col = c("grey", "black", "black", "black", "black"),  # Colors for the legend
        cex = text_params$cex_lab,  # Text size for the legend
        inset = c(0.02, 0.02),  # Slight offset from the top-right corner
        bty = "n") 
    }  

    title(main = "Best fit stable isotope", cex.main = text_params$cex_title)
    mtext( lab_stable,2,line=6,cex=1.5)   
    magaxis(side = 1:2, majorn = 5, minorn = 'auto', tcl = -0.5,las=1,cex.axis=1.5,mgp=c(3,1,0))
    dev.off()

   # Ensemble solutions plot for stable isotope
    if(dim(ensemble_stable)[1]!=0) # checks whether more than one solution has been retained, and skips the graphic is only the best fit is available.
    {
	y1 <- colMaxs(as.matrix(ensemble_stable))
	y2 <- colMins(as.matrix(ensemble_stable))
	png("ensemble_solutions_stable.png", height = 2000, width = 2000, res = 200)
    par(mar = c(5, 9, 5, 1))  
	plot(stable_output_date, stable_output, type = "p", pch = 1,
		xlim = c(x_inf_stable, x_sup_stable), ylim = c(y_inf_stable-0.5, y_sup_stable+0.5),
		xaxt = "n", yaxt = "n", las = 1, xlab = "Time", ylab = "", bty = "l", cex.lab = text_params$cex_lab)
	lines(inputdates1, y1, type = "l", col = "grey")
	lines(inputdates1, y2, type = "l", col = "grey")
	polygon(c(inputdates1, rev(inputdates1)), c(y2, rev(y1)), col = "lightgrey", border = NA)
	lines(inputdates1, y_stable, lwd = 2)
	points(stable_output_date, stable_output, pch = 19, col = "black") # measures, following the logic of line67
	title(main = "Ensemble Solutions stable isotope", cex.main = text_params$cex_title)
    mtext(lab_stable,2,line=6,cex=1.5)   
	legend("bottomright", 
       legend = c("Observations", "Ensemble solutions", "Best fit"),
       pch = c(19, NA , NA),  # Filled circles for observations, open circles for simulations, NA for line
       lty = c(NA, 1, 1),  
       col = c("black", "grey", "black"),  
       lwd = c(NA, 4, 2),  
       cex = text_params$cex_lab,  # Size of legend text
       inset = c(0.02, 0.02),  # Slight inset from the bottom right corner
       bty = "n")  # No box around the legend
    magaxis(side = 1:2, majorn = 5, minorn = 'auto', tcl = -0.5,las=1,cex.axis=1.5,mgp=c(3,1,0))
	dev.off()
    }

	# Residuals plot for stable isotope
	png("residuals_stable.png", height = 2000, width = 2500, res = 200)
	if (tracer=="H3+ST") layout(matrix(c(2, 1, 4, 3), nrow = 2, byrow = TRUE), widths = c(1, 3))
	if (tracer=="ST") layout(matrix(c(2,1),1,2, byrow = TRUE), widths = c(1, 3))
	par(mar = c(5, 6, 2, 2), oma = c(2, 2, 4, 2))

	# Panel 1
	plot(outputdates1, y_stable_residuals, type = "p", pch = 19, las = 1, col = "black",
		xlim = c(x_inf_stable, x_sup_stable), 
		ylim = range(y_stable_residuals) * c(1.3, 1.3),
		xaxt = "n", yaxt = "n", xlab = "Time", ylab ="",
		bty = "l", cex.lab = text_params$cex_lab, cex.axis = text_params$cex_main)
    magaxis(side = 1:2, majorn = 5, minorn = 'auto', tcl = -0.5,las=1,cex.axis=1.5,mgp=c(3,1,0))
	title(main = "Stable isotope best fit residuals", cex.main = text_params$cex_title)
    mtext(lab_stable,2,line=4,cex=1.5)   
    #if (tracer=="H3+ST")  legend("topleft", legend = c("stable isotope"), col = "black", pch = 19, cex = text_params$cex_lab, bty = "n")

	# Panel 2
    boxplot(unlist(y_stable_residuals),las=1, ylab ="",cex.lab=1.5,ylim=range(y_stable_residuals) * c(1.3, 1.3),horizontal=F,xaxt = "n", yaxt = "n") 
    mtext(lab_stable,2,line=4,cex=1.5)   
    magaxis(side = 2, majorn = 5, minorn = 'auto', tcl = -0.5,las=1,cex.axis=1.5,mgp=c(3,1,0))

	# Panel 3
    if (tracer=="H3+ST") { # adds the residual plot for the compromise solution
	plot(outputdates1, y_combined_stable_residuals, type = "p", pch = 19, las = 1, col = "black",
		xlim = c(x_inf_stable, x_sup_stable), 
		ylim = range(y_stable_residuals) * c(1.3, 1.3),
		xaxt = "n", yaxt = "n", xlab = "Time", ylab ="",
		bty = "l", cex.lab = text_params$cex_lab, cex.axis = text_params$cex_main)
    magaxis(side = 1:2, majorn = 5, minorn = 'auto', tcl = -0.5,las=1,cex.axis=1.5,mgp=c(3,1,0))
	title(main = "Stable isotope residuals constrained by tritium", cex.main = text_params$cex_title)
    mtext(lab_stable,2,line=4,cex=1.5)   
	#legend("topleft", legend = c("Tritium + Stable Isotopes"), col = "black", pch = 19, cex = text_params$cex_lab, bty = "n")

	# Panel 4
    boxplot(unlist(y_combined_stable_residuals),las=1, ylab ="",cex.lab=1.5,ylim=range(y_stable_residuals) * c(1.3, 1.3),horizontal=F,xaxt = "n", yaxt = "n") 
    mtext(lab_stable,2,line=4,cex=1.5)   
    magaxis(side = 2, majorn = 5, minorn = 'auto', tcl = -0.5,las=1,cex.axis=1.5,mgp=c(3,1,0))

    }

	dev.off()

    if(tracer=="ST") {
    time_labels="Time"
    if(time_unit==1) time_labels="Time [years]"
    if(time_unit==12) time_labels="Time [months]"
    if(time_unit==52) time_labels="Time [weeks]"
    if(time_unit==365) time_labels="Time [days]"
    png("TTD.png",height=2000,width=2000,res=200)
    par(mar=c(5,6,1,1))
    plot(TTD[,1],TTD[,2],type="l",pch=1,xlim=c(0,max(TTD[,1])),ylim=c(0,max(TTD[,2])),las=1,xlab=time_labels,ylab="g(t/tt)",bty="l")
    if(tracer=="H3+ST") {lines(TTD[,1],TTD[,2])}
    dev.off()
    }

    if(tracer=="H3+ST") {
    time_labels="Time"
    if(time_unit==1) time_labels="Time [years]"
    if(time_unit==12) time_labels="Time [months]"
    if(time_unit==52) time_labels="Time [weeks]"
    if(time_unit==365) time_labels="Time [days]"
    png("TTD.png",height=2000,width=2000,res=200)
    par(mar=c(5,6,1,1))
    plot(TTD1[,1],TTD1[,2],type="l",pch=1,xlim=c(0,max(TTD1[,1])),ylim=c(0,max(TTD1[,2])),las=1,xlab=time_labels,ylab="g(t/tt)",bty="l")
    lines(TTD2[,1],TTD2[,2],lty=2)
    lines(TTD3[,1],TTD3[,2],lty=3)
    lines(TTD4[,1],TTD4[,2],lty=4)
	legend("bottomright", 
       legend = c("stable isotope","tritium","stable istope constrained by H3","H3 constrained by stable isotope"),
       lty = c(1, 2, 3, 4),  
       lwd = c(2, 2, 2, 2),  
       cex = text_params$cex_lab,  # Size of legend text
       inset = c(0.02, 0.02),  # Slight inset from the bottom right corner
       bty = "n")  # No box around the legend 
    dev.off()
    }

}

if (tracer=="H3" | tracer=="H3+ST") {

# Best-fit plot for tritium
    png("best-fit_h3.png", height = 2000, width = 2000, res = 200)
    par(mar = c(5, 6, 5, 1))  
    plot(h3_output_date, h3_output, type = "p", pch = 19, col = "grey",
         xlim = c(x_inf_h3, x_sup_h3), ylim = c(0, y_sup_h3),
         xaxt = "n", yaxt = "n", las = 1, xlab = "Time", ylab = "[TU]", 
         bty = "l", cex.lab = text_params$cex_lab)   
    if (time_step != "R") points(median_h3_output_date, median_h3_output, pch = 19)
    if (error_bars==1) arrows(h3_output_date,h3_output,h3_output_date,h3_output-h3_error,length=0.1,angle=90,col="black",lwd=1) # analytical error bars
    if (error_bars==1) arrows(h3_output_date,h3_output,h3_output_date,h3_output+h3_error,length=0.1,angle=90,col="black",lwd=1) # analytical error bars
    lines(inputdates2, y_h3, lwd = 2)
    if (tracer == "H3") {
        legend("bottomright", 
        legend = c("Observations", "Mean values used for fitting"),
        pch = c(19, 19),  # Symbol: filled circle for observations, NA for lines
        col = c("grey", "black"),  # Colors for the legend
        cex = text_params$cex_lab,  # Text size for the legend
        inset = c(0.02, 0.02),  # Slight offset from the bottom right corner
        bty = "n") 
    }  

    if (tracer == "H3+ST") {
        lines(inputdates2, y_combined_h3, lwd = 2, lty = 2)
        legend("bottomright", 
        legend = c("Observations", "Absolute best fit", "Best fit constrained by stable isotope"),
        pch = c(19,  NA, NA),  # Symbol: filled circle for observations, NA for lines
        lty = c(NA,  1, 2),  # Line types
        lwd = c(NA,  2, 2),  # Line widths
        col = c("black", "black", "black"),  # Colors for the legend
        cex = text_params$cex_lab,  # Text size for the legend
        inset = c(0.02, 0.02),  # Slight offset from the bottom right corner
        bty = "n") 
    }  
    title(main = "Best fit Tritium", cex.main = text_params$cex_title)   
    magaxis(side = 1:2, majorn = 5, minorn = 'auto', tcl = -0.5,las=1,cex.axis=1.5,mgp=c(3,1,0))
    dev.off()

   # Ensemble solutions plot for tritium
    if(dim(ensemble_h3)[1]!=0) # checks whether more than one solution has been retained, and skips the graphic is only the best fit is available.
    {
	y1 <- colMaxs(as.matrix(ensemble_h3))
	y2 <- colMins(as.matrix(ensemble_h3))
	png("ensemble_solutions_h3.png", height = 2000, width = 2000, res = 200)
	par(mar = c(5, 6, 5, 1))  # Increase space for title
	plot(h3_output_date, h3_output, type = "p", pch = 1,
		xlim = c(x_inf_h3, x_sup_h3), ylim = c(0, y_sup_h3),
		xaxt = "n", yaxt = "n", las = 1, xlab = "Time", ylab = "[TU]", bty = "l", cex.lab = text_params$cex_lab)
	lines(inputdates2, y1, type = "l", col = "grey")
	lines(inputdates2, y2, type = "l", col = "grey")
	polygon(c(inputdates2, rev(inputdates2)), c(y2, rev(y1)), col = "lightgrey", border = NA)
	lines(inputdates2, y_h3, lwd = 2)
	points(h3_output_date, h3_output, pch = 19, col = "black") # measures, following the logic of line67
	title(main = "Ensemble Solutions Tritium", cex.main = text_params$cex_title)
	legend("bottomright", 
       legend = c("Observations", "Ensemble solutions", "Best fit"),
       pch = c(19, NA , NA),  # Filled circles for observations, open circles for simulations, NA for line
       lty = c(NA, 1, 1),  
       col = c("black", "grey", "black"),  
       lwd = c(NA, 4, 2),  
       cex = text_params$cex_lab,  # Size of legend text
       inset = c(0.02, 0.02),  # Slight inset from the bottom right corner
       bty = "n")  # No box around the legend
    magaxis(side = 1:2, majorn = 5, minorn = 'auto', tcl = -0.5,las=1,cex.axis=1.5,mgp=c(3,1,0))
	dev.off()
    }

	# Residuals plot for tritium
	png("residuals_h3.png", height = 2000, width = 2500, res = 200)
	if (tracer=="H3+ST") layout(matrix(c(2, 1, 4, 3), nrow = 2, byrow = TRUE), widths = c(1, 3))
	if (tracer=="H3") layout(matrix(c(2,1),1,2, byrow = TRUE), widths = c(1, 3))
	par(mar = c(5, 6, 2, 2), oma = c(2, 2, 4, 2))

	# Panel 1
	plot(outputdates2, y_h3_residuals, type = "p", pch = 19, las = 1, col = "black",
		xlim = c(x_inf_h3, x_sup_h3), 
		ylim = range(y_h3_residuals) * c(1.3, 1.3),
		xaxt = "n", yaxt = "n", xlab = "Time", ylab = "[TU]",
		bty = "l", cex.lab = text_params$cex_lab, cex.axis = text_params$cex_main)
    magaxis(side = 1:2, majorn = 5, minorn = 'auto', tcl = -0.5,las=1,cex.axis=1.5,mgp=c(3,1,0))
	title(main = "Tritium best fit residuals", cex.main = text_params$cex_title)
    #if (tracer=="H3+ST")  legend("topleft", legend = c("Tritium"), col = "black", pch = 19, cex = text_params$cex_lab, bty = "n")

	# Panel 2
    boxplot(unlist(y_h3_residuals),las=1, ylab = "[TU]",cex.lab=1.5,ylim=range(y_h3_residuals) * c(1.3, 1.3),horizontal=F,xaxt = "n", yaxt = "n") 
    magaxis(side = 2, majorn = 5, minorn = 'auto', tcl = -0.5,las=1,cex.axis=1.5,mgp=c(3,1,0))
    if (tracer=="H3+ST") { # adds the residual plot for the compromise solution
	# Panel 3
	plot(outputdates2, y_combined_h3_residuals, type = "p", pch = 19, las = 1, col = "black",
		xlim = c(x_inf_h3, x_sup_h3), 
		ylim = range(y_combined_h3_residuals) * c(1.3, 1.3),
		xaxt = "n", yaxt = "n", xlab = "Time", ylab = "[TU]",
		bty = "l", cex.lab = text_params$cex_lab, cex.axis = text_params$cex_main)
    magaxis(side = 1:2, majorn = 5, minorn = 'auto', tcl = -0.5,las=1,cex.axis=1.5,mgp=c(3,1,0))
	title(main = "Tritium residuals constrained by stable isotope", cex.main = text_params$cex_title)
	#legend("topleft", legend = c("Tritium + Stable Isotopes"), col = "black", pch = 19, cex = text_params$cex_lab, bty = "n")

	# Panel 4
    boxplot(unlist(y_combined_h3_residuals),las=1, ylab = "[TU]",cex.lab=1.5,ylim=range(y_h3_residuals) * c(1.3, 1.3),horizontal=F,xaxt = "n", yaxt = "n") 
    magaxis(side = 2, majorn = 5, minorn = 'auto', tcl = -0.5,las=1,cex.axis=1.5,mgp=c(3,1,0))

    }

	dev.off()

    if(tracer=="H3") {
    time_labels="Time"
    if(time_unit==1) time_labels="Time [years]"
    if(time_unit==12) time_labels="Time [months]"
    if(time_unit==52) time_labels="Time [weeks]"
    if(time_unit==365) time_labels="Time [days]"
    png("TTD.png",height=2000,width=2000,res=200)
    par(mar=c(5,6,1,1))
    plot(TTD[,1],TTD[,2],type="l",pch=1,xlim=c(0,max(TTD[,1])),ylim=c(0,max(TTD[,2])),las=1,xlab=time_labels,ylab="g(t/tt)",bty="l")
    dev.off()
    }

    }



     
