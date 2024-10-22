
if(tracer=="H3+ST" | tracer=="ST") {
    o18lab=expression(paste("",delta^18,"",O ,"[\u2030]"))
    h2lab=expression(paste("",delta^2,"",H ,"[\u2030]"))
    if (stable_iso=="O18") {lab_stable=o18lab} else {lab_stable=h2lab}
    }

#### setting automatically the time boundaries #####

if (tracer=="ST" | tracer=="H3+ST") {

    x_inf_stable=floor(stable_output_date[1])-2
    x_sup_stable=ceiling(stable_output_date[length(stable_output_date)])+2
    if(calib_subset=="N") {x_inf_stable=floor(calib_dates[1]); x_sup_stable=ceiling(calib_dates[2])}
    x_inf_stable_input=floor(stable_input_date[1])
    x_sup_stable_input=ceiling(stable_input_date[length(stable_input_date)])

    if(stable_iso=="H2") {

        y_inf_stable=floor(min(stable_output)/10)*10
        y_sup_stable=ceiling(max(stable_output)/10)*10
        y_inf_stable_input=floor(min(stable_input)/10)*10
        y_sup_stable_input=ceiling(max(stable_input)/10)*10

        y_inf_stable_residuals=floor(min(y_stable_residuals)/10)*10-20
        y_sup_stable_residuals=ceiling(max(y_stable_residuals)/10)*10+20
        }

    if(stable_iso=="O18") {

        y_inf_stable=floor(min(stable_output))
        y_sup_stable=ceiling(max(stable_output))
        y_inf_stable_input=floor(min(stable_input))
        y_sup_stable_input=ceiling(max(stable_input))

        y_inf_stable_residuals=floor(min(y_stable_residuals))-2
        y_sup_stable_residuals=ceiling(max(y_stable_residuals))+2
        }


    }

if (tracer=="H3" | tracer=="H3+ST") {

    x_inf_h3=floor(h3_output_date[1]/10)*10-2
    x_sup_h3=ceiling(h3_output_date[length(h3_output_date)]/10)*10+2
    if(calib_subset=="N") {x_inf_h3=floor(calib_dates[1]); x_sup_h3=ceiling(calib_dates[2])}
    x_inf_h3_input=floor(h3_input_date[1])
    x_sup_h3_input=ceiling(h3_input_date[length(h3_input_date)])

    y_sup_h3_input=ceiling(max(h3_input)/10)*10
    y_sup_h3=ceiling(max(h3_output)/10)*10
    y_inf_h3_residuals=floor(min(y_h3_residuals)/10)*10-20
    y_sup_h3_residuals=ceiling(max(y_h3_residuals)/10)*10+20

    }

