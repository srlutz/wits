setup_params=list(calib,obj_fun,weight_input,weight_months,lpm_model,mtt_range,par4_range) # assembles all input parameters into an single object
names(setup_params)=c("algorithm","fit","weight","months","model","mtt","pf") # names all parameters

# adds parameters that are not present in all runs if needed
if(exists("td")) {setup_params=list(calib,obj_fun,weight_input,weight_months,lpm_model,mtt_range,par4_range,td);names(setup_params)=c("algorithm","fit","weight","months","model","mtt","pf","td")}
if(exists("par2_range")) {setup_params=list(calib,obj_fun,weight_input,weight_months,lpm_model,mtt_range,par4_range,par2_range);names(setup_params)=c("algorithm","fit","weight","months","model","mtt","pf","par2")}
if(exists("td") & exists("par2_range")) {setup_params=list(calib,obj_fun,weight_input,weight_months,lpm_model,mtt_range,par4_range,td,par2_range);names(setup_params)=c("algorithm","fit","weight","months","model","mtt","pf","td","par2")}
# saves the object to a file in the "results" folder of the corresponding run
output_path=paste(output_dir,"/parameters_summary.csv",sep="")
write.csv(as.data.frame(setup_params), file =output_path,quote=F,row.names=F)
rm(output_path)
