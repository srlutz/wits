#setup_params=list(calib,obj_fun,weight_input,weight_months,sas_fun,stor_range,sas_ini) # assembles all input parameters into an single object
#names(setup_params)=c("algorithm","fit","weight","months","model","storage","itracer") # names all parameters

if(sas_fun==1) {setup_params=list(calib,obj_fun,weight_input,weight_months,sas_fun,stor_range,sas_ini,kQ_range, kET_range);names(setup_params)=c("algorithm","fit","weight","months","model","storage","itracer","kQ","kET")}
if(sas_fun==2) {setup_params=list(calib,obj_fun,weight_input,weight_months,sas_fun,stor_range,sas_ini,kminQ_range,kmaxQ_range, betaET_range);names(setup_params)=c("algorithm","fit","weight","months","model","storage","itracer","kQmin","kQmax","betaET")}
if(sas_fun==3) {setup_params=list(calib,obj_fun,weight_input,weight_months,sas_fun,stor_range,sas_ini,alpha_range,beta_range, betaET_range);names(setup_params)=c("algorithm","fit","weight","months","model","storage","itracer","alpha","beta","betaET")}

# saves the object to a file in the "results" folder of the corresponding run
output_path=paste(output_dir,"/parameters_summary.csv",sep="")
write.csv(as.data.frame(setup_params), file =output_path,quote=F,row.names=F)
rm(output_path)
