path_res=paste(results_dir,"/run",k,sep="") # automatically creates file paths to store the results of each run separately (main directory is xxxx/run, which itself contains two subdirectories for the figures and the tables respectively)
path_figs=paste(path_res,"/figures",sep="")
path_tables=paste(path_res,"/output",sep="")
dir.create(path_res, showWarnings = FALSE) # automatically creates the folders to store the results of each run separately
dir.create(path_figs, showWarnings = FALSE)
dir.create(path_tables, showWarnings = FALSE)
output_dir<-c(path_tables) # assigns the current path to the directory storing the tables.
figures_dir<-c(path_figs) # assigns the current path to the directory storing the figures.

