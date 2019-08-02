##HYPR Ploidy analysis code


Flow_jo_data_analysis <- function(Expected_genome_sizes_df, Flow_jo_data){
  scer<- 24 #size of diploid S. cer genome to scale by
  control<- which(Flow_jo_data[,1]=="H134")
  #finds the control diploid S. cerevisiae strain 
  Expected_genome_sizes_df$Expected_FITC<- NA
  Expected_genome_sizes_df$obs_MB_scaled_by_HU_mean<- NA
  Expected_genome_sizes_df$obs_MB_scaled_by_G1_mean<-NA
  for (i in 1:nrow(Flow_jo_data)){
    Expected_genome_sizes_df[i,3]<- (Flow_jo_data[control,5]*Expected_genome_sizes_df[i,2])/scer
    Expected_genome_sizes_df[i,4]<- (scer*Flow_jo_data[i,3])/Flow_jo_data[control,5]
    Expected_genome_sizes_df[i,5]<- (scer*Flow_jo_data[i,5])/Flow_jo_data[control,5]
  }
return(Expected_genome_sizes_df)
}
