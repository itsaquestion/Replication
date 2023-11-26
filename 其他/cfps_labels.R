library(dplyr)
library(haven)
library(purrr)
library(writexl)


file_path = "C:/Users/lee/nutcloud/Data/cfps/2012//cfps2012child_201906.dta"

extract_dta_labels = function(file_path){
  # 提取一个dta文件的var:label
  
  print(file_path)
  
  df = read_dta(file_path)
  
  
  get_var_info = function(x){
    label = attr(x,"label")
    
    if(length(label)>1){
      label = ''
    }
    
    if(is.null(label)){
      label = ''
    }
    
    labels = attr(x,"labels")
    
    # labels = labels[labels>]
    
    labels_str = ""
    
    # print(names(x))
    labels = labels[labels>=0]
    
    if (length(labels) > 0) {
      
      labels_str = paste0(labels, ":",  names(labels), collapse = ", ")
    }

   
    data.frame(label = label, labels = labels_str)
  }
  
  all_vars = reduce(lapply(df, get_var_info),rbind)
  
  all_vars = data.frame(var = names(df), all_vars)
  
  all_vars
  
}



base_path_year = 'C:/Users/lee/nutcloud/Data/cfps/2014/'

dta_list = list.files(base_path_year,'.dta$')

file_name = dta_list[1]


# extract_dta_labels(full_path)

list_of_dfs = lapply(dta_list, \(file_name){
  full_path = file.path(base_path_year,file_name)
  extract_dta_labels(full_path)
})


# file_path = full_path

list_of_dfs = setNames(list_of_dfs,dta_list)

write_xlsx(list_of_dfs, path = file.path(base_path_year,'cfps_2014_var_label.xlsx'))















