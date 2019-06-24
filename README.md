# SmoothExport
 Installation:
 There is a bug in R (3.4.0) which prevents user from installing the package from GitHub by using devtools::install_github() function.
 This bug should be fixed in >= 3.6.0 R versions. 
 Another way to setup the package:
1. Download the .zip archived source files from GitHub;  
2. Convert .zip archive into .tar.gz archive;
3. In Rstudio console perform the following command:
   utils::install.packages("your_dir//SmoothExport.tar.gz", repos = NULL, type = "source")
   where your_dir - is a directory where the archieve is stored.
   
