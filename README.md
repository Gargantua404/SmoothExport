# SmoothExport
## Installation
 *There is a bug in R (3.4.0) which prevents user from installing the package from GitHub by using devtools::install_github() function.
 This bug should be fixed in >= 3.6.0 R versions. Another way to setup the package:*
 
1. Download the .zip archived source files from GitHub;  
2. Convert .zip archive into .tar.gz archive;
3. In Rstudio console perform the following command:\
   utils::install.packages("dir_path//SmoothExport.tar.gz", repos = NULL, type = "source")\
   where *dir_path* - is a directory where the archieve is stored.

 ## Macros usage
 Macroses, being wriiten on Visual Basic, are part of the Microsoft Office system on a local computer and are accessible from any Office applications (Word, Excel, Powerpoint). Next instruction describes the way to embed files with macroses into the Word:

 1. In the control panel: Developer -> Macroses (Alt + F8)
 2. In a popped-up window: Visual Basic
 3. In the project panel: Normal -> Modules. Right click mouse: Insert Module. Name of the module can be arbitrary.
 4. The macros code can be copied into the popped-up text editor window or imported as a separate file. 

   
