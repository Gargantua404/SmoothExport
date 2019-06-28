# SmoothExport
## Installation
 *There is a bug in R (3.4.0) which prevents users from installing the package from GitHub by executing `devtools::install_github()` function.
 This bug should be fixed in >= 3.6.0 R versions. Here is another way to setup the package:*

There are two ways to install the package 
###### Manual installation|
1. Download the .zip archived source files from GitHub
2. In Rstudio console perform the following command:\
   `devtools::install_local("dir_path\\SmoothExport.zip", dependencies = c("Depends, Imports"))`\
   where *dir_path* - is a directory where the archieve is stored

###### Automatic installation
1. Download the .zip archived source files from GitHub
2. Convert the .zip archive into the .tar.gz archive
3. Pass the generated archieve to **Packages -> Install**

 ## Macros usage
 Macroses, being written on Visual Basic, are part of the Microsoft Office system on a local computer and are accessible from any Office applications (Word, Excel, PowerPoint). The following instructions describe the way to embed the files with macroses into an Office system through a Word document:

 1. In the control panel: **Developer -> Macroses (Alt + F8)**
 2. In a popped-up window: **Visual Basic**
 3. In the project panel: **Normal -> Modules**. Right click mouse: **Insert Module**. Name of the module can be arbitrary.
 4. The macros code can be copied into the popped-up text editor window or imported as a separate file. 
 5. Now the macros is embedded into the Office system and can be called from the subwindow **Developer -> Macroses (Alt + F8)**

   
