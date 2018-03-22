
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **Repository for SPL Winter Term 17/18** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)


# Information and Important Instructions
**Topic**:  
*Employment Effects of the new German minimum wage (SOEP Dataset)*

Humboldt-University of Berlin


**Team Members:**  
Albert Thieme  
Timm Walz  
Meret Borchmann  
Jupp Kerschek  


**Instruction for the Source Code:**  
The quantlets are ordered by numbers (Q1-Q8) and we strongly recommend to execute them number-by-number, as the quantlets partly refer to previously created objects and variables. In the metadata.txt as well as in the README.md of each quantlet, the exact quantlet-dependencies are listed.

The source code uses additional packages beside the standard packages in RStudio. Therefore, the *Install-Packages.R* should be executed at first. This script installs all required packages. On top of each RScript, the required packages get loaded.

## Important Note:
> Set your working directory, where the **InstallPackages.R** is located!  
This is really important to execute the quantlets correctly, as the execution of the Quantlets is always initiated by the root directory.

>**Examples:**  
setwd("Your/Directory/spl-pirates") -> **RIGHT**  
setwd("Your/Directory/spl-pirates/SOEPQ1_ImportPrepareData") -> **WRONG!**
