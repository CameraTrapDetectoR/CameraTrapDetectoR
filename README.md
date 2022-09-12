# CameraTrapDetectoR: Detect, classify, and count animals in camera trap images  

Use deep learning computer vision models to automatically detect, count, and classify common North American domestic and wild species in camera trap images. CameraTrapDetectoR can be deployed as an R function or in an interactive Shiny application.  
  
Four types of models are available: a taxonomic class model that classifies objects as mammal or avian; a taxonomic family model that recognizes 31 mammal, avian, and reptile families; a pig-only model that recognizes wild pigs and classifies all other detections as not-pig; a species model that recognizes 75 unique domestic and wild species including all North American wild cat species, bear species, and Canid species. Each model also includes a category for vehicles and empty images.  

\
*Note: this package takes over development of previous github repository 'TabakM/CameraTrapDetectoR'. If you have downloaded the previous version of CameraTrapDetectoR, please uninstall and reinstall from this repository to stay connected with ongoing package developments.*  
  


## Install CameraTrapDetectoR

Install devtools if you don't have it, then install CameraTrapDetectoR:  

```
devtools::install_github("CameraTrapDetectoR/CameraTrapDetectoR")
```
Agree to update all necessary packages. 

See the [Installation Guide](https://github.com/CameraTrapDetectoR/CameraTrapDetectoR/wiki/Installation) for more details.  
  

## Load the Package
```
library(CameraTrapDetectoR)
```
  
The package downloads weights and model architecture of the large neural network; if you are on a slow internet connection, you may need to modify your options. By default, R will timeout downloads at 60 seconds. Running the line below will increase the operation timeout (units are in seconds). Feel free to use a larger number than 200 if you are on a very slow connection.

```
options(timeout=200)
```  
If you are connected to VPN, you may also try disconnecting from VPN while downloading model weights.  


## Deploy the Model in Console  

Deploy the model from the console with `deploy_model`. Here is a brief example:  
  
```
# specify the path to your images
data_dir = "C:/Users/..." # if you don't know how to specify paths, use the shiny app below. 

# deploy the model and store the output dataframe as predictions
predictions <- deploy_model(data_dir,
                            make_plots=TRUE, # this will plot the image and predicted bounding boxes
                            sample50 = TRUE) # this will cause the model to only work on 50 random images in your dataset. To do the whole dataset, set this to FALSE
```
There are many more options for this function. Type `?deploy_model`, see the user manual, or consult the [wiki](https://github.com/CameraTrapDetectoR/CameraTrapDetectoR/wiki/Function-Arguments) for details. 

## Deploy the Model in Shiny
Copy + paste this code to the R console to launch the interactive app:
```
runShiny("deploy")
```
This will launch a Shiny App on your computer. See the **Shiny Demo** vignette or the [wiki](https://github.com/CameraTrapDetectoR/CameraTrapDetectoR/wiki/Shiny-Tutorial) for a complete example on using the Shiny app. 


## Install from source
If you could not install package from github, follow these instructions to install from source

### Download CameraTrapDetectoR
This [link](https://github.com/CameraTrapDetectoR/CameraTrapDetectoR/blob/main/CameraTrapDetectoR_0.2.0.zip) holds the latest version of the package. DO NOT unzip this folder. 

### Install dependencies
Copy this code and paste it into your console. It will install all necessary R packages
```
install_dependencies <-function(packages=c('torchvision', 'torch', 'magick', 
                                           'shiny', 'shinyFiles', 'shinyBS', 
                                           'shinyjs', 'rappdirs', 'fs', 'sf', 
					   'operators', 'torchvisionlib')) {
  cat(paste0("checking package installation on this computer"))
  libs<-unlist(list(packages))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  
  if(length(need)>0){ 
    cat(paste0("The packages: ", need, "\n Need to be installed. Installing them now.\n"))
    utils::install.packages(need)
    lapply(need,require,character.only=TRUE)
  } else{
    cat("All necessary packages are installed on this computer. Proceed.\n")
  }
}
install_dependencies()
```

### Install CameraTrapDetectoR from source
- In RStudio, Click on `Packages`, then click `Install` (just below and to the left of `Packages`)
- In the install menu, click on the arrow by `Install From`
- Click on `Package Achive File`
- Click `Browse` and navigate to the zip file that you just downloaded. 
- click `install`


## Citation

Tabak, M. A., Falbel, D., Hamzeh, T., Brook, R. K., Goolsby, J. A., Zoromski, L. D., Boughton, R. K., Snow, N. P., VerCauteren, K. C., & Miller, R. S. (2022). CameraTrapDetectoR: Automatically detect, classify, and count animals in camera trap images using artificial intelligence (p. 2022.02.07.479461). bioRxiv. [link to manuscript](https://doi.org/10.1101/2022.02.07.479461)

Or\
@article {Tabak2022.02.07.479461,
	author = {Tabak, Michael A and Falbel, Daniel and Hamzeh, Tess and Brook, Ryan K and Goolsby, John A and Zoromski, Lisa D and Boughton, Raoul K and Snow, Nathan P and VerCauteren, Kurt C and Miller, Ryan S},
	title = {CameraTrapDetectoR: Automatically detect, classify, and count animals in camera trap images using artificial intelligence},
	elocation-id = {2022.02.07.479461},
	year = {2022},
	doi = {10.1101/2022.02.07.479461},
	publisher = {Cold Spring Harbor Laboratory},,
	URL = {https://www.biorxiv.org/content/early/2022/02/09/2022.02.07.479461},
	eprint = {https://www.biorxiv.org/content/early/2022/02/09/2022.02.07.479461.full.pdf},
	journal = {bioRxiv}
}
