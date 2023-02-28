#!/bin/bash

#SBATCH --job-name='deployCTD'
#SBATCH --partition='partition'
#SBATCH --qos='qos'
#SBATCH --mail-user=First.Last@myemail.com
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --nodes

module purge

module load r/4.2.0

# navigate to dir where your R script is stored
cd /path/to/project/dir/to/Rfiles/

# run R script with user arguments specified
Rscript deploy_model_hpc.R --data_dir '/path/to/images/for/processing' --model_type 'species' --output_dir '/path/to/store/output/' --sample50 TRUE