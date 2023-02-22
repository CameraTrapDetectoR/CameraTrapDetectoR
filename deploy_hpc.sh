#!/bin/bash Rscript

# Template bash script to run CameraTrapDetectoR in HPC environment


#SBATCH --partition=partition
#SBATCH --qos=qos
#SBATCH --job-name="runCTD"
#SBATCH --mail-user=my.name@myemail.com
#SBATCH --mail-type=BEGIN,END,FAIL

module purge

module load miniconda
source activate CTDtrain

module unload miniconda
module load python      # check for latest python release
export PYTHONPATH="/project/cameratrapdetector/train"
python /project/cameratrapdetector/train/trainCTD.py
# END