#!/bin/bash
#SBATCH --job-name=r_features
#SBATCH --output=out.csv
#SBATCH --partition=nukwa
#SBATCH --ntasks=1
#SBATCH --time=70:00:00

Rscript main.r
