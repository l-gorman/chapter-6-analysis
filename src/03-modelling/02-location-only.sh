#!/bin/bash

#SBATCH --job-name=overall_models
#SBATCH -o ./Report/output_location_only
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=4
#SBATCH --time=0-12:00:00
#SBATCH --mem=8G
#SBATCH --account=sscm012844

cd "${SLURM_SUBMIT_DIR}"

echo "Running on host $(hostname) \n"
echo "Time is $(date) \n"
echo "Directory is $(pwd) \n"
echo "Slurm job ID is ${SLURM_JOBID} \n"
echo "This jobs runs on the following machines: \n"
echo "${SLURM_JOB_NODELIST}"

echo "Keep track of job by entering sacct -j ${SLURM_JOBID}  \n"
echo "Cancel your job by entering scancel ${SLURM_JOBID}  \n"
echo "More details on submitting jobs here https://www.acrc.bris.ac.uk/protected/hpc-docs/job_types/ \n"

module add languages/r/4.1.0

out_directory="/user/work/lg14410/chapter-6/outputs/"
data_directory="/user/work/lg14410/chapter-6/data/"
iterations=4000
warmup=2000
cores=4

Rscript "./src/03-modelling/02-location-only.R" -i $iterations -w $warmup -d $data_directory -o $out_directory -c $cores

unset out_directory
unset data_directory
unset iterations
unset warmup
unset cores