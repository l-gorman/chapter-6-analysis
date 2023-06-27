#!/bin/bash

#SBATCH --job-name=overall_models
#SBATCH -o ./Report/output.projpred.%a.out
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=4
#SBATCH --time=3-00:00:00
#SBATCH --mem=150G
#SBATCH --account=sscm012844
#SBATCH --array=1-2


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

tools/cmake/3.22.2
module add  lang/r/4.3.0-gcc

out_directory="/user/work/lg14410/chapter-6/outputs/overall_models/variable_addition"
cores=24

Rscript "./src/03-modelling/04-projpred.R" -o $out_directory -c $cores -j ${SLURM_ARRAY_TASK_ID}

unset out_directory
unset cores
