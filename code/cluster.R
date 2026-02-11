usethis::use_git_config(user.name = "NeveTong", user.email = "yiyeqqa@163.com")
credentials::set_github_pat("YourPAT")
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


## how to submit job
# sbatch psu_batch.sh 

## how to view jobs full name
# squeue --format="%.18i %.9P %.30j %.8u %.2T %.10M %.9l %.6D %C %R" --me

## how to use job array
# https://guiesbibtic.upf.edu/recerca/hpc/array-jobs

## how to delete job
# scancel {start_job_id..end_job_id}

## How to Estimate Wait Time:
# squeue -u zt23g
# See your job's status and priority.
# 
# scontrol show job <jobid>
# Get detailed info on your job, including its Priority, Partition, ReqNodes, etc.
# 
# sprio
# Show job priorities and factors affecting them.
# 
# squeue --start -j <jobid>
# Estimate of when your job might start (based on current queue state, can change).

## How to send notification to Telegram when a job finishes
# JOB_ID=xxxxx
# ./monitor_job.sh "$JOB_ID" &
