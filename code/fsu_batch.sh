#!/bin/bash

a=1
while [ $a -le 1 ]
do 

b=1
while [ $b -le 1 ]
do

c=2
while [ $c -le 2 ]
do

d=2
while [ $d -le 2 ]
do

e=1
while [ $e -le 1 ]
do

    sbatch << EOJ
#!/bin/bash

#SBATCH --job-name=X${a}C${b}N${c}P${d}J${e}
#SBATCH -o $PWD/log/X${a}C${b}N${c}P${d}J${e}.txt
#SBATCH -e $PWD/log/X${a}C${b}N${c}P${d}J${e}.log
#SBATCH --mail-type=FAIL
#SBATCH -n 11
#SBATCH -p genacc_q
#SBATCH -t 12:00:00

cd $PWD

module purge
module load gnu/13.2.0 openmpi/4.1.6 R/4.4.0

mpirun -n 11 Rscript code/main.R ${a} ${b} ${c} ${d} ${e} 1

EOJ

((e++))
done

((d++))
done

((c++))
done

((b++))
done

((a++))
done

