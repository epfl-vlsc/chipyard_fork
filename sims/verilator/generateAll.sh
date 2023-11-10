#!/bin/bash
genTask() {
    # sleep 0.5; echo "$1";
    echo "======================== Generating config $1 =============================="
    make -f simple.mk CONFIG=chipyard.example.simulation.$1 source_tar
    # make -f simple.mk CONFIG=chipyard.example.simulation.$1
    echo "Done generating $1"
    scp chipyard.example.simulation.${1}_sources.tar.gz IPU1:docker-scratch/ipu_eval
    echo "=========================== Finished $1 ====================================="
}

config_list=()

addConfig() {
    config_list+=" $1 "
    echo $config_list
}


# Bus-based
for cores in 6; do
    addConfig SmallBoom${cores}CoreBusConfig
    addConfig MediumBoom${cores}CoreBusConfig
    addConfig LargeBoom${cores}CoreBusConfig
    addConfig MegaBoom${cores}CoreBusConfig
    addConfig SmallRocket${cores}CoreBusConfig
    addConfig LargeRocket${cores}CoreBusConfig
done

# rocket flat mesh
for cores in 2 3 4 5 6 7 8 9 10 11 12 14 14 15; do
    addConfig SmallRocket${cores}x${cores}MeshConfig
done
for cores in 2 3 4 5 6 7 8 9 10; do
    addConfig LargeRocket${cores}x${cores}MeshConfig
done

# hierarchical mesh, rocket only
for cores in 2 3 4 5 6 7 8 9 10; do
    addConfig SmallRocket${cores}x${cores}Core2x1MeshConfig
    addConfig SmallRocket${cores}x${cores}Core2x2MeshConfig
done

# boom flat mesh
for cores in 2 3 4 5 6 7 8; do
    addConfig SmallBoom${cores}x${cores}CoreMeshConfig
    addConfig LargeBoom${cores}x${cores}CoreMeshConfig
done


# initialize a semaphore with a given number of tokens
open_sem(){
    mkfifo pipe-$$
    exec 3<>pipe-$$
    rm pipe-$$
    local i=$1
    for((;i>0;i--)); do
        printf %s 000 >&3
    done
}

# run the given command asynchronously and pop/push tokens
run_with_lock(){
    local x
    # this read waits until there is something to read
    read -u 3 -n 3 x && ((0==x)) || exit $x
    (
     ( "$@"; )
    # push the return code of the command to the semaphore
    printf '%.3d' $? >&3
    )&
}

N=4

open_sem $N
for cfg in $config_list; do
    run_with_lock genTask $cfg

done


wait;
