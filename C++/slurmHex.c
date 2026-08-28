/******************************************************************************
* FILE: slurmHex.c
* DESCRIPTION:
* AUTHOR: Blaise Barney
* LAST REVISED: 06/22/17
******************************************************************************/
#include "mpi.h"
#include <stdio.h>
#include <stdlib.h>

int main (int argc, char *argv[])
{
int   numtasks, rank, len;
char hostname[MPI_MAX_PROCESSOR_NAME];

sleep(5);

MPI_Init(&argc, &argv);
MPI_Comm_size(MPI_COMM_WORLD, &numtasks);
MPI_Comm_rank(MPI_COMM_WORLD,&rank);
MPI_Get_processor_name(hostname, &len);
printf ("Task %d of %d running on %s\n", rank,numtasks,hostname);

sleep(60);

MPI_Finalize();

}

