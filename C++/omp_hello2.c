/******************************************************************************
* FILE: omp_hello.c
* DESCRIPTION:
*   OpenMP Example - Hello World - C/C++ Version
* AUTHOR: Blaise Barney  5/99
* LAST REVISED: 04/06/05
* MODIFIED: MS 09/27/23 as obtained from https://hpc-tutorials.llnl.gov/openmp/exercise1/
******************************************************************************/
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>

int main (int argc, char *argv[])
{
int nthreads, tid;
double start, end;
/* array asignments */
int N=1000, i;
double a[N];
/*----*/
start = omp_get_wtime();
/* Fork a team of threads giving them their own copies of variables */
#pragma omp parallel private(nthreads, tid)
  {

  /* Obtain thread number */
  tid = omp_get_thread_num();
  printf("Hello World from thread = %d\n", tid);

  /* Only master thread does this */
  if (tid == 0)
    {
    nthreads = omp_get_num_threads();
    printf("Number of threads = %d\n", nthreads);
    }
#pragma omp for
  for (i=0; i<N; i++){
    a[i]=sqrt((double) i);
    sleep(0.0001);
  }
  }
end = omp_get_wtime();
printf("Execution took %f seconds\n", end - start);
}
