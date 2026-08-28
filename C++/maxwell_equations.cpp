// fdtd_hybrid.cpp
// Plug-and-play single-file hybrid FDTD (MPI + OpenMP + optional OpenACC)
// Save as fdtd_hybrid.cpp
//
// Compile examples:
//  CPU-only (MPI + OpenMP):
//    mpicxx -O3 -fopenmp -std=c++17 fdtd_hybrid.cpp -o fdtd_cpu
//
//  NVHPC (OpenACC GPU offload via nvc++):
//    nvc++ -O3 -acc -Minfo=accel -mp=nonuma -std=c++17 fdtd_hybrid.cpp -o fdtd_gpu
//
//  GCC with OpenACC (if available):
//    g++ -O3 -fopenmp -fopenacc -std=c++17 fdtd_hybrid.cpp -o fdtd_gpu
//
// Run example (single node, 4 ranks):
//    mpirun -np 4 ./fdtd_cpu --nx 256 --ny 256 --nz 128 --steps 200 --px 2 --py 2 --pz 1 --verify
//
// Notes:
//  - This is a demonstration-level solver implementing the standard Yee updates.
//  - Uses single-precision floats for memory efficiency.
//  - Halo exchange packs faces into host buffers (works with non-CUDA-aware MPI).
//  - If compiling with OpenACC support, set --use-gpu at runtime to enable offload.

#include <mpi.h>
#include <omp.h>
#include <cmath>
#include <vector>
#include <iostream>
#include <cstring>
#include <chrono>
#include <sstream>
#include <iomanip>

using real = float;

// Toggle: if compiler defines __OPENACC__ we'll enable the OpenACC code paths.
// Additionally you can compile with -DUSE_OPENACC to force it.
#if defined(__OPENACC__) || defined(USE_OPENACC)
#define HAVE_OPENACC 1
#include <openacc.h>
#else
#define HAVE_OPENACC 0
#endif

// Index helper for 3D arrays with halo
inline size_t idx(int i, int j, int k, int nx, int ny, int nz){
    return (size_t)k * ny * nx + (size_t)j * nx + (size_t)i;
}

struct Opts {
    int NX=256, NY=256, NZ=128;
    int steps=100;
    int px=1, py=1, pz=1;
    bool use_gpu=false;
    bool verify=false;
} opts;

void parse_args(int argc, char** argv){
    for(int a=1;a<argc;a++){
        std::string s(argv[a]);
        if(s=="--nx" && a+1<argc) opts.NX = atoi(argv[++a]);
        else if(s=="--ny" && a+1<argc) opts.NY = atoi(argv[++a]);
        else if(s=="--nz" && a+1<argc) opts.NZ = atoi(argv[++a]);
        else if(s=="--steps" && a+1<argc) opts.steps = atoi(argv[++a]);
        else if(s=="--px" && a+1<argc) opts.px = atoi(argv[++a]);
        else if(s=="--py" && a+1<argc) opts.py = atoi(argv[++a]);
        else if(s=="--pz" && a+1<argc) opts.pz = atoi(argv[++a]);
        else if(s=="--use-gpu") opts.use_gpu = true;
        else if(s=="--verify") opts.verify = true;
        else if(s=="--help"){
            if(MPI::COMM_WORLD.Get_rank()==0){
                std::cout << "Usage: ./fdtd [--nx N --ny N --nz N --steps S --px a --py b --pz c --use-gpu --verify]\n";
            }
            MPI_Finalize();
            exit(0);
        }
    }
}

// pack/unpack helpers for faces
void pack_face_x(const std::vector<real>& F, std::vector<real>& buf, int i_src, int nx, int ny, int nz){
    size_t p=0;
    for(int k=0;k<nz;k++){
        for(int j=0;j<ny;j++){
            buf[p++] = F[idx(i_src,j,k,nx,ny,nz)];
        }
    }
}
void unpack_face_x(std::vector<real>& F, const std::vector<real>& buf, int i_dst, int nx, int ny, int nz){
    size_t p=0;
    for(int k=0;k<nz;k++){
        for(int j=0;j<ny;j++){
            F[idx(i_dst,j,k,nx,ny,nz)] = buf[p++];
        }
    }
}
void pack_face_y(const std::vector<real>& F, std::vector<real>& buf, int j_src, int nx, int ny, int nz){
    size_t p=0;
    for(int k=0;k<nz;k++){
        for(int i=0;i<nx;i++){
            buf[p++] = F[idx(i,j_src,k,nx,ny,nz)];
        }
    }
}
void unpack_face_y(std::vector<real>& F, const std::vector<real>& buf, int j_dst, int nx, int ny, int nz){
    size_t p=0;
    for(int k=0;k<nz;k++){
        for(int i=0;i<nx;i++){
            F[idx(i,j_dst,k,nx,ny,nz)] = buf[p++];
        }
    }
}
void pack_face_z(const std::vector<real>& F, std::vector<real>& buf, int k_src, int nx, int ny, int nz){
    size_t p=0;
    for(int j=0;j<ny;j++){
        for(int i=0;i<nx;i++){
            buf[p++] = F[idx(i,j,k_src,nx,ny,nz)];
        }
    }
}
void unpack_face_z(std::vector<real>& F, const std::vector<real>& buf, int k_dst, int nx, int ny, int nz){
    size_t p=0;
    for(int j=0;j<ny;j++){
        for(int i=0;i<nx;i++){
            F[idx(i,j,k_dst,nx,ny,nz)] = buf[p++];
        }
    }
}

int main(int argc, char** argv){
    MPI_Init(&argc, &argv);
    int rank, nprocs;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);

    parse_args(argc, argv);

    if(opts.px * opts.py * opts.pz != nprocs){
        if(rank==0) std::cerr << "Error: px*py*pz must equal number of MPI ranks\n";
        MPI_Abort(MPI_COMM_WORLD, -1);
    }

    if(opts.use_gpu && !HAVE_OPENACC){
        if(rank==0) std::cerr << "Warning: OpenACC unavailable at compile-time; running CPU-only.\n";
        opts.use_gpu = false;
    }

    int dims[3] = {opts.px, opts.py, opts.pz};
    int periods[3] = {0,0,0};
    MPI_Comm cart;
    MPI_Cart_create(MPI_COMM_WORLD, 3, dims, periods, 1, &cart);
    int coords[3];
    MPI_Cart_coords(cart, rank, 3, coords);
    int rx = coords[0], ry = coords[1], rz = coords[2];

    int gx = opts.NX, gy = opts.NY, gz = opts.NZ;
    int base_x = gx / opts.px, rem_x = gx % opts.px;
    int lx = base_x + (rx < rem_x ? 1 : 0);
    int base_y = gy / opts.py, rem_y = gy % opts.py;
    int ly = base_y + (ry < rem_y ? 1 : 0);
    int base_z = gz / opts.pz, rem_z = gz % opts.pz;
    int lz = base_z + (rz < rem_z ? 1 : 0);

    int x0 = rx * base_x + std::min(rx, rem_x);
    int y0 = ry * base_y + std::min(ry, rem_y);
    int z0 = rz * base_z + std::min(rz, rem_z);

    const int H = 1;
    int nx = lx + 2*H;
    int ny = ly + 2*H;
    int nz = lz + 2*H;
    size_t vol = (size_t)nx * ny * nz;

    if(rank==0){
        std::cout << "Global: " << gx << "x" << gy << "x" << gz
                  << "  procs: " << nprocs << " layout: " << opts.px << "x" << opts.py << "x" << opts.pz << "\n";
    }

    std::vector<real> Ex(vol, 0.0f), Ey(vol,0.0f), Ez(vol,0.0f);
    std::vector<real> Hx(vol, 0.0f), Hy(vol,0.0f), Hz(vol,0.0f);

    int nbr_px, nbr_mx, nbr_py, nbr_my, nbr_pz, nbr_mz;
    int tmpcoords[3];
    tmpcoords[0]=rx+1; tmpcoords[1]=ry; tmpcoords[2]=rz;
    MPI_Cart_rank(cart, tmpcoords, &nbr_px); if(tmpcoords[0]>=opts.px) nbr_px = MPI_PROC_NULL;
    tmpcoords[0]=rx-1; tmpcoords[1]=ry; tmpcoords[2]=rz;
    MPI_Cart_rank(cart, tmpcoords, &nbr_mx); if(tmpcoords[0]<0) nbr_mx = MPI_PROC_NULL;
    tmpcoords[0]=rx; tmpcoords[1]=ry+1; tmpcoords[2]=rz;
    MPI_Cart_rank(cart, tmpcoords, &nbr_py); if(tmpcoords[1]>=opts.py) nbr_py = MPI_PROC_NULL;
    tmpcoords[0]=rx; tmpcoords[1]=ry-1; tmpcoords[2]=rz;
    MPI_Cart_rank(cart, tmpcoords, &nbr_my); if(tmpcoords[1]<0) nbr_my = MPI_PROC_NULL;
    tmpcoords[0]=rx; tmpcoords[1]=ry; tmpcoords[2]=rz+1;
    MPI_Cart_rank(cart, tmpcoords, &nbr_pz); if(tmpcoords[2]>=opts.pz) nbr_pz = MPI_PROC_NULL;
    tmpcoords[0]=rx; tmpcoords[1]=ry; tmpcoords[2]=rz-1;
    MPI_Cart_rank(cart, tmpcoords, &nbr_mz); if(tmpcoords[2]<0) nbr_mz = MPI_PROC_NULL;

    size_t facesz_x = (size_t)H * ny * nz;
    size_t facesz_y = (size_t)nx * H * nz;
    size_t facesz_z = (size_t)nx * ny * H;

    std::vector<real> sbuf_x(facesz_x), rbuf_x(facesz_x);
    std::vector<real> sbuf_y(facesz_y), rbuf_y(facesz_y);
    std::vector<real> sbuf_z(facesz_z), rbuf_z(facesz_z);

    real dx = 1.0f, dy=1.0f, dz=1.0f;
    real c = 1.0f;
    real dt = 0.5f * std::min({dx,dy,dz}) / c;
    real ce = dt;
    real ch = dt;

    int sgx = gx/2, sgy = gy/2, sgz = gz/2;
    bool have_source = (sgx >= x0 && sgx < x0+lx && sgy >= y0 && sgy < y0+ly && sgz >= z0 && sgz < z0+lz);
    int si = (sgx - x0) + H;
    int sj = (sgy - y0) + H;
    int sk = (sgz - z0) + H;

    MPI_Barrier(cart);
    double tstart = MPI_Wtime();

    for(int step=0; step<opts.steps; ++step){
#if HAVE_OPENACC
        if(opts.use_gpu){
#pragma acc data present(Ex[0:vol],Ey[0:vol],Ez[0:vol],Hx[0:vol],Hy[0:vol],Hz[0:vol])
#pragma acc parallel loop collapse(3) present(Ex,Ey,Ez,Hx,Hy,Hz)
            for(int k=H;k<nz-H;k++){
                for(int j=H;j<ny-H;j++){
                    for(int i=H;i<nx-H;i++){
                        size_t id = idx(i,j,k,nx,ny,nz);
                        real dEy_dz = (Ey[idx(i,j,k+1,nx,ny,nz)] - Ey[id]) / dz;
                        real dEz_dy = (Ez[idx(i,j+1,k,nx,ny,nz)] - Ez[id]) / dy;
                        Hx[id] -= ch * (dEy_dz - dEz_dy);

                        real dEz_dx = (Ez[idx(i+1,j,k,nx,ny,nz)] - Ez[id]) / dx;
                        real dEx_dz = (Ex[idx(i,j,k+1,nx,ny,nz)] - Ex[id]) / dz;
                        Hy[id] -= ch * (dEz_dx - dEx_dz);

                        real dEx_dy = (Ex[idx(i,j+1,k,nx,ny,nz)] - Ex[id]) / dy;
                        real dEy_dx = (Ey[idx(i+1,j,k,nx,ny,nz)] - Ey[id]) / dx;
                        Hz[id] -= ch * (dEx_dy - dEy_dx);
                    }
                }
            }
        } else
#endif
        {
#pragma omp parallel for collapse(3) schedule(static)
            for(int k=H;k<nz-H;k++){
                for(int j=H;j<ny-H;j++){
                    for(int i=H;i<nx-H;i++){
                        size_t id = idx(i,j,k,nx,ny,nz);
                        real dEy_dz = (Ey[idx(i,j,k+1,nx,ny,nz)] - Ey[id]) / dz;
                        real dEz_dy = (Ez[idx(i,j+1,k,nx,ny,nz)] - Ez[id]) / dy;
                        Hx[id] -= ch * (dEy_dz - dEz_dy);

                        real dEz_dx = (Ez[idx(i+1,j,k,nx,ny,nz)] - Ez[id]) / dx;
                        real dEx_dz = (Ex[idx(i,j,k+1,nx,ny,nz)] - Ex[id]) / dz;
                        Hy[id] -= ch * (dEz_dx - dEx_dz);

                        real dEx_dy = (Ex[idx(i,j+1,k,nx,ny,nz)] - Ex[id]) / dy;
                        real dEy_dx = (Ey[idx(i+1,j,k,nx,ny,nz)] - Ey[id]) / dx;
                        Hz[id] -= ch * (dEx_dy - dEy_dx);
                    }
                }
            }
        }

        pack_face_x(Hx, sbuf_x, H, nx, ny, nz);
        pack_face_x(Hx, sbuf_x, nx - H - 1, nx, ny, nz);

        MPI_Request reqs[6];
        int rcount = 0;

        MPI_Irecv(rbuf_x.data(), (int)facesz_x, MPI_FLOAT, nbr_px, 100, cart, &reqs[rcount++]);
        MPI_Isend(sbuf_x.data(), (int)facesz_x, MPI_FLOAT, nbr_mx, 100, cart, &reqs[rcount++]);

        pack_face_y(Hy, sbuf_y, H, nx, ny, nz);
        pack_face_y(Hy, sbuf_y, ny - H - 1, nx, ny, nz);
        MPI_Irecv(rbuf_y.data(), (int)facesz_y, MPI_FLOAT, nbr_py, 200, cart, &reqs[rcount++]);
        MPI_Isend(sbuf_y.data(), (int)facesz_y, MPI_FLOAT, nbr_my, 200, cart, &reqs[rcount++]);

        pack_face_z(Hz, sbuf_z, H, nx, ny, nz);
        pack_face_z(Hz, sbuf_z, nz - H - 1, nx, ny, nz);
        MPI_Irecv(rbuf_z.data(), (int)facesz_z, MPI_FLOAT, nbr_pz, 300, cart, &reqs[rcount++]);
        MPI_Isend(sbuf_z.data(), (int)facesz_z, MPI_FLOAT, nbr_mz, 300, cart, &reqs[rcount++]);

#if HAVE_OPENACC
        if(opts.use_gpu){
#pragma acc data present(Hx[0:vol],Hy[0:vol],Hz[0:vol],Ex[0:vol],Ey[0:vol],Ez[0:vol])
#pragma acc parallel loop collapse(3) present(Hx,Hy,Hz,Ex,Ey,Ez)
            for(int k=H;k<nz-H;k++){
                for(int j=H;j<ny-H;j++){
                    for(int i=H;i<nx-H;i++){
                        size_t id = idx(i,j,k,nx,ny,nz);
                        real dHz_dy = (Hz[idx(i,j,k,nx,ny,nz)] - Hz[idx(i,j-1,k,nx,ny,nz)]) / dy;
                        real dHy_dz = (Hy[idx(i,j,k,nx,ny,nz)] - Hy[idx(i,j,k-1,nx,ny,nz)]) / dz;
                        Ex[id] += ce * (dHz_dy - dHy_dz);

                        real dHx_dz = (Hx[idx(i,j,k,nx,ny,nz)] - Hx[idx(i,j,k-1,nx,ny,nz)]) / dz;
                        real dHz_dx = (Hz[idx(i,j,k,nx,ny,nz)] - Hz[idx(i-1,j,k,nx,ny,nz)]) / dx;
                        Ey[id] += ce * (dHx_dz - dHz_dx);

                        real dHy_dx = (Hy[idx(i,j,k,nx,ny,nz)] - Hy[idx(i-1,j,k,nx,ny,nz)]) / dx;
                        real dHx_dy = (Hx[idx(i,j,k,nx,ny,nz)] - Hx[idx(i,j-1,k,nx,ny,nz)]) / dy;
                        Ez[id] += ce * (dHy_dx - dHx_dy);
                    }
                }
            }
        } else
#endif
        {
#pragma omp parallel for collapse(3) schedule(static)
            for(int k=H;k<nz-H;k++){
                for(int j=H;j<ny-H;j++){
                    for(int i=H;i<nx-H;i++){
                        size_t id = idx(i,j,k,nx,ny,nz);
                        real dHz_dy = (Hz[idx(i,j,k,nx,ny,nz)] - Hz[idx(i,j-1,k,nx,ny,nz)]) / dy;
                        real dHy_dz = (Hy[idx(i,j,k,nx,ny,nz)] - Hy[idx(i,j,k-1,nx,ny,nz)]) / dz;
                        Ex[id] += ce * (dHz_dy - dHy_dz);

                        real dHx_dz = (Hx[idx(i,j,k,nx,ny,nz)] - Hx[idx(i,j,k-1,nx,ny,nz)]) / dz;
                        real dHz_dx = (Hz[idx(i,j,k,nx,ny,nz)] - Hz[idx(i-1,j,k,nx,ny,nz)]) / dx;
                        Ey[id] += ce * (dHx_dz - dHz_dx);

                        real dHy_dx = (Hy[idx(i,j,k,nx,ny,nz)] - Hy[idx(i-1,j,k,nx,ny,nz)]) / dx;
                        real dHx_dy = (Hx[idx(i,j,k,nx,ny,nz)] - Hx[idx(i,j-1,k,nx,ny,nz)]) / dy;
                        Ez[id] += ce * (dHy_dx - dHx_dy);
                    }
                }
            }
        }

        MPI_Waitall(rcount, reqs, MPI_STATUSES_IGNORE);

        if(nbr_px != MPI_PROC_NULL){
            unpack_face_x(Hx, rbuf_x, nx - H, nx,ny,nz);
        }
        if(nbr_mx != MPI_PROC_NULL){
            unpack_face_x(Hx, rbuf_x, 0, nx,ny,nz);
        }
        if(nbr_py != MPI_PROC_NULL){
            unpack_face_y(Hy, rbuf_y, ny - H, nx,ny,nz);
        }
        if(nbr_my != MPI_PROC_NULL){
            unpack_face_y(Hy, rbuf_y, 0, nx,ny,nz);
        }
        if(nbr_pz != MPI_PROC_NULL){
            unpack_face_z(Hz, rbuf_z, nz - H, nx,ny,nz);
        }
        if(nbr_mz != MPI_PROC_NULL){
            unpack_face_z(Hz, rbuf_z, 0, nx,ny,nz);
        }

        if(have_source){
            size_t id = idx(si,sj,sk,nx,ny,nz);
            Ex[id] += 1e-2f * std::sin(2.0f * 3.14159265f * 0.01f * step);
        }

        if(step % std::max(1, opts.steps/5) == 0 && rank==0){
            std::cout << "step " << step << " / " << opts.steps << "\n";
        }
    }

    MPI_Barrier(cart);
    double tend = MPI_Wtime();
    double elapsed = tend - tstart;
    double local_cells = (double)lx * ly * lz;
    double cells_steps = local_cells * opts.steps;
    double perfs = cells_steps / elapsed;
    double global_perf;
    MPI_Reduce(&perfs, &global_perf, 1, MPI_DOUBLE, MPI_SUM, 0, cart);

    if(rank==0){
        std::cout << std::fixed << std::setprecision(3)
                  << "Elapsed (s, max across ranks): " << elapsed << "\n"
                  << "Aggregate cells*steps/sec (sum): " << global_perf << "\n";
    }

    if(opts.verify){
        double local_norm=0.0;
        for(size_t p=0;p<vol;p++) local_norm += (double)Ex[p]*(double)Ex[p];
        double global_norm;
        MPI_Allreduce(&local_norm, &global_norm, 1, MPI_DOUBLE, MPI_SUM, cart);
        global_norm = std::sqrt(global_norm);
        if(rank==0) std::cout << "Global L2 norm Ex: " << global_norm << "\n";
    }

    MPI_Finalize();
    return 0;
}