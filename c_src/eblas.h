#ifndef EWB_INCLUDED
#define EWB_INCLUDED
#include "erl_nif.h"
#include <cblas.h>

// Types translator
typedef enum types {e_int, e_lint, e_float, e_double, e_ptr, e_cste_ptr, e_float_complex, e_double_complex, e_end} etypes;
int translate(ErlNifEnv* env, const ERL_NIF_TERM* terms, const etypes* format, ...);


// C binary definition
// --------------------------------------------

typedef enum sizes {s_bytes=4, d_bytes=8, c_bytes=8, z_bytes=16, no_bytes=0} bytes_sizes;

typedef struct{
    unsigned int size;
    unsigned int offset;
    unsigned char* ptr;
} c_binary;

inline void* get_ptr(c_binary cb){return (void*) cb.ptr + cb.offset;}
int get_c_binary(ErlNifEnv* env, const ERL_NIF_TERM term, c_binary* result);
int in_bounds(int elem_size, int n_elem, int inc, c_binary b);

typedef struct{
    unsigned int size;
    unsigned int offset;
    const unsigned char* ptr;
    double tmp;
    etypes type;
} cste_c_binary;

inline const void* get_cste_ptr(cste_c_binary cb){return (void*) cb.ptr + cb.offset;}
int get_cste_binary(ErlNifEnv* env, const ERL_NIF_TERM term, cste_c_binary* result);
int in_cste_bounds(int elem_size, int n_elem, int inc, cste_c_binary b);

void set_cste_c_binary(cste_c_binary* ccb, etypes type, unsigned char* ptr);
ERL_NIF_TERM cste_c_binary_to_term(ErlNifEnv* env, cste_c_binary ccb);


// Private stuff
int debug_write(const char* fmt, ...);
ErlNifResourceType *c_binary_resource;
int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
int upgrade(ErlNifEnv* caller_env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);
int unload(ErlNifEnv* caller_env, void* priv_data);

ERL_NIF_TERM new(ErlNifEnv* env, int argc, const ERL_NIF_TERM* argv);
ERL_NIF_TERM copy(ErlNifEnv* env, int argc, const ERL_NIF_TERM* argv);
ERL_NIF_TERM to_binary(ErlNifEnv* env, int argc, const ERL_NIF_TERM* argv);



// Blas wrapper
// --------------------------------------------

unsigned long hash(char *str);
int load_ebw(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
ERL_NIF_TERM unwrapper(ErlNifEnv* env, int argc, const ERL_NIF_TERM* argv);

typedef enum BLAS_NAMES {
    saxpy=210727551034,
    daxpy=210709762219,
    caxpy=210708576298,
    zaxpy=210735852481,
    scopy=210727613107,
    dcopy=210709824292,
    ccopy=210708638371,
    zcopy=210735914554,
    sswap=210728196307,
    dswap=210710407492,
    cswap=210709221571,
    zswap=210736497754,
    sscal=210728174523,
    dscal=210710385708,
    cscal=210709199787,
    csscal=6953404169886,
    zscal=210736475970,
    zdscal=6954286495110,
    sdot=6385686335,
    ddot=6385147280,
    cdotu=210708674436,
    zdotu=210735950619,
    cdotc=210708674418,
    zdotc=210735950601,
    dsdot=210710387267,
    sdsdot=6954012548918,

    snrm2=210728011511,
    dnrm2=210710222696,
    scnrm2=6954011198426,
    dznrm2=6953451443714,
    sasum=210727545742,
    dasum=210709756927,
    scasum=6954010732657,
    dzasum=6953450977945,
    isamax=6953638346280,
    idamax=6953620557465,
    icamax=6953619371544,
    izamax=6953646647727,

    srot=6385701581,
    drot=6385162526,
    csrot=210709216592,
    zdrot=210735953720,

    srotg=210728152276,
    drotg=210710363461,
    crotg=210709177540,
    zrotg=210736453723,

    //______________

    srotmg=6954029025409,
    drotmg=6953441994514,
    
    srotm=210728152282,
    drotm=210710363467,

    blas_name_end=0
} blas_names;

bytes_sizes pick_size(long hash, blas_names names[], bytes_sizes sizes []);

#endif