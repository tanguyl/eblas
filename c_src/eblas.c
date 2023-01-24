#include "eblas.h"
#include "string.h"
#include <cblas.h>
#include <complex.h>
#include "string.h"

//Various utility functions
//--------------------------------

int translate(ErlNifEnv* env, const ERL_NIF_TERM* terms, const etypes* format, ...){
    va_list valist;
    va_start(valist, format);
    int valid = 1;

    for(int curr=0; format[curr] != e_end; curr++){
        debug_write("Unwrapping term number %i\n", curr);
        switch(format[curr]){
            case e_int:
                valid = enif_get_int(env, terms[curr], va_arg(valist, int*));
            break;
            case e_lint:
                valid = enif_get_int64(env, terms[curr], va_arg(valist, ErlNifSInt64*));
            break;
            
            case e_float:
                double val;
                float* dest = va_arg(valist, float*);
                valid = enif_get_double(env, terms[curr], &val);
                if(valid)
                    *dest = (float) val;    
            break;
            case e_double:
                valid = enif_get_double(env, terms[curr], va_arg(valist, double*));
            break;

            case e_ptr:
                valid = get_c_binary(env, terms[curr], va_arg(valist, c_binary*));
            break;
            case e_cste_ptr:
                valid = get_cste_binary(env, terms[curr], va_arg(valist, cste_c_binary*));
            break;

            default:
                valid = 0;
            break;
        }

        if(!valid){
            va_end(valist);
            return curr + 1;
        }
    }
    
    va_end(valist);
    return 0;
}

// C_binary definitions/functions

//Used for debug purpose.
//Likely thread unsafe.
//Usage: debug_write("A double: %lf, an int:%d", double_val, int_val);
int debug_write(const char* fmt, ...){
    FILE* fp = fopen("priv/debug.txt", "a");
    va_list args;

    va_start(args, fmt);
    vfprintf(fp, fmt, args);
    va_end(args);

    fclose(fp);
    return 1;
}

int get_c_binary(ErlNifEnv* env, const ERL_NIF_TERM term, c_binary* result){
    int arity;
    const ERL_NIF_TERM* terms;
    void* resource;

    // Return false if: incorect record size, resource size does not match
    int b =  enif_get_tuple(env, term, &arity, &terms)
                && arity == 4
                && enif_get_uint(env, terms[1], &result->size)
                && enif_get_uint(env, terms[2], &result->offset)
                && enif_get_resource(env, terms[3], c_binary_resource, &resource)
                && result->size == enif_sizeof_resource(resource);

    result->ptr = (unsigned char*) resource;
    
    return b;
}

// *UnIVeRSAl pointer to an erlang type; currently, binary/double/c_binarry.
int get_cste_binary(ErlNifEnv* env, const ERL_NIF_TERM term, cste_c_binary* result){
    if(enif_is_binary(env, term)){
        // Read a binary.
        ErlNifBinary ebin;
        if(!enif_inspect_binary(env, term, &ebin))
            return 0;
        
        result->size    = ebin.size;
        result->offset  = 0;
        result->ptr     = ebin.data;
    }
    else{
        // Read a cbin.
        c_binary cbin;
        if(get_c_binary(env, term, &cbin)){
            result->size    = cbin.size;
            result->offset  = cbin.offset;
            result->ptr     = (const unsigned char*) cbin.ptr;
        }
        else{
            // Read a double.
            if(!enif_get_double(env, term, &result->tmp))
                return 0;

            result->size    = 8;
            result->offset  = 0;
            result->ptr     = (const unsigned char*) &result->tmp;
        }
    }
    return 1;
}


int in_bounds(int elem_size, int n_elem, int inc, c_binary b){
    int end_offset = b.offset + (elem_size*n_elem*inc);
    debug_write("end max offset: %i  offset: %u\n", end_offset, b.size);
    return (elem_size > 0 && end_offset >= 0 && end_offset <= b.size)? 0:20;
}

int in_cste_bounds(int elem_size, int n_elem, int inc, cste_c_binary b){
    int end_offset = b.offset + (elem_size*n_elem*inc);
    debug_write("end max offset: %i  offset: %u\n", end_offset, b.size);
    return (elem_size > 0 && end_offset >= 0 && end_offset <= b.size)?0:20;
}

void set_cste_c_binary(cste_c_binary *ccb, bytes_sizes type, unsigned char* ptr){
    ccb->size   = type;
    ccb->offset = 0;
    ccb->ptr    = ptr;
}

ERL_NIF_TERM cste_c_binary_to_term(ErlNifEnv* env, cste_c_binary ccb){
    ErlNifBinary bin;
    ERL_NIF_TERM result;

    debug_write("Creating binarry...\n");
    if(enif_alloc_binary(ccb.size, &bin)){
        memcpy(bin.data, ccb.ptr, ccb.size);
        debug_write("FInished copying!\n");
        if((result = enif_make_binary(env, &bin)))
            return result;
        else
            enif_release_binary(&bin);
    }

    return enif_make_badarg(env);
}



ERL_NIF_TERM new(ErlNifEnv* env, int argc, const ERL_NIF_TERM* argv){
    int size = 0;
    if(!enif_get_int(env, argv[0], &size)) return enif_make_badarg(env);

    void* ptr = enif_alloc_resource(c_binary_resource, size);
    ERL_NIF_TERM resource = enif_make_resource(env, ptr);
    enif_release_resource(ptr);
    return resource; 
}

ERL_NIF_TERM copy(ErlNifEnv* env, int argc, const ERL_NIF_TERM* argv){
    ErlNifBinary bin;
    c_binary cbin;

    if(!enif_inspect_binary(env, argv[0], &bin)|| !get_c_binary(env, argv[1], &cbin))
         return enif_make_badarg(env);

    memcpy(cbin.ptr + cbin.offset, bin.data, bin.size);

    return enif_make_atom(env, "ok");
}

ERL_NIF_TERM to_binary(ErlNifEnv* env, int argc, const ERL_NIF_TERM* argv){
    c_binary cbin;
    ErlNifBinary bin;
    unsigned size;

    if(!enif_get_uint(env, argv[0], &size)
        || !get_c_binary(env, argv[1], &cbin)
        || !enif_alloc_binary(size, &bin))
        return enif_make_badarg(env);

    memcpy(bin.data, cbin.ptr + cbin.offset, size);

    return enif_make_binary(env, &bin);
}

// UNWRAPPER

// https://stackoverflow.com/questions/7666509/hash-function-for-string
unsigned long hash(char *str){
    unsigned long hash = 5381;
    int c;

    while ((c = *str++))
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

    return hash;
}


bytes_sizes pick_size(long hash, blas_names names[], bytes_sizes sizes []){
    for(int curr=0; names[curr] != blas_name_end; curr++)
        if(names[curr]==hash)
            return sizes[curr];
    
    return 0;
}


ERL_NIF_TERM unwrapper(ErlNifEnv* env, int argc, const ERL_NIF_TERM* argv){
    int narg;
    const ERL_NIF_TERM* elements;
    char name[20];

    if(!enif_get_tuple(env, *argv, &narg, &elements)
        || !enif_get_atom(env, elements[0], name, 20, ERL_NIF_LATIN1)
    ){
        return enif_make_badarg(env);
    }

    
    int error;
    narg--;
    elements++;
    unsigned long hash_name = hash(name);
    ERL_NIF_TERM result = 0;
    //debug_write("%s=%lu\n", name, hash_name);
    switch(hash_name){

        case saxpy: case daxpy: case caxpy: case zaxpy: {
            int n; cste_c_binary alpha; cste_c_binary x; int incx; c_binary y; int incy;
            bytes_sizes type = pick_size(hash_name, (blas_names []){saxpy, daxpy, caxpy, zaxpy, blas_name_end}, (bytes_sizes[]){s_bytes, d_bytes, c_bytes, z_bytes, no_bytes});
            
            if( !(error = narg == 6? 0:21)
                && !(error = translate(env, elements, (etypes[]) {e_int, e_cste_ptr, e_cste_ptr, e_int, e_ptr, e_int, e_end}, &n, &alpha, &x, &incx, &y, &incy))
                && !(error = in_cste_bounds(type, 1, 1, alpha)) && !(error = in_cste_bounds(type, n, incx, x))  && !(error = in_bounds(type, n, incy, y))
            )
             switch(hash_name){
                case saxpy: cblas_saxpy(n, *(double*)get_cste_ptr(alpha), get_cste_ptr(x), incx, get_ptr(y), incy); break;
                case daxpy: cblas_daxpy(n, *(double*)get_cste_ptr(alpha), get_cste_ptr(x), incx, get_ptr(y), incy); break;
                case caxpy: cblas_caxpy(n,           get_cste_ptr(alpha), get_cste_ptr(x), incx, get_ptr(y), incy); break;
                case zaxpy: cblas_zaxpy(n,           get_cste_ptr(alpha), get_cste_ptr(x), incx, get_ptr(y), incy); break;
                default: error = -2; break;
            }

        break;}

        case scopy: case dcopy: case ccopy: case zcopy:  {
            int n;  cste_c_binary x; int incx; c_binary y; int incy;
            bytes_sizes type = pick_size(hash_name, (blas_names []){scopy, dcopy, ccopy, zcopy,  blas_name_end}, (bytes_sizes[]){s_bytes, d_bytes, c_bytes, z_bytes, no_bytes});
            
            if( !(error = narg == 5? 0:21)
                && !(error = translate(env, elements, (etypes[]) {e_int, e_cste_ptr, e_int, e_ptr, e_int, e_end}, &n, &x, &incx, &y, &incy))
                && !(error = in_cste_bounds(type, n, incx, x)) && !(error = in_bounds(type, n, incy, y))
            )
            switch(hash_name){
                case scopy: cblas_scopy(n, get_cste_ptr(x), incx, get_ptr(y), incy); break;
                case dcopy: cblas_dcopy(n, get_cste_ptr(x), incx, get_ptr(y), incy); break;
                case ccopy: cblas_ccopy(n, get_cste_ptr(x), incx, get_ptr(y), incy); break;
                case zcopy: cblas_zcopy(n, get_cste_ptr(x), incx, get_ptr(y), incy); break;
                default: error = -2; break;
            }

        break;}

        case sswap: case dswap: case cswap: case zswap:  {
            int n;  c_binary x; int incx; c_binary y; int incy;
            bytes_sizes type = pick_size(hash_name, (blas_names []){sswap, dswap, cswap, zswap, blas_name_end}, (bytes_sizes[]){s_bytes, d_bytes, c_bytes, z_bytes, no_bytes});
            
            if( !(error = narg == 5? 0:21)
                && !(error = translate(env, elements, (etypes[]) {e_int, e_ptr, e_int, e_ptr, e_int, e_end}, &n, &x, &incx, &y, &incy))
                && !(error = in_bounds(type, n, incx, x)) && !(error = in_bounds(type, n, incy, y))
            )
            switch(hash_name){
                case sswap: cblas_sswap(n, get_ptr(x), incx, get_ptr(y), incy); break;
                case dswap: cblas_dswap(n, get_ptr(x), incx, get_ptr(y), incy); break;
                case cswap: cblas_cswap(n, get_ptr(x), incx, get_ptr(y), incy); break;
                case zswap: cblas_zswap(n, get_ptr(x), incx, get_ptr(y), incy); break;
                default: error = -2; break;
            }
            
        break;}

        case sscal: case dscal: case cscal: case zscal: case csscal: case zdscal:  {
            int n;  cste_c_binary alpha; c_binary x; int incx;
            bytes_sizes type = pick_size(hash_name, (blas_names []){sscal, dscal, cscal, zscal,  csscal, zdscal, blas_name_end}, (bytes_sizes[]){s_bytes, d_bytes, c_bytes, z_bytes, c_bytes, z_bytes, no_bytes});
            
            if( !(error = narg == 4? 0:21)
                && !(error = translate(env, elements, (etypes[]) {e_int, e_cste_ptr, e_ptr, e_int, e_end}, &n, &alpha, &x, &incx))
                && !(error = in_cste_bounds(type, 1, 1, alpha) ) && !(error = in_bounds(type, n, incx, x))
            )
            switch(hash_name){
                case sscal:  cblas_sscal(n, *(double*) get_cste_ptr(alpha), get_ptr(x), incx); break;
                case dscal:  cblas_dscal(n, *(double*) get_cste_ptr(alpha), get_ptr(x), incx); break;
                case cscal:  cblas_cscal(n,            get_cste_ptr(alpha), get_ptr(x), incx); break;
                case zscal:  cblas_zscal(n,            get_cste_ptr(alpha), get_ptr(x), incx); break;
                case csscal: cblas_sscal(n, *(double*) get_cste_ptr(alpha), get_ptr(x), incx); break;
                case zdscal: cblas_dscal(n, *(double*) get_cste_ptr(alpha), get_ptr(x), incx); break;
                default: error = -2; break;
            }
            
        break;}

        case sdot: case ddot: case dsdot: case cdotu: case zdotu: case cdotc: case zdotc: {
            cste_c_binary dot_result;

            int n;  cste_c_binary x; int incx; cste_c_binary y; int incy;
            bytes_sizes type = pick_size(hash_name, (blas_names []){sdot, ddot, dsdot, cdotu, zdotu, cdotc, zdotc, blas_name_end}, (bytes_sizes[]){s_bytes, d_bytes, s_bytes, c_bytes, z_bytes, c_bytes, z_bytes, no_bytes});
            
            if( !(error = narg == 5? 0:21)
                && !(error = translate(env, elements, (etypes[]) {e_int, e_cste_ptr, e_int, e_cste_ptr, e_int, e_end}, &n, &x, &incx, &y, &incy))
                && !(error = in_cste_bounds(type, n, incx, x) ) && !(error = in_cste_bounds(type, n, incy, y))
            )
            switch(hash_name){
                case sdot:                   float  f_result  = cblas_sdot (n, get_cste_ptr(x), incx, get_cste_ptr(y), incy); set_cste_c_binary(&dot_result, type, (unsigned char*) &f_result);  break;
                case ddot:                   double d_result  = cblas_ddot (n, get_cste_ptr(x), incx, get_cste_ptr(y), incy); set_cste_c_binary(&dot_result, type, (unsigned char*) &d_result);  break;
                case dsdot:                  double ds_result = cblas_dsdot(n, get_cste_ptr(x), incx, get_cste_ptr(y), incy); set_cste_c_binary(&dot_result, type, (unsigned char*) &ds_result); break;
                case cdotu: openblas_complex_float  c_result  = cblas_cdotu(n, get_cste_ptr(x), incx, get_cste_ptr(y), incy); set_cste_c_binary(&dot_result, type, (unsigned char*) &c_result);  break;
                case zdotu: openblas_complex_double z_result  = cblas_zdotu(n, get_cste_ptr(x), incx, get_cste_ptr(y), incy); set_cste_c_binary(&dot_result, type, (unsigned char*) &z_result);  break;
                case cdotc: openblas_complex_float  cd_result = cblas_cdotc(n, get_cste_ptr(x), incx, get_cste_ptr(y), incy); set_cste_c_binary(&dot_result, type, (unsigned char*) &cd_result); break;
                case zdotc: openblas_complex_double zd_result = cblas_zdotc(n, get_cste_ptr(x), incx, get_cste_ptr(y), incy); set_cste_c_binary(&dot_result, type, (unsigned char*) &zd_result); break;
                default: error = -2; break;
            }

            result = cste_c_binary_to_term(env, dot_result);
            
        break;}

        case sdsdot: {
            cste_c_binary dot_result;

            int n;  cste_c_binary b; cste_c_binary x; int incx; cste_c_binary y; int incy;
            bytes_sizes type = s_bytes;
            
            if( !(error = narg == 6? 0:21)
                && !(error = translate(env, elements, (etypes[]) {e_int, e_cste_ptr, e_cste_ptr, e_int, e_cste_ptr, e_int, e_end}, &n, &b, &x, &incx, &y, &incy))
                && !(error = in_cste_bounds(type, n, incx, x) ) && !(error = in_cste_bounds(type, n, incy, y))
            ){
                float f_result  = cblas_sdsdot (n, *(double*) get_cste_ptr(b), get_cste_ptr(x), incx, get_cste_ptr(y), incy); set_cste_c_binary(&dot_result, type, (unsigned char*) &f_result);
                result = cste_c_binary_to_term(env, dot_result);
            }
        break;}


        default:
            error = -1;
        break;
    }

    switch(error){
        case -1:
            debug_write("%s=%lu,\n", name, hash_name);
            return enif_raise_exception(env, enif_make_atom(env, "Unknown blas."));
        case 0:
            return !result? enif_make_atom(env, "ok"): result;
        break;
        case 1 ... 19:
            char buff[50];
            sprintf(buff, "Could not translate argument %i.", error - 1);
            return enif_raise_exception(env, enif_make_atom(env, buff));
        break;
        case 20:
            return enif_raise_exception(env, enif_make_atom(env, "Array overflow."));
        break;
        case 21:
            return enif_raise_exception(env, enif_make_atom(env, "Invalid number of arguments."));
        break;

        default:
            return enif_make_badarg(env);
        break;
    }
}

int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info){
    c_binary_resource = enif_open_resource_type(env, "c_binary", "c_binary_resource", NULL, ERL_NIF_RT_CREATE, NULL);
    debug_write("\nNew session\n-----------\n");
    return 0;
}

ErlNifFunc nif_funcs[] = { 
    {"new_nif", 1, new},
    {"copy_nif", 2, copy},
    {"bin_nif", 2, to_binary},

    {"dirty_unwrapper", 1, unwrapper},
    {"clean_unwrapper", 1, unwrapper}
};


ERL_NIF_INIT(eblas, nif_funcs, load, NULL, NULL, NULL)