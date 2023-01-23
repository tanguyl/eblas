#include "eblas.h"
#include "string.h"
#include <cblas.h>

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
    FILE* fp = fopen("debug.txt", "a");
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

            result->size    = 64;
            result->offset  = 0;
            result->ptr     = (const unsigned char*) &result->tmp;
        }
    }
    return 1;
}


int in_bounds(int elem_size, int n_elem, c_binary b){
    int end_offset = b.offset + (elem_size*n_elem);
    return (elem_size > 0 && end_offset >= 0 && end_offset <= b.size)? 0:20;
}

int in_cste_bounds(int elem_size, int n_elem, cste_c_binary b){
    int end_offset = b.offset + (elem_size*n_elem);
    return (elem_size > 0 && end_offset >= 0 && end_offset <= b.size)?0:20;
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
    unsigned long hash_name = hash(name);
    switch(hash_name){

        case saxpy: case daxpy: case caxpy: case zaxpy: {
            int n; cste_c_binary alpha; cste_c_binary x; int incx; c_binary y; int incy;
            bytes_sizes type = pick_size(hash_name, (blas_names []){saxpy, daxpy, caxpy, zaxpy, blas_name_end}, (bytes_sizes[]){s_bytes, d_bytes, c_bytes, z_bytes, no_bytes});
            
            if( !(error = translate(env, elements+1, (etypes[]) {e_int, e_cste_ptr, e_cste_ptr, e_int, e_ptr, e_int, e_end}, &n, &alpha, &x, &incx, &y, &incy))
                && !(error = (in_cste_bounds(type, 1, alpha) || in_cste_bounds(type, n, x) || in_bounds(type, n, y)))
            )
             switch(hash_name){
                case saxpy: cblas_saxpy(n, *(double*)get_cste_ptr(alpha), get_cste_ptr(x), incx, get_ptr(y), incy); break;
                case daxpy: cblas_daxpy(n, *(double*)get_cste_ptr(alpha), get_cste_ptr(x), incx, get_ptr(y), incy); break;
                case caxpy: cblas_caxpy(n,           get_cste_ptr(alpha), get_cste_ptr(x), incx, get_ptr(y), incy); break;
                case zaxpy: cblas_zaxpy(n,           get_cste_ptr(alpha), get_cste_ptr(x), incx, get_ptr(y), incy); break;
                default: error = -1; break;
            }

        break;}
        

        default:
            debug_write("%s:  %lu,\n", name, hash(name));
            error = -1;
        break;
    }

    debug_write("Error: %i\n", error);
    switch(error){
        case 0:         
            return enif_make_atom(env, "ok");
        break;
        case 1 ... 19:
            char buff[50];
            sprintf(buff, "Could not translate argument %i.", error - 1);
            return enif_raise_exception(env, enif_make_atom(env, buff));
        break;
        case 20:
            return enif_raise_exception(env, enif_make_atom(env, "Array overflow."));
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