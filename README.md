eblas
=====

This early draft consists is a CBLAS wrapper. It features scheduling control (execution in dirty/clean nifs), as well as type checking, and array overflow detection (to avoid sigsev related crashes).


Usage
-----

sasum example:

```erlang
Bin = chain:ltb(s, [1,2,4,3]),  
% ltb(T, L):
% transforms list L into a binary,
% using encoding T: s->single, d->double,
%                   c->single complex
%                   z->single double

Ptr = eblas:new(Bin),           
% new(Binary):
% creates a mutable c binary, of initial
% content Bin.

Sum = eblas:run({sasum, 4, Ptr, 1 }).
% run({Blas_fct_name, arg0, arg1,...}).
% Executes blas Blas_fct_name with given arguments.
```

caxpy example:

```erlang
% List representation of X,Y,A
[X,Y,A]   =  [[1,2,1,3,1,4],[3,2,3,4,3,6],[1,0]],
% Binary representation of X,Y,A
[Xb,Yb,Ab] = lists:map(fun(L)->chain:ltb(c, L) end, [X,Y,A]),
% C_Binary representation of X,Y,A
[Xc,Yc,Ac] = lists:map(fun eblas:new/1, [Xb,Yb,Ab]),

ok    = eblas:run({caxpy, 3, Ac, Xc, 1, Yc, 1}),

% Reset Yc
eblas:copy(Yb, Yc),
ok    = eblas:run({caxpy, 3, Ab, Xb, 1, Yc, 1}).
```

Available
----

_axpy _copy _swap _scal _dot _nrm2 _asum i_amax _rot _rotg _rotm _rotmg