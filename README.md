eblas
=====

An OTP library, created to wrap cblas.

Build
-----

    $ rebar3 compile

Usage examples
----
Xs = eblas:new(chain:ltb(s, [1, 2, 3, 4])).
Ys = eblas:new(chain:ltb(s, [0, 0, 0, 0])).
eblas:run({sdot, 3, Xs, 1, Ys, 1}).
eblas:run({sdsdot, 3, 5, Xs, 1, Ys, 1}).
eblas:run({saxpy, 3, 1.0, Xs, 1, Ys, 1}).

f().
Ptr = eblas:new(chain:ltb(s, [2,1,0,0])). 
Args = lists:map(fun(S)->eblas:shift(S*4, Ptr) end, lists:seq(0,3)).
eblas:run(list_to_tuple([srotg] ++ Args)).
chain:btl(s, eblas:to_bin(Ptr)).

Available
----

_axpy 
_copy
_swap
_scal
_dot
_nrm2
_asum
i_amax
_rot

Todo
----
_rotg
_rotm
_rotmg

