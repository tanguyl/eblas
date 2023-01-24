eblas
=====

An OTP library, created to wrap cblas.

Build
-----

    $ rebar3 compile

Usage
----
Xs = eblas:new(chain:ltb(s, [1,2,1,3,1,4])).
Ys = eblas:new(chain:ltb(s, [1,2,3,4,5,6])).
eblas:run({sdot, 3, Xs, 1, Ys, 1}).
eblas:run({sdsdot, 3, 5, Xs, 1, Ys, 1}).

eblas:run({saxpy, 3, 1.0, Xs, 1, Ys, 1}).

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

Todo
----
_rot
_rotg
_rotm
_rotmg

