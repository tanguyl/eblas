-module(chain_test).
-include_lib("eunit/include/eunit.hrl").

daxpy_test()->
    X = c_binary:new(chain:ltb(d,[1,2,3,4,5])),
    Y = c_binary:new(chain:ltb(d,[2,1,2,1,2])),
    ewb:run({daxpy, 5, 1, X, 1, Y, 1}),
    chain:ltb(d, [2,2,6,4,10]) =:= c_binary:to_bin(Y).