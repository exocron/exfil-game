-module(keygen).
-export([generate_key/0]).

generate_key() ->
    Chars = <<"39CDEFHJKLMNPRUVWXY">>,
    Rand = [X rem 19 || <<X>> <= crypto:strong_rand_bytes(9)],
    <<<<(binary:at(Chars, X))>> || X <- Rand>>.
