-module(simple_jSON@foreign).
-export(['_parseJSON'/0, '_undefined'/0, stringifyJSON/1]).

'_parseJSON'() -> fun (S) ->
    jsx:decode(S, [return_maps])
end.

% This is a lie for sure, but there is no such thing as undefined in JSON, the JS version relies on objects
% containing properties with undefined values to be omitted, while we would have to recurse here before passing
% to JSX to remove them or 
'_undefined'() -> null.

stringifyJSON(J) -> jsx:encode(J).

