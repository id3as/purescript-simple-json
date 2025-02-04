-module(simple_jSON@foreign).
-export(['_parseJSON'/0, '_undefined'/0, stringifyJSON/1, base64Decode/1, base64Encode/1]).

'_parseJSON'() -> fun (S) ->
  jsx:decode(S, [return_maps])
end.

'_undefined'() -> undefined.

stringifyJSON(J) -> jsx:encode(remove_undefined(J)).

remove_undefined(Map) when is_map(Map) ->
  maps:fold(fun(_Key, undefined, Acc) ->
                Acc;
               (Key, Value, Acc) ->
                maps:put(Key, remove_undefined(Value), Acc)
            end,
            #{},
            Map);

remove_undefined(Tuple) when is_tuple(Tuple) ->
  list_to_tuple([remove_undefined(Value) || Value <- tuple_to_list(Tuple)]);

remove_undefined(List) when is_list(List) ->
  [remove_undefined(Item) || Item <- List];

remove_undefined(Value) ->
  Value.

base64Decode(Encoded) when is_binary(Encoded) ->
    try
        {just, base64:decode(Encoded)}
    catch
        _:_:_ ->
            {nothing}
    end;
base64Decode(_) ->
  {nothing}.

base64Encode(Plain) ->
    base64:encode(Plain).
