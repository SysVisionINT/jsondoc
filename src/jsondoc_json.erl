%%
%% Copyright 2015-16 Joaquim Rocha <jrocha@gmailbox.org>
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%

-module(jsondoc_json).

-include("jsondoc.hrl").

-define(IS_SPACE(C), (C =:= $\s orelse C =:= $\t orelse C =:= $\r orelse C =:= $\n)).
-define(IS_NUMBER_TERMINATOR(C), (C =:= $} orelse C =:= $, orelse C =:= $])).

%% ====================================================================
%% API functions
%% ====================================================================
-export([decode/1]).
-export([encode/1]).

decode(JSon) when is_binary(JSon) -> 
	case scan(JSon) of
		nil -> erlang:error(no_json);
		{Other, Rest} -> 
			case scan(Rest) of
				nil -> Other;
				_ -> erlang:error(invalid_json)
			end
	end;
decode(JSon) when is_list(JSon) ->
	decode(list_to_binary(JSon)).

encode(Term) -> 
	JSon = encode_term(Term, []),
	iolist_to_binary(lists:reverse(JSon)).

%% ====================================================================
%% Decode
%% ====================================================================

scan(<<${, T/binary>>) -> 
	parse_object(T, {[]});
scan(<<$[, T/binary>>) -> 
	parse_array(T, []);
scan(<<C, T/binary>>) when ?IS_SPACE(C) -> 
	scan(T);
scan(<<Char, T/binary>>) -> 
	parse_value(T, Char);
scan(<<>>) -> nil.

parse_object(<<>>, _Obj) ->
	erlang:error(object_not_closed);
parse_object(<<$}, T/binary>>, Obj) -> {reverse_fields(Obj), T};
parse_object(<<",", T/binary>>, Obj) -> 
	parse_object(T, Obj);
parse_object(<<C, T/binary>>, Obj) when ?IS_SPACE(C) -> 
	parse_object(T, Obj);
parse_object(T, Obj) ->
	case search_for(T, $") of
		false -> erlang:error(invalid_object);
		{true, Rest} -> 
			{Key, Rest1} = parse_string(Rest, []),
			case search_for(Rest1, $:) of
				false -> erlang:error(invalid_object);
				{true, Rest2} -> 
					case scan(Rest2) of
						nil -> erlang:error(object_not_closed);
						{Value, Rest3} -> 
							parse_object(Rest3, add_field_to_object(Obj, Key, Value))
					end
			end
	end.

reverse_fields({List}) -> {lists:reverse(List)}.

add_field_to_object({List}, Key, Value) -> {[{Key, Value}|List]}.

search_for(<<C, T/binary>>, C) -> {true, T};
search_for(<<C, T/binary>>, S) when ?IS_SPACE(C) ->
	search_for(T, S);
search_for(_, _) -> false.

parse_array(<<>>, _Acc) ->
	erlang:error(array_not_closed);
parse_array(<<$], T/binary>>, Acc) -> {lists:reverse(Acc), T};
parse_array(<<",", T/binary>>, Acc) -> 
	parse_array(T, Acc);
parse_array(<<C, T/binary>>, Acc) when ?IS_SPACE(C) -> 
	parse_array(T, Acc);
parse_array(T, Acc) ->
	case scan(T) of
		nil -> erlang:error(array_not_closed);
		{Other, Rest} -> parse_array(Rest, [Other|Acc])
	end.

parse_value(<<"rue", T/binary>>, $t) -> {true, T};
parse_value(<<"alse", T/binary>>, $f) -> {false, T};
parse_value(<<"ull", T/binary>>, $n) -> {null, T};
parse_value(Value, $") -> parse_string(Value, []);
parse_value(Value, Char) -> parse_number(Value, [Char]).

parse_string(<<$", T/binary>>, Acc) -> 
	{iolist_to_binary(lists:reverse(Acc)), T};
parse_string(<<"\\\"", T/binary>>, Acc) -> 
	parse_string(T, [$"|Acc]);
parse_string(<<"\\\\", T/binary>>, Acc) -> 
	parse_string(T, [$\\|Acc]);
parse_string(<<"\\b", T/binary>>, Acc) -> 
	parse_string(T, [8|Acc]);
parse_string(<<"\\t", T/binary>>, Acc) -> 
	parse_string(T, [9|Acc]);
parse_string(<<"\\n", T/binary>>, Acc) -> 
	parse_string(T, [10|Acc]);
parse_string(<<"\\f", T/binary>>, Acc) -> 
	parse_string(T, [12|Acc]);
parse_string(<<"\\r", T/binary>>, Acc) -> 
	parse_string(T, [13|Acc]);
parse_string(<<"\\u", E3, E2, E1, E0, T/binary>>, Acc) ->
	C = erlang:list_to_integer([E3, E2, E1, E0], 16),
	{Char, Rest} = if C >= 16#D800 andalso C < 16#DC00 ->
			case T of
				<<"\\u", D3, D2, D1, D0, R/binary>> ->
					D = erlang:list_to_integer([D3, D2, D1, D0], 16),
					X = (C - 16#d800) * 16#400 + (D - 16#dc00) + 16#10000,
					{X, R};
				_ -> 
					erlang:error(invalid_string)
			end;
		true ->
			{C, T}
	end,
	parse_string(Rest, [<<Char/utf8>>|Acc]);
parse_string(<<Char, T/binary>>, Acc) -> 
	parse_string(T, [Char|Acc]);
parse_string(<<>>, _Acc) -> 
	erlang:error(string_not_closed).

parse_number(<<>>, Acc) -> 
	{list_to_number(lists:reverse(Acc)), <<>>};
parse_number(<<C, T/binary>>, Acc) when ?IS_SPACE(C) -> 
	{list_to_number(lists:reverse(Acc)), T};
parse_number(T = <<C, _/binary>>, Acc) when ?IS_NUMBER_TERMINATOR(C) -> 
	{list_to_number(lists:reverse(Acc)), T};
parse_number(<<$+, T/binary>>, Acc) ->
	parse_number(T, [$+|Acc]);
parse_number(<<$-, T/binary>>, Acc) ->
	parse_number(T, [$-|Acc]);
parse_number(<<$e, T/binary>>, Acc) ->
	parse_number(T, [$e|Acc]);
parse_number(<<$E, T/binary>>, Acc) ->
	parse_number(T, [$e|Acc]);
parse_number(<<".", T/binary>>, Acc) ->
	parse_number(T, [$.|Acc]);
parse_number(<<C, T/binary>>, Acc) when C >= $0 andalso C =< $9 ->
	parse_number(T, [C|Acc]);
parse_number(_, _) ->
	erlang:error(invalid_char_in_number).

list_to_number(Value) ->
	case try_integer(Value) of
		error ->
			case try_float(Value) of
				error -> erlang:error(invalid_number);
				Float -> Float
			end;
		Int -> Int
	end.

try_integer(Value) ->
	case string:to_integer(Value) of
		{Int, []} -> Int;
		_ -> error
	end.

try_float(Value) ->
	case string:to_float(Value) of
		{Float, []} -> Float;
		_ -> error
	end.

%% ====================================================================
%% Encode
%% ====================================================================

encode_term({Term}, Acc) when is_list(Term) -> 
	encode_object(Term, true, [${|Acc]);
encode_term(Term = [{Name, _}|_], Acc) when is_binary(Name) orelse is_atom(Name) -> 
	encode_object(Term, true, [${|Acc]);
encode_term(Term, Acc) when is_list(Term) -> 
	encode_array(Term, true, [$[|Acc]);
encode_term(Term, Acc) when ?IS_MAP(Term) -> 
	encode_object(maps:to_list(Term), true, [${|Acc]);
encode_term(Term, Acc) when is_binary(Term) ->
	encode_string(Term, Acc);
encode_term(true, Acc) ->
	[<<"true">>|Acc];
encode_term(false, Acc) ->
	[<<"false">>|Acc];
encode_term(null, Acc) ->
	[<<"null">>|Acc];
encode_term(Term, Acc) when is_atom(Term) ->
	encode_string(Term, Acc);
encode_term(Term, Acc) when is_integer(Term) ->
	[integer_to_binary(Term)|Acc];
encode_term(Term, Acc) when is_float(Term) ->
	[encode_float(Term)|Acc];
encode_term({Term, float_decimals, Decimals}, Acc) when is_float(Term) ->
	[float_to_binary(Term, [{decimals, Decimals}])|Acc];
encode_term(_, _) ->
	erlang:error(not_valid_ejson).

encode_object([{Key, Value}|T], First, Acc) ->
	Acc1 = field_separator(First, Acc),
	Acc2 = encode_string(Key, Acc1),
	Acc3 = encode_term(Value, [$:|Acc2]),
	encode_object(T, false, Acc3);
encode_object([], _First, Acc) -> [$}|Acc].

field_separator(false, Acc) -> [$,|Acc];
field_separator(_, Acc) -> Acc.

encode_array([Value|T], First, Acc) ->
	Acc1 = item_separator(First, Acc),
	Acc2 = encode_term(Value, Acc1),
	encode_array(T, false, Acc2);
encode_array([], _First, Acc) -> [$]|Acc].

item_separator(false, Acc) -> [$,|Acc];
item_separator(_, Acc) -> Acc.

encode_float(0.0) ->
	<<"0.0">>;
encode_float(Value) when Value < 0.0000000001 andalso Value > -0.0000000001 ->
	[float_to_binary(Value)];
encode_float(Value) ->
	float_to_binary(Value, [{decimals, 10}, compact]).

encode_string(Value, Acc) when is_binary(Value) ->
	Acc1 = safe_string(Value, [$"|Acc]),
	[$"|Acc1];
encode_string(Term, Acc) when is_atom(Term) ->
	encode_string(atom_to_binary(Term, utf8), Acc);
encode_string(_, _) -> 
	erlang:error(invalid_string).

safe_string(<<"\"", T/binary>>, Acc) -> 
	safe_string(T, [<<"\\\"">>|Acc]);
safe_string(<<"\\", T/binary>>, Acc) -> 
	safe_string(T, [<<"\\\\">>|Acc]);
safe_string(<<X, T/binary>>, Acc) when X >= 32 andalso X =< 127 ->
	safe_string(T, [X|Acc]);	
safe_string(<<X/utf8, T/binary>>, Acc) -> 
	C = escape_char(X),
	safe_string(T, [C|Acc]);
safe_string(<<>>, Acc) -> Acc.

escape_char(8) -> <<"\\b">>;
escape_char(9) -> <<"\\t">>;
escape_char(10) -> <<"\\n">>;
escape_char(12) -> <<"\\f">>;
escape_char(13) -> <<"\\r">>;
escape_char(X) -> uescape(X).

uescape(C) when C < 65536 ->
	<<D3:4, D2:4, D1:4, D0:4>> = <<C:16>>,
	E3 = hex(D3),
	E2 = hex(D2),
	E1 = hex(D1),
	E0 = hex(D0),
	<<"\\u", E3, E2, E1, E0>>;
uescape(C) ->
	X = C - 16#10000,
	<<A:10, B:10>> = <<X:20>>,
	U1 = uescape(A + 16#d800),
	U2 = uescape(B + 16#dc00),
	<<U1/binary, U2/binary>>.

hex(0) -> $0;
hex(1) -> $1;
hex(2) -> $2;
hex(3) -> $3;
hex(4) -> $4;
hex(5) -> $5;
hex(6) -> $6;
hex(7) -> $7;
hex(8) -> $8;
hex(9) -> $9;
hex(10) -> $a;
hex(11) -> $b;
hex(12) -> $c;
hex(13) -> $d;
hex(14) -> $e;
hex(15) -> $f.
