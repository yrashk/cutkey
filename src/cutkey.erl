%% -------------------------------------------------------------------
%%
%% Copyright (c) 2011 Andrew Tunnell-Jones. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(cutkey).
-export([start/0, stop/0, rsa/2, rsa/3]).

-include_lib("public_key/include/public_key.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(APP_NAME, ?MODULE).

start() ->
    application:start(?APP_NAME).

stop() ->
    application:stop(?APP_NAME).

rsa(Bits, E) when is_integer(Bits), Bits > 0, E band 1 =:= 1 ->
    rsa(Bits, E, [{return, bare}, {erlint, false}]).

rsa(Bits, E, Opts) when is_integer(Bits), Bits > 0, E band 1 =:= 1 ->
    Return = proplists:get_value(return, Opts, bare),
    ErlInt = proplists:get_bool(erlint, Opts),
    case lists:member(Return, [bare, full, key]) of
	true ->
	    case cutkey_server:gen_rsa(Bits, E) of
		{ok, MPInts} = FullResult when is_list(MPInts) ->
		    case Return of
			bare when ErlInt ->
			    {ok, erlint(lists:sublist(MPInts, 3))};
			bare -> {ok, lists:sublist(MPInts, 3)};
			full when ErlInt -> {ok, erlint(MPInts)};
			full -> FullResult;
			key ->
			    [E, N, D, P, Q, DMP1, DMQ1, IQMP] = erlint(MPInts),
			    Key = #'RSAPrivateKey'{version = 'two-prime',
						   modulus = N,
						   publicExponent = E,
						   privateExponent = D,
						   prime1 = P,
						   prime2 = Q,
						   exponent1 = DMP1,
						   exponent2 = DMQ1,
						   coefficient = IQMP},
			    {ok, Key}
		    end;
		Other -> Other
	    end;
	false -> erlang:error(badarg)
    end.

erlint(MPInts) -> [ crypto:erlint(X) || X <- MPInts ].

-ifdef(TEST).

erlint_test() -> [1] = erlint([<<1:32,1>>]).

bare_test_() ->
    {setup,
     fun() -> start() end,
     fun(_) -> stop() end,
     [fun() ->
	      {ok, MPInts} = cutkey:rsa(512, 65537, [{return, bare}]),
	      3 = length(MPInts),
	      Fun = fun(X) -> is_integer(crypto:erlint(X)) end,
	      true = lists:all(Fun, MPInts)
      end,
      fun() ->
	      {ok, ErlInts} = cutkey:rsa(512, 65537, [{return, bare},erlint]),
	      3 = length(ErlInts),
	      true = lists:all(fun erlang:is_integer/1, ErlInts)
      end]}.

full_test_() ->
    {setup,
     fun() -> start() end,
     fun(_) -> stop() end,
     [fun() ->
	      {ok, MPInts} = cutkey:rsa(512, 65537, [{return, full}]),
	      8 = length(MPInts),
	      Fun = fun(X) -> is_integer(crypto:erlint(X)) end,
	      true = lists:all(Fun, MPInts)
      end,
      fun() ->
	      {ok, ErlInts} = cutkey:rsa(512, 65537, [{return, full},erlint]),
	      8 = length(ErlInts),
	      true = lists:all(fun erlang:is_integer/1, ErlInts)
      end]}.

entry_test_() ->
    {setup,
     fun() -> start() end,
     fun(_) -> stop() end,
     fun() ->
	     {ok, Entry} = cutkey:rsa(512, 65537, [{return, key}]),
	     Text = <<?MODULE_STRING>>,
	     Enc0 = public_key:encrypt_public(Text, Entry),
	     Text = public_key:decrypt_private(Enc0, Entry),
	     Enc1 = public_key:encrypt_private(Text, Entry),
	     Text = public_key:decrypt_public(Enc1, Entry)
     end}.

-endif.
