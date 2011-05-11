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
%% @private
-module(cutkey_server).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).
-export([gen_rsa/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {port, requests=[]}).

%%%===================================================================
%%% API
%%%===================================================================

stop() ->
    gen_server:call(?SERVER, stop).

gen_rsa(Bits, E) when is_integer(Bits), Bits > 0, E band 1 =:= 1 ->
    gen_server:call(?SERVER, {gen_rsa, Bits, E}, infinity).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Port = cutkey_drv:open(),
    {ok, #state{port = Port}}.

handle_call(stop, _From, #state{} = State) ->
    {stop, normal, ok, State};
handle_call({gen_rsa, Bits, E}, From, #state{} = State) ->
    Ref = erlang:phash2(make_ref()),
    case cutkey_drv:gen_rsa(State#state.port, Ref, Bits, E) of
	ok ->
	    NewRequests = [{Ref, From}|State#state.requests],
	    NewState = State#state{requests = NewRequests},
	    {noreply, NewState};
	{error, _ErrNo} = Reply ->
	    {reply, Reply, State}
    end;
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({Port, Ref, Data}, #state{port = Port} = State) ->
    case lists:keytake(Ref, 1, State#state.requests) of
	{value, {Ref, From}, NewRequests} ->
	    gen_server:reply(From, {ok, Data}),
	    NewState = State#state{requests = NewRequests},
	    {noreply, NewState};
	false ->
	    {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{port = Port} = _State) ->
    ok = cutkey_drv:close(Port).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
