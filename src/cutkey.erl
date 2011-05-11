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
-export([start/0, stop/0, rsa/2]).

-define(APP_NAME, ?MODULE).

start() ->
    application:start(?APP_NAME).

stop() ->
    application:stop(?APP_NAME).

rsa(Bits, E) when is_integer(Bits), Bits > 0, E band 1 =:= 1 ->
    cutkey_server:gen_rsa(Bits, E).
