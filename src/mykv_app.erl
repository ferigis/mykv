%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015 Felipe Ripoll, Inc. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% @author Felipe Ripoll <ferigis@gmail.com>
%%% @copyright (C) 2015, <Felipe Ripoll>, All Rights Reserved.
%% @doc mykv app.
%% @end
%%%-------------------------------------------------------------------
-module(mykv_app).

-behaviour(application).

%% Application callbacks
-export([start/0]).
-export([start/2]).
-export([stop/1]).
-export([start_phase/3]).

%%====================================================================
%% API
%%====================================================================

-spec start() -> {ok, _} | {error, term()}.
start() ->
  application:ensure_all_started(mykv).

start(_StartType, _StartArgs) ->
  mykv_sup:start_link().

start_phase(start_datastore, _StartType, []) ->
  ok = start_store(),
  ok.

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private
start_store() ->
  Nodes = [node()],
  application:stop(mnesia),
  mnesia:create_schema(Nodes),
  application:start(mnesia),
  ok.