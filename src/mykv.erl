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
%% @doc mykv API.
%% @end
%%%-------------------------------------------------------------------
-module(mykv).

%% API
-export([start/0]).
-export([get/2]).
-export([set/3]).
-export([delete/2]).
-export([delete_bucket/1]).
-export([join/1]).
-export([leave/0]).
-export([get_cluster_nodes/0]).

-export([setup_cluster/1]).
-export([get_locally/2]).
-export([get_replicas/0]).

%%%===================================================================
%%% Types & Macros
%%%===================================================================

-type bucket()  :: atom().
-type key()     :: any().
-type value()   :: any().

-define(REPLICAS, 3).

%%====================================================================
%% API functions
%%====================================================================

start() ->
  mykv_app:start().

-spec get(bucket(), key()) -> {any(), key(), value()} | not_found | bucket_not_found.
get(Bucket, Key) ->
  mykv_store:get(Bucket, Key, get_replicas()).

-spec set(bucket(), key(), value()) -> ok.
set(Bucket, Key, Value) ->
  mykv_store:set(Bucket, Key, Value, get_replicas()).

-spec delete(bucket(), key()) -> ok.
delete(Bucket, Key) ->
  mykv_store:delete(Bucket, Key, get_replicas()).

-spec delete_bucket(bucket()) -> ok.
delete_bucket(Bucket) ->
  mykv_store:delete_bucket(Bucket).

%%====================================================================
%% API functions Cluster Management
%%====================================================================

-spec join(node()) -> ok | {error, Reason :: string()}.
join(Node) ->
  mykv_store:join(Node).

-spec leave() -> ok.
leave() ->
  mykv_store:leave().

get_cluster_nodes() -> 
  case mykv_store:get_nodes() of
    [] -> no_cluster;
    Nodes -> Nodes 
  end.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private
get_replicas() ->
  case application:get_env(mykv, replicas) of
    {ok, Replicas} -> Replicas;
    undefined -> ?REPLICAS
  end.


%%====================================================================
%% Testing purpose functions
%%====================================================================

-spec setup_cluster(list()) -> ok.
setup_cluster(NodeNames) when is_list(NodeNames) ->
  start(),
  F = fun(Node) ->
      net_adm:ping(Node),
      rpc:call(Node, ?MODULE, start, [])
  end,
  F2 = fun(Node) ->
      rpc:call(Node, ?MODULE, join, [node()])
  end,
  lists:map(F, NodeNames),
  lists:map(F2, NodeNames),
  ok.

-spec get_locally(bucket(), key()) -> {any(), key(), value()} | not_found | bucket_not_found.
get_locally(Bucket, Key) ->
  mykv_store:get_locally2(Bucket, Key).

