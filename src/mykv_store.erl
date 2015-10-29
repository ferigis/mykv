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
%% @doc gen_server with all the logic.
%% @end
%%%-------------------------------------------------------------------
-module(mykv_store).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([get/3]).
-export([set/4]).
-export([delete/3]).
-export([delete_bucket/1]).
-export([join/1]).
-export([get_nodes/0]).
-export([add_to_cluster/1]).
-export([leave/0]).

-export([get_locally2/2]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  code_change/3, terminate/2]).

%%%===================================================================
%%% Types & Macros
%%%===================================================================

-define(SERVER, ?MODULE).

%% row spec in mnesia db
-record(mykv_record, {key   :: any(), 
                      value :: any()}).

%% state
-record(state, {nodes :: list() %% cluster nodes list
                }).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec get(mykv:bucket(), mykv:key(), integer()) -> term().
get(Bucket, Key, Replicas) ->
  gen_server:call(?SERVER, {get, Bucket, Key, Replicas}).

get_locally2(Bucket, Key) ->
  gen_server:call(?SERVER, {get_locally, Bucket, Key}).

-spec set(mykv:bucket(), mykv:key(), mykv:value(), integer()) -> term().
set(Bucket, Key, Value, Replicas) ->
  gen_server:call(?SERVER, {set, Bucket, Key, Value, Replicas}).

-spec delete(mykv:bucket(), mykv:key(), integer()) -> term().
delete(Bucket, Key, Replicas) ->
  gen_server:call(?SERVER, {delete, Bucket, Key, Replicas}).

-spec delete_bucket(mykv:bucket()) -> term().
delete_bucket(Bucket) ->
  gen_server:call(?SERVER, {delete_bucket, Bucket}).

-spec join(node()) -> term().
join(ClusterNode) ->
  gen_server:call(?SERVER, {join, ClusterNode}).

-spec get_nodes() -> term().
get_nodes() ->
  gen_server:call(?SERVER, get_nodes).

-spec add_to_cluster(node()) -> term().
add_to_cluster(Node) ->
  gen_server:call(?SERVER, {add_to_cluster, Node}).

-spec leave() -> term().
leave() ->
  gen_server:call(?SERVER, leave).

%%====================================================================
%% gen_server functions
%%====================================================================

%% @hidden
init([]) ->
  {ok, #state{nodes = []}}.

%% @hidden
handle_call({get, Bucket, Key, Replicas}, _From, State) ->
  Result = case mykv_util:key_belogs_to_node(Key, State#state.nodes, Replicas) of
    true -> 
      get_locally(Bucket, Key);
    false ->
      get_from_cluster(Bucket, Key, Replicas, State#state.nodes)
  end,
  {reply, Result, State};
handle_call({get_locally, Bucket, Key}, _From, State) ->
  Result = get_locally(Bucket, Key),
  {reply, Result, State};
handle_call({set, Bucket, Key, Value, Replicas}, _From, State) ->
  case mykv_util:key_belogs_to_node(Key, State#state.nodes, Replicas) of
    true -> 
      ok = set_locally(Bucket, Key, Value);
    false ->
      ok = set_on_cluster(Bucket, Key, Value, Replicas, State#state.nodes)
  end,
  {reply, ok, State};
handle_call({delete, Bucket, Key, Replicas}, _From, State) ->
  case mykv_util:key_belogs_to_node(Key, State#state.nodes, Replicas) of
    true -> 
      ok = delete_locally(Bucket, Key);
    false ->
      ok = delete_on_cluster(Bucket, Key, Replicas, State#state.nodes)
  end,
  {reply, ok, State};
handle_call({delete_bucket, Bucket}, _From, State) ->
  ok = delete_bucket_locally(Bucket),
  {reply, ok, State};
handle_call({join, ClusterNode}, _From, State) ->
  {Result, State1} = case net_adm:ping(ClusterNode) of
    pang ->  {{error, cant_reach_node}, State};
    pong ->   
        case rpc:call(ClusterNode, mykv_store, add_to_cluster, [node()]) of
          already_in_the_cluster -> {already_in_the_cluster, State};
          Nodes -> 
            {ok, State#state{nodes = Nodes}}
        end
  end,
  {reply, Result, State1};
handle_call(get_nodes, _From, State) ->
  {reply, State#state.nodes, State};
handle_call({add_to_cluster, Node}, _From, #state{nodes = Nodes} = State) ->
  {Result, State1} = case lists:member(Node, Nodes) of
    true ->  {already_in_the_cluster, State};
    false ->   
        case Nodes of
          [] -> 
            Nodes1 = [node(), Node];
          _ -> 
            Nodes1 = lists:append(Nodes, [Node])
        end,
        update_cluster_broadcast(lists:delete(Node, Nodes1), Nodes1),
        {Nodes1, State#state{nodes = Nodes1}}
  end,
  {reply, Result, State1};
handle_call({update_cluster, Nodes}, _From, State) ->
  Nodes1 = case lists:delete(node(), Nodes) of
    [] -> [];
    _ -> Nodes
  end,
  {reply, ok, State#state{nodes = Nodes1}};
handle_call(leave, _From, State) ->
  Nodes = lists:delete(node(), State#state.nodes),
  update_cluster_broadcast(Nodes, Nodes),
  {reply, ok, State#state{nodes = []}}.

%% @hidden
handle_cast(_Msg, State) ->
  {noreply, State}.

%% @hidden
handle_info(Msg, State) ->
  io:format("Unknown msg: ~p~n", [Msg]),
  {noreply, State}.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
 
 %% @hidden
terminate(_Reason, _State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private
create_bucket(Bucket) ->
  mnesia:create_table(Bucket,
                        [{attributes, record_info(fields, mykv_record)},
                        {record_name, mykv_record},
                        {disc_copies, [node()]}
                        ]).

%% @private
table_exists(TableName) ->
   Tables = mnesia:system_info(tables),
   lists:member(TableName,Tables).

%% @private
update_cluster_broadcast(Nodes, NewNodes) ->
  gen_server:multi_call(lists:delete(node(), Nodes), ?SERVER, {update_cluster, NewNodes}).

%% @private
set_locally(Bucket, Key, Value) ->
  case table_exists(Bucket) of
    false -> create_bucket(Bucket);
    true -> ok
  end,
  F = fun() ->
    mnesia:write(Bucket, #mykv_record{key = Key,
                              value = Value}, write)
  end,
  mnesia:activity(transaction, F),
  ok.

%% @private
set_on_cluster(Bucket, Key, Value, Replicas, Nodes) ->
  KeyNodes = mykv_util:get_key_nodes(Key, Nodes, Replicas),
  F = fun(RemoteNode) ->
    rpc:call(RemoteNode, ?MODULE, set, [Bucket, Key, Value, Replicas])
  end,
  lists:map(F, KeyNodes),
  io:format("Key ~p is stored on Nodes ~p~n", [Key, KeyNodes]),
  ok.

%% @private
get_locally(Bucket, Key) ->
  Value = case table_exists(Bucket) of
    false -> bucket_not_found;
    true -> 
      case mnesia:dirty_read(Bucket, Key) of
      [] -> not_found;
      [Value1] -> Value1
    end
  end,
  Value.

%% @private
get_from_cluster(Bucket, Key, Replicas, Nodes) ->
  [Node | _RestNodes] = mykv_util:get_key_nodes(Key, Nodes, Replicas),
  rpc:call(Node, ?MODULE, get, [Bucket, Key, Replicas]).

%% @private
delete_locally(Bucket, Key) ->
  case table_exists(Bucket) of
    false ->  bucket_not_found;
    true ->   
      F = fun() -> 
        mnesia:delete({Bucket, Key})
      end,
      mnesia:activity(transaction, F),
      case mnesia:table_info(Bucket, size) of
        0 -> delete_bucket_locally(Bucket);
        _ -> ok
      end
  end,
  ok.

%% @private
delete_on_cluster(Bucket, Key, Replicas, Nodes) ->
  KeyNodes = mykv_util:get_key_nodes(Key, Nodes, Replicas),
  F = fun(RemoteNode) ->
    rpc:call(RemoteNode, ?MODULE, delete, [Bucket, Key, Replicas])
  end,
  lists:map(F, KeyNodes),
  ok.

%% @private
delete_bucket_locally(Bucket) ->
  case table_exists(Bucket) of
    false ->  bucket_not_found;
    true ->   
        mnesia:delete_table(Bucket),
        ok
  end.