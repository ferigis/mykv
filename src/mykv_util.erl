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
%% @doc mykv utilities
%% @end
%%%-------------------------------------------------------------------
-module(mykv_util).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([get_key_nodes/3]).
-export([key_belogs_to_node/3]).

%%====================================================================
%% API functions
%%====================================================================

%% This method returns a list with the nodes which match the hash.
get_key_nodes(Key, Nodes, _Replicas) when Nodes == [] ->
  get_key_nodes(Key, [node()], 1);
get_key_nodes(Key, Nodes, Replicas) when Replicas > length(Nodes) ->
  get_key_nodes(Key, Nodes, length(Nodes));
get_key_nodes(Key, Nodes, Replicas) when Replicas =< length(Nodes) ->
  %% Adding 1 because phash2 returns values between 0..N-1
  NodeNumber = erlang:phash2(Key, length(Nodes)) + 1, 
  get_replicas(Nodes, NodeNumber, Replicas).


%% This method checks if one Key belongs to local Node.
key_belogs_to_node(Key, Nodes, Replicas) ->
  ReplicaNodes = get_key_nodes(Key, Nodes, Replicas),
  lists:member(node(), ReplicaNodes).

%%====================================================================
%% Internal functions
%%====================================================================

%% @private
get_replicas(List, Start, Replicas) ->
  get_replicas(List, Start, Replicas, []).
get_replicas(_, _, 0, Acc) ->
  Acc;
get_replicas(List, Start, Replicas, Acc) ->
  Start1 = case Start > length(List) of 
    true -> 1;
    false -> Start 
  end,
  Node = lists:nth(Start1, List),
  get_replicas(List, Start1+1, Replicas-1, Acc ++ [Node]).

-ifdef(TEST).

%%====================================================================
%% Unit Testing
%%====================================================================

get_key_nodes_test() ->
  Nodes = [node1, node2, node3, node4],
  Key = this_is_a_key,
  Key2 = this_is_a_key2,
  Replicas = 2,
  [node2, node3] = get_key_nodes(Key, Nodes, Replicas),
  [node3, node4] = get_key_nodes(Key2, Nodes, Replicas).

key_belogs_to_node_test() ->
  Nodes = [node1, node(), node3, node4],
  Key = this_is_a_key,
  Key2 = this_is_a_key2,
  Replicas = 2,
  true = key_belogs_to_node(Key, Nodes, Replicas),
  false = key_belogs_to_node(Key2, Nodes, Replicas).

get_replicas_test() ->
  List = [1, 2, 3, 4, 5, 6, 7],
  [1, 2] = get_replicas(List, 1, 2),
  [1, 2, 3, 4] = get_replicas(List, 1, 4),
  [6, 7] = get_replicas(List, 6, 2),
  [7, 1, 2] = get_replicas(List, 7, 3).

-endif.