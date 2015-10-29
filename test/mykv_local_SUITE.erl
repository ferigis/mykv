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
%% @doc mykv local test suite.
%% @end
%%%-------------------------------------------------------------------
-module(mykv_local_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Common test
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

%% Tests
-export([basic_test/1]).

%%%===================================================================
%%% Common Test
%%%===================================================================

all() ->
  [basic_test].

init_per_suite(Config) ->
  ok = application:start(mykv),
  Config.

end_per_suite(Config) ->
  ok = application:stop(mykv),
  Config.

%%%===================================================================
%%% Exported Tests Functions
%%%===================================================================
basic_test(_Config) ->
  Bucket1 = bucket1,
  Key1 = key1,
  Value11 = value1,
  bucket_not_found = mykv:get(Bucket1, Key1),
  ok = mykv:set(Bucket1, Key1, Value11),
  {_, Key1, Value11} = mykv:get(Bucket1, Key1),
  ok = mykv:delete(Bucket1, Key1),
  bucket_not_found = mykv:get(Bucket1, Key1),
  Bucket2 = bucket2,
  Key2 = key2,
  Value12 = value2,
  bucket_not_found = mykv:get(Bucket2, Key2),
  ok = mykv:set(Bucket2, Key2, Value12),
  {_, Key2, Value12} = mykv:get(Bucket2, Key2),
  ok = mykv:delete(Bucket2, Key2),
  bucket_not_found = mykv:get(Bucket2, Key2),
  ok.
