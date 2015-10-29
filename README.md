mykv
=====

__Authors:__ Felipe Ripoll. ([`ferigis@gmail.com`](mailto:ferigis@gmail.com)).

Distributed Key/Value store.

__Note:__ mykv is still under development. Currently the number of nodes must be immovable. There are more things to add like handoff, distributed tests, consistent hashing, adding quorums...

About
----

Mykv is a distributed key/value store written in Erlang/OTP. It uses Mnesia as a backend. This app was built for learning purposes, this is not suitable for use in production applications.

Build
-----

Mykv uses [Rebar3](https://www.rebar3.org/).

    $ git clone https://github.com/ferigis/mykv.git
    $ cd mykv
    $ make compile

Example
-----

We want to create a 2 nodes cluster. First we need to decide which number of Replicas we want. We are going to choose R=2 (full consistency!). To acomplish that we must change to 2 the  `replicas` value from `config/mykv.config` file. 
Then start two Erlang consoles:

    $ erl -name node1@127.0.0.1 -pa _build/default/lib/mykv/ebin/ -config config/mykv.config

and 

    $ erl -name node2@127.0.0.1 -pa _build/default/lib/mykv/ebin/ -config config/mykv.config

Then start mykv in both nodes:

```erlang
application:start(mykv).
ok
```

Now we create the cluster calling the method `mykv:join(Node)` from `node1` to `node2` (or viceversa). If we want to add a new node to the cluster we have to call that `join` method from the new node to one in the cluster.

```erlang
(node1@127.0.0.1)2> mykv:join('node2@127.0.0.1').
ok
```
We can check the nodes in the cluster in both nodes:

```erlang
(node2@127.0.0.1)2> mykv:get_cluster_nodes().
['node2@127.0.0.1','node1@127.0.0.1']
```

Now we have the cluster created!! but... what if we have many nodes? do we have to repeat this N times?? well, actually we can call an aux method. Lets do it with 5 nodes and 3 Replicas (remember change `replicas` on `config/mykv.conf`).
First open 5 Erlang consoles as we did before (from node1@127.0.0.1 to node5@127.0.0.1). After that call the `setup_cluster` method on node1:

```erlang
(node1@127.0.0.1)2> mykv:setup_cluster(['node2@127.0.0.1','node3@127.0.0.1','node4@127.0.0.1','node5@127.0.0.1']).
ok
```

Checking the nodes in `node3`(for example):

```erlang
(node3@127.0.0.1)1> mykv:get_cluster_nodes().
['node1@127.0.0.1','node2@127.0.0.1','node3@127.0.0.1',
 'node4@127.0.0.1','node5@127.0.0.1']
```

 Much easy on that way, right?
 Now we are going to add some data. Lets pick one node, `node2` for example

```erlang
(node2@127.0.0.1)6> mykv:set(bucket1, key1, value1).
Key key1 is stored on Nodes ['node4@127.0.0.1','node5@127.0.0.1',
                          'node1@127.0.0.1']
ok
```

Mykv says the `key1` from `bucket1` has been stored on `node4`, `node5` and `node1`. Lets try to get the data on `node2` and on `node4`.

```erlang
(node2@127.0.0.1)7> mykv:get_locally(bucket1, key1).
bucket_not_found


(node4@127.0.0.1)1> mykv:get_locally(bucket1, key1).
{mykv_record,key1,value1}
```

 That is correct. So, how can we get the `key1` value on `node2`? just calling the `get` mehod:

```erlang
(node2@127.0.0.1)8> mykv:get(bucket1, key1).        
{mykv_record,key1,value1}
```

Here it is the correct value!.

Running Tests
-----

    $ make test