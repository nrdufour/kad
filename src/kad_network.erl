-module(kad_network).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("kad.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, myself/0, myport/0, setport/1, store_contact/1, find_contact/1, dump/0, find_closest_contacts_to/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

myself() ->
  gen_server:call(?SERVER, {myself}).

myport() ->
  gen_server:call(?SERVER, {myport}).

setport(Port) ->
  gen_server:call(?SERVER, {setport, Port}).

store_contact(Contact) ->
  gen_server:call(?SERVER, {store_contact, Contact}).

find_contact(NodeId) ->
  gen_server:call(?SERVER, {find_contact, NodeId}).

find_closest_contacts_to(SearchedKey) ->
  gen_server:call(?SERVER, {find_closest_contacts_to, SearchedKey}).

dump() ->
  gen_server:call(?SERVER, {dump}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
  Myself = kad_logic:generate_node_id(),
  Buckets = dict:new(),
  Network = #network{myself = Myself, buckets = Buckets},
  {ok, Network}.

handle_call({myport}, _From, Network) ->
  {reply, Network#network.port, Network};

handle_call({setport, Port}, _From, Network) ->
  io:format("kad_network setting port to ~p~n", [Port]),
  {reply, ok, Network#network{port = Port}};

handle_call({myself}, _From, Network) ->
  {reply, Network#network.myself, Network};

handle_call({store_contact, Contact}, _From, Network) ->
  Myself = Network#network.myself,
  ContactId = Contact#contact.nodeId,
  Buckets = Network#network.buckets,

  %% compute the distance
  Distance = kad_logic:distance(Myself, ContactId),
  io:format("distance between ~p and ~p is: ~p~n", [Myself, ContactId, Distance]),

  %% find the right bucket
  BucketId = kad_logic:distance_to_bucket_id(Distance),
  Bucket = case dict:is_key(BucketId, Buckets) of
    true  -> dict:fetch(BucketId, Buckets);
    false -> kad_bucket:new()
  end,

  %% update the bucket
  UpdatedBucket = kad_bucket:update_contact(Bucket, Contact),

  %% update the state
  UpdatedBuckets = dict:store(BucketId, UpdatedBucket, Buckets),

  {reply, ok, Network#network{buckets = UpdatedBuckets}};

handle_call({find_contact, NodeId}, _From, Network) ->
  Myself = Network#network.myself,
  Buckets = Network#network.buckets,

  %% compute the distance
  Distance = kad_logic:distance(Myself, NodeId),

  %% Find the right bucket
  BucketId = kad_logic:distance_to_bucket_id(Distance),
  Reply = case dict:is_key(BucketId, Buckets) of
      true  ->
          Bucket = dict:fetch(BucketId, Buckets),
          kad_bucket:find_contact(Bucket, NodeId);
      false ->
          not_found
  end,
  {reply, Reply, Network};

handle_call({dump}, _From, Network) ->
  io:format("Contacts for node ~p:~n", [Network#network.myself]),
  io:format("~p~n", [dict:to_list(Network#network.buckets)]),
  {reply, ok, Network};

handle_call({find_closest_contacts_to, SearchedKey}, _From, Network) ->
  Myself = Network#network.myself,
  Buckets = Network#network.buckets,

  Distance = kad_logic:distance(Myself, SearchedKey),
  BucketId = kad_logic:distance_to_bucket(Distance),
  
  %% TODO TO IMPLEMENT ;-)
  {reply, [], Network}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

