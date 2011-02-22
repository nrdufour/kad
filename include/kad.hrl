%%
%%
%%

-define(KEY_SIZE, 160).
-define(BUCKET_SIZE, 20).
-define(ALPHA, 3).

-define(T_EXPIRE, 86400).
-define(T_REFRESH, 3600).
-define(T_REPLICATE, 3600).
-define(T_REPUBLISH, 86400).

-record(contact, {
  nodeId,
  ipAddress,
  udpPort
}).

-record(network, {
  myself,
  port = 4000,
  buckets
}).

