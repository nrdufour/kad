%%
%% A bucket is a list of contacts for with the distance
%% between the current node and them match the bucket id
%%
%% A bucket is also 2 things:
%% - a catalog of contacts
%% - a fixed sized list in which contacts are stored from the oldest
%%   to the youngest in term of ping time (the first hasn't been pinged for a
%%   long time, the last one has been recently pinged -- relatively)
%%

-module(kad_bucket).

-include_lib("kad.hrl").

-export([distance_to_bucket_id/1, new/1, find_contact/2, update_contact/2]).

-record(bucket, {
    id = 0,                %% bucket id [log(D)/log(2) as an int]
    updates = [],          %% fixed size array (refer the contact id)
    contacts = dict:new()  %% catalog of contact (store the nodeid and the full contact)
}).

%% Compute the right bucket Id from the distance
%% the bigger the id, the bigger the distance is
distance_to_bucket_id(X) when X ==0 ->
    0;
distance_to_bucket_id(X) ->
    Exp = math:log(X) / math:log(2),
    kad_logic:floor(Exp).

%% create a new bucket
new(Id) ->
    #bucket{id = Id}.

find_contact(Bucket, NodeId) ->
    case lists:member(NodeId, Bucket#bucket.updates) of
        true  ->
            Contact = dict:fetch(NodeId, Bucket#bucket.contacts),
            {ok, Contact};
        false ->
            not_found
    end.

%% add or update a contact
%% TODO: this bucket has no limit in size...
update_contact(Bucket, Contact) ->
    ContactId = Contact#contact.nodeId,

    NewUpdates = case Bucket#bucket.updates of
        [] -> [ContactId];
        _  ->
            case lists:member(ContactId, Bucket#bucket.updates) of
                true  ->
                    B1 = lists:delete(ContactId, Bucket#bucket.updates),
                    lists:append(B1, [ContactId]);
                false ->
                    lists:append(Bucket#bucket.updates, [ContactId])
            end
    end,
    NewContacts = dict:store(ContactId, Contact, Bucket#bucket.contacts),

    #bucket{updates = NewUpdates, contacts = NewContacts}.
