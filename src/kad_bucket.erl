%%
%%

-module(kad_bucket).

-include_lib("kad.hrl").

-export([new/0, find_contact/2, update_contact/2]).

-record(bucket, {
    updates = [],
    contacts = dict:new()
}).

new() ->
    #bucket{}.

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

