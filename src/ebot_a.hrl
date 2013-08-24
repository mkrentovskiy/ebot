-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_jid.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

-define(FROMP(P), exmpp_xml:get_attribute(P#received_packet.raw_packet, <<"from">>, <<"me@localhost">>)).
-define(FROMM(P), P#received_packet.from).
-define(FROM(P), exmpp_jid:parse(?FROMP(P))).
-define(BODY(P), exmpp_message:get_body(P#received_packet.raw_packet)).
-define(JID(S), exmpp_jid:parse(S)).
-define(JIDCMP(A, B), exmpp_jid:bare_compare(A, B)).

-define(L(S), binary_to_list(S)).
-define(AS(A), proc_lib:spawn(fun() -> A end)).

-define(G, ebot:env(guru, "root@localhost")).

-define(TIMEOUT, 1 * 60 * 1000).

-record(src, {url = undefine, id = 0, type = page, state = []}).