%% +---------------------------------------------------------------------------+
%% | File:         sip_stack.erl					       |
%% +---------------------------------------------------------------------------+
%% | Author:       Romin Per						       |
%% +---------------------------------------------------------------------------+
%% | Description: The SIP phones stack modules				       |
%% |                                                                           |
%% |                                                                           |
%% +---------------------------------------------------------------------------+

-module(sip_stack).
-export([sip_message/2]).
-include("h323_phone_options.hrl").
-include("h323_phone_debug.hrl").

-export([gatekeeper_ipaddress/1,get_sip_port/1,get_random_4digit/0,get_CSeq/0]).

sip_message(invite,#sip_phone_options{
	      aprty_name = Aparty_name,
	      state=outgoing_digit_rec
	     } = Options) ->
    Options1 = Options#sip_phone_options{
		 branch = get_Branch(),
		 callid = get_Callid() ++ "@" ++ phone_ipaddress(Options),
		 aprty_tag = get_Tag(),
		 cseq = get_CSeq(),
		 state=outgoing_callproc
		},
    Message =
	"INVITE sip:" ++ bprty_phone_number_ip(Options1) ++ " " ++ sip_version() ++ "\r\n" ++
	"Via: " ++ sip_version() ++ "/UDP " ++ phone_ipaddress_port(Options1) ++ Options1#sip_phone_options.branch ++ "\r\n" ++
	"To: <sip:" ++ bprty_phone_number_ip(Options) ++">\r\n" ++
	"From:"++Aparty_name++" <sip:" ++ aprty_phone_number_ip(Options) ++ ">" ++ Options1#sip_phone_options.aprty_tag ++ "\r\n" ++
	"Call-ID: " ++ Options1#sip_phone_options.callid ++ "\r\n" ++
	"CSeq: " ++ Options1#sip_phone_options.cseq ++ " INVITE\r\n" ++
	"Max-Forwards: 70\r\n" ++
	 get_UserAgent(Options1)++
%	"User-Agent: app.voip.icp\r\n"
	"Contact: <sip:" ++ own_phone_number(Options1) ++ "@" ++ phone_ipaddress(Options1) ++ ":" ++ phone_portno(Options1) ++ ">;g.ericsson.voip-p2p=\"true\"\r\n" ++
	"Content-Length: 268\r\n" ++ 
	"Content-Type: application/sdp\r\n" ++
	"Supported: replaces,timer\r\n" ++
	"Session-Expires: 1800\r\n" ++
	"Call-Info: <wap://" ++ phone_ipaddress_wspport(Options1) ++ ">" ++ get_wapref() ++ "\r\n" ++
	"Accept-Contact: *;q.ericsson.voip-p2p=\"true\"\r\n" ++
	"Allow: INVITE, ACK, BYE, UPDATE, INFO, CANCEL, NOTIFY\r\n" ++
	"\r\n",
    {Message,Options1};

sip_message(invite,#sip_phone_options{
	      aprty_name = Aparty_name,
	      state=_State
	     } = Options) ->
    Options1 = Options#sip_phone_options{
		 branch = get_Branch(),
		 callid = get_Callid() ++ "@" ++ phone_ipaddress(Options),
		 aprty_tag = get_Tag(),
		 cseq = get_CSeq(),
		 state=outgoing_callproc
		},
    Message =
	"INVITE sip:" ++ bprty_phone_number_ip(Options1) ++ " " ++ sip_version() ++ "\r\n" ++
	"Via: " ++ sip_version() ++ "/UDP " ++ phone_ipaddress_port(Options1) ++ Options1#sip_phone_options.branch ++ "\r\n" ++
	"To: <sip:" ++ bprty_phone_number_ip(Options) ++">\r\n" ++
	"From:"++Aparty_name++" <sip:" ++ aprty_phone_number_ip(Options) ++ ">" ++ Options1#sip_phone_options.aprty_tag ++ "\r\n" ++
	"Call-ID: " ++ Options1#sip_phone_options.callid ++ "\r\n" ++
	"CSeq: " ++ Options1#sip_phone_options.cseq ++ " INVITE\r\n" ++
	"Max-Forwards: 70\r\n" ++
	 get_UserAgent(Options1)++
%	"User-Agent: app.voip.icp\r\n"
	"Contact: <sip:" ++ own_phone_number(Options1) ++ "@" ++ phone_ipaddress(Options1) ++ ":" ++ phone_portno(Options1) ++ ">;g.ericsson.voip-p2p=\"true\"\r\n" ++
	"Content-Length: 268\r\n" ++ 
	"Content-Type: application/sdp\r\n" ++
	"Supported: replaces,timer\r\n" ++
	"Session-Expires: 1800\r\n" ++
	"Call-Info: <wap://" ++ phone_ipaddress_wspport(Options1) ++ ">" ++ get_wapref() ++ "\r\n" ++
	"Accept-Contact: *;q.ericsson.voip-p2p=\"true\"\r\n" ++
	"Allow: INVITE, ACK, BYE, UPDATE, INFO, CANCEL, NOTIFY\r\n" ++
	"\r\n",
    {Message,Options1};

sip_message(sip200ok_invite,#sip_phone_options{
	      branch = Branch,
	      callid = _Callid,
	      aprty_tag = _Aprty_tag,
	      cseq=_Cseq} = Options) ->
    Message =
	sip_version()++" 200 OK\r\n" ++
	"Via: " ++ sip_version() ++ "/UDP " ++ gatekeeper_ipaddress(Options) ++":"++ integer_to_list(get_sip_port(Options)) ++ Branch ++ ";rport="++ phone_portno(Options) ++"\r\n" ++
	"To: <sip:" ++ bprty_phone_number_ip(Options) ++";user=phone>" ++ Options#sip_phone_options.bprty_tag ++ "\r\n" ++
	"From: <sip:" ++ aprty_phone_number_ip(Options) ++";user=phone>" ++ Options#sip_phone_options.aprty_tag ++ "\r\n" ++
	"Call-ID: " ++ Options#sip_phone_options.callid ++ "\r\n" ++
	"CSeq: " ++ Options#sip_phone_options.cseq ++ " INVITE\r\n" ++
	"Max-Forwards: 70\r\n" ++
	"Server: MX-ONE/Resiprocate\r\n"++
	"Record-Route: <sip:"++gatekeeper_ipaddress(Options) ++":"++ integer_to_list(get_sip_port(Options))++">;lr\r\n"++
	"Contact: <sip:" ++ own_phone_number(Options) ++ "@" ++ phone_ipaddress(Options) ++ ":" ++ phone_portno(Options) ++ ";user=phone>\r\n" ++
	"Content-Length: 147\r\n" ++ 
	"Content-Type: application/sdp\r\n" ++
	"Allow: INVITE, ACK, BYE, UPDATE, NOTIFY, INFO, CANCEL\r\n" ++
	"Session-Expires: 1800;refresher=uas\r\n" ++
	"Min-SE: 90\r\n" ++
	"Supported: replaces,timer\r\n"++
	"\r\n",
    {Message,Options};

sip_message(sip200ok_after_answer,#sip_phone_options{
	      branch = Branch,
	      callid = _Callid,
	      aprty_tag = _Aprty_tag,
	      cseq=_Cseq} = Options) ->
    Message =
	sip_version()++" 200 OK\r\n" ++
	"Via: " ++ sip_version() ++ "/UDP " ++ gatekeeper_ipaddress(Options) ++":"++ integer_to_list(get_sip_port(Options)) ++ Branch ++ ";rport="++ phone_portno(Options) ++"\r\n" ++
	"To: <sip:" ++ bprty_phone_number_ip(Options) ++">" ++ Options#sip_phone_options.bprty_tag ++ "\r\n" ++
	"From: <sip:" ++ aprty_phone_number_ip(Options) ++">" ++ Options#sip_phone_options.aprty_tag ++ "\r\n" ++
	"Call-ID: " ++ Options#sip_phone_options.callid ++ "\r\n" ++
	"CSeq: " ++ Options#sip_phone_options.cseq ++ " INVITE\r\n" ++
	"Max-Forwards: 70\r\n" ++
	"Server: Ericsson DBC425_02 HWVer-R2B ApplicRev-R7C BootRev-R3L\r\n"++
	"Contact: <sip:" ++ own_phone_number(Options) ++ "@" ++ phone_ipaddress(Options) ++ ":" ++ phone_portno(Options) ++ ">;g.ericsson.voip-p2p=\"true\"\r\n" ++
	"Content-Length: 167\r\n" ++ 
	"Content-Type: application/sdp\r\n" ++
	"Allow: INVITE, ACK, BYE, MESSAGE, REFER, OPTIONS, UPDATE, NOTIFY, INFO, CANCEL\r\n" ++
	"Session-Expires: 1800;refresher=uas\r\n" ++
	"Min-SE: 90\r\n" ++
	"Supported: replaces,timer\r\n"++
	"\r\n",
    {Message,Options};

%sip_message(options,Options) ->
%    Message =
%	"OPTIONS sip:carol@chicago.com " ++ sip_version() ++ "\r\n" ++
%	"Via: " ++ sip_version() ++ "/UDP pc33.atlanta.com;branch=z9hG4bKhjhs8ass877\r\n" ++
%	"Max-Forwards: 70\r\n" ++
%	"To: <sip:carol@chicago.com>\r\n" ++
%	"From: Alice <sip:alice@atlanta.com>;tag=1928301774\r\n" ++
%	"Call-ID: a84b4c76e66710\r\n" ++
%	"CSeq: 63104 OPTIONS\r\n" ++
%	"Contact: <sip:alice@pc33.atlanta.com>\r\n" ++
%	"Accept: application/sdp\r\n" ++
%	"Content-Length: 0\r\n\r\n",
%    {Message,Options};

%sip_message(sip200ok_options,Options) ->
%    Message =
%	sip_version() ++ "200 OK\r\n" ++
%	"Via: " ++ sip_version() ++ "/UDP pc33.atlanta.com;branch=z9hG4bKhjhs8ass877\r\n" ++
%	" ;received=192.0.2.4\r\n" ++
%	"To: <sip:carol@chicago.com>;tag=93810874\r\n" ++
%	"From: Alice <sip:alice@atlanta.com>;tag=1928301774\r\n" ++
%	"Call-ID: a84b4c76e66710\r\n" ++
%	"CSeq: 63104 OPTIONS\r\n" ++
%	"Contact: <sip:carol@chicago.com>\r\n" ++
%	"Contact: <mailto:carol@chicago.com>\r\n" ++
%	"Allow: INVITE, ACK, CANCEL, OPTIONS, BYE\r\n" ++
%	"Accept: application/sdp\r\n" ++
%	"Accept-Encoding: gzip\r\n" ++
%	"Accept-Language: en\r\n" ++
%	"Supported: foo\r\n" ++
%	"Content-Type: application/sdp\r\n" ++
%	"Content-Length: 274\r\n\r\n",
%    {Message,Options};

sip_message(ack,#sip_phone_options{
	      contact = Contact,
	      branch = Branch,
	      cseq = _Cseq
	     } = Options) ->
%    Branch2=get_Branch(),
    Message =
	"ACK sip:" ++ bprty_phone_number_ip(Options) ++ ";user=phone " ++ sip_version() ++ "\r\n" ++
	"Via: " ++ sip_version() ++ "/UDP " ++ phone_ipaddress_port(Options) ++ Branch ++ ";rport="++ phone_portno(Options) ++"\r\n" ++
	"Max-Forwards: 70\r\n" ++
	case Contact of
	    ""->"";
	    Contact -> "Contact: <sip:"++Contact++">\r\n"
	end ++
	"To: <sip:" ++ bprty_phone_number_ip(Options) ++";user=phone>" ++ Options#sip_phone_options.bprty_tag ++ "\r\n" ++
	"From: <sip:" ++ aprty_phone_number_ip(Options) ++ ";user=phone>" ++ Options#sip_phone_options.aprty_tag ++ "\r\n" ++
	"Call-ID: " ++ Options#sip_phone_options.callid ++ "\r\n" ++
	"CSeq: " ++ Options#sip_phone_options.cseq ++ " ACK\r\n" ++
%	"Route: <sip:" ++ bprty_phone_number_ip(Options) ++ ";lr>\r\n"
	"User-Agent: MX-ONE/Resiprocate\r\n"++
	"Content-Length: 0\r\n\r\n",
    {Message,Options#sip_phone_options{contact = "",
				       branch=Branch}};

sip_message(ack_term,#sip_phone_options{
	      contact = Contact,
	      branch = Branch,
	      cseq = _Cseq
	     } = Options) ->
    Message =
	"ACK sip:" ++ bprty_phone_number_ip(Options) ++ ";user=phone " ++ sip_version() ++ "\r\n" ++
	"Via: " ++ sip_version() ++ "/UDP " ++ phone_ipaddress_port(Options) ++ Branch ++"\r\n" ++
	"Max-Forwards: 70\r\n" ++
	case Contact of
	    ""->"";
	    Contact -> "Contact: <sip:"++Contact++">\r\n"
	end ++
	"To: <sip:" ++ bprty_phone_number_ip(Options) ++";user=phone>" ++ Options#sip_phone_options.bprty_tag ++ "\r\n" ++
	"From: <sip:" ++ aprty_phone_number_ip(Options) ++ ";user=phone>" ++ Options#sip_phone_options.aprty_tag ++ "\r\n" ++
	"Call-ID: " ++ Options#sip_phone_options.callid ++ "\r\n" ++
	"CSeq: " ++ Options#sip_phone_options.cseq ++ " ACK\r\n" ++
	"User-Agent: MX-ONE/Resiprocate\r\n"++
	"Content-Length: 0\r\n\r\n",
    {Message,Options#sip_phone_options{
	       contact = ""
	      }};

sip_message(bye_200_ok,#sip_phone_options{
	      branch = Branch,
	      cseq = _Cseq
	     } = Options) ->
    Message =
	sip_version() ++ " 200 OK" ++ "\r\n" ++
	"Via: " ++ sip_version() ++ "/UDP " ++ gatekeeper_ipaddress(Options) ++":"++ integer_to_list(get_sip_port(Options)) ++ Branch ++ ";rport="++ phone_portno(Options) ++"\r\n" ++
	"To: <sip:" ++ own_phone_number_ip(Options) ++ ">" ++ Options#sip_phone_options.aprty_tag ++ "\r\n" ++
	"From: <sip:" ++ bprty_phone_number_ip(Options) ++">" ++ Options#sip_phone_options.bprty_tag ++ "\r\n" ++
	"Call-ID: " ++ Options#sip_phone_options.callid ++ "\r\n" ++
	"CSeq: " ++ Options#sip_phone_options.cseq ++ " BYE\r\n" ++
	"Max-Forwards: 70\r\n" ++
	"Server: app.voip.icp\r\n"++
	"Contact: <sip:" ++ own_phone_number(Options) ++ "@" ++ phone_ipaddress(Options) ++ ":" ++ phone_portno(Options) ++ ">;expires=3600;g.ericsson.voip-p2p=\"true\"\r\n" ++
	"Content-Length: 0\r\n"++
	"Content-Type: application/sdp\r\n"++
	"Allow: INVITE, ACK, BYE, MESSAGE, REFER, OPTIONS, UPDATE, NOTIFYE, INFO, PRACK\r\n\r\n",
    {Message,Options#sip_phone_options{state=idle}};

sip_message(cancel_200_ok,#sip_phone_options{
	      branch = Branch,
	      cseq = _Cseq
	     } = Options) ->
    Message =
	sip_version() ++ " 200 OK" ++ "\r\n" ++
	"Via: " ++ sip_version() ++ "/UDP " ++ gatekeeper_ipaddress(Options) ++":"++ integer_to_list(get_sip_port(Options)) ++ Branch ++ ";rport="++ phone_portno(Options) ++"\r\n" ++
	"To: <sip:" ++ own_phone_number_ip(Options) ++ ";user=phone>" ++ Options#sip_phone_options.aprty_tag ++ "\r\n" ++
	"From: <sip:" ++ bprty_phone_number_ip(Options) ++";user=phone>" ++ Options#sip_phone_options.bprty_tag ++ "\r\n" ++
	"Call-ID: " ++ Options#sip_phone_options.callid ++ "\r\n" ++
	"CSeq: " ++ Options#sip_phone_options.cseq ++ " CANCEL\r\n" ++
	"Max-Forwards: 70\r\n" ++
	"Content-Length: 0\r\n\r\n",
    {Message,Options#sip_phone_options{state=idle}};

sip_message(reqterm_487,#sip_phone_options{
	      branch = Branch,
	      cseq = _Cseq
	     } = Options) ->
    Message =
	sip_version() ++ " 487 Request Terminated" ++ "\r\n" ++
	"Via: " ++ sip_version() ++ "/UDP " ++ gatekeeper_ipaddress(Options) ++":"++ integer_to_list(get_sip_port(Options)) ++ Branch ++ ";rport="++ phone_portno(Options) ++"\r\n" ++
	"To: <sip:" ++ own_phone_number_ip(Options) ++ ">" ++ Options#sip_phone_options.aprty_tag ++ "\r\n" ++
	"From: <sip:" ++ bprty_phone_number_ip(Options) ++">" ++ Options#sip_phone_options.bprty_tag ++ "\r\n" ++
	"Call-ID: " ++ Options#sip_phone_options.callid ++ "\r\n" ++
	"CSeq: " ++ Options#sip_phone_options.cseq ++ " INVITE\r\n" ++
	"Max-Forwards: 70\r\n" ++
	"Server: MX-ONE/Resiprocate\r\n"++
	"Contact: <sip:" ++ own_phone_number(Options) ++ "@" ++ phone_ipaddress(Options) ++ ":" ++ phone_portno(Options) ++ ";user=phone>\r\n" ++
	"Content-Length: 0\r\n"++
	"Allow: INVITE, ACK, BYE, UPDATE, NOTIFY, INFO, CANCEL\r\n\r\n",
    {Message,Options#sip_phone_options{state=idle}};

sip_message(incomming_bye,#sip_phone_options{
	     } = Options) ->%outgoing disconnect after answer incommingcall
    Cseq = step_CSeq(Options),
    Message =
	"BYE sip:" ++ aprty_phone_number_ip(Options) ++ " " ++ sip_version() ++ "\r\n" ++
	"Via: " ++ sip_version() ++ "/UDP " ++ phone_ipaddress_port(Options) ++ get_Branch() ++ "\r\n" ++
	"To: <sip:" ++ aprty_phone_number_ip(Options) ++";user=phone>" ++ Options#sip_phone_options.aprty_tag ++ "\r\n" ++
	"From: <sip:" ++ bprty_phone_number_ip(Options) ++ ";user=phone>" ++ Options#sip_phone_options.bprty_tag ++ "\r\n" ++
	"Call-ID: " ++ Options#sip_phone_options.callid ++ "\r\n" ++
	"CSeq: " ++ step_CSeq(Options) ++ " BYE\r\n" ++
	"Max-Forwards: 70\r\n" ++
	"User-Agent: MX-ONE/Resiprocate\r\n"++
	"Route: <sip:" ++ aprty_phone_number_ip(Options) ++ ";lr>\r\n"++
	"Content-Length: 0\r\n"++
	"supported: timer\r\n"++
	"\r\n",
    {Message,Options#sip_phone_options{
	       state=idle,
	       cseq = Cseq}};

sip_message(outgoing_bye,#sip_phone_options{
	     } = Options) ->%outgoing disconnect after answer outgoingcall
    Cseq = step_CSeq(Options),
    Message =
	"BYE sip:" ++ bprty_phone_number_ip(Options) ++ " " ++ sip_version() ++ "\r\n" ++
	"Via: " ++ sip_version() ++ "/UDP " ++ phone_ipaddress_port(Options) ++ get_Branch() ++ "\r\n" ++
	"To: <sip:" ++ bprty_phone_number_ip(Options) ++";user=phone>" ++ Options#sip_phone_options.bprty_tag ++ "\r\n" ++
	"From: <sip:" ++ own_phone_number_ip(Options) ++ ";user=phone>" ++ Options#sip_phone_options.aprty_tag ++ "\r\n" ++
	"Call-ID: " ++ Options#sip_phone_options.callid ++ "\r\n" ++
	"CSeq: " ++ step_CSeq(Options) ++ " BYE\r\n" ++
	"User-Agent: app.voip.icp\r\n" ++
	"Route: <sip:" ++ bprty_phone_number_ip(Options) ++ ";lr>\r\n"++
	"Max-Forwards: 70\r\n" ++
	"Content-Length: 0\r\n"++
	"supported: replaces,timer\r\n"
	"Accept-Contact: *;g.ericsson.voip-p2p=\"true\"\r\n\r\n",
    {Message,Options#sip_phone_options{
	       state=idle,
	       contact = "",
	       cseq = Cseq}};

sip_message(register,Options) ->
    Options1 = Options#sip_phone_options{
		 branch = get_Branch(),
		 callid = get_Callid() ++ "@" ++ phone_ipaddress(Options),
		 aprty_tag = get_Tag(),
		 cseq = get_CSeq()
		},
    Message =
	"REGISTER sip:" ++ gatekeeper_ipaddress(Options1) ++ " " ++ sip_version() ++ "\r\n" ++
	"Via: " ++ sip_version() ++ "/UDP " ++ phone_ipaddress_port(Options1) ++ Options1#sip_phone_options.branch ++ "\r\n" ++
	"To: <sip:" ++ own_phone_number_ip(Options1) ++">\r\n" ++
	"From: <sip:" ++ own_phone_number_ip(Options1) ++ ">" ++ Options1#sip_phone_options.aprty_tag ++ "\r\n" ++
	"Call-ID: " ++ Options1#sip_phone_options.callid ++ "\r\n" ++
	"CSeq: " ++ Options1#sip_phone_options.cseq ++ " REGISTER\r\n" ++
	"Max-Forwards: 70\r\n" ++
	get_UserAgent(Options1) ++
	"Contact: <sip:" ++ own_phone_number(Options1) ++ "@" ++ phone_ipaddress(Options1) ++ ":" ++ phone_portno(Options1) ++ ">;expires=3600;g.ericsson.voip-p2p=\"true\"\r\n" ++
	"Content-Length: 0\r\n"
	"call_info: " ++ get_CallInfo(Options1) ++ "\r\n\r\n",
    {Message,Options1};

sip_message(unregister,Options) ->
    Options1 = Options#sip_phone_options{
		 branch = get_Branch(),
		 callid = get_Callid() ++ "@" ++ phone_ipaddress(Options),
		 aprty_tag = get_Tag(),
		 cseq = get_CSeq()
		},
    Message =
	"REGISTER sip:" ++ gatekeeper_ipaddress(Options1) ++ " " ++ sip_version() ++ "\r\n" ++
	"Via: " ++ sip_version() ++ "/UDP " ++ phone_ipaddress_port(Options1) ++ Options1#sip_phone_options.branch ++ "\r\n" ++
	"To: <sip:" ++ own_phone_number_ip(Options1) ++">\r\n" ++
	"From: <sip:" ++ own_phone_number_ip(Options1) ++">"++ Options1#sip_phone_options.aprty_tag ++ "\r\n" ++
	"Max-Forwards: 70\r\n" ++
	"Call-ID: " ++ Options1#sip_phone_options.callid ++ "\r\n" ++
	"CSeq: " ++ Options1#sip_phone_options.cseq ++ " REGISTER\r\n" ++
	get_UserAgent(Options1) ++
	"Contact: <sip:" ++ own_phone_number(Options1) ++ "@" ++ phone_ipaddress(Options1) ++ ":" ++ phone_portno(Options1) ++ ">;expires=0\r\n" ++
	"Content-Length: 0\r\n"
	"call_info: " ++ get_CallInfo(Options1) ++ "\r\n\r\n",
    {Message,Options1};

%sip_message(sip200ok_register,Options) ->
%    Message =
%	sip_version() ++ " 200 OK\r\n" ++
%	"Via: " ++ sip_version() ++ "/UDP "++ phone_ipaddress_port(Options) ++ ";branch=z9hG4bKnashds7\r\n" ++
%	" ;received=192.0.2.4\r\n" ++
%	"To: Bob <sip:bob@biloxi.com>;tag=2493k59kd\r\n" ++
%	"From: Bob <sip:bob@biloxi.com>;tag=456248\r\n" ++
%	"Call-ID: 843817637684230@998sdasdh09\r\n" ++
%	"CSeq: 1826 REGISTER\r\n" ++
%	"Contact: <sip:bob@192.0.2.4>\r\n" ++
%	"Expires: 7200\r\n" ++
%	"Content-Length: 0\r\n\r\n",
%    {Message,Options};

sip_message(sip100_trying,#sip_phone_options{
	      aprty_name = Aparty_name,
	      callid=Callid,
	      branch=Branch
	     } = Options) ->
    Message =
	sip_version() ++ " 100 Trying\r\n" ++
	"Via: " ++ sip_version() ++ "/UDP " ++ gatekeeper_ipaddress(Options) ++":"++ integer_to_list(get_sip_port(Options)) ++ Branch ++ ";rport="++ phone_portno(Options) ++"\r\n" ++
%	" ;received=192.0.2.1\r\n" ++
	"To: <sip:" ++ bprty_phone_number_ip(Options) ++";user=phone>\r\n" ++
	"From:"++Aparty_name++" <sip:" ++ aprty_phone_number_ip(Options) ++";user=phone>"++ Options#sip_phone_options.aprty_tag ++ "\r\n" ++
	"Call-ID: " ++ Callid ++ "\r\n" ++
	"CSeq: " ++ Options#sip_phone_options.cseq ++ " INVITE\r\n" ++
	"Max-Forwards: 70\r\n" ++
	"Content-Length: 0\r\n\r\n",
    {Message,Options};

sip_message(sip180_ringing,#sip_phone_options{
	      aprty_name = Aparty_name,
	      callid=Callid,
	      branch = Branch
	     } = Options) ->
    BParty_tag=get_Tag(),
    Message =
	sip_version() ++ " 180 Ringing\r\n" ++
	"Via: " ++ sip_version() ++ "/UDP " ++ gatekeeper_ipaddress(Options) ++":"++ integer_to_list(get_sip_port(Options)) ++ Branch ++ ";rport="++ phone_portno(Options) ++"\r\n" ++
	"To: <sip:" ++ bprty_phone_number_ip(Options) ++";user=phone>"++BParty_tag++"\r\n" ++
	"From: "++Aparty_name++" <sip:" ++ aprty_phone_number_ip(Options) ++";user=phone>"++ Options#sip_phone_options.aprty_tag ++ "\r\n" ++
	"Call-ID: " ++ Callid ++ "\r\n" ++
	"CSeq: " ++ Options#sip_phone_options.cseq ++ " INVITE\r\n" ++
        "Max-Forwards: 70\r\n"++
        "Server: MX-ONE/Resiprocate\r\n"++
	"Contact: <sip:" ++ own_phone_number(Options) ++ "@" ++ phone_ipaddress(Options) ++ ":" ++ phone_portno(Options) ++ ";user=phone>\r\n" ++
	"Content-Length: 0\r\n"++
	"Allow: INVITE, ACK, BYE, UPDATE, NOTIFY, INFO, CANCEL\r\n\r\n",
    {Message,Options#sip_phone_options{bprty_tag=BParty_tag}};

sip_message(bye,#sip_phone_options{%outgoing disconnect before answer
	      callid=Callid,
	      branch = Branch
	     } = Options) ->
    Message =
	"BYE sip:" ++ bprty_phone_number_ip(Options) ++ " " ++ sip_version() ++ "\r\n" ++
	"Via: " ++ sip_version() ++ "/UDP " ++ phone_ipaddress(Options) ++ ":" ++ phone_portno(Options) ++ Branch ++ "\r\n" ++
	"To: <sip:" ++ bprty_phone_number_ip(Options) ++">"++ Options#sip_phone_options.bprty_tag ++ "\r\n" ++
	"From: <sip:" ++ aprty_phone_number_ip(Options) ++">"++ Options#sip_phone_options.aprty_tag ++ "\r\n" ++
	"Call-ID: " ++ Callid ++ "\r\n" ++
	"CSeq: " ++ Options#sip_phone_options.cseq ++ " BYE\r\n" ++
	"Max-Forwards: 70\r\n"++
        "User-Agent: MX-ONE/Resiprocate\r\n"++
        "Route: <sip:" ++gatekeeper_ipaddress(Options) ++":"++ integer_to_list(get_sip_port(Options))++">;lr\r\n"++
	"Content-Length: 0\r\n"++
	"\r\n",
    {Message,Options};

sip_message(bye_switchAB,#sip_phone_options{%outgoing disconnect before answer
	      callid=Callid,
	      aprty_name=Aparty_name
	     } = Options) ->
    Branch = get_Branch(),
    Message =
	"BYE sip:" ++ aprty_phone_number_ip(Options) ++ " " ++ sip_version() ++ "\r\n" ++
	"Via: " ++ sip_version() ++ "/UDP " ++ phone_ipaddress(Options) ++ ":" ++ phone_portno(Options) ++ Branch ++ "\r\n" ++
	"To: "++Aparty_name++" <sip:" ++ aprty_phone_number_ip(Options) ++";user=phone>"++Options#sip_phone_options.aprty_tag++"\r\n" ++
	"From: <sip:" ++ bprty_phone_number_ip(Options) ++">"++ Options#sip_phone_options.bprty_tag ++ "\r\n" ++
	"Call-ID: " ++ Callid ++ "\r\n" ++
	"CSeq: " ++ step_CSeq(Options) ++ " BYE\r\n" ++
	"Max-Forwards: 70\r\n"++
        "User-Agent: MX-ONE/Resiprocate\r\n"++
        "Route: <sip:" ++gatekeeper_ipaddress(Options) ++":"++ integer_to_list(get_sip_port(Options))++">;lr\r\n"++
	"Content-Length: 0\r\n"++
	"Supported: timer\r\n"++
	"\r\n",
    {Message,Options};

sip_message(cancel,#sip_phone_options{%outgoing disconnect before answer
	      callid=Callid,
	      branch = Branch
	     } = Options) ->
    Message =
	"CANCEL sip:" ++ bprty_phone_number_ip(Options) ++ " " ++ sip_version() ++ "\r\n" ++
	"Via: " ++ sip_version() ++ "/UDP " ++ phone_ipaddress(Options) ++ ":" ++ phone_portno(Options) ++ Branch ++ "\r\n" ++
	"To: <sip:" ++ bprty_phone_number_ip(Options) ++">\r\n" ++
	"From: <sip:" ++ aprty_phone_number_ip(Options) ++">"++ Options#sip_phone_options.aprty_tag ++ "\r\n" ++
	"Call-ID: " ++ Callid ++ "\r\n" ++
	"CSeq: " ++ Options#sip_phone_options.cseq ++ " CANCEL\r\n" ++
	"Max-Forwards: 70\r\n"++
	"Content-Length: 0\r\n"++
	"\r\n",
    {Message,Options};

sip_message(info,#sip_phone_options{%sending of service number for example callback.
	      callid=Callid,
	      aprty_name=Aparty_name
	     } = Options) ->
    Branch = get_Branch(),
    Cseq = get_CSeq(),
    Message =
	"INFO sip:" ++ aprty_phone_number_ip(Options) ++ " " ++ sip_version() ++ "\r\n" ++
	"Via: " ++ sip_version() ++ "/UDP " ++ phone_ipaddress(Options) ++ ":" ++ phone_portno(Options) ++ Branch ++ "\r\n" ++
	"To: "++Aparty_name++" <sip:" ++ aprty_phone_number_ip(Options) ++";user=phone>"++Options#sip_phone_options.aprty_tag++"\r\n" ++
	"From: <sip:" ++ bprty_phone_number_ip(Options) ++";user=phone>"++ Options#sip_phone_options.bprty_tag ++ "\r\n" ++
	"Call-ID: " ++ Callid ++ "\r\n" ++
	"CSeq: " ++ Cseq ++ " INFO\r\n" ++
	"Max-Forwards: 70\r\n"++
        "Route: <sip:" ++gatekeeper_ipaddress(Options) ++":"++ integer_to_list(get_sip_port(Options))++">;lr\r\n"++

	"Content-Length: 25\r\n"++
        "Content-Type: application/dtmf-relay\r\n"++
	"\r\n"++
	"Signal = "++Options#sip_phone_options.service_no ++ "\r\n"++
	"Duration = 70",
    
    {Message,Options#sip_phone_options{
		 branch = Branch}};

sip_message(sdp,#sip_phone_options{
	      rtplocal_port = {Rtp_port,_RTPsock,_RTP_Pid}
	     } = Options) ->
    SessionId = get_SDPcallid(),
    Options1 = Options#sip_phone_options{sdp_callid = SessionId},
    Message = 
	"v=0\r\n"++
	"o=- "++ SessionId ++ " "++ SessionId ++ " IN IP4 " ++ phone_ipaddress(Options1) ++ "\r\n" ++
	"s=D4 SIP-phone"++ "\r\n" ++
	"c=IN IP4 "++ phone_ipaddress(Options1) ++ "\r\n" ++
	"t=0 0"++ "\r\n" ++
%	"a=direction:active\r\n" ++
	"m=audio "++int_to_string(Rtp_port)++" RTP/AVP 8 0 18 18 4 101\r\n" ++
	"a=rtpmap:8 PCMA/8000\r\n" ++
	"a=rtpmap:0 PCMU/8000\r\n" ++
	"a=rtpmap:18 G729/8000\r\n" ++
	"a=fmtp:18 annexb=no\r\n" ++
	"a=rtpmap:18 G729/8000\r\n" ++
	"a=rtpmap:4 G723/8000\r\n",
%	"a=rtpmap:101 telephone-event/8000\r\n" ++
%	"a=fmtp:101 0-16\r\n",
    {Message,Options1};

sip_message(sdp_phone,#sip_phone_options{
	      rtplocal_port = {Rtp_port,_RTPsock,_RTP_Pid}
	     } = Options) ->
    SessionId = get_SDPcallid(),
    Options1 = Options#sip_phone_options{sdp_callid = SessionId},
    Message = 
	"v=0\r\n"++
	"o=- "++ SessionId ++ " "++ SessionId ++ " IN IP4 " ++ phone_ipaddress(Options1) ++ "\r\n" ++
	"s=D4 SIP-phone"++ "\r\n" ++
	"c=IN IP4 "++ phone_ipaddress(Options1) ++ "\r\n" ++
	"t=0 0"++ "\r\n" ++
	"m=audio "++int_to_string(Rtp_port)++" RTP/AVP 4\r\n" ++
	"a=direction:active\r\n" ++
	"a=rtpmap:4 G723/8000\r\n",
	%"a=rtpmap:101 telephone-event/8000\r\n" ++
	%"a=fmtp:101 0-16\r\n",

    {Message,Options1};

sip_message([$S,$I,$P,$/,$2,$.,$0,$ ,$1,$0,$0,$ |RestMes],Options) ->%Trying
    Message="Unknown",
    Int=string:str(RestMes,"To: <sip:"),
    To_string=string:sub_string(RestMes,Int),
    [_A,Tag|_Rest] = string:tokens(To_string, ">\r"),
    {Message,Options#sip_phone_options{bprty_tag=Tag}};%store bparty tag.

sip_message([$S,$I,$P,$/,$2,$.,$0,$ ,$1,$8,$0|_RestMes],Options) ->%Ringing
%SIP/2.0 180 Ringing
%Via: SIP/2.0/UDP 130.100.182.126:58405;branch=911411.911409
%Record-Route: <sip:130.100.10.72:5060>
%Contact: <sip:41932@130.100.10.72>
%To: <sip:41932@130.100.10.72>;tag=f5b2a102
%From: <sip:41931@130.100.10.72>;tag=911422
%Call-ID: 911418911417@130.100.182.126
%CSeq: 272 INVITE
%User-Agent: MX-ONE/Resiprocate
%Content-Length: 0
    gen_server:cast(self(), {tone,ring_tone}),
    {"Unknown",Options#sip_phone_options{state=outgoing_ring}};

sip_message([$S,$I,$P,$/,$2,$.,$0,$ ,$1,$8,$1|_RestMes],Options) ->%Call is being forwarded

%Via: SIP/2.0/UDP 130.100.182.126:58405;branch=911411.911409
%Record-Route: <sip:130.100.10.72:5060>
%Contact: <sip:41932@130.100.10.72>
%To: <sip:41932@130.100.10.72>;tag=f5b2a102
%From: <sip:41931@130.100.10.72>;tag=911422
%Call-ID: 911418911417@130.100.182.126
%CSeq: 272 INVITE
%User-Agent: MX-ONE/Resiprocate
%Content-Length: 0
%    {Cseq,Token}=get_cseq(RestMes),
    %Int=string:str(RestMes,"CSeq:"),
    %To_string=string:sub_string(RestMes,Int),
    %[_A,_B,Token|_D] = string:tokens(To_string, " \r"),
%    case Token of
%	"INVITE"->
%	    sip_message(ack,Options#sip_phone_options{cseq=Cseq});
%	_->
	    {"Unknown",Options};
%    end;

sip_message([$S,$I,$P,$/,$2,$.,$0,$ ,$1,$8,$3|_RestMes],Options) ->%Session Progress
%    {Cseq,Token}=get_cseq(RestMes),
    %Int=string:str(RestMes,"CSeq:"),
    %To_string=string:sub_string(RestMes,Int),
    %[_A,_B,Token|_D] = string:tokens(To_string, " \r"),
%    case Token of
%	"INVITE"->
%	    sip_message(ack,Options#sip_phone_options{cseq=Cseq});
%	_->
	    {"Unknown",Options};
%    end;

sip_message([$S,$I,$P,$/,$2,$.,$0,$ ,$2,$0,$0|RestMes],%200 OK
	    #sip_phone_options{
	      aprty_tag=_Aprty_tag,
	      branch=_Branch,
	      rtplocal_port = {_RTP_port,_RTPSock,RTP_Pid}
	     } = Options) ->
    {Cseq,Token}=get_cseq(RestMes),
    Own_name=
       case string:str(RestMes,"Contact: <sip") of
          Int when Int == 0 ->
	      case string:str(RestMes,"Contact:") of
	          Int2 when Int2 > 0 ->
		      To_string=string:sub_string(RestMes,Int2),
		      [_A,Own_name_temp|_D] = string:tokens(To_string, " \r<"),
		      Own_name_temp;
		  _-> ""
	      end;
	  _-> ""
       end,

    case Token of
	"REGISTER"->
	    gen_server:cast(self(), registered),
	    {"Unknown",Options#sip_phone_options{own_name = Own_name}};
	"INVITE"->

	    CIPInt=string:str(RestMes,"c=IN IP4"),
	    CIPTo_string=string:sub_string(RestMes,CIPInt+9),
	    [RTP_IPaddress|_CIPRstD] = string:tokens(CIPTo_string, "\r"),

	    MAInt=string:str(RestMes,"m=audio"),
	    MATo_string=string:sub_string(RestMes,MAInt),
	    [_MAA,RTP_Port|_MARstD] = string:tokens(MATo_string, " "),

	    case is_pid(RTP_Pid) of
		true->
	    	    RTP_Pid ! {remote_address, {RTP_IPaddress, list_to_integer(RTP_Port)}};
		false->
		    true
	    end,
	
	    gen_server:cast(self(), answer),
	    sip_message(ack,Options#sip_phone_options{cseq=Cseq,
						      rtpremote_ip={RTP_IPaddress,RTP_Port},
						      state=outgoing_answer});
	"CANCEL"->
	    {"Unknown",Options};
	"INFO"->
	    {"Unknown",Options#sip_phone_options{cseq=Cseq}};
	"BYE"->
	    case get_bprtyinfo(RestMes,Options) of
		{[$ ,$",$B,$u,$s,$y|_],_,_}->%callback case
		   {Message1,Options2} = sip_stack:sip_message(sip200ok_invite,Options),
		   {Message2,Options3} = sip_stack:sip_message(sdp_phone,Options2),
		   Message3 = string:concat(Message1,Message2),
		   {Message3,Options3};
		{[$ ,$",$A,$c,$c,$e,$p,$t,$e,$d|_],_,_}->%service accepted
		   {"Unknown",Options};
		_->{"Unknown",Options}
	    end;
	_->
	    {"Unknown",Options}
    end;
%Via: SIP/2.0/UDP 130.100.182.126:58269;branch=77444.77443
%Record-Route: <sip:130.100.10.72:5060>
%To: <sip:41931@130.100.10.72>;tag=d335cf7f
%From: <sip:41931@130.100.10.72>;tag=737149
%Call-ID: 7745177450@130.100.182.126
%CSeq: 203 REGISTER
%User-Agent: MX-ONE/Resiprocate
%Content-Length: 0

sip_message([$S,$I,$P,$/,$2,$.,$0,$ ,$3|RestMes],Options) ->%3++ send ack
    {_Cseq,Token}=get_cseq(RestMes),
    %Int=string:str(RestMes,"CSeq:"),
    %To_string=string:sub_string(RestMes,Int),
    %[_A,_B,Token|_D] = string:tokens(To_string, " \r"),
    case Token of
	"INVITE"->
	    sip_message(ack,Options);
	_->
	    {"Unknown",Options}
    end;
sip_message([$S,$I,$P,$/,$2,$.,$0,$ ,$4,$8,$0|RestMes],Options) ->%Temporarily unavailable
    gen_server:cast(self(), {display,"Busy"}),
    gen_server:cast(self(), {tone,busy_tone}),
    Branch2=get_branch(RestMes),
    %BInt=string:str(RestMes,";branch="),
    %BTo_string=string:sub_string(RestMes,BInt),
    %[Branch2|_BD] = string:tokens(BTo_string, ";\r"),

    Int=string:str(RestMes,"CSeq:"),
    To_string=string:sub_string(RestMes,Int),
    [_A,_B,Token|_D] = string:tokens(To_string, " \r"),
    case Token of
	"INVITE"->
	    sip_message(ack_term,Options#sip_phone_options{branch=Branch2});
	_->
	    {"Unknown",Options}
    end;
sip_message([$S,$I,$P,$/,$2,$.,$0,$ ,$4,$8,$6|RestMes],Options) ->%Busy Here

%Via: SIP/2.0/UDP 130.100.182.126:58405;branch=911411.911409
%To: <sip:41932@130.100.10.72>;tag=f5b2a102
%From: <sip:41931@130.100.10.72>;tag=911422
%Call-ID: 911418911417@130.100.182.126
%CSeq: 272 INVITE
%User-Agent: MX-ONE/Resiprocate
%Content-Length: 0
    gen_server:cast(self(), {display,"Busy"}),
    gen_server:cast(self(), {tone,busy_tone}),
    {_Cseq,Token}=get_cseq(RestMes),
    %Int=string:str(RestMes,"CSeq:"),
    %To_string=string:sub_string(RestMes,Int),
    %[_A,_B,Token|_D] = string:tokens(To_string, " \r"),
    case Token of
	"INVITE"->
	    sip_message(ack_term,Options);
	_->
	    {"Unknown",Options}
    end;

sip_message([$S,$I,$P,$/,$2,$.,$0,$ ,$4,$8,$7|RestMes],#sip_phone_options{
	      state=outgoing_busy,
	      old_call_info = Old_call_info}
	    =Options) ->%request terminated

%Via: SIP/2.0/UDP 130.100.182.126:58405;branch=911411.911409
%To: <sip:41932@130.100.10.72>;tag=f5b2a102
%From: <sip:41931@130.100.10.72>;tag=911422
%Call-ID: 911418911417@130.100.182.126
%CSeq: 272 INVITE
%User-Agent: MX-ONE/Resiprocate
%Content-Length: 0
    gen_server:cast(self(), {tone,busy_tone}),
    {_Cseq,Token}=get_cseq(RestMes),
    %Int=string:str(RestMes,"CSeq:"),
    %To_string=string:sub_string(RestMes,Int),
    %[_A,_B,Token|_D] = string:tokens(To_string, " \r"),

    case Token of
	"INVITE"->
	    {Message1,Options1} = sip_stack:sip_message(sip200ok_invite,Options),
	    {Message2,Options2} = sip_stack:sip_message(sdp_phone,Options1),
	    Message3 = string:concat(Message1,Message2),
	    {BranchOld ,AnumberOld,Aprty_tagOld,BnumberOld,Bprty_tagOld,OldCseq,OldCallid} = Old_call_info,
	    {Message4,_OldOptions3}=sip_message(ack_term,Options2
							#sip_phone_options{
							branch=BranchOld,
							aprty_phoneno = AnumberOld,
							aprty_tag = Aprty_tagOld,
							bprty_phoneno = BnumberOld,
							bprty_tag = Bprty_tagOld,
							cseq = OldCseq,
							callid = OldCallid}),%ack old callid.
	    {[Message3,Message4],Options2};
	_->
	    {"Unknown",Options}
    end;

sip_message([$S,$I,$P,$/,$2,$.,$0,$ ,$4,$8,$7|RestMes],Options) ->%request terminated
%Via: SIP/2.0/UDP 130.100.182.126:58405;branch=911411.911409
%To: <sip:41932@130.100.10.72>;tag=f5b2a102
%From: <sip:41931@130.100.10.72>;tag=911422
%Call-ID: 911418911417@130.100.182.126
%CSeq: 272 INVITE
%User-Agent: MX-ONE/Resiprocate
%Content-Length: 0
%    gen_server:cast(self(), {display,"Congestion"}),
%    gen_server:cast(self(), {tone,congestion_tone}),
    {Cseq,Token}=get_cseq(RestMes),
    %Int=string:str(RestMes,"CSeq:"),
    %To_string=string:sub_string(RestMes,Int),
    %[_A,_B,Token|_D] = string:tokens(To_string, " \r"),

    case Token of
	"INVITE"->
	    {Message,Options1} = sip_stack:sip_message(ack_term,
					Options#sip_phone_options{cseq=Cseq}),
	    {Message,Options1};

	_->
	    {"Unknown",Options}
    end;

sip_message([$S,$I,$P,$/,$2,$.,$0,$ ,$4,$8,$8|RestMes],Options) ->
    gen_server:cast(self(), {display,"Busy"}),
    gen_server:cast(self(), {tone,busy_tone}),
    {Cseq,Token}=get_cseq(RestMes),
    %Int=string:str(RestMes,"CSeq:"),
    %To_string=string:sub_string(RestMes,Int),
    %[_A,_B,Token|_D] = string:tokens(To_string, " \r"),
    case Token of
	"INVITE"->
	    sip_message(ack_term,Options#sip_phone_options{cseq=Cseq});
	_->
	    {"Unknown",Options}
    end;

sip_message([$S,$I,$P,$/,$2,$.,$0,$ ,$4|RestMes],Options) ->%4++ send ack
%    gen_server:cast(self(), {display,"Busy"}),
    gen_server:cast(self(), {tone,congestion_tone}),
    
    Branch2=get_branch(RestMes),
    %Int=string:str(RestMes,";branch="),
    %To_string=string:sub_string(RestMes,Int),
    %[Branch2|_D] = string:tokens(To_string, ";\r"),

    TBInt=string:str(RestMes,";tag="),
    TBTo_string=string:sub_string(RestMes,TBInt),
    [TBpartyTag2|_TBRstD] = string:tokens(TBTo_string, "\r"),


    {Aname2,Anumber2,TApartyTag2} = get_aprtyinfo(RestMes,Options),

	%case string:str(RestMes,"\r\nFrom: <sip:") of
	%    0-> 
	%	AInt=string:str(RestMes,"\r\nFrom: "),
	%	ATo_string=string:sub_string(RestMes,AInt),
	%	[_AA,Aname,_AB,Anumber|AGatekeeprIPRst] = string:tokens(ATo_string, ":<>@"),	
	%	AGatekeeprIPjoin = string:join(AGatekeeprIPRst,", "),
	%	TAInt=string:str(AGatekeeprIPjoin,";tag="),
	%	TTo_string=string:sub_string(AGatekeeprIPjoin,TAInt),
	%	[TApartyTag1|_TRstD] = string:tokens(TTo_string, "\r"),
	%	{Aname,Anumber,TApartyTag1};
	%    I->
	%	ATo_string=string:sub_string(RestMes,I),
	%	[_AA,_AB,Anumber|AGatekeeprIPRst] = string:tokens(ATo_string, ":>@"),
	%	AGatekeeprIPjoin = string:join(AGatekeeprIPRst,", "),
	%	TInt=string:str(AGatekeeprIPjoin,";tag="),
	%	TTo_string=string:sub_string(AGatekeeprIPjoin,TInt),
	%	[TApartyTag1|_TRstD] = string:tokens(TTo_string, "\r"),
	%	{"",Anumber,TApartyTag1}
	%end,

    Callid = get_callid(RestMes),

    {Cseq,Token}=get_cseq(RestMes),
    %CInt=string:str(RestMes,"CSeq:"),
    %CTo_string=string:sub_string(RestMes,CInt),
    %[_CA,_CB,Token|_CD] = string:tokens(CTo_string, " \r"),
    case Token of
	"INVITE"->
	    {Message,_Options} = sip_message(ack_term,Options#sip_phone_options{
							   callid=Callid,
							   branch=Branch2,
							   cseq=Cseq,
							   aprty_tag=TApartyTag2,
							   aprty_name=Aname2,
							   aprty_phoneno=Anumber2,
							   bprty_tag=TBpartyTag2}),
	    {Message,Options};
	_->
	    {"Unknown",Options}
    end;

sip_message([$S,$I,$P,$/,$2,$.,$0,$ ,$5,$0,$3|RestMes],Options) ->%service Unavailable
    gen_server:cast(self(), {display,"Congestion"}),
    gen_server:cast(self(), {tone,congestion_tone}),
    {Cseq,Token}=get_cseq(RestMes),
    %Int=string:str(RestMes,"CSeq:"),
    %To_string=string:sub_string(RestMes,Int),
    %[_A,_B,Token|_D] = string:tokens(To_string, " \r"),
    case Token of
	"INVITE"->
	    sip_message(ack_term,Options#sip_phone_options{cseq=Cseq});
	_->
	    {"Unknown",Options}
    end;

sip_message([$S,$I,$P,$/,$2,$.,$0,$ ,$5|RestMes],Options) ->%5++ send ack
    gen_server:cast(self(), {display,"Congestion"}),
    gen_server:cast(self(), {tone,congestion_tone}),
    {Cseq,Token}=get_cseq(RestMes),
    %Int=string:str(RestMes,"CSeq:"),
    %To_string=string:sub_string(RestMes,Int),
    %[_A,_B,Token|_D] = string:tokens(To_string, " \r"),
    case Token of
	"INVITE"->
	    sip_message(ack_term,Options#sip_phone_options{cseq=Cseq});
	_->
	    {"Unknown",Options}
    end;

sip_message([$B,$Y,$E|RestMes],#sip_phone_options{
	      aprty_tag=_Aprty_tag,%Incoming disconnect
	      bprty_tag=_Bprty_tag,
	      branch=_Branch
	     } = Options) ->
    
    Branch2=get_branch(RestMes),
    {Cseq,_Token}=get_cseq(RestMes),
    gen_server:cast(self(), clear),
    sip_message(bye_200_ok,Options#sip_phone_options{branch=Branch2,
						     cseq=Cseq,
						     state=idle});

sip_message([$I,$N,$V,$I,$T,$E|RestMes],%INVITE
	    #sip_phone_options{
	      cseq = _CSeq,
	      callid = _Callid,
	      aprty_tag = _Aprty_tag,
	      branch = _Branch,
	      state=idle
	     }=Options) ->

    Branch2=get_branch(RestMes),
    %Int=string:str(RestMes,";branch="),
    %To_string=string:sub_string(RestMes,Int),
    %[Branch2|_D] = string:tokens(To_string, ";\r"),

    BInt=string:str(RestMes,"\r\nTo: <sip:"),
    BTo_string=string:sub_string(RestMes,BInt),
    [_BA,_BB,Bnumber,_BGatekeeprIP|_BRstD] = string:tokens(BTo_string, ":>@"),

    {Aname2,Anumber2,_AprtyTag} = get_aprtyinfo(RestMes,Options),
%	case string:str(RestMes,"\r\nFrom: <sip:") of
%	    0-> AInt=string:str(RestMes,"\r\nFrom: "),
%%		ATo_string=string:sub_string(RestMes,AInt),
%		[_AA,Aname,_AB,Anumber,_AGatekeeprIP|_ARstD] = string:tokens(ATo_string, ":<>@"),
%		{Aname,Anumber};
%	    I->I,
%	       ATo_string=string:sub_string(RestMes,I),
%		 [_AA,_AB,Anumber,_AGatekeeprIP|_ARstD] = string:tokens(ATo_string, ":>@"),
%	       {"",Anumber}
%	end,
    
    CoInt=string:str(RestMes,"Contact:"),
    CoTo_string=string:sub_string(RestMes,CoInt),
    [_CoA,_CoB,Contact,CoSipPort|_CoRstD] = string:tokens(CoTo_string, ":>"),

    CaTypestring2 = case string:str(RestMes,"call-type=") of
			0->"no_ring";
			CaTInt->
			    CaTTo_string=string:sub_string(RestMes,CaTInt+10),
			    [CaTypestring|_CaTRstD] = string:tokens(CaTTo_string, "\r"),
			    case CaTypestring of
				"callback" -> "call_back_ring";
				Ca -> Ca++"_ring"
			    end
		    end,

    TInt=string:str(RestMes,";tag="),
    TTo_string=string:sub_string(RestMes,TInt),
    [TApartyTag2|_TRstD] = string:tokens(TTo_string, "\r"),

    Callid2 = get_callid(RestMes),
    %CaInt=string:str(RestMes,"Call-ID:"),
    %CaTo_string=string:sub_string(RestMes,CaInt),
    %[_CaA,Callid2|_CaRstD] = string:tokens(CaTo_string, " \r"),

    {Cseq2,_Token} = get_cseq(RestMes),

    CIPInt=string:str(RestMes,"c=IN IP4"),
    CIPTo_string=string:sub_string(RestMes,CIPInt+9),
    [RTP_IPaddress|_CIPRstD] = string:tokens(CIPTo_string, "\r"),

    RTP_Port=
	case string:str(RestMes,"m=audio") of
	    0->"0";
	    MAInt->
		MATo_string=string:sub_string(RestMes,MAInt),
		[_MAA,RTP_Port1|_MARstD] = string:tokens(MATo_string, " "),
		gen_server:cast(self(), {start_rtp,{RTP_IPaddress,list_to_integer(RTP_Port1)}}),
		RTP_Port1
	end,
    Int2=string:str(RestMes,"\r\n\r\n"),
    SDP_string=string:sub_string(RestMes,Int2),
    Options1=Options#sip_phone_options{
	       branch = Branch2,
	       callid = Callid2,
	       cseq = Cseq2,
	       bprty_phoneno = Bnumber,
	       aprty_phoneno = Anumber2,
	       aprty_name = Aname2,
	       aprty_tag = TApartyTag2,
	       sdp_string = SDP_string,
	       contact = Contact ++ ":" ++ CoSipPort
	      },
    
    {Message,Options2}=sip_message(sip100_trying,Options1),
    {Message2,Options3}=sip_message(sip180_ringing,Options2),
    gen_server:cast(self(), {ringType_info, list_to_atom(CaTypestring2)}),
    gen_server:cast(self(), {anumber,{Aname2,Anumber2}}),

    {[Message,Message2],Options3#sip_phone_options{rtpremote_ip={RTP_IPaddress,RTP_Port},
						   state = incomming_ring}};

sip_message([$I,$N,$V,$I,$T,$E|_RestMes],%INVITE message is sent again for some reason
	    #sip_phone_options{
%	      cseq = CSeq,
	      callid = _Callid,
	      aprty_tag = _Aprty_tag,
	      branch = _Branch,
	      state=incomming_ring
	     }=Options) ->
    

    {Message,Options2}=sip_message(sip100_trying,Options),
    {Message2,Options3}=sip_message(sip180_ringing,Options2),
%    gen_server:cast(self(), {ringType_info, list_to_atom(CaTypestring2)}),
%    gen_server:cast(self(), {anumber,{Aname2,Anumber2}}),

    {[Message,Message2],Options3};

sip_message([$I,$N,$V,$I,$T,$E|RestMes],
	    #sip_phone_options{
	      rtplocal_port = {Rtp_port_old,_RTPsock,_RTP_Pid},
	      cseq = CSeq,
	      callid = Callid,
	      aprty_tag = Aprty_tag,
	      bprty_tag = Bprty_tag,
	      branch = Branch,
	      bprty_phoneno = Bnumber,
	      aprty_phoneno = Anumber,
	      state=incomming_answer
	     }=Options) ->
    
    {Aname2,Anumber2,TApartyTag2} = get_aprtyinfo(RestMes,Options),
    {Bname2,Bnumber2,TBpartyTag2} = get_bprtyinfo(RestMes,Options),
    RTP_Port=
	case string:str(RestMes,"m=audio") of
	    0->"0";
	    MAInt->
		CIPInt=string:str(RestMes,"c=IN IP4"),
    		CIPTo_string=string:sub_string(RestMes,CIPInt+9),
    		[RTP_IPaddress|_CIPRstD] = string:tokens(CIPTo_string, "\r"),

		MATo_string=string:sub_string(RestMes,MAInt),
		[_MAA,RTP_Port1|_MARstD] = string:tokens(MATo_string, " "),
		gen_server:cast(self(), {start_rtp,{RTP_IPaddress,list_to_integer(RTP_Port1)}}),
		RTP_Port1
	end,
    Branch2=get_branch(RestMes),
    {Cseq2,_Token}=get_cseq(RestMes),
    Callid2 = get_callid(RestMes),
    Options1=Options#sip_phone_options{
	      aprty_tag = TApartyTag2,
	      bprty_tag = TBpartyTag2,
	      bprty_phoneno = Bnumber2,
	      aprty_phoneno = Anumber2,
	      branch = Branch2,
	      cseq = Cseq2,
	      callid = Callid2,
	      old_call_info = {Branch,Anumber,Aprty_tag,Bnumber,Bprty_tag,CSeq,Callid}},
    {Message6,Message7,Message8,Options4} =
        case string:str(RestMes,"Replaces:") of
	    0->
		{"Unknown","Unknown","Unknown",Options1};
            _ReplInt->
		{Message1,Options2}=sip_message(sip100_trying,Options1),
	    	{Message2,Options3}=sip_message(sip180_ringing,Options2),
	    	{Message3,_Options5}=sip_message(bye_switchAB,Options),%send bye
								       %swith old data
	    	{Message1,Message2,Message3,Options3}
	end,
    {Message9,Options5} = sip_stack:sip_message(sip200ok_after_answer,Options4),
    {Message10,Options6} = sip_stack:sip_message(sdp_phone,Options5),
    Message11 = string:concat(Message9,Message10),

    {[Message6,Message7,Message8,Message11],Options6};

sip_message([$I,$N,$V,$I,$T,$E|RestMes],
	    #sip_phone_options{
	      cseq = CSeq,
	      callid = Callid,
	      aprty_tag = Aprty_tag,
	      bprty_tag = Bprty_tag,
	      branch = Branch,
	      bprty_phoneno = Bnumber,
	      aprty_phoneno = Anumber,
	      state=outgoing_answer
	     }=Options) ->
    
    {Aname2,Anumber2,TApartyTag2} = get_aprtyinfo(RestMes,Options),
    {Bname2,Bnumber2,TBpartyTag2} = get_bprtyinfo(RestMes,Options),
    Branch2=get_branch(RestMes),
    {Cseq2,_Token}=get_cseq(RestMes),
    Options1=Options#sip_phone_options{
	      aprty_tag = TApartyTag2,
	      bprty_tag = TBpartyTag2,
	      bprty_phoneno = Bnumber2,
	      aprty_phoneno = Anumber2,
	      branch = Branch2,
	      cseq = Cseq2,
	      old_call_info = {Branch,Anumber,Aprty_tag,Bnumber,Bprty_tag,CSeq,Callid}},
    {Message3,Options2} = sip_stack:sip_message(sip200ok_after_answer,Options1),
    {Message4,Options3} = sip_stack:sip_message(sdp_phone,Options2),
    Message5 = string:concat(Message3,Message4),

    {Message5,Options3};

sip_message([$I,$N,$V,$I,$T,$E|RestMes],%INVITE
	    #sip_phone_options{
	      cseq = Cseq,
	      callid = Callid,
	      bprty_phoneno = Bnumber,
	      bprty_tag = Bprty_tag,
	      aprty_phoneno = Anumber,
	      aprty_tag = Aprty_tag,
	      branch = Branch,
	      state = _State
	     }=Options) ->

    Branch2=get_branch(RestMes),
    %Int=string:str(RestMes,";branch="),
    %To_string=string:sub_string(RestMes,Int),
    %[Branch2|_D] = string:tokens(To_string, ";\r"),

    BInt=string:str(RestMes,"\r\nTo: <sip:"),
    BTo_string=string:sub_string(RestMes,BInt),
    [_BA,_BB,Bnumber2,_BGatekeeprIP|_BRstD] = string:tokens(BTo_string, ":>@"),

    {Aname2,Anumber2,TApartyTag2} = get_aprtyinfo(RestMes,Options),

    Callid2 = get_callid(RestMes),
    %CaInt=string:str(RestMes,"Call-ID:"),
    %CaTo_string=string:sub_string(RestMes,CaInt),
    %[_CaA,Callid2|_CaRstD] = string:tokens(CaTo_string, " \r"),

    {Cseq2,_Token}=get_cseq(RestMes),
    %CInt=string:str(RestMes,"CSeq:"),
    %CTo_string=string:sub_string(RestMes,CInt),
    %[_CA,Cseq2|_CRstD] = string:tokens(CTo_string, " "),

    Int2=string:str(RestMes,"\r\n\r\n"),
    SDP_string=string:sub_string(RestMes,Int2),
    Options1=Options#sip_phone_options{
	       branch = Branch2,
	       callid = Callid2,
	       cseq = Cseq2,
	       bprty_phoneno = Bnumber2,
	       aprty_phoneno = Anumber2,
	       aprty_name = Aname2,
	       aprty_tag = TApartyTag2,
	       sdp_string = SDP_string,
	       old_call_info = {Branch,Anumber,Aprty_tag,Bnumber,Bprty_tag,Cseq,Callid}
	      },

    {Message11,Message21,Options4} = 
	case string:str(RestMes,"refresher=uas") of
	    0->
		{Message1,Options2}=sip_message(sip100_trying,Options1),
		{Message2,Options3}=sip_message(sip180_ringing,Options2),
		{Message1,Message2,Options3};
	    _->

		CIPInt=string:str(RestMes,"c=IN IP4"),
		CIPTo_string=string:sub_string(RestMes,CIPInt+9),
		[RTP_IPaddress|_CIPRstD] = string:tokens(CIPTo_string, "\r"),
		
		MAInt=string:str(RestMes,"m=audio"),
		MATo_string=string:sub_string(RestMes,MAInt),
		[_MAA,RTP_Port|_MARstD] = string:tokens(MATo_string, " "),    

		gen_server:cast(self(), {start_rtp,{RTP_IPaddress,list_to_integer(RTP_Port)}}),
		{"Unknown","Unknown",Options1
		 #sip_phone_options{rtpremote_ip={RTP_IPaddress,RTP_Port}
				    }}
	end,
    {Message31,Options7}=
	case string:str(RestMes,"call-type=") of
	    0->	 %cancel old callid.
		case string:str(RestMes,"Replaces:") of
		    0->	
			{Message3,Options5} = sip_stack:sip_message(sip200ok_invite,Options4),
			{Message4,Options6} = sip_stack:sip_message(sdp_phone,Options5),
			Message5 = string:concat(Message3,Message4),
			{Message5,Options6};
		    _ReplInt->
		        case string:str(RestMes,"early-only") of
			   0->
%			      timer:sleep(200),
			      {Message3,_Options5}=sip_message(bye_switchAB,Options),%send bye
									 %with old data
			      {Message3,Options4};

			   _->
			      {Message3,_Options5}=sip_message(cancel,Options),%send cancel
									    %with old data
			      {Message3,Options4}
			end
		end;
	    CaTInt->
		CaTTo_string=string:sub_string(RestMes,CaTInt+10),
		[CaTypestring|_CaTRstD] = string:tokens(CaTTo_string, "\r"),
		case CaTypestring of
		    "callback" -> 
			{Message3,Options5} = sip_stack:sip_message(sip200ok_invite,Options4),
			{Message4,Options6} = sip_stack:sip_message(sdp_phone,Options5),
			Message5 = string:concat(Message3,Message4),
			{Message5,Options6};
		    _Ca ->
			{"Unknown",Options4}
		end
	end,
    
%    gen_server:cast(self(), {ringType_info, list_to_atom(CaTypestring2)}),
    gen_server:cast(self(), {anumber,{Aname2,Anumber2}}),

    {[Message11,Message21,Message31],Options7#sip_phone_options{
				       state = outgoing_busy}};

sip_message([$C,$A,$N,$C,$E,$L,$ |RestMes],Options) -> %cancel incomming call before answer
%   CANCEL sip:41932@130.100.182.126:49455;user=phone SIP/2.0
%Via: SIP/2.0/UDP 130.100.10.72:5060;branch=z9hG4bK-d87543-f257ff23125f072c-1
%   --d87543-;rport
%To: <sip:41932@130.100.10.72;user=phone>
%From: <sip:41931@130.100.10.72;user=phone>;tag=405ae353
%Call-ID: ZmEwMjczNzAxNzI3ZTA1MWNhZTM3OWFkOGZmYjQ1OWM.
%CSeq: 1 CANCEL
%User-Agent: MX-ONE/Resiprocate   
%Content-Length: 0

    Branch = get_branch(RestMes),
    {Aname,Anumber,ApartyTag} = get_aprtyinfo(RestMes,Options),
    Callid = get_callid(RestMes),
    {Cseq,_Token}=get_cseq(RestMes),

    {Message1,Options1}=sip_message(cancel_200_ok,Options#sip_phone_options{
	branch=Branch,
	aprty_name = Aname,
	aprty_phoneno = Anumber,
	aprty_tag=ApartyTag,
	callid=Callid,
	cseq=Cseq
	}),
    {Message2,Options2}=sip_message(reqterm_487,Options1),
    gen_server:cast(self(), clear),    
    {[Message1,Message2],Options2};

sip_message(RestMes,Options) when list(RestMes) -> 
    {Cseq,Token}=get_cseq(RestMes),
    %Int=string:str(RestMes,"CSeq:"),
    %To_string=string:sub_string(RestMes,Int),
    %[_A,_B,Token|_D] = string:tokens(To_string, " \r"),
    case Token of
	"INVITE"->
	    sip_message(ack,Options#sip_phone_options{cseq=Cseq});
	_->
	    {"Unknown",Options}
    end;

sip_message(RestMes,Options) -> 
    ?DEBUG_PRINT("~w: got unknown sip_message: ~w~n",[?MODULE, RestMes]),
    {"Unknown",Options}.

sip_version() ->
   "SIP/2.0". 

get_SDPcallid() ->
    {_,_,Branch1} = now(),
    {_,_,Branch2} = now(),
    integer_to_list(Branch2) ++ string:substr(integer_to_list(Branch1),2,4).

get_Callid() ->
    {_,_,Branch1} = now(),
    {_,_,Branch2} = now(),
    integer_to_list(Branch2) ++"-1s3f5." ++ integer_to_list(Branch1).

get_Branch() ->
    {_,_,Branch1} = now(),
    {_,_,Branch2} = now(),
    ";branch=z9hG4bK-" ++ integer_to_list(Branch2) ++ ".fhjd." ++ integer_to_list(Branch1).

get_Tag() ->
    {_,Tag_int2,Tag_int} = now(),
    ";tag=" ++ integer_to_list(Tag_int) ++ "-bc34." ++ integer_to_list(Tag_int2).	%tag=456248

get_wapref() ->
    {_,_,Wap_int} = now(),
    ";wap-callref=" ++ integer_to_list(Wap_int).			%wap-callref=456248

get_random_4digit() ->
    lists:map(fun(X) ->
		      43 + (X band 2#11111)
	      end, tuple_to_list(now())).
get_CSeq() ->
    lists:map(fun(X) ->
		      49 + (X band 2#111)
	      end, tuple_to_list(now())).
step_CSeq(#sip_phone_options{cseq=CSeq} = _Options) ->
    CSeqInt = list_to_integer(CSeq) + 1,
    integer_to_list(CSeqInt).



get_CallInfo(#sip_phone_options{wsp_port = WAP_port} = Options) ->
    Wsp_port = integer_to_list(WAP_port),
    "<wap://" ++ gatekeeper_ipaddress(Options) ++ ":" ++ Wsp_port ++ ">;purpuse=essp;ui=3".
get_UserAgent(#sip_phone_options{sw_version = UserAgent} = _Options) ->
    "User-Agent: Ericsson DBC425_02 " ++ UserAgent ++ "\r\n".

get_sip_port(#sip_phone_options{sip_port = Sip_port} = _Options) ->
    Sip_port.                                                  %5060.

%get_sip_port_string(#sip_phone_options{sip_port = Sip_port} = _Options) ->
%    integer_to_list(Sip_port).                                                  %"5060".

gatekeeper_ipaddress(#sip_phone_options{gatekeeper_host = Gatekeeper_ip} = _Options) ->
    Gatekeeper_ip.                                                  %"130.100.10.72".

phone_ipaddress_port(#sip_phone_options{
phone_port = Phone_port,
client_ipaddress = Client_ip} = _Options) ->
    Phone_portno = integer_to_list(Phone_port),
    Client_ip ++ ":" ++ Phone_portno.           %"130.100.56.207:xxx".  %bobspc.biloxi.com:5060 
						%h323_client:get_local_ipAddress()
						%get_tag_option(phone_portno,StartOptions,{}),

phone_ipaddress_wspport(#sip_phone_options{
wsp_port = Wsp_port,
client_ipaddress = Client_ip} = _Options) ->
    Wsp_portno = integer_to_list(Wsp_port),
    Client_ip ++ ":" ++ Wsp_portno.           %"130.100.56.207:9201".

phone_portno(#sip_phone_options{phone_port = Phone_port
			       } = _Options) ->
    _Phone_portno = integer_to_list(Phone_port).

phone_ipaddress(#sip_phone_options{client_ipaddress = Client_ip
				  } = _Options) ->
    Client_ip.

bprty_phone_number_ip(#sip_phone_options{bprty_phoneno = Phone_number,
				       gatekeeper_host = Gatekeeper_ip
				       } = _Options) ->
    Phone_number ++ "@" ++ Gatekeeper_ip.        %41931@130.100.10.72".	%Alice <sip:alice@biloxi.com

aprty_phone_number_ip(#sip_phone_options{aprty_phoneno = Phone_number,
					 gatekeeper_host = Gatekeeper_ip
					} = _Options) ->
    Phone_number ++ "@" ++ Gatekeeper_ip.        %41932@130.100.10.72>".	%Bob <sip:bob@biloxi.com

own_phone_number_ip(#sip_phone_options{own_phoneno = Phone_number,
				    gatekeeper_host = Gatekeeper_ip
				} = _Options) ->
    Phone_number ++ "@" ++ Gatekeeper_ip.        %41932@130.100.10.72>".	%Bob <sip:bob@biloxi.com

own_phone_number(#sip_phone_options{own_phoneno = Phone_number
			       } = _Options) ->
    Phone_number.                                %41932".	%Bob <sip:bob@biloxi.com

int_to_string(Int) ->
    lf:sformat("~w",[Int]).

get_aprtyinfo(RestMes,#sip_phone_options{aprty_name = _Aname_old} = _Options)->
    AInt = string:str(RestMes,"\r\nFrom: "),
    ATo_string=string:sub_string(RestMes,AInt),
    [_AA,Aname,_AB,Anumber,_Aparty_IP,TApartyTag1|_AGatekeeprIPRst] = string:tokens(ATo_string, ":<>@\r\n"),
    {Aname,Anumber,TApartyTag1}.

get_bprtyinfo(RestMes, _Options)->
    BInt = string:str(RestMes,"\r\nTo: "),
    BTo_string=string:sub_string(RestMes,BInt),
    [_BA,Bname,_BB,Bnumber,_Bparty_IP,TBpartyTag1|_BGatekeeprIPRst] = string:tokens(BTo_string, ":<>@\r\n"),
    {Bname,Bnumber,TBpartyTag1}.
   
get_branch(RestMes)->
    Int=string:str(RestMes,";branch="),
    To_string=string:sub_string(RestMes,Int),
    [Branch|_D] = string:tokens(To_string, ";\r"),
    ";"++Branch.

get_callid(RestMes)->
    CaInt=string:str(RestMes,"Call-ID:"),
    CaTo_string=string:sub_string(RestMes,CaInt),
    [_CaA,Callid2|_CaRstD] = string:tokens(CaTo_string, " \r"),
    Callid2.

get_cseq(RestMes)->
    Int=string:str(RestMes,"CSeq:"),
    To_string=string:sub_string(RestMes,Int),
    [_A,Cseq,Token|_D] = string:tokens(To_string, " \r"),
    {Cseq,Token}.
%%----------------------------E-N-D---O-F---F-I-L-E---------------------------%%
