%% +---------------------------------------------------------------------------+
%% | File:         sip_phone.erl					               |
%% +---------------------------------------------------------------------------+
%% | Author:       Romin Per                             		       |
%% +---------------------------------------------------------------------------+
%% | Description: 							       |
%% |               Implementation of the sip_phone                             |
%% |                                                                           |
%% +---------------------------------------------------------------------------+

-module(sip_phone).

-behaviour(gen_server).

%% This module implements the sip_phone. This is a really simple phone that can 
%% do no callback (as for now). The sip funktionality is made with two different
%% modules. sip_phone.erl and sip_stack.erl. The sip_stack module handles the stack
%% which means, the messages that are sent between GDS-X and the Gatekeeper, in this
%% case the MX-ONE. The sip_phone handles the state mashine.

%% -----------------------------------------------------------------------------
%% API exports
%% -----------------------------------------------------------------------------
-export([start/1,start_link/1,l/0]).
-export([quit/1,send_host/4,attach_phone/2,
	 date/1,format_string/2]).
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% gen_server exports.
%% -----------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([code_change/3]).
-import(h323_client, [get_time_str/0, get_time_lst/0, get_local_ipAddress/0, get_16byte/0]).
%% -----------------------------------------------------------------------------
-export([register_loop/1,date_loop/1,handle_button_offhook/1]).
%% -----------------------------------------------------------------------------
%% Constants.
%% -----------------------------------------------------------------------------
-define(BUFFER_TIME,5000).                  % Keep strings in buffer in millisec.

%% -----------------------------------------------------------------------------
%% Include files.
%% -----------------------------------------------------------------------------
-include("h323_phone_options.hrl").
-include("../elu/elu.hrl").
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% Record definitions.
%% -----------------------------------------------------------------------------
-record(gs_ids, {win,
		 display1,
		 display2,
		 display3,
		 display4,
		 display5,
		 display6,
		 display7,
		 hook,
		 quit,
		 ring_type,
		 state,
		 options,
		 rtpinfo,
		 register,
		 peer_user,
		 peer_prod,
		 peer_vers,
		 e164,
		 gk_id,
		 auto_answer,
		 button_speaker,
		 button_clear,
		 button_f1,
		 button_f2,
		 button_f3,
		 button_f4,
		 button_1,
		 button_2,
		 button_3,
		 button_4,
		 button_5,
		 button_6,
		 button_7,
		 button_8,
		 button_9,
		 button_star,
		 button_0,
		 button_hash}).
%% -----------------------------------------------------------------------------

-record(gdsx_info, {dtmf,
		    payload_type,
		    packet_type,
		    data,
		    node_equ}).

-record(phone_state, 
	{gs_pid, 
	 gs_ids=#gs_ids{},
	 options = #sip_phone_options{},
	 filename = "h323_phone_options.ini",
	 logname = "",
	 gdsx_info = #gdsx_info{},
	 filedesc,
	 socket,
	 wsp_socket,
	 option_pid,
	 phone_pid,
	 gui_pid,
	 registered,
	 dialled_no = "",
	 dts_status = {replace, no_ring,[],[],{t,e,m}},%{Handset, Ringstate, Ledstate, Display, {Tone, Language, Msn}}
	 gdr_state,
	 stable_delay,
	 interdigit_delay,
	 instr=[],              

	 buffer_namelist = [],
	 state=idle,
	 buffer=void,                       % Here we collect incomming strings.
	 next_slot=1,                       % Points to next unused slot.
	 oldest_slot=0,                     % Points to slot behind oldest in use.
	 register_loop_pid}).
%% -----------------------------------------------------------------------------



%% =============================================================================
%% API functions
%% =============================================================================

%% This API was introduced when it was discovered that sm_config actually does not
%% create any kind of supervisor process. The process creating the sip_phone process
%% simply terminates, causing problems here since this process then believes its
%% supervisor to have terminated. This problem accelerates when this process
%% starts to trap exists.
start(Options) ->
    gen_server:start(?MODULE,Options,[]).
%% -----------------------------------------------------------------------------

start_link(Options) ->
    gen_server:start_link(?MODULE,Options,[]).
%% -----------------------------------------------------------------------------

quit(Pid) when pid(Pid) ->
    gen_server:cast(Pid,quit).
%% -----------------------------------------------------------------------------

%% send_host(Socket,Strings)=nothing significant
%%   Socket=phone_communication_handler()
%%   Strings=[String,...], list()
%%     String=string()
%% API sending the strings in Strings to the phone using the communication handler.
%% Returns nothing significant.
send_host(Socket,Options,Strings,GdrState) ->
    send_host(Socket,Options,Strings,[],GdrState).

send_host(Socket,Options,["Unknown"|Rest],Mes,GdrState) ->
    send_host(Socket,Options,Rest,Mes,GdrState);
send_host(Socket,Options,[String|Rest],_M,GdrState) ->
    Mes = send_to_host(Socket,Options,String,GdrState),
    timer:sleep(200),
    send_host(Socket,Options,Rest,Mes,GdrState);
send_host(_Socket,_Options,[],Mes,_GdrState) ->
    Mes.
%% -----------------------------------------------------------------------------

%% attach_phone(Parent,Socket)=ok | {error,Reason}
%%   Parent=address to sip_phone process, pid().
%%   Socket=phone_communication_handler()
%% Attaches the calling process as a process allowed to send strings to the
%% communication channel. Note that in some situations nothing is actually done!
attach_phone(_Parent,{telnet,_}) ->         % Not necessary to do anything!
    ok;
attach_phone(_Parent,{ssh,{CM,_Channel}}) ->
    ok=ssh_cm:attach(CM,5000);
attach_phone(_Parent,Socket) ->
    {error,{unknown_phonehandler,Socket}}.
%% -----------------------------------------------------------------------------


%% =============================================================================
%% gen_server call-backs.
%% =============================================================================

init([IpID,Type,StartOptions]) ->
    process_flag(trap_exit,true),
    {FileName,{Options,GdrState}}=
	case lists:keysearch(filename,1,StartOptions) of
	    {value,{_,FileN}} when list(FileN) ->
		{FileN,{read_options(FileN),undefined}};
	    false ->
		{"",start_options(IpID,Type,StartOptions)}
	end,
    GdrState1=
	case Type of
	    T when atom(T) ->
		GdrState#gdr_state{type=Type};
	    _->
		GdrState
	end,
    {ok,PhoneSocket}=gen_udp:open(Options#sip_phone_options.phone_port,[{reuseaddr,true}]),
    {ok,Phone_port} = inet:port(PhoneSocket),
    Ledstate=h323_phone:indicators(GdrState1#gdr_state.type),
    Display = h323_phone:display(GdrState1#gdr_state.type),
    Dts_status = {replace, no_ring, Ledstate, Display, {t,e,m}},
    eaa_dts:gdr_update(Dts_status, GdrState1),

    {LogName,FileDesc}=open_logfile(StartOptions),
    Wsp_port = Options#sip_phone_options.wsp_port,
    {ok,WspSocket}=gen_udp:open(Wsp_port,[{reuseaddr,true}]),
    {ok,Wsp_port2} = inet:port(WspSocket),
    {ok,#phone_state{
       options=Options#sip_phone_options{wsp_port=Wsp_port2,
					 phone_port = Phone_port
					},
       dts_status = Dts_status,
       registered=false,
       socket=PhoneSocket,
       wsp_socket=WspSocket,
       filename=FileName,
       logname=LogName,
       filedesc=FileDesc,
       gdr_state=GdrState1,
       buffer=mk_buffer()
      }};

init(StartOptions) ->
    init(["IpID",dbc42502,StartOptions]).
%% -----------------------------------------------------------------------------

start_options(IpID,_Type,StartOptions) ->
    %Version=get_tag_option(version,StartOptions,version_2),
%    Faststart=get_tag_option(fastStart,StartOptions,{fastStart,true}),
    Version=get_tag_option(sw_version,StartOptions,"HWver-R2B ApplicRev-R4C BootRev-R3C"),
    Client_ip=get_tag_option(client_ipaddress,
				   StartOptions,
				   fun() ->
					   list_to_string(h323_client:get_local_ipAddress())
				   end),
    Sip_port=get_tag_option(sip_port,StartOptions,5060),
    Phone_port=get_tag_option(phone_port,StartOptions,0),%0 if free one is choosed 5060 if std.
    Own_phoneno=get_tag_option(own_phoneno,StartOptions,{own_phoneno,"41931"}),
    Wsp_port=get_tag_option(wsp_port,StartOptions,0),
    Rtp_feature=get_tag_option(rtp_feature,StartOptions,[{match,true}]),
    Gatekeeper_host=get_tag_option(gatekeeper_host,StartOptions,"130.100.10.72"),
    {#sip_phone_options{sw_version=Version,
			client_ipaddress=Client_ip,
			gatekeeper_host=Gatekeeper_host,
			sip_port=Sip_port,
			phone_port=Phone_port,
			own_phoneno=Own_phoneno,
			wsp_port=Wsp_port,
			rtp_feature=Rtp_feature
		       },
     #gdr_state{cat=phone,
		type=sip_phone,
		id=IpID++" "++Own_phoneno,
		comment="",
		connected_to=""}}.
%% -----------------------------------------------------------------------------


%% -----------------------------------------------------------------------------
%% gen_server handle call function.
%% -----------------------------------------------------------------------------

%% Start of Erlang phone
handle_call(start_gs,_From,State) ->
%    case os:getenv("DEBUG") of
%	"on" ->
%	    {GS_Pid, GS_IDs} = display_phone(),
%	    #gs_ids{state=DisplayState} = GS_IDs,
%	    gs:config(DisplayState, {label, {text, lf:sformat("idle ",[])}}),
%	    {reply,State#phone_state.phone_pid,State#phone_state{gs_pid=GS_Pid,gs_ids=GS_IDs}};
%	_ ->
    {reply, State#phone_state.phone_pid,State};

%    end;

handle_call(option_closed,_From,State) ->
    {reply,ok,State#phone_state{option_pid=undefined}};

handle_call({set_options,Options},_From,State) ->
    handle_set_options(Options,State),
    {reply,ok,State#phone_state{options=Options}};

handle_call(Request,From,State) ->
    ?DEBUG_PRINT("unknown call:~w ~w~n",[From, Request]),
    {reply,ok,State}.
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% gen_server handle cast function.
%% -----------------------------------------------------------------------------

handle_cast(quit,State) ->
    {stop,normal,State};

handle_cast(option_closed,State) ->
    {noreply,State#phone_state{option_pid=undefined}};

handle_cast({peer_info,PeerInfo},State) ->
    ?DEBUG_PRINT("peer_info:~w~n",[PeerInfo]),
    {noreply,handle_peerInfo(PeerInfo, State)};

handle_cast(set_stable_delay,State) ->
    Stable_delay=eaa_common:get_default_stable_delay(),
    InterDigit_delay=eaa_common:get_default_inter_digit_delay(),
    {noreply,State#phone_state{stable_delay=Stable_delay,interdigit_delay=InterDigit_delay}};

%% This is receiving the final result to a profit command from the
%% oip_instr process. Note that Slot indicates the youngest string slot
%% consumed by the instruction. Hence all slots from it and older can be
%% removed now.
%handle_cast({instruction_result,Result,Slot,OipInstr},State=#phone_state{instr=Instr}) ->
%    case lists:keysearch(OipInstr,3,Instr) of
%	{value,{Cmd,From,OipInstr}} ->      % It is one of our oip_instr.
%	    ?DEBUG_PRINT("Instruction result with slot ~w received from:~w~n",
%			 [Result,OipInstr]),
%	    timer:sleep(State#phone_state.stable_delay),
%	    eaa_common:instruction_res(From,Cmd,Result),
%	    NewInstr=lists:keydelete(OipInstr,3,Instr),
%	    {NewBuffer,NewNextSlot,NewOldestSlot}=
%		trim_buffer(State#phone_state.buffer,
%			    State#phone_state.next_slot,
%			    State#phone_state.oldest_slot,
%			    Slot,
%			    length(NewInstr)), % The number of active oip_instr.
%	    {noreply,State#phone_state{instr=NewInstr,
%				       buffer=NewBuffer,
%				       next_slot=NewNextSlot,
%				       oldest_slot=NewOldestSlot}};
%	false ->                            % Strange, but ignore it.
%	    ?DEBUG_PRINT("Received unknown instruction_result ~p from:~p~n",[Result,OipInstr]),
%	    {noreply,State}
%    end;
%%% As above but we do not get an indication to which was the youngest string consumed
%%% by the oip_instr process. That implies that we have to scratch the entire buffer.
%%% This alternative is most commonly used in a time-out situation in oip_instr.
%handle_cast({instruction_result,Result,OipInstr},State=#phone_state{instr=Instr}) ->
%    case lists:keysearch(OipInstr,3,Instr) of
%	{value,{Cmd,From,OipInstr}} ->      % It is one of our oip_instr.
%	    ?DEBUG_PRINT("Instruction result w/o slot ~w received from:~w~n",
%			 [Result,OipInstr]),
%	    timer:sleep(State#phone_state.stable_delay),
%	    eaa_common:instruction_res(From,Cmd,Result),
%	    NewInstr=lists:keydelete(OipInstr,3,Instr),
%	    {NewBuffer,NewNextSlot,NewOldestSlot}=
%		trim_buffer(State#phone_state.buffer,
%			    State#phone_state.next_slot,
%			    State#phone_state.oldest_slot,
%			    State#phone_state.next_slot-1, % Discard entire buffer!
%			    length(NewInstr)), % The number of active oip_instr.
%	    {noreply,State#phone_state{instr=NewInstr,
%				       buffer=NewBuffer,
%				       next_slot=NewNextSlot,
%				       oldest_slot=NewOldestSlot}};
%	false ->                            % Strange, but ignore it.
%	    ?DEBUG_PRINT("Received unknown instruction_result ~p from:~p~n",[Result,OipInstr]),
%	    {noreply,State}
%    end;
%handle_cast({instruction_noop_result,Result,OipInstr},State=#phone_state{instr=Instr}) ->
%    case lists:keysearch(OipInstr,3,Instr) of
%	{value,{Cmd,From,OipInstr}} ->      % It is one of our oip_instr.
%	    ?DEBUG_PRINT("Instruction without slot consumption, ~w received from:~w~n",
%			 [Result,OipInstr]),
%	    timer:sleep(State#phone_state.stable_delay),
%	    eaa_common:instruction_res(From,Cmd,Result),
%	    NewInstr=lists:keydelete(OipInstr,3,Instr),
%	    {noreply,State#phone_state{instr=NewInstr}};
%	false ->                            % Strange, but ignore it.
%	    ?DEBUG_PRINT("Received unknown instruction_noop_result ~p from:~p~n",[Result,OipInstr]),
%	    {noreply,State}
%    end;

handle_cast(clear, State) ->
    handle_button_clear(State);

handle_cast({display,Disp},#phone_state{dts_status = Dts_status,
				   gdr_state=GdrState
				  } = State) ->
    Dts_status1=write_display(Disp,{0,2},Dts_status,GdrState),
    {noreply, State#phone_state{dts_status = Dts_status1}};

handle_cast({tone,TT},#phone_state{dts_status = Dts_status,
				   gdr_state=_GdrState
				  } = State) ->
    {H, R, L, D, {_T,La,M}} = Dts_status,
    {noreply, State#phone_state{dts_status = {H, R, L, D, {TT,La,M}}}};

handle_cast({ringType_info, RT},#phone_state{dts_status = Dts_status,
					     gdr_state=GdrState
					    } = State) ->
    Dts_status2 = 
	eaa_dts:ringstate(RT, Dts_status, GdrState),
    Dts_status3 = case RT of 
	no_ring ->
	    Dts_status2;
	_->
	    led(11,slow,Dts_status2,GdrState)
    end,
    {noreply, State#phone_state{dts_status = Dts_status3}};

handle_cast({start_rtp, {RTP_IP,RTP_Port}},#phone_state{options = #sip_phone_options{
							  rtp_feature = RTP_feature} = Options
						       } = State) ->
    {RTPport,RTPsock,RTP_Pid} = h323_client:open_rtp({string_to_tuple(RTP_IP), RTP_Port, 0, RTP_feature}),
    {noreply, State#phone_state{options = Options#sip_phone_options{rtpremote_port={RTPport,RTPsock,RTP_Pid}}}};

handle_cast({own_phoneno, "", Own_phoneno},#phone_state{dts_status = Dts_status,
						   gdr_state=GdrState
						  } = State) ->
    Dts_status1=write_display(Own_phoneno,{2,2},Dts_status,GdrState),
    {noreply, State#phone_state{dts_status = Dts_status1}};
handle_cast({own_phoneno, Own_name, Own_phoneno},#phone_state{dts_status = Dts_status,
						   gdr_state=GdrState
						  } = State) ->
    Dts_status1=write_display(Own_name ++ " " ++ Own_phoneno,{2,2},Dts_status,GdrState),
    {noreply, State#phone_state{dts_status = Dts_status1}};

handle_cast(answer,#phone_state{dts_status = Dts_status,
				gdr_state=GdrState,
				options= #sip_phone_options{bprty_phoneno = Bprty_number} = _Options
			       } = State) ->
    {H, R, L, D, {_T,La,M}} = Dts_status,
    Dts_status1=write_display(string:left("",40,$ ),{0,3},{H, R, L, D, {silence,La,M}},GdrState),
    Dts_status2=write_display(string:left(Bprty_number,20,$ ),{0,3},Dts_status1,GdrState),
    {noreply, State#phone_state{dts_status = Dts_status2}};

handle_cast({anumber, {Aname,Anumber}},#phone_state{dts_status = Dts_status,
					    gdr_state=GdrState
					   } = State) ->
    Aname2=case Aname of
	       ""->"Unlisted";
	       Aname->Aname
	   end,
    Dts_status1=write_display("Incoming",{0,2},Dts_status,GdrState),
    Dts_status2=write_display(Aname2++" "++Anumber,{0,3},Dts_status1,GdrState),
    {noreply, State#phone_state{dts_status = Dts_status2}};

handle_cast(date_loop, #phone_state{dts_status=Dts_status,gdr_state=GdrState} = State) ->
    String = date(now),
    NewDts_status=write_display(String,{2,1},Dts_status,GdrState),
    {noreply, State#phone_state{dts_status = NewDts_status}};

handle_cast(registered, #phone_state{registered = false} = State)->
    {noreply, State};

handle_cast(registered, #phone_state{registered = true,
				     gs_ids = GS_IDs,
				     
				     options = #sip_phone_options{own_phoneno = Own_phoneno,
								  own_name = Own_name
								  } = _Options
								 } = State) ->
    ?DEBUG_PRINT("registered~n",[]),
    #gs_ids{state=DisplayState} = GS_IDs,
    %% Check if Erlang graphics is used for debug purposes
    {RegPid1,DatePid1} =
	case State#phone_state.register_loop_pid of
	    {undefined,undefined}->
		RegPid=spawn(?MODULE, register_loop,[State]),
		DatePid=spawn(?MODULE, date_loop,[self()]),
		{RegPid,DatePid};
	    {RegPid,DatePid}->{RegPid,DatePid}
	end,
    
    gen_server:cast(self(), {own_phoneno, string:strip(Own_name, both, $"), Own_phoneno}),
    check_call_toGS(DisplayState,"registered"),
%    gs:config(DisplayState, {label, {text, lf:sformat("registered",[])}}),
    {noreply, State#phone_state{	 
		register_loop_pid = {RegPid1,DatePid1}}
    };

handle_cast(Msg,State) ->
    ?DEBUG_PRINT("unknown cast:~w~n",[Msg]),
    {noreply,State}.
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% gen_server handle info function.
%% -----------------------------------------------------------------------------

%% Receiving data from the phone, if using the telnet alternative.
%handle_info({tcp,Socket,Data},State=#phone_state{socket={telnet,Socket}}) ->
%    ?DEBUG_PRINT("~nSip_info TCP:~s~n",[Data]),
%    NewState=handle_data(Data,State),
%    print_toLog(Data,NewState),                % Print to phone log in cwd directory.
%    {NewBuffer,NewNextSlot,NewOldestSlot}=
%	send_to_all_oip_instr(Data,
%			      NewState#phone_state.instr,
%			      NewState#phone_state.buffer,
%			      NewState#phone_state.next_slot,
%			      NewState#phone_state.oldest_slot),
%    {noreply,NewState#phone_state{buffer=NewBuffer,
%				  next_slot=NewNextSlot,
%				  oldest_slot=NewOldestSlot}};

handle_info({udp, Socket, _, _, Data}, #phone_state{
	      options=Options,
	      gdr_state=GdrState,
	      socket=Socket} = State) ->
    ?DEBUG_PRINT("Sip_info Phone_socket ~s <==~n~s~n",[get_time_str(),Data]),
    case GdrState#gdr_state.log of
	opened->
	    Sip_Message = format_string(incoming,Data),
	    Signal_head = {incoming,lf:sformat("~s  SIP  <==",[ltime:format_time(get_time_lst())])},
	    Formatted = [{comment,""},Signal_head|Sip_Message],
%	    Formatted = eaa_common:format_gdr_signal(GdrState#gdr_state.id,incoming,{0,a,"  SIP <==~n "},get_time_lst()),% ++ Data
	    eaa_common:send_to_gui(?GS_REG, GdrState, {log ,Formatted});
	_->
	    []
    end,
    {Message,Options1}=sip_stack:sip_message(Data,Options),
    case Message of
	"Unknown"-> true;
	[Message1,Message2,Message3,Message4]->send_host(Socket,Options1,[Message1,Message2,Message3,Message4],GdrState);
	[Message1,Message2,Message3]->send_host(Socket,Options1,[Message1,Message2,Message3],GdrState);
	[Message1,Message2]->send_host(Socket,Options1,[Message1,Message2],GdrState);
	Mes->    send_host(Socket,Options1,[Mes],GdrState)
%	Mes when list(Mes) ->    send_host(Socket,Options1,[Mes],GdrState);
    end,
    {noreply,State#phone_state{options=Options1}};

handle_info({udp, Socket, _, _, Data}, #phone_state{wsp_socket=Socket} = State) ->
    ?DEBUG_PRINT("Sip_info WSP_socket ~s <==~n~s~n",[get_time_str(),Data]),

    {noreply,State};

handle_info({Log_Pid,{opened,log}}, #phone_state{gdr_state=GdrState,
						 gs_ids = _GS_IDs} = State) ->
    eaa_common:send_to_gui(Log_Pid, GdrState, {opened_res, ok}),
    {noreply,State#phone_state{
	       gdr_state=GdrState#gdr_state{log = opened}
	      }};

handle_info({Log_Pid,{closed,log}}, #phone_state{gdr_state=GdrState,
						 gs_ids = _GS_IDs} = State) ->
    eaa_common:send_to_gui(Log_Pid, GdrState, {closed_res, ok}),
    {noreply,State#phone_state{
	       gdr_state=GdrState#gdr_state{log = closed}
	      }};

handle_info({gs,Button,click,[],_}, #phone_state{gs_ids = GS_IDs} = State) ->
    ?DEBUG_PRINT("click:~w~n",[Button]),
    #gs_ids{quit = Button_quit,
	    options = Button_option,
	    rtpinfo = Button_rtp,
	    register = Button_reg,
	    auto_answer = Button_autoanswer,
	    button_clear = Button_clear,
	    hook = Button_hook} = GS_IDs,
    case Button of
	Button_quit -> handle_button_quit(State);
	Button_hook -> handle_button_offhook(State);
	Button_option -> handle_button_option(State);
	Button_rtp -> handle_button_rtpinfo(State);
	Button_reg -> handle_button_register(State);
	Button_clear -> handle_button_clear(State);
	Button_autoanswer -> handle_button_auto_answer(State);
	_ -> handle_keypad_button(Button, State)
    end;

handle_info({rtp_info, {rtp_packets_lost}}, #phone_state{gdsx_info = GDSX_info} = State) ->
    ?DEBUG_PRINT("handle_info rtp_packets_lost: ~n",[]),
    {noreply, State#phone_state{gdsx_info = GDSX_info#gdsx_info{payload_type = undefined}}};

handle_info({rtp_info, {PayloadType,Timeslot}}, #phone_state{gdsx_info = GDSX_info} = State) ->
    {noreply, State#phone_state{gdsx_info = GDSX_info#gdsx_info{payload_type = {PayloadType,Timeslot}}}};

handle_info({rtp_info, Packet}, #phone_state{gdsx_info = GDSX_info,
					     options = #sip_phone_options{own_phoneno = OwnNumber} = _Options
					    } = State) ->
    
    PT = rtp:get_pt(Packet),
    Bytes = rtp:get_data(Packet),
    ?DEBUG_PRINT("Set PayloadType  = ~w for ~s~n",[PT,OwnNumber]),
    {noreply, State#phone_state{gdsx_info = GDSX_info#gdsx_info{payload_type = {PT,0},
								data = Bytes}}};

%% Erlang phone window closed
handle_info({gs,__Win,destroy,[],_}, #phone_state{gs_ids = GS_IDs} = State) ->
    ?DEBUG_PRINT("destroy:~w~n",[__Win]),
    #gs_ids{win = Win} = GS_IDs,
    case __Win of
	Win -> handle_button_quit(State);
	_ -> {noreply, State}
    end;

handle_info(alerted, State) ->
    ?DEBUG_PRINT("alerted:~n",[]),
    {noreply,handle_alerted(State)};

handle_info(alerting, State) ->
    ?DEBUG_PRINT("alerting:~n",[]),
    {noreply,handle_alerting(State)};

handle_info(callProceeding, State) ->
    ?DEBUG_PRINT("callProceeding:~n",[]),
    {noreply,handle_callProceeding(State)};

%handle_info(connected, State) ->
%    ?DEBUG_PRINT("connected:~n",[]),
%    {noreply,handle_connected(State)};

%handle_info(disconnected, State) ->
%    ?DEBUG_PRINT("disconnected:~n",[]),
%    {noreply,handle_disconnected(State)};

%handle_info(forced_unregistration, State) ->
%    ?DEBUG_PRINT("Forced unregistration~n",[]),
%    handle_button_register(State);

handle_info({dtmf, DTMF}, State) ->
    ?DEBUG_PRINT("DTMF:~s~n",[DTMF]),
    handle_dtmf(DTMF, State),
    {noreply, State};

%handle_info({registered_info, E164, GatekeeperID}, State) ->
%    ?DEBUG_PRINT("registered_info:~s,~s~n",[E164,GatekeeperID]),
%    Newstate=handle_regInfo({E164, GatekeeperID}, State),
%    {noreply, NewState};

handle_info({GuiPid,{gdr_comment_update_req}}, #phone_state{gdr_state=GdrState} = State) ->
    ?DEBUG_PRINT("gdr_comment_update_req~n",[]),
    
    eaa_common:gdr_comment_update_req(GuiPid, GdrState),
    {noreply, State};

handle_info({GuiPid,{opened,gdr}}, #phone_state{
						gdr_state=GdrState} = State) ->
    eaa_common:send_to_gui(GuiPid, GdrState, {opened_res, ok}),
    ?DEBUG_PRINT("{opened,gdr}~n",[]),
    {noreply, State#phone_state{gui_pid = GuiPid,
				gdr_state=GdrState#gdr_state{gdr=opened}}};

handle_info({GuiPid,{closed,gdr}}, #phone_state{gdr_state=GdrState} = State) ->
    eaa_common:send_to_gui(GuiPid, GdrState, {closed_res, ok}),
    ?DEBUG_PRINT("{closed,gdr}~n",[]),
    {noreply, State#phone_state{gui_pid = empty,
				gdr_state=GdrState#gdr_state{gdr=closed}}};

handle_info({GuiPid,{gdr_update_req}}, #phone_state{gdr_state=GdrState} = State) ->
    eaa_common:send_to_gui(GuiPid, GdrState, {gdr_update_req_res, ok}),
    {Handset, Ringstate, _L, _D, {T,La,M}} = State#phone_state.dts_status,
    Ledstate=h323_phone:indicators(GdrState#gdr_state.type),
    Display = h323_phone:display(GdrState#gdr_state.type),
    eaa_dts:gdr_update({Handset,Ringstate,Ledstate,Display,{T,La,M}}, GdrState),
    {noreply, State#phone_state{dts_status = {Handset, Ringstate, Ledstate, Display, {T,La,M}}}};

handle_info({From,{gdr_handset,lift}},#phone_state{options=_Options,
						   dts_status=Dts_status,
						   gdr_state=GdrState,
						   socket=_Socket}=State) ->
    eaa_common:send_to_gui(From, GdrState, {gdr_handset_res, ok}),
    Dts_status2 = led(11,double,Dts_status,GdrState),
    handle_button_offhook(State#phone_state{dts_status = Dts_status2});

handle_info({From,{gdr_handset,replace}},#phone_state{options=_Options,
						      gdr_state=GdrState,
						      socket=_Socket}=State) ->
    eaa_common:send_to_gui(From, GdrState, {gdr_handset_res, ok}),
    handle_button_onhook(State);

handle_info({From,{gdr_button,register}},#phone_state{options=_Options,
						      gdr_state=GdrState,
						      socket=_Socket}=State) ->
    eaa_common:send_to_gui(From, GdrState, {gdr_button_res, ok}),
    handle_button_register(State);

handle_info({From,{gdr_button,kspeak}},#phone_state{options=_Options,
						    dts_status = Dts_status1,
						    gdr_state=GdrState,
						    socket=_Socket}=State) ->
    eaa_common:send_to_gui(From, GdrState, {gdr_button_res, ok}),
    Dts_status2 = led(11,double,Dts_status1,GdrState),
    Dts_status3 = led(-1,double,Dts_status2,GdrState),
    handle_button_offhook(State#phone_state{dts_status = Dts_status3});

handle_info({From,{gdr_button,kacc1}},#phone_state{dts_status = Dts_status1,
						   options = #sip_phone_options{state=idle} = Options,
						   gdr_state=GdrState,
						   socket= _Socket}=State) ->
    eaa_common:send_to_gui(From, GdrState, {gdr_button_res, ok}),
    Dts_status2 = led(11,double,Dts_status1,GdrState),
    gen_server:cast(self(), {tone,dial_tone}),
    {_Dialled_no,Dts_status3} = write_dialled_to_display("",Dts_status2,State),
    {noreply,State#phone_state{dts_status = Dts_status3,
			       options=Options#sip_phone_options{
					 state=outgoing_digit_rec,
					 bprty_phoneno = ""}}};

handle_info({From,{gdr_button,kclear}},#phone_state{options=_Options,
						     gdr_state=GdrState,
						     socket=_Socket}=State) ->
    eaa_common:send_to_gui(From, GdrState, {gdr_button_res, ok}),
    handle_button_clear(State);

handle_info({_From,{gdr_button,[]}},State)->
    {noreply,State};
handle_info({From,{gdr_button,[Button|Remaing]}},State)->
    {noreply,State1} = handle_info({From,{gdr_button,Button}},State),
    timer:sleep(State1#phone_state.interdigit_delay),
    handle_info({From,{gdr_button,Remaing}},State1);

handle_info({From,{gdr_button,Button}},#phone_state{dts_status = Dts_status1,
						    gdr_state=GdrState,
						    options = #sip_phone_options{state=idle} = Options,
						    socket=_Socket}=State) ->
    Dts_status2 = led(11,double,Dts_status1,GdrState),
    handle_info({From,{gdr_button,Button}},State#phone_state{dts_status = Dts_status2,
							     dialled_no="",
							     options=Options#sip_phone_options{
								       state=outgoing_digit_rec,
								       bprty_phoneno = ""}});

handle_info({From,{gdr_button,Button}},#phone_state{dts_status=Dts_status,
						    gdr_state=GdrState,
						    options=#sip_phone_options{
						      state=incomming_answer} = Options,
						    socket=Socket
						   }=State) ->
    eaa_common:send_to_gui(From, GdrState, {gdr_button_res, ok}),
    Button_string = get_button_string(Button),
    State1 = 
	case Button_string of
	    undefined ->
		State;
	    _ ->
		{Message,Options1} = sip_stack:sip_message(info,Options#sip_phone_options{service_no = Button_string}),
		send_host(Socket,Options1,[Message],GdrState),
		Dts_status2 = write_display(Button_string,{3,2},Dts_status,GdrState),
		State#phone_state{options=Options1,
				  dts_status=Dts_status2
				 }
	end,
    {noreply,State1};
handle_info({From,{gdr_button,Button}},#phone_state{dts_status=Dts_status,
						    gdr_state=GdrState,
						    options=#sip_phone_options{
						      state=outgoing_answer} = Options,
						    socket=Socket
						   }=State) ->
    eaa_common:send_to_gui(From, GdrState, {gdr_button_res, ok}),
    Button_string = get_button_string(Button),
    State1 = 
	case Button_string of
	    undefined ->
		State;
	    _ ->
		{Message,Options1} = sip_stack:sip_message(info,Options#sip_phone_options{service_no = Button_string}),
		send_host(Socket,Options1,[Message],GdrState),
		Dts_status2 = write_display(Button_string,{3,2},Dts_status,GdrState),
		State#phone_state{options=Options1,
				  dts_status=Dts_status2
				 }
	end,
    {noreply,State1};
handle_info({From,{gdr_button,Button}},#phone_state{dts_status=Dts_status,
						    gdr_state=GdrState,
						    options=#sip_phone_options{
						      state=outgoing_busy} = Options,
						    socket=Socket
						   }=State) ->
    eaa_common:send_to_gui(From, GdrState, {gdr_button_res, ok}),
    Button_string = get_button_string(Button),
    State1 = 
	case Button_string of
	    undefined ->
		State;
	    _ ->
		{Message,Options1} = sip_stack:sip_message(info,Options#sip_phone_options{service_no = Button_string}),
		send_host(Socket,Options1,[Message],GdrState),
		Dts_status2 = write_display(Button_string,{3,2},Dts_status,GdrState),
		State#phone_state{options=Options1,
				  dts_status=Dts_status2
				 }
	end,
    {noreply,State1};

handle_info({From,{gdr_button,Button}},#phone_state{dts_status=Dts_status,
						    gdr_state=GdrState,
						    options=Options,
						    socket=_Socket}=State) ->
    eaa_common:send_to_gui(From, GdrState, {gdr_button_res, ok}),
    Button_string = get_button_string(Button),
    State1 = 
	case Button_string of
	    undefined ->
		State;
	    _ ->
		Bprty_number = Options#sip_phone_options.bprty_phoneno,
		Bprty_number1 = string:concat(Bprty_number, Button_string),
		{Dialled_no,NewDts_status} = write_dialled_to_display(Button,Dts_status,State),
		State#phone_state{options=Options#sip_phone_options{bprty_phoneno = Bprty_number1},
				  dts_status=NewDts_status,
				  dialled_no=Dialled_no}
	end,
    {noreply,State1};

handle_info({_From,{end_test}},State) ->
    ?DEBUG_PRINT("end_test~n",[]),
    {noreply,State};
handle_info({From,{clear_tc}},#phone_state{dts_status=Dts_status,
					   socket=_Socket,
					   options=_Options}=State) ->
    ?DEBUG_PRINT("clear_tc~n",[]),
    {_Noreply, State2} = 
	case element(1,Dts_status) of
	    replace->{noreply, State};
	    _->
		handle_button_onhook(State)
	end,
    From ! {self(),{clear_tc_res,ok}},
    {noreply,State2};

%%% This is most likely the case when an oip_instr process terminated. If the
%%% process still is in the #phone_state.instr list, it is an error. Otherwise
%%% it is ok.
%handle_info({'EXIT',OipInstr,Reason},State=#phone_state{instr=Instr}) ->
%    case lists:keysearch(OipInstr,3,Instr) of
%	{value,{Cmd,From,OipInstr}} ->      % This means somekind of error.
%	    eaa_common:instruction_res(From,
%				       Cmd,
%				       {error,
%					lf:sformat("Real OIP PROFIT execution process"
%						   " failed:~p,~n internal GDS-X error!",
%						   [Reason])}),
%	    NewInstr=lists:keydelete(OipInstr,3,Instr),
%	    {NewBuffer,NewNextSlot,NewOldestSlot}=
%		trim_buffer(State#phone_state.buffer,
%			    State#phone_state.next_slot,
%			    State#phone_state.oldest_slot,
%			    State#phone_state.next_slot-1, % Discard entire buffer!
%			    length(NewInstr)), % The number of active oip_instr.
%	    {noreply,State#phone_state{instr=NewInstr,
%				       buffer=NewBuffer,
%				       next_slot=NewNextSlot,
%				       oldest_slot=NewOldestSlot}};
%	false ->                            % Either ok or otherwise unknown.
%	    {noreply,State}
%    end;

%% This is receiving a profit command. It will most often result in the creation
%% of an oip_instr process. We must update #phone_state.instr with {Cmd,From,OipRealPid}.
%% We must also push any strings in the buffer to the newly created oip_instr.
handle_info({From,{instruction,Instruction, Parameters}},State) ->
    State1 = handle_instruction(Instruction, Parameters, State, From),
    {noreply,State1};

%% This is receiving a request for data.
%% Currently this is not supported(!) This since there must be somkind of state
%% kept by the this process or a help process in order to answer these kind of
%% requests.

handle_info({From,{get_data,ringstate}}, State) ->
    eaa_common:get_data_res(From, ringstate, {ok,element(2,State#phone_state.dts_status)}),
    ?DEBUG_PRINT("get_data,ringstate~n",[]),
    {noreply, State};
handle_info({From,{get_data,indicators}}, State) ->
    eaa_common:get_data_res(From, indicators, {ok,element(3,State#phone_state.dts_status)}),
    ?DEBUG_PRINT("get_data,indicators~n",[]),
    {noreply, State};
handle_info({From,{get_data,display}}, State) ->
    eaa_common:get_data_res(From, display, {ok,element(4,State#phone_state.dts_status)}),
    ?DEBUG_PRINT("get_data,display~n",[]),
    {noreply, State};
handle_info({From,{get_data,hook}}, State) ->
    eaa_common:get_data_res(From, hook, {ok,element(1,State#phone_state.dts_status)}),
    ?DEBUG_PRINT("get_data,hook~n",[]),
    {noreply, State};
%handle_info({From,{get_data,tone}}, #phone_state{pids = #pids{dba_pid=DBA_Pid}} =State) ->
%    eaa_common:get_tone(From,DBA_Pid,0),%Ind ?????
%    ?DEBUG_PRINT("get_data,hook~n",[]),
%    {noreply, State};

handle_info({From,{get_data,DataID}}, State) ->
    eaa_common:not_supported(From, get_data, DataID),
    ?DEBUG_PRINT("get_data,unknown~n",[]),
    {noreply, State};
%handle_info({From,{get_data,DataID}},State) ->
%    case handle_get_data(DataID,From,State) of
%	false ->                            % No oip_instr was started.
%	    {noreply,State}
%    end;

handle_info({_From,{shutdown}},State) ->    % This is GDSx stop signal,
    {stop,shutdown,State};                  % instead of a proper EXIT from supervisor!

%% Help functions for connection test, Gateway calls
handle_info({From,{get_dba_id_ind}}, #phone_state{gdsx_info = GDSX_info
						 } = State) ->
    DBA_Pid = self(),
    
    Items = sm_device_db:get_items_with_pid(DBA_Pid),
    case lists:keysearch(DBA_Pid,3,Items) of
	{value,Tuple} ->
	    Board_info = tuple_to_list(Tuple),
	    Board_tuple = lists:nth(8,Board_info),
	    case Board_tuple of
		{equipment,Node,_,_} ->
		    case GDSX_info#gdsx_info.payload_type of
			{_PayloadType,Timeslot}->
			    From ! {self(),{get_dba_id_ind_res,{ok, Node, Timeslot}}};
			_->
			    From ! {self(),{get_dba_id_ind_res,{ok, Node, not_connected}}}
		    end;
		_->
		    From ! {self(),{get_dba_id_ind_res,{ok, lists:nth(2,Board_info), not_connected}}}
	    
	    end;
	_->
	    From ! {self(),{get_dba_id_ind_res,{ok, not_connected, 0}}}
    end,
    {noreply, State};

%% Help functions for connection test, Non Gateway calls
handle_info({From,{connection_test_send,_Ind}},#phone_state{
						options = #sip_phone_options{
						rtplocal_port={RTPport,RTPsock,RTP_Pid}}
							   } = State) ->
    case RTP_Pid of
	Pid when is_pid(Pid) == true ->
	    From ! {self(),{connection_test_send_result,ok}};
	_->	    
	    From ! {self(),{connection_test_send_result,nok}}
    end,
    {noreply, State};
handle_info({From,{connection_test_detect,_Ind}},#phone_state{gdsx_info = GDSX_info
							   } = State) ->
    case GDSX_info#gdsx_info.payload_type of
	{_PayloadType,_Timeslot}->
	    From ! {self(),{connection_test_detect_result,ok}};
	_->
	    From ! {self(),{connection_test_detect_result,nok}}
    end,
    {noreply, State};
handle_info({From,{connection_test_send_stop,_Ind}}, State) ->
    {noreply, State};
handle_info({From,{connection_test_detect_stop,_Ind}}, State) ->
    {noreply, State};
handle_info({From,{get_transmission_state}}, State) ->
    From ! {self(),{get_transmission_state_res,{ok,on}}},
    {noreply, State};

handle_info(Info,State) ->
    ?DEBUG_PRINT("unknown info:~w~n",[Info]),
    {noreply,State}.
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% gen_server terminate function.
%% -----------------------------------------------------------------------------

terminate(_Reason,State) ->
%    write_options(State#phone_state.filename,State#phone_state.options),
    handle_button_quit(State),
    close_logfile(State#phone_state.filedesc),
    gen_udp:close(State#phone_state.socket),
    ?DEBUG_PRINT("Phone log closed.~n",[]),
    ok.
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% gen_server code_change function.
%% -----------------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%% -----------------------------------------------------------------------------


%% =============================================================================
%% First level help functions.
%% =============================================================================

%% -----------------------------------------------------------------------------

handle_set_options(Options, #phone_state{state=idle,
					 phone_pid = Pid} = State) ->
    #sip_phone_options{bprty_phoneno = Bprty_phoneno,
		       user_id = User_id,
		       fastStart = FastStart,
		       gatekeeper = Gatekeeper,
		       gatekeeper_host = GK_Host,
		       gatekeeper_id = GK_ID,
		       own_phoneno = Phone_number} = Options,
    h323_phone:set_user(Pid, Bprty_phoneno),
    h323_phone:set_user_id(Pid, User_id),      
    h323_phone:set_phone_number(Pid, Phone_number),     
    Registered =
	case Gatekeeper of
	    true ->
		h323_phone:set_gatekeeper(Pid, GK_Host, GK_ID),
		State#phone_state.registered;
	    false ->
		h323_phone:set_gatekeeper(Pid, "", ""),
		undefined
	end,
    h323_phone:set_fastStart(Pid, FastStart),
    h323_phone:set_info(Pid),     
%    write_options(Filename, Options),
    State#phone_state{registered = Registered};
handle_set_options(_, State) ->
    State.
%% -----------------------------------------------------------------------------

handle_peerInfo({_,[]}, State) ->
    State;
handle_peerInfo({display, OnDisplay}, #phone_state{dts_status = Dts_status,
						   gdr_state=GdrState,
						   gs_ids = GS_IDs} = State) ->
    #gs_ids{display1 = Display1,
	    display2 = Display2,
	    display3 = Display3,
	    display4 = Display4,
	    display5 = Display5,
	    display6 = Display6,
	    display7 = Display7} = GS_IDs,
    Filtered = 
	lists:filter(fun(X) ->
			     if
				 X >= 32, X =< 126 -> true;
				 true -> false
			     end
		     end,
		     OnDisplay),
						%    {H, R, L, D, {T,La,M}} = Dts_status,
    Previous_Displ = case get(display) of
			 undefined->[];
			 Val when list(Val)->Val;
			 _->[]
		     end,
    
    Filtered_compl = Previous_Displ ++ Filtered,
    Dts_status1 =
	case string:str(Filtered_compl, "Display 0") of
	    0 ->Dts_status;
	    Ind0->
		List0 = lists:nthtail(Ind0,Filtered_compl),
		case List0 of
		    L0 when length(L0) >= 68 ->
			ListTA1 = lists:sublist(List0, 12, 68),
			%% Check if Erlang graphics is used for debug purposes
			check_call_toGS(Display1,ListTA1),
			eaa_dts:display({0,0},{write,ListTA1},Dts_status, GdrState);
		    _ ->Dts_status
		end
	end,
    Dts_status2 =
	case string:str(Filtered_compl, "Display 1") of
	    0 ->Dts_status1;
	    Ind1->
		List1 = lists:nthtail(Ind1,Filtered_compl),
		case List1 of
				L1 when length(L1) >= 68 ->
			ListTB1 = lists:sublist(List1, 12, 68),
			check_call_toGS(Display2,ListTB1),		
			eaa_dts:display({0,1},{write,ListTB1},Dts_status1, GdrState);
		    _ ->Dts_status1
		end
	end,
    Dts_status3 =
	case string:str(Filtered_compl, "Display 2") of
	    0 ->Dts_status2;
	    Ind2->
		List2 = lists:nthtail(Ind2,Filtered_compl),
		case List2 of
		    L2 when length(L2) >= 68 ->
			ListTC1 = lists:sublist(List2, 12, 68),
			check_call_toGS(Display3,ListTC1),
			eaa_dts:display({0,2},{write,ListTC1},Dts_status2, GdrState);
		    _ ->Dts_status2
		end
	end,
    
    Dts_status4 =
	case string:str(Filtered_compl, "Display 3") of
	    0 ->Dts_status3;
	    Ind3->
		List3 = lists:nthtail(Ind3,Filtered_compl),
		case List3 of
		    L3 when length(L3) >= 68 ->
			ListTD1 = lists:sublist(List3, 10, 68),
			check_call_toGS(Display4,ListTD1),
			eaa_dts:display({0,3},{write,ListTD1},Dts_status3, GdrState);
		    _ ->Dts_status3
		end
	end,
    
    Dts_status5 =
	case string:str(Filtered_compl, "Display 4") of
	    0 ->Dts_status4;
	    Ind4->
		List4 = lists:nthtail(Ind4,Filtered_compl),
		case List4 of
		    L4 when length(L4) >= 68 ->
			ListTE1 = lists:sublist(List4, 10, 68),
			check_call_toGS(Display5,ListTE1),
			eaa_dts:display({0,4},{write,ListTE1},Dts_status4, GdrState);
		    _ ->Dts_status4
		end
	end,
    
    Dts_status6 =
	case string:str(Filtered_compl, "Display 5") of
	    0 ->Dts_status5;
	    Ind5->
		List5 = lists:nthtail(Ind5,Filtered_compl),
		case List5 of
		    L5 when length(L5) >= 60 ->
			ListTF1 = lists:sublist(List5, 10, 60),
			check_call_toGS(Display6,ListTF1),
			eaa_dts:display({0,5},{write,ListTF1},Dts_status5, GdrState);
		    _ ->Dts_status5
		end
	end,
    
    Dts_status7 = 
	case string:str(Filtered_compl, "isplay 6") of
	    0 ->Dts_status6;
	    Ind6->
		List6 = lists:nthtail(Ind6,Filtered_compl),
		case List6 of
		    L6 when length(L6) >= 60 ->
			ListTG1 = lists:sublist(List6, 9, 60),
			check_call_toGS(Display7,ListTG1),
			eaa_dts:display({0,6},{write,ListTG1},Dts_status6, GdrState);
		    _->Dts_status6
		end
	end,
    
    State#phone_state{dts_status = Dts_status7};

handle_peerInfo({display2, OnDisplay}, #phone_state{dts_status = Dts_status,
						   gdr_state=GdrState,
						   gs_ids = GS_IDs} = State) ->
    #gs_ids{display1 = _Display1,
	    display2 = _Display2,
	    display3 = _Display3,
	    display4 = _Display4,
	    display5 = _Display5,
	    display6 = _Display6,
	    display7 = _Display7} = GS_IDs,

    ListA1 = lists:sublist(" Log" ++ OnDisplay, 1, 8),
    ListA2 = lists:sublist(OnDisplay, 14, 10),
    ListA3 = lists:sublist(OnDisplay, 31, 10),
    ListA4 = lists:sublist(OnDisplay, 48, 10),

    Dts_status1 = eaa_dts:display({3,4},{write,ListA4},Dts_status, GdrState),
    Dts_status2 = eaa_dts:display({2,4},{write,ListA3},Dts_status1, GdrState),
    Dts_status3 = eaa_dts:display({1,4},{write,ListA2},Dts_status2, GdrState),
    Dts_status_new = eaa_dts:display({0,4},{write,ListA1},Dts_status3, GdrState),

    State#phone_state{dts_status = Dts_status_new};

handle_peerInfo({display3, OnDisplay}, #phone_state{dts_status = Dts_status,
						   gdr_state=GdrState,
						   gs_ids = GS_IDs} = State) ->
    #gs_ids{display1 = _Display1,
	    display2 = _Display2,
	    display3 = _Display3,
	    display4 = _Display4,
	    display5 = _Display5,
	    display6 = _Display6,
	    display7 = Display7} = GS_IDs,
    
    [ListTG1|_Stat1] = string:tokens(OnDisplay, "\n"),
    %% Check if Erlang graphics is used for debug purposes
    check_call_toGS(Display7,ListTG1),
    Dts_status_new = eaa_dts:display({0,6},{write,ListTG1},Dts_status, GdrState),
    State#phone_state{dts_status = Dts_status_new};

handle_peerInfo({user, User}, #phone_state{gs_ids = GS_IDs} = State) ->
    #gs_ids{peer_user = PeerUser} = GS_IDs,
    set_label(PeerUser, User),
    State;

handle_peerInfo({vendor, ProductID, VersionID}, #phone_state{gs_ids = GS_IDs} = State) ->
    #gs_ids{peer_prod = PeerProd,
	    peer_vers = PeerVers} = GS_IDs,
    set_label(PeerProd, ProductID),
    set_label(PeerVers, VersionID),
    State;
handle_peerInfo({led, Led}, #phone_state{dts_status = Dts_status,
						   gdr_state=GdrState,
						   gs_ids = _GS_IDs} = State) ->
    %%LED Number= 1, Status=CAD0 %%Callback
    %%LED Number= 7, Status=CAD0 %%kinq_led
    %%LED Number= 9, Status=CAD0 %%kacc1_led
    %%LED Number= , Status=CAD0 %%kacc2_led
    %%LED Number=10, Status=ON   %%menu_led
    %%LED Number=88, Status=ON   %%kspeak_led
%    {_H, _R, _L, _D, {_T,_La}} = Dts_status,

    Dts_status_new =
	case length(Led) of
	    Len when Len < 10->
		Dts_status;
	    _Len ->
		Dts_status1 = 
		    case string:str(Led, "value = 0") of
			0 ->Dts_status;
			_Ind->
			    led(all,off,Dts_status,GdrState)
		    end,
		set_led(string:tokens(Led, "\n"),Dts_status1,GdrState)
	end,
    
    State#phone_state{dts_status = Dts_status_new};
handle_peerInfo({ledoff,_Led}, #phone_state{dts_status = Dts_status,
					    gdr_state=GdrState,
					    gs_ids = _GS_IDs} = State) ->
    Dts_status_new =  led(all,off,Dts_status,GdrState),
    
    State#phone_state{dts_status = Dts_status_new};

handle_peerInfo(_, State) ->
    State.
%% -----------------------------------------------------------------------------

%% Help function handling incomming data from the phone. Some of the data shall
%% be handled, other strings are just left without further action.
%% Returns a new #phone_state.
%handle_data([255,251|_],State=#phone_state{socket=Socket}) ->
%    send_to_host(Socket,[16#ff,16#fd,16#01]), % Telnet message "Do Echo"
%    State;
%handle_data("\r\nVxWorks login: ",State=#phone_state{socket=Socket,options=Options}) ->
%    Message=Options#h323_phone_options.user++[16#0d,16#0a],
%    send_to_host(Socket,Message),
%    State;
%handle_data("Password: ",State=#phone_state{socket=Socket,options=Options}) ->
%    Message=Options#h323_phone_options.user_pw++[16#0d,16#0a],
%    send_to_host(Socket,Message),
%    State;
%handle_data("\r\nLogin timed out after 60 seconds!\r\n",State) ->
%    State;
%handle_data("\r\n\r\nLogin timed out after 60 seconds!\r\n",State) ->
%    State;
%handle_data("\r\n->",State) ->
%    State#phone_state{options=(State#phone_state.options)#h323_phone_options{phone_connect=true}};
%handle_data("printDebugDisp"++Msg,State) ->
%    handle_peerInfo({display,Msg},State);
%handle_data("bugDisplay"++Msg,State)  ->
%    handle_peerInfo({display,Msg},State);
%handle_data("printDebugDisp"++_,State=#phone_state{socket=Socket}) ->
%    send_to_host(Socket,[13,10]),             % \r\0
%    State;
%handle_data("rtpDetector"++_,State=#phone_state{socket=Socket}) ->
%    send_to_host(Socket,[13,10]),             % \r\0
%    State;
%handle_data(" \r\nDisplay "++Msg,State) ->
%    handle_peerInfo({display,Msg},State);
%handle_data("\r\nDisplay "++Msg,State) ->
%    handle_peerInfo({display,Msg},State);
%handle_data("Display "++Msg,State) ->
%    handle_peerInfo({display,Msg},State);
%handle_data(Msg=" off"++_,State) ->
%    handle_peerInfo({display2,Msg},State);
%handle_data(Msg="     "++_,State) ->
%    handle_peerInfo({display3,Msg},State);
%handle_data(Msg="o    "++_,State) ->
%    handle_peerInfo({display3,"N"++Msg},State);
%handle_data("xit  "++Msg,State) ->
%    handle_peerInfo({display3," Exit "++Msg},State);
%handle_data("isplay 6:"++Msg,State) ->
%    handle_data("Display 6:"++Msg,State);
%handle_data("printDebugLED\r\nvalue"++Msg,State) ->
%    handle_peerInfo({ledoff,Msg},State);
%handle_data("printDebugLED"++Msg,State) ->
%    handle_peerInfo({led,Msg},State);
%handle_data("simulateKeyPush",State) ->
%    State;
%handle_data(String, #phone_state{gdr_state=GdrState,
%				 options=Options,
%				 socket=Socket
%				} = State) when length(String) > 10->
%    State1 = 
%	case string:str(String, "Display ") of
%	    0 ->State;
%	    Ind->
%		Str = lists:nthtail(Ind-1,String),
%		handle_peerInfo({display,Str},State)
%	end,
%    State2 = 
%	case string:str(String, "LED Number") of
%	    0 ->State1;
%	    Ind2->
%		Str2 = lists:nthtail(Ind2-1,String),
%		handle_peerInfo({led,Str2},State1)	    
%	end,
%    State3 = 
%	case string:str(String, "SETVAR WAP msg rec, Data=rt=") of
%	    0 ->State2;
%	    Ind3->
%		{Handset2, _Ringstate2, Ledstate2, Display2, {T2,La2,M2}} = State2#phone_state.dts_status,
%		Ring3 = h323_client_rwsp_2:ringType(string:substr(String, Ind3+28, 2)),
%		eaa_dts:gdr_update({Handset2,Ring3,Ledstate2,Display2,{T2,La2,M2}}, GdrState),
%		send_host(Socket,Options,["printDebugDisplay","printDebugLED"],GdrState),
%		State2#phone_state{dts_status = {Handset2,Ring3,Ledstate2,Display2,{T2,La2,M2}}}
%    end,
%    State4 = 
%	case string:str(String, "SETVAR WAP msg rec, Data=tt=") of
%	    0 ->State3;
%	    Ind4->
%		{Handset3, Ringstate3, Ledstate3, Display3, {_T3,La3,M3}} = State3#phone_state.dts_status,
%		Tone3 = h323_client_rwsp_2:toneType(string:substr(String, Ind4+28, 2)),
%		eaa_dts:gdr_update({Handset3,Ringstate3,Ledstate3,Display3,{Tone3,La3,M3}}, GdrState),
%		State3#phone_state{dts_status = {Handset3,Ringstate3,Ledstate3,Display3,{Tone3,La3,M3}}}
%	end,
%    State5 = 
%	case string:str(String, "SETVAR WAP msg rec, Data=ln=") of
%	    0 ->State4;
%	    Ind5->
%		{Handset4, Ringstate4, Ledstate4, Display4, {T4,_La4,M4}} = State4#phone_state.dts_status,
%		Lang4 = h323_client_rwsp_2:langType(string:substr(String, Ind5+28, 1)),
%		eaa_dts:gdr_update({Handset4,Ringstate4,Ledstate4,Display4,{T4,Lang4,M4}}, GdrState),
%		State4#phone_state{dts_status = {Handset4,Ringstate4,Ledstate4,Display4,{T4,Lang4,M4}}}
%	end,
%    case string:str(String, "STOP FLASHING WAP") of
%	0 ->State5;
%	_Ind9->
%	    send_host(Socket,Options,["printDebugDisplay","printDebugLED"],GdrState),
%	    State5
%    end;

%handle_data(_String,State) ->
%    State.    
%% -----------------------------------------------------------------------------


%% -----------------------------------------------------------------------------
%% GS handling help functions.
%% -----------------------------------------------------------------------------

handle_button_rtpinfo(State) ->
%    send_to_host(Socket,"rtpDetector"),
    {noreply,State}.
%% -----------------------------------------------------------------------------

handle_button_clear(#phone_state{dts_status=Dts_status,
				 gdr_state=GdrState,
				 registered = false,
				 socket=Socket,
				 options = #sip_phone_options{rtplocal_port={_RTPport,RTPsock,RTP_Pid},
							      rtpremote_port={_RTPRport,RTPRsock,RTPR_Pid},
							      own_phoneno=Own_phoneno,
							      own_name = Own_name,
							      gatekeeper_host = GKHost} = Options
				} = State) ->
    {Message,Options1} = sip_stack:sip_message(bye,Options),
    send_host(Socket,Options1,[Message],GdrState),
    {H, _R, _L, D, {_T,La,M}} = Dts_status,
    {NewGdrstate, NewDts_status} =
	init_display(GKHost,
		     {H, no_ring, h323_phone:indicators(GdrState#gdr_state.type),D, {silence,La,M}},
		     GdrState),
    gen_server:cast(self(), {own_phoneno, string:strip(Own_name, both, $"), Own_phoneno}),
    gen_server:cast(self(), date_loop),
    close_RTP_port(RTP_Pid,RTPsock),
    close_RTP_port(RTPR_Pid,RTPRsock),
    {noreply, State#phone_state{dts_status = NewDts_status,
				gdr_state=NewGdrstate,
				options = Options1#sip_phone_options{
					    rtplocal_port={0,0,0},
					    rtpremote_port={0,0,0},
					    rtpremote_ip="",
					    old_call_info={},
					    contact = "",
					    state=idle
					   },
				state=idle,
				registered = false}};

handle_button_clear(#phone_state{dts_status=Dts_status,
				 gdr_state=GdrState,
				 registered = true,
				 socket = Socket,
				 options = #sip_phone_options{rtplocal_port={_RTPport,RTPsock,RTP_Pid},
							      rtpremote_port={_RTPRport,RTPRsock,RTPR_Pid},
							      own_phoneno=Own_phoneno,
							      own_name = Own_name,
							      gatekeeper_host=GKHost,
							      state=outgoing_ring} = Options
				} = State) ->
    {Message,Options1} = sip_stack:sip_message(cancel,Options),
    send_host(Socket,Options1,[Message],GdrState),
    {H, _R, _L, D, {_T,La,M}} = Dts_status,
    {NewGdrstate, NewDts_status} =
	init_display(GKHost,
		     {H, no_ring, h323_phone:indicators(GdrState#gdr_state.type),D, {silence,La,M}},
		     GdrState), 
    gen_server:cast(self(), {own_phoneno, string:strip(Own_name, both, $"), Own_phoneno}),
    gen_server:cast(self(), date_loop),
    close_RTP_port(RTP_Pid,RTPsock),
    close_RTP_port(RTPR_Pid,RTPRsock),
    {noreply, State#phone_state{dts_status = NewDts_status,
				gdr_state=NewGdrstate,
				options = Options1#sip_phone_options{
					    rtplocal_port={0,0,0},
					    rtpremote_port={0,0,0},
					    rtpremote_ip="",
					    old_call_info={},
					    contact = "",
					    state=idle
					   },
				state=idle,
				registered = true}};

handle_button_clear(#phone_state{dts_status=Dts_status,
				 gdr_state=GdrState,
				 registered = true,
				 socket=Socket,
				 options = #sip_phone_options{rtpremote_port={_RTPRport,RTPRsock,RTPR_Pid},
							      rtplocal_port={_RTPport,RTPsock,RTP_Pid},
							      own_phoneno=Own_phoneno,
							      own_name = Own_name,
							      gatekeeper_host=GKHost,
							      state=outgoing_answer} = Options
				} = State) ->
    {Message,Options1} = sip_stack:sip_message(outgoing_bye,Options),
    send_host(Socket,Options1,[Message],GdrState),
    {H, _R, _L, D, {_T,La,M}} = Dts_status,
    {NewGdrstate, NewDts_status} =
	init_display(GKHost,
		     {H, no_ring, h323_phone:indicators(GdrState#gdr_state.type),D, {silence,La,M}},
		     GdrState),
    gen_server:cast(self(), {own_phoneno, string:strip(Own_name, both, $"), Own_phoneno}),
    gen_server:cast(self(), date_loop),
    close_RTP_port(RTP_Pid,RTPsock),
    close_RTP_port(RTPR_Pid,RTPRsock),
    {noreply, State#phone_state{dts_status = NewDts_status,
				gdr_state=NewGdrstate,
				options = Options1#sip_phone_options{
					    rtplocal_port={0,0,0},
					    rtpremote_port={0,0,0},
					    rtpremote_ip="",
					    old_call_info={},
					    contact = "",
					    state=idle
					   },
				state=idle,
				registered = true}};

handle_button_clear(#phone_state{dts_status=Dts_status,
				 gdr_state=GdrState,
				 registered = true,
				 socket=_Socket,
				 options = #sip_phone_options{rtplocal_port={_RTPport,RTPsock,RTP_Pid},
							      rtpremote_port={_RTPRport,RTPRsock,RTPR_Pid},
							      own_phoneno=Own_phoneno,
							      own_name = Own_name,
							      gatekeeper_host=GKHost,
							      state=idle} = Options
				} = State) ->
    {H, _R, _L, D, {_T,La,M}} = Dts_status,
    {NewGdrstate, NewDts_status} =
	init_display(GKHost,
		     {H, no_ring, h323_phone:indicators(GdrState#gdr_state.type),D, {silence,La,M}},
		     GdrState),
    gen_server:cast(self(), {own_phoneno, string:strip(Own_name, both, $"), Own_phoneno}),
    gen_server:cast(self(), date_loop),
    close_RTP_port(RTP_Pid,RTPsock),
    close_RTP_port(RTPR_Pid,RTPRsock),
    {noreply, State#phone_state{dts_status = NewDts_status,
				gdr_state=NewGdrstate,
				options = Options#sip_phone_options{
					    rtplocal_port={0,0,0},
					    rtpremote_port={0,0,0},
					    rtpremote_ip="",
					    old_call_info={},
					    contact = "",
					    state=idle
					   },
				state=idle,
				registered = true}};

handle_button_clear(#phone_state{dts_status=Dts_status,
				 gdr_state=GdrState,
				 registered = true,
				 socket=Socket,
				 options = #sip_phone_options{rtplocal_port={_RTPport,RTPsock,RTP_Pid},
							      rtpremote_port={_RTPRport,RTPRsock,RTPR_Pid},
							      own_phoneno=Own_phoneno,
							      own_name = Own_name,
							      gatekeeper_host=GKHost} = Options
				} = State) ->
    {Message,Options1} = sip_stack:sip_message(incomming_bye,Options),
    send_host(Socket,Options1,[Message],GdrState),
    {_H, _R, _L, D, {_T,La,M}} = Dts_status,
    {NewGdrstate, NewDts_status} =
	init_display(GKHost,
		     {replace, no_ring, h323_phone:indicators(GdrState#gdr_state.type),D, {silence,La,M}},
		     GdrState),
    gen_server:cast(self(), {own_phoneno, string:strip(Own_name, both, $"), Own_phoneno}),
    gen_server:cast(self(), date_loop),
    close_RTP_port(RTP_Pid,RTPsock),
    close_RTP_port(RTPR_Pid,RTPRsock),
    {noreply, State#phone_state{dts_status = NewDts_status,
				gdr_state=NewGdrstate,
				options = Options1#sip_phone_options{
					    rtplocal_port={0,0,0},
					    rtpremote_port={0,0,0},
					    rtpremote_ip="",
					    old_call_info={},
					    contact = "",
					    state=idle
					   },
				state=idle,
				registered = true}}.

%%handle_button_register(State) ->
%%    {noreply, State}.

%% Erlang GS handling
handle_button_register(#phone_state{gdr_state=GdrState,
				    dts_status=Dts_status,
				    registered = false,
				    socket=Socket,
				    options = #sip_phone_options{gatekeeper_host=GKHost} = Options
				   } = State) ->   
   % RegPid=spawn(?MODULE, register_loop,[State]),
   % DatePid=spawn(?MODULE, date_loop,[self()]),
    {Message,Options1} = sip_stack:sip_message(register,Options),
    send_host(Socket,Options1,[Message],GdrState),
    {H, _R, _L, D, {_T,La,M}} = Dts_status,
    {NewGdrstate, NewDts_status} =
	init_display(GKHost,
		     {H, no_ring, h323_phone:indicators(GdrState#gdr_state.type),D, {silence,La,M}},
		     GdrState),
   % gen_server:cast(self(), {own_phoneno, string:strip(Own_name, both, $"), Own_phoneno}),
    {noreply, State#phone_state{gdr_state=NewGdrstate,
				dts_status=NewDts_status,
				register_loop_pid = {undefined,undefined},
				options = Options1#sip_phone_options{
					    rtplocal_port={0,0,0},
					    rtpremote_port={0,0,0},
					    rtpremote_ip="",
					    old_call_info={},
					    contact = "",
					    state=idle
					   },
				state=idle,
				registered = true}};

handle_button_register(#phone_state{gdr_state=GdrState,
				    dts_status=Dts_status,
				    register_loop_pid={RegPid,DatePid},
				    registered = true,
				    socket=Socket,
				    options = #sip_phone_options{
				    	rtplocal_port={_RTPport,RTPsock,RTP_Pid},
				    	rtpremote_port={_RTPRport,RTPRsock,RTPR_Pid}
					} = Options
				   } = State) ->
    close_RTP_port(RTP_Pid,RTPsock),
    close_RTP_port(RTPR_Pid,RTPRsock),
    case RegPid of
	undefined -> true;
	Register_loop_Pid -> exit(Register_loop_Pid, shutdown)
    end,
    case DatePid of
	undefined -> true;
	Date_loop_Pid -> exit(Date_loop_Pid, shutdown)
    end,
    {Message,Options1} = sip_stack:sip_message(unregister,Options),
    send_host(Socket,Options1,[Message],GdrState),
    {H, _R, _L, D, {_T,La,M}} = Dts_status,
    {NewGdrstate, NewDts_status} =
	init_display("Not connected",
		     {H, no_ring, h323_phone:indicators(GdrState#gdr_state.type),D, {silence,La,M}},
		     GdrState),
    {noreply, State#phone_state{dts_status = NewDts_status,
				gdr_state=NewGdrstate,
				options = Options1#sip_phone_options{
					    rtplocal_port={0,0,0},
					    rtpremote_port={0,0,0},
					    rtpremote_ip="",
					    old_call_info={},
					    contact = "",
					    state=idle
					   },
				state=idle,
				registered = false}};

handle_button_register(State) ->
    {noreply, State}.
%% -----------------------------------------------------------------------------

%% Erlang GS handling
handle_button_auto_answer(#phone_state{state=idle,
				       phone_pid = undefined} = State) ->
    {noreply, State};
handle_button_auto_answer(#phone_state{state=idle,
				 gs_ids = GS_IDs,
				 phone_pid = Phone_Pid} = State) ->
    #gs_ids{auto_answer = AutoAnswer} = GS_IDs,
    Enable = gs:read(AutoAnswer, select),
    Send =
	case Enable of
	    true -> {true, ?DEFAULT_AUTO_ANSWER_TIME};
	    false -> false
	end,
    h323_phone:set_auto_answer(Phone_Pid, Send),
    {noreply, State};
handle_button_auto_answer(State) ->
    {noreply, State}.
%% -----------------------------------------------------------------------------

handle_button_quit(State) ->
    case State#phone_state.option_pid of
	Pid when pid(Pid) -> gen_server:cast(Pid, quit);
	_ -> continue
    end,
    {stop, normal, State}.
%% -----------------------------------------------------------------------------

handle_button_offhook(#phone_state{state=_idle,registered=false}=State) ->
    {noreply,State};
handle_button_offhook(#phone_state{state=idle,
				   dts_status = Dts_status,
				   gdr_state=GdrState,
				   options = #sip_phone_options{
				     state=idle,
				     own_phoneno = Own_phoneno,
				     rtp_feature = RTP_feature,
				     rtplocal_port = {RTPport0,_RTPsock0,_RTP_Pid0},
				     gatekeeper_host = Gatekeeper_host
				    } = Options,
				   socket=Socket}=State) ->
    {RTPport,RTPsock,RTP_Pid} = h323_client:open_rtp({string_to_tuple(Gatekeeper_host), RTPport0, 0, RTP_feature}),
    {Message1,Options1} = sip_stack:sip_message(invite,Options#sip_phone_options{
							 bprty_phoneno = "",
							 aprty_phoneno = Own_phoneno,
							 aprty_name=""
							 }),
    {Message2,Options2} = sip_stack:sip_message(sdp,Options1),
    Message3=string:concat(Message1,Message2),
    send_host(Socket,Options2,[Message3],GdrState),
    {_H, R, L, D, {T,La,Mn}} = Dts_status,
    Dts_status2 = {lift,R,L,D,{T,La,Mn}},
    eaa_dts:gdr_update(Dts_status2, GdrState),
    {noreply,State#phone_state{state=outgoing_callproc,
			       dts_status = Dts_status2,
			       options = Options2#sip_phone_options{
					   state=outgoing_callproc,
					   rtplocal_port={RTPport,RTPsock,RTP_Pid}}}};

handle_button_offhook(#phone_state{state=idle,
				   dts_status = Dts_status,
				   gdr_state=GdrState,
				   options = #sip_phone_options{
				     gatekeeper_host = RTP_IPAddress,
				     rtp_feature = RTP_feature,
				     rtplocal_port={RTPport0,_RTPsock0,_RTP_Pid0},
				     state=outgoing_digit_rec,
				     own_phoneno = Own_phoneno} = Options,
				   socket=Socket}=State) ->
    {RTPport,RTPsock,RTP_Pid} = h323_client:open_rtp({string_to_tuple(RTP_IPAddress), RTPport0, 0, RTP_feature}),
    {Message1,Options1} = sip_stack:sip_message(invite,Options#sip_phone_options{
							 rtplocal_port={RTPport,RTPsock,RTP_Pid},
							 aprty_phoneno = Own_phoneno,
							 aprty_name=""
							 }),
    {Message2,Options2} = sip_stack:sip_message(sdp,Options1),
    Message3=string:concat(Message1,Message2),
    send_host(Socket,Options2,[Message3],GdrState),
    {_H, R, L, D, {T,La,Mn}} = Dts_status,
    Dts_status2 = {lift,R,L,D,{T,La,Mn}},
    eaa_dts:gdr_update(Dts_status2, GdrState),
    {noreply,State#phone_state{state=outgoing_callproc,
			       dts_status = Dts_status2,
			       options = Options2#sip_phone_options{
					   state=outgoing_callproc,
					   rtplocal_port={RTPport,RTPsock,RTP_Pid}}}};

handle_button_offhook(#phone_state{state=idle,
				   dts_status = Dts_status,
				   gdr_state=GdrState,
				   options = #sip_phone_options{
				     gatekeeper_host = RTP_IPAddress,
				     rtp_feature = RTP_feature,
				     rtplocal_port={RTPport0,_RTPsock0,_RTP_Pid0},
				     state=outgoing_ring,
				     own_phoneno = Own_phoneno} = Options,
				   socket=Socket}=State) ->
    {RTPport,RTPsock,RTP_Pid} = h323_client:open_rtp({string_to_tuple(RTP_IPAddress), RTPport0, 0, RTP_feature}),
    {Message1,Options1} = sip_stack:sip_message(invite,Options#sip_phone_options{
							 rtplocal_port={RTPport,RTPsock,RTP_Pid},
							 aprty_phoneno = Own_phoneno,
							 aprty_name=""
							 }),
    {Message2,Options2} = sip_stack:sip_message(sdp,Options1),
    Message3=string:concat(Message1,Message2),
    send_host(Socket,Options2,[Message3],GdrState),
    {_H, R, L, D, {T,La,Mn}} = Dts_status,
    Dts_status2 = {lift,R,L,D,{T,La,Mn}},
    eaa_dts:gdr_update(Dts_status2, GdrState),
    {noreply,State#phone_state{state=outgoing_callproc,
			       dts_status = Dts_status2,
			       options = Options2#sip_phone_options{
					   state=outgoing_callproc,
					   rtplocal_port={RTPport,RTPsock,RTP_Pid}}}};

handle_button_offhook(#phone_state{state=idle,
				   dts_status=Dts_status,
				   gdr_state=GdrState,
				   options = #sip_phone_options{aprty_phoneno = Anumber,
							     	gatekeeper_host = RTP_IPAddress,
				     				rtplocal_port = {RTPport0,_RTPsock0,_RTP_Pid0},
								rtpremote_port = {_RTP_port,_RTPRsock0,_RTPR_Pid},
								rtp_feature = RTP_feature,
								state=incomming_ring} = Options,
				   socket = Socket}=State) ->
    {RTPport,RTPsock,RTP_Pid} = h323_client:open_rtp({string_to_tuple(RTP_IPAddress), RTPport0, 0, RTP_feature}),
    {Message1,Options1} = sip_stack:sip_message(sip200ok_invite,Options#sip_phone_options{rtplocal_port={RTPport,RTPsock,RTP_Pid}}),
    {Message2,Options2} = sip_stack:sip_message(sdp_phone,Options1),
    Message3=string:concat(Message1,Message2),
    send_host(Socket,Options2,[Message3],GdrState),
    gen_server:cast(self(), {ringType_info, no_ring}),
    Dts_status1 = write_display(string:left("",20,$ ),{0,2}, Dts_status, GdrState),
    Dts_status2 = write_display(string:left("",40,$ ),{0,3},Dts_status1,GdrState),
    Dts_status3 = write_display(string:left(Anumber,20,$ ),{0,3}, Dts_status2, GdrState),
    {_H, R, L, D, {T,La,Mn}} = Dts_status3,
    Dts_status4 = {lift,R,L,D,{T,La,Mn}},
    eaa_dts:gdr_update(Dts_status4, GdrState),
    {noreply,State#phone_state{dts_status=Dts_status4,
			       state=incomming_answer,
			       options = Options2#sip_phone_options{state=incomming_answer}}};

handle_button_offhook(#phone_state{state=_idle,
				   dts_status=_Dts_status,
				   gdr_state=_GdrState,
				   options = #sip_phone_options{aprty_phoneno = _Anumber,
								rtpremote_port = {_RTP_port,_RTPsock0,_RTP_Pid},
								rtp_feature = _RTP_feature,
								state=_State} = _Options,
				   socket = _Socket,
				   registered=_false}=State) ->
    {noreply,State}.


%------------------------------------------------------------------------------
handle_button_onhook(#phone_state{state=idle,registered=false}=State) ->
    {noreply,State};
handle_button_onhook(#phone_state{dts_status=Dts_status,
				  gdr_state=GdrState,
				  options = #sip_phone_options{rtplocal_port={_RTPport,RTPsock,RTP_Pid},
							       rtpremote_port={_RTPRport,RTPRsock,RTPR_Pid},
							       own_phoneno=Own_phoneno,
 							       own_name = Own_name,
							       gatekeeper_host=GKHost,
							       state=outgoing_ring} = Options,
				socket = Socket} = State) ->

    {Message1,Options1} = sip_stack:sip_message(cancel,Options),
    send_host(Socket,Options1,[Message1],GdrState),
    {_H, _R, _L, D, {_T,La,M}} = Dts_status,
    {NewGdrstate, Dts_status2} =
	init_display(GKHost,
		     {replace, no_ring, h323_phone:indicators(GdrState#gdr_state.type),D, {silence,La,M}},
		     GdrState),
    gen_server:cast(self(), {own_phoneno, string:strip(Own_name, both, $"), Own_phoneno}),
    gen_server:cast(self(), date_loop),
    close_RTP_port(RTP_Pid,RTPsock),
    close_RTP_port(RTPR_Pid,RTPRsock),
    {noreply,State#phone_state{dts_status = Dts_status2,
			       gdr_state=NewGdrstate,
			       state=idle,
			       options = Options#sip_phone_options{
					   rtplocal_port={0,0,0},
					   rtpremote_port={0,0,0},
					   rtpremote_ip="",
					   old_call_info={},
					   contact = "",
					   state=idle
					  }}};

handle_button_onhook(#phone_state{dts_status=Dts_status,
				  gdr_state=GdrState,
				  options = #sip_phone_options{rtplocal_port={_RTPport,RTPsock,RTP_Pid},
							       rtpremote_port={_RTPRport,RTPRsock,RTPR_Pid},
							       own_phoneno=Own_phoneno,
							       own_name = Own_name,
							       gatekeeper_host=GKHost,
							       state=outgoing_answer} = Options,
				  socket = Socket} = State) ->
    {Message1,Options1} = sip_stack:sip_message(outgoing_bye,Options),
    send_host(Socket,Options1,[Message1],GdrState),
    {_H, _R, _L, D, {_T,La,M}} = Dts_status,
    {NewGdrstate, Dts_status2} =
	init_display(GKHost,
		     {replace, no_ring, h323_phone:indicators(GdrState#gdr_state.type),D, {silence,La,M}},
		     GdrState),
    gen_server:cast(self(), {own_phoneno, string:strip(Own_name, both, $"), Own_phoneno}),
    gen_server:cast(self(), date_loop),
    close_RTP_port(RTP_Pid,RTPsock),
    close_RTP_port(RTPR_Pid,RTPRsock),
    {noreply,State#phone_state{dts_status = Dts_status2,
			       gdr_state=NewGdrstate,
			       state=idle,
			       options = Options1#sip_phone_options{
					   rtplocal_port={0,0,0},
					   rtpremote_port={0,0,0},
					   rtpremote_ip="",
					   old_call_info={},
					   contact = "",
					   state=idle
					  }}};

handle_button_onhook(#phone_state{dts_status=Dts_status,
				gdr_state=GdrState,
				options = #sip_phone_options{rtplocal_port={_RTPport,RTPsock,RTP_Pid},
							     rtpremote_port={_RTPRport,RTPRsock,RTPR_Pid},
							     own_phoneno=Own_phoneno,
							     own_name = Own_name,
							     gatekeeper_host=GKHost} = Options,
				socket = Socket} = State) ->
    {Message1,Options1} = sip_stack:sip_message(incomming_bye,Options),
    send_host(Socket,Options1,[Message1],GdrState),
    {_H, _R, _L, D, {_T,La,M}} = Dts_status,
    {NewGdrstate, Dts_status2} =
	init_display(GKHost,
		     {replace, no_ring, h323_phone:indicators(GdrState#gdr_state.type),D, {silence,La,M}},
		     GdrState),
    gen_server:cast(self(), {own_phoneno, string:strip(Own_name, both, $"), Own_phoneno}),
    gen_server:cast(self(), date_loop),
    close_RTP_port(RTP_Pid,RTPsock),
    close_RTP_port(RTPR_Pid,RTPRsock),
    {noreply,State#phone_state{dts_status = Dts_status2,
			       gdr_state=NewGdrstate,
			       state=idle,
			       options = Options1#sip_phone_options{
					   rtplocal_port={0,0,0},
					   rtpremote_port={0,0,0},
					   rtpremote_ip="",
					   old_call_info={},
					   contact = "",
					   state=idle
					  }}}.
%--------------------------------------------------------------------------------
handle_keypad_button([Button|Remaing],State)->
    {noreply,State2}=handle_keypad_button(Button,State),
    timer:sleep(State2#phone_state.interdigit_delay),
    handle_keypad_button(Remaing,State2);
handle_keypad_button(Button,#phone_state{
		       options = #sip_phone_options{state=idle} = Options,
		       state=_CallState
		      } = State) ->

    handle_keypad_button(Button,State#phone_state{dialled_no="",
						  options = Options#sip_phone_options{
							      state=outgoing_digit_rec,
							      service_no = "",
							      bprty_phoneno = ""}});
handle_keypad_button(Button,#phone_state{gs_ids = GS_IDs,
					 dts_status = Dts_status,
					 options = Options,
					 socket = _Socket,
					 state=outgoing_digit_rec} = State) ->

    #gs_ids{
	    button_speaker = Button_speaker,
	    button_clear = Button_clear,
	    button_f1 = Button_f1,
	    button_f2 = Button_f2,
	    button_f3 = Button_f3,
	    button_f4 = Button_f4,
	    button_1 = Button_1,
	    button_2 = Button_2,
	    button_3 = Button_3,
	    button_4 = Button_4,
	    button_5 = Button_5,
	    button_6 = Button_6,
	    button_7 = Button_7,
	    button_8 = Button_8,
	    button_9 = Button_9,
	    button_star = Button_star,
	    button_0 = Button_0,
	    button_hash = Button_hash}=GS_IDs,
    Message =
	case Button of
	    Button_speaker -> ?Button_speaker; 
	    Button_clear -> ?Button_clear; 
	    Button_f1 -> ?Button_F1; 
	    Button_f2 -> ?Button_F2; 
	    Button_f3 -> ?Button_F3; 
	    Button_f4 -> ?Button_F4; 
	    Button_1 -> ?Button_Digit1; 
	    Button_2 -> ?Button_Digit2; 
	    Button_3 -> ?Button_Digit3;
	    Button_4 -> ?Button_Digit4;
	    Button_5 -> ?Button_Digit5; 
	    Button_6 -> ?Button_Digit6; 
	    Button_7 -> ?Button_Digit7;
	    Button_8 -> ?Button_Digit8;
	    Button_9 -> ?Button_Digit9;
	    Button_star -> "simulateKeyPush *";
	    Button_0 -> ?Button_Digit0; 
	    Button_hash -> "simulateKeyPush #";
	    _ ->
		undefined
	end,
    Message1 = string:substr(Message,17,1),
    State1 = 
	case Message1 of
	    undefined ->
		State;
	    _ ->
		Bprty_number = Options#sip_phone_options.bprty_phoneno,
		Bprty_number1 = string:concat(Bprty_number, Message1),
		{Dialled_no,NewDts_status} = write_dialled_to_display(Message1,Dts_status,State),
		State#phone_state{options=Options#sip_phone_options{bprty_phoneno = Bprty_number1},
				  dts_status=NewDts_status,
				  dialled_no=Dialled_no}
	end,
    {noreply,State1}.
%% -----------------------------------------------------------------------------

handle_button_option(#phone_state{gdr_state=GdrState,
				  options = Options,
				  socket = Socket} = State) ->
    send_host(Socket,Options,["printDebugDisplay","printDebugLED"],GdrState),
    {noreply, State}.
% This clause will never match!
%handle_button_option(#phone_state{option_pid=Pid} = State) ->
%    gen_server: cast(Pid, raise),
%    {noreply, State}.
%% -----------------------------------------------------------------------------


handle_alerted(#phone_state{gs_ids = GS_IDs} = State) ->
    #gs_ids{state=DisplayState} = GS_IDs,
    %% Check if Erlang graphics is used for debug purposes
    check_call_toGS(DisplayState,"Alerting"),
    State#phone_state{state=alerted}.
%% -----------------------------------------------------------------------------

handle_alerting(#phone_state{gs_ids = GS_IDs} = State) ->
    #gs_ids{state=DisplayState,
	    ring_type = RingType} = GS_IDs,
    %% Check if Erlang graphics is used for debug purposes
    check_call_toGS(DisplayState,"Alerted"),
    check_call_toGS(RingType,"ringing"),
    State#phone_state{state=alerting}.
%% -----------------------------------------------------------------------------

handle_callProceeding(#phone_state{gs_ids = GS_IDs} = State) ->
    #gs_ids{state=DisplayState} = GS_IDs,
    %% Check if Erlang graphics is used for debug purposes
    check_call_toGS(DisplayState,"CallProceeding"),
    State#phone_state{state=callProceeding}.
%% -----------------------------------------------------------------------------

handle_dtmf(DTMF, #phone_state{gs_ids = GS_IDs}) ->
    #gs_ids{display1 = Display1} = GS_IDs,
    %% Check if Erlang graphics is used for debug purposes
    check_call_toGS(Display1,DTMF).
%% -----------------------------------------------------------------------------

%handle_regInfo({E164, GatekeeperID}, #phone_state{gs_ids = GS_IDs} = State) ->
%    #gs_ids{e164 = GS_E164,
%	    gk_id = GS_GK_ID} = GS_IDs,
%    set_label(GS_E164, E164),
%    set_label(GS_GK_ID, GatekeeperID),
%    handle_disconnected(State),
%    State#phone_state{registered = true}.
%% -----------------------------------------------------------------------------


%% =============================================================================
%% Second level help functions to handler functions.
%% =============================================================================


check_call_toGS(undefined,_ListT) ->
    true;
check_call_toGS(Display,ListT) ->
    gs:config(Display, {label, {text, ListT}}).
%% -----------------------------------------------------------------------------

set_led([[$v,$a,$l,$u,$e,$ ,$=,$ ,$0|_LED1]|_Tail],Dts_status,_GdrState) ->
    Dts_status;
set_led([[$L,$E,$D|LED1]|Tail],Dts_status,GdrState) ->
    {Lno1,Ledstate} =
	case string:tokens(LED1, ",") of
	    [L1|[Stat1]] ->
		case string:tokens(Stat1, "=") of
		    [_,[$C,$A,$D,$0|_]]-> {L1,double};
		    [_,[$C,$A,$D,$1|_]]-> {L1,fast};
		    [_,[$O,$N|_]]-> {L1,on};
		    _ -> {L1,off}
		end;
	    _ ->
		{"undefined","undefined"}
	end,
    Dts_status_new = 
	case string:tokens(Lno1, "=") of
	    [_,[$ ,$1|_]]->
		led(1, Ledstate,Dts_status,GdrState);
	    [_,[$ ,$2|_]]->
		led(2, Ledstate,Dts_status,GdrState);
	    [_,[$ ,$3|_]]->
		led(3, Ledstate,Dts_status,GdrState);
	    [_,[$ ,$6|_]]->
		led(12,Ledstate,Dts_status,GdrState);
	    [_,[$ ,$7|_]]->
		led(9,Ledstate,Dts_status,GdrState);
	    [_,[$ ,$8|_]]->
		led(10,Ledstate,Dts_status,GdrState);
	    [_,[$ ,$9|_]]->
		led(11,Ledstate,Dts_status,GdrState);
	    [_,[$1,$0|_]]->
		led(8, Ledstate,Dts_status,GdrState);
	    [_,[$8,$8|_]]->
		led(-1,Ledstate,Dts_status,GdrState);
	    _->Dts_status
	end,
    set_led(Tail,Dts_status_new,GdrState);
set_led(_Info,Dts_status,_GdrState) ->
    Dts_status.
%% -----------------------------------------------------------------------------

set_label(Label, String) ->
    Filtered = 
	lists:filter(fun(X) ->
			     if
				 X >= 32, X =< 126 -> true;
				 true -> false
			     end
		     end,
		     String),
    %% Check if Erlang graphics is used for debug purposes
    check_call_toGS(Label,Filtered).
%% -----------------------------------------------------------------------------

init_display(IP_string, Dts_status, GdrState) ->
    NewGdrstate=GdrState#gdr_state{connected_to = IP_string},
    Display = h323_phone:display(GdrState#gdr_state.type),
    {H, R, L, _D, {T,La,M}} = Dts_status,
    NewDts_status = {H, R, L, Display, {T,La,M}},
    eaa_dts:gdr_update(NewDts_status, NewGdrstate),
    {NewGdrstate, NewDts_status}.

write_display(DisplayRow,Pos,Dts_status,GdrState) ->
    case DisplayRow of
	"" ->
	    eaa_dts:display(Pos,{write,"          "},Dts_status, GdrState);
	_ ->
	    eaa_dts:display(Pos,{write,DisplayRow},Dts_status, GdrState)
    end.
write_dialled_to_display(Digit,Dts_status,#phone_state{gdr_state=GdrState} = State)->
    case GdrState#gdr_state.type of 
	dbc425->
	    Dialled_no = string:concat(State#phone_state.dialled_no,[Digit]),
	    Dialled_no2 = string:left(Dialled_no, 30, $ ),
	    Dts_status2 = write_display(Dialled_no2,{1,3}, Dts_status, State#phone_state.gdr_state),
	    Dts_status3 = write_display("Enter no:",{0,3}, Dts_status2, State#phone_state.gdr_state),
	    {Dialled_no,Dts_status3};
	_->
	    Dialled_no = string:concat(State#phone_state.dialled_no,[Digit]),
	    Dialled_no2 = string:right(Dialled_no, 40, $ ),
	    NewDts_status = write_display(Dialled_no2,{0,2}, Dts_status, State#phone_state.gdr_state),
	    {Dialled_no,NewDts_status}
    end.

%% -----------------------------------------------------------------------------

%% Changes the state of one or all of the LEDs in the DTS_Status.
led(all,Ledstate,{H,R,I,D,T},GdrState) ->
    {H,R,led_change_all(Ledstate,I,GdrState),D,T};

led(N,Ledstate,{H,R,I,D,T},GdrState) ->
    case lists:keysearch(N,1,I) of
	{value,{_,Name,_}} ->
	    eaa_common:gdr_led_update({Name,Ledstate},GdrState),
	    {H,R,lists:keyreplace(N,1,I,{N,Name,Ledstate}),D,T};
	_ ->
	    {H,R,I,D,T}    % e.g. dbc501 has no speaker indicator...
    end.

led_change_all(Ledstate,[{N,Name,_}|T],GdrState) ->
    eaa_common:gdr_led_update({Name,Ledstate},GdrState),
    [{N,Name,Ledstate}|led_change_all(Ledstate,T,GdrState)];
led_change_all(_,[],_) ->
    [].
%% -----------------------------------------------------------------------------


%% =============================================================================
%% General help functions
%% =============================================================================

%% -----------------------------------------------------------------------------
%% Functions handling connection to the IP phone.
%% -----------------------------------------------------------------------------

%% Init help function opening communication to the phone. Communication can be
%% done over different protocols. The field phone_host contains an address
%% read directly from a configuration file. Note that there is an old and a new
%% format for defining telnet sockets.
%% Returns a phone communication handler, which is:
%% {telnet,Socket} or {ssh,{CM,Channel}}, or 'undefined' if it fails to open.
%open_phone_communication(Opts=#h323_phone_options{phone_host=Addr,phone_port=PortNo}) ->
%    case Addr of                            % Examining definition.
%	{telnet,Address} ->                 % "New" telnet definition.
%	    open_phone_communication_telnet(Address,PortNo);
%	{ssh,Address} ->
%	    open_phone_communication_ssh(Address,PortNo,Opts);
%	Address when is_list(Address);(is_tuple(Address) and size(Address)==4) ->
%	    open_phone_communication_telnet(Address,PortNo)
%    end.

%open_phone_communication_telnet(Address,PortNo) ->
%    case gen_tcp:connect(Address,PortNo,[{reuseaddr,true}]) of
%	{ok,Socket} ->
%	    ?DEBUG_PRINT("Connected to socket ~p~n",[Socket]),
%	    {telnet,Socket};                % phone_communication_handle().
%	{error,Reason} ->
%	    ?DEBUG_PRINT("  error, Reason:~s~n",[inet:format_error(Reason)]),
%	    undefined
%    end.

%open_phone_communication_ssh(Address,{},Opts) ->
%    open_phone_communication_ssh(Address,22,Opts);
%open_phone_communication_ssh(Address,PortNo,Opts) ->
%    User=Opts#h323_phone_options.user,
%    Passwd=Opts#h323_phone_options.user_pw,
%    case ssh_cm:connect(Address,
%			PortNo,
%			[{user,User},{password,Passwd},{silently_accept_hosts,true}]) of
%	{ok,CM} ->
%	    case ssh_cm:session_open(CM,10000) of
%		{ok,Channel} ->
%		    case ssh_cm:shell(CM,Channel) of
%			ok ->
%			    {ssh,{CM,Channel}}; % phone_communication_handle().
%			Error ->
%			    ?DEBUG_PRINT("Error ssh shell initiation ~p~n",[Error]),
%			    ssh_cm:close(CM,Channel),
%			    undefined
%		    end;
%		Error ->
%		    ?DEBUG_PRINT("Error ssh session_open ~p~n",[Error]),
%		    undefined
%	    end;
%	Error ->str()}};
%	   ?DEBUG_PRINT("Error ssh connect ~p~n",[Error]), 
%	    undefined
%    end.
%% -----------------------------------------------------------------------------

%% Help function doing the actual sending to the phone. Here we must have
%% knowledge of the various communication methods.
%% Returns 'ok' or {error,Reason}.
send_to_host(Socket,Options,String,GdrState) when list(String) ->
    ?DEBUG_PRINT("SIP ~s ==>~n~s~n",[get_time_str(),String]),
    case GdrState#gdr_state.log of
	opened->
	    Sip_Message = format_string(outgoing,String),
	    Signal_head = {outgoing,lf:sformat("~s  SIP  ==>",[ltime:format_time(get_time_lst())])},
	    Formatted = [{comment,""},Signal_head|Sip_Message],
%	    Formatted = eaa_common:format_gdr_signal(GdrState#gdr_state.id,outgoing,{0,a,"  SIP ==> ~n"},get_time_lst()), % ++ String
	    eaa_common:send_to_gui(?GS_REG, GdrState, {log ,Formatted});
	_->
	    []
    end,
    Port = sip_stack:get_sip_port(Options),
    GKAddress = sip_stack:gatekeeper_ipaddress(Options),
    gen_udp:send(Socket, GKAddress, Port, String);
%    case gen_tcp:send(Socket,String) of
%	ok ->
%	    ?DEBUG_PRINT("String sent to socket ~p~n",[String]),
%	    ok;
%	{error,Reason} ->
%	    ?DEBUG_PRINT("Error sending string to socket ~p~n",[Reason]),
%	    {actions,["Error sending string to socket"]}
%    end;
send_to_host(Socket,_Options,_,_GdrState) ->
    ?DEBUG_PRINT("Unknown phone com handler ~p~n",[Socket]),
    ok.
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% Functions handling the logfile.
%% -----------------------------------------------------------------------------

%% Help function opening a logfile if one is specified in the StartOptions.
%% Returns {LogFileName,LogFileDescriptor}.
open_logfile(StartOptions) ->
    case get_tag_option(logname,StartOptions,"") of
	"" ->                               % No logfile to open.
	    {"",undefined};
	Logname ->
	    FD=try
		case file:open(Logname,[append]) of
		    {ok,FD2} ->
			{ok,Cwd}=file:get_cwd(),
			?DEBUG_PRINT("File ~s/~s opened.~n",[Cwd,Logname]),
			FD2;
		    {error,Reason2} ->
			?DEBUG_PRINT("File opening error, Reason:~s.~n",
				     [file:format_error(Reason2)]),
			undefined
		end
		catch
		    error:Reason ->
			?DEBUG_PRINT("File opening failure, Reason:~p.~n",[Reason]),
			undefined
		end,
    {Logname,FD}
end.
%% -----------------------------------------------------------------------------
	    
close_logfile(undefined) ->
    ok;
close_logfile(FD) ->
    file:close(FD).
%% -----------------------------------------------------------------------------

%% Print to log for fault isolation, in PWD or path given in .sw file.
%print_toLog(Data,#phone_state{filedesc = FileDesc}) ->
%    case catch file:write(FileDesc,[Data]) of
%	ok ->
%	    ?DEBUG_PRINT("Data written to file.~n",[]);
%	{error,Reason} ->
%	    ?DEBUG_PRINT("File writing error, Reason:~s.~n",[file:format_error(Reason)]);
%	{'EXIT',Reason} ->
%	    ?DEBUG_PRINT("File writing failure, Reason:~s.~n",[Reason])
%    end.
%% -----------------------------------------------------------------------------


%% -----------------------------------------------------------------------------
%% Functions handling the options file.
%% -----------------------------------------------------------------------------

%% Help function reading a configuration file and setting the various record
%% field accordingly.
%% Returns a #h323_phone_options.
read_options(Filename) ->
    {ok,Cwd}=file:get_cwd(),
    ?DEBUG_PRINT("Reading options from ~s/~s.~n",[Cwd,Filename]),
    case catch file:consult(Filename) of
	{ok,Options} ->
	    Version = get_tag_option(version,Options,undefined),
	    CL_IP = get_tag_option(client_ipaddress,Options,undefined),
	    sip_port = get_tag_option(sip_port,Options,undefined),
	    Phone_port = get_tag_option(phone_port,Options,undefined),
	    Phone_number = get_tag_option(own_phoneno,Options,undefined),
	    Wsp_port = get_tag_option(wsp_port,Options,undefined),
	    Rtp_feature = get_tag_option(rtp_feature,Options,undefined),
	    GK_Host = get_tag_option(gatekeeper_host,Options,undefined),
	    #sip_phone_options{sw_version=Version,
			       client_ipaddress=CL_IP,
			       sip_port=sip_port,
			       phone_port=Phone_port,
			       own_phoneno=Phone_number,
			       wsp_port=Wsp_port,
			       rtp_feature=Rtp_feature,
			       gatekeeper_host=GK_Host
			      };
	{error,Reason} ->
	    ?DEBUG_PRINT("Reading error, Reason:~s.~n",[file:format_error(Reason)]),
	    #sip_phone_options{};
	{'EXIT',Reason} ->
	    ?DEBUG_PRINT("Reading failure, Reason:~s.~n",[Reason]),
	    #sip_phone_options{}
    end.
%% -----------------------------------------------------------------------------

%% Help function writing current configuration back the start-up configuration
%% file.
%write_options("",_Options) ->
%    ok;
%write_options(Filename,Options) ->
%    #h323_phone_options{bprty_phoneno=Bprty_phoneno,
%			elu32=ELU32,
%			client_ipaddress=CL_ip,
%			phone_connect=_Gatekeeper,
%			phone_host=GK_Host,
%			phone_port=GK_ID
%		       }=Options,
%    {ok,PWD}=file:get_cwd(),
%    ?DEBUG_PRINT("Writing options to ~s/~s.~n",[PWD,Filename]),
%    case file:open(Filename,[write]) of
%	{ok, FD} ->
%	    io:format(FD, "{fastStart,~w}.~n",[FastStart]),
%	    io:format(FD, "{user,\"~s\"}.~n",[User]),
%	    io:format(FD, "{user_pw,\"~s\"}.~n",[User_id]),
%	    io:format(FD, "{user_e164(phone_number),\"~s\"}.~n",[Phone_number]),
%	    io:format(FD, "{elu32,\"~s\"}.~n",[ELU32]),
%	    io:format(FD, "{client_ipaddress,\"~s\"}.~n",[CL_ip]),
%	    io:format(FD, "{phone_host,\"~s\"}.~n",[GK_Host]),
%	    io:format(FD, "{phone_port,\"~w\"}.~n",[GK_ID]),
%	    file:close(FD);
%	{error,Reason} ->
%	    ?DEBUG_PRINT("Writing error, Reason:~s.~n",[file:format_error(Reason)]),
%	    ok
%    end.
%% -----------------------------------------------------------------------------


%% Returns a new string buffer.
mk_buffer() ->
    ets:new(sip_phone_buffer,[set,{keypos,1}]).
%% -----------------------------------------------------------------------------

%% inserts a string into the slot NextSlot. Will also clean away strings that
%% are to old. Returns {NewBuffer,NewNextSlot,NewLastSlot}.
%insert_buffer(Buffer,NextSlot,OldestSlot,String) ->
%    NewOldestSlot=remove_expired_strings(Buffer,NextSlot,OldestSlot),
%    ets:insert(Buffer,{NextSlot,erlang:now(),String}),
%    {Buffer,NextSlot+1,NewOldestSlot}.

%% Removes all strings from the buffer that are older than the maximum time
%% we shall save a string.
%% Returns NewOldestSlot.
%remove_expired_strings(_Buffer,NextSlot,OldestSlot) when OldestSlot+1==NextSlot ->
%    OldestSlot;                          % No need to clean if empty.
%remove_expired_strings(Buffer,NextSlot,OldestSlot) ->
%    [{_,Now,_String}]=ets:lookup(Buffer,OldestSlot+1),
%    case is_timeout(Now) of
%	true ->                          % Yes this one shall be removed!
%	    ets:delete(Buffer,OldestSlot+1),
%	    remove_expired_strings(Buffer,NextSlot,OldestSlot+1);
%	false ->                         % No keep it a bit longer.
%	    OldestSlot                   % Done then.
%    end.

%% Function calculating the time difference between the argument (expressed in
%% Erlang now()) and current now() time.
%% Returns 'true' if to long time has elapsed, 'false' otherwise.
%% This implementation is intended for small time differences not causing big
%% nums to be activated (from efficiency reasons).
%is_timeout({MegS0,Sec0,MicS0}) ->
%    {MegS1,Sec1,MicS1}=erlang:now(),
%    DiffMicS=(Sec1-Sec0+(MegS1-MegS0)*1000000)*1000000 + MicS1-MicS0,
%    if
%	DiffMicS>?BUFFER_TIME*1000 ->
%	    true;
%	true ->
%	    false
%    end.
%% -----------------------------------------------------------------------------

%%% Trims the buffer upto Slot. Meaning that all older slots including Slot are
%%% discarded. Note that if the buffer becomes empty (OldestSlot+1==NextSlot), and
%%% there are no active oip_instr processes, we may reset the slot counters.
%%% Returns {NewBuffer,NewNextSlot,NewOldestSlot}.
%trim_buffer(Buffer,NextSlot,OldestSlot,_,0) when OldestSlot+1==NextSlot ->
%    {Buffer,1,0};                        % Buffer is empty and no active oip_instr.
%trim_buffer(Buffer,NextSlot,OldestSlot,_,_) when OldestSlot+1==NextSlot ->
%    {Buffer,NextSlot,OldestSlot};        % Buffer is empty.
%trim_buffer(Buffer,NextSlot,OldestSlot,Slot,NoOfOipInstr) when OldestSlot+1=<Slot ->
%    ets:delete(Buffer,OldestSlot+1),     % There is at least one to remove.
%    trim_buffer(Buffer,NextSlot,OldestSlot+1,Slot,NoOfOipInstr);
%trim_buffer(Buffer,NextSlot,OldestSlot,_,_) -> % Do not remove more slots!
%    {Buffer,NextSlot,OldestSlot}.
%% -----------------------------------------------------------------------------

%% Function sending all waiting strings in Buffer to the now newly created
%% oip_instr process OipInstr. Returns nothing significant.
%push_buffer(Buffer,NextSlot,OldestSlot,OipInstr) when OldestSlot+1<NextSlot ->
%    [{_,_,String}]=ets:lookup(Buffer,OldestSlot+1),
%    oip_instr:send_buffer(OipInstr,OldestSlot+1,String),
%    push_buffer(Buffer,NextSlot,OldestSlot+1,OipInstr);
%push_buffer(_,_,_,_) ->                  % Now we have sent all waiting strings.
%    true.
%% -----------------------------------------------------------------------------


%% -----------------------------------------------------------------------------
%% Other common help functions.
%% -----------------------------------------------------------------------------

%% Help function picking out a value for a certain tag in a list of option-tuples.
%% Note that the default value may be a fun() which calculates a default value.
%% Returns *the* value.
get_tag_option(Tag,Options,Default) ->
    case lists:keysearch(Tag,1,Options) of
	{value,{_,Value}} ->
	    Value;
	false ->
	    if
		function(Default) ->
		    Default();
		true ->
		    Default
	    end
    end.
%% -----------------------------------------------------------------------------

%% Help function sending the list of strings to all currently active oip_instr
%% processes. The strings are at the same time inserted into the local buffer.
%% Any to old strings are removed from the buffer.
%% Returns {NewBuffer,NewNextSlot,NewOldestSlot}.
%send_to_all_oip_instr(Data,Instr,Buffer,NextSlot,OldestSlot) ->
%    Strings=string:tokens(Data,[13,10]),
%    send_to_all_oip_instr_2(Instr,Buffer,NextSlot,OldestSlot,Strings).

%send_to_all_oip_instr_2(Instr,Buffer,NextSlot,OldestSlot,[String|Rest]) ->
%    {NewBuffer,NewNextSlot,NewOldestSlot}=
%	insert_buffer(Buffer,NextSlot,OldestSlot,String),
%    send_to_all_oip_instr_3(Instr,NextSlot,String),
%    send_to_all_oip_instr_2(Instr,NewBuffer,NewNextSlot,NewOldestSlot,Rest);
%send_to_all_oip_instr_2(_,Buffer,NextSlot,OldestSlot,[]) ->
%    {Buffer,NextSlot,OldestSlot}.

%send_to_all_oip_instr_3([{_Cmd,_From,OipInstr}|Rest],Slot,String) ->
%    oip_instr:send_buffer(OipInstr,Slot,String),
%    send_to_all_oip_instr_3(Rest,Slot,String);
%send_to_all_oip_instr_3([],_,_) ->
%    true.
%% -----------------------------------------------------------------------------


%% =============================================================================
%% Handle instruction functions.
%% =============================================================================
handle_instruction(start_test_case,_Params,State,From) ->
    eaa_common:instruction_res(From,start_test_case,ok),
    State;
handle_instruction(ini_ip_phone,_Params, State, _From) ->
    State;
handle_instruction(register_ip,_Params, #phone_state{registered=Registered} = State,From) ->
    case Registered of
	false->
	    {noreply,State2} = handle_button_register(State),
	    eaa_common:instruction_res(From,register_ip,ok),
	    State2;
	_-> 
	    {noreply,State2} = handle_button_register(State),
	    timer:sleep(300),
	    {noreply,State3} = handle_button_register(State2),
	    eaa_common:instruction_res(From,register_ip, {error,lf:sformat("Faulty register_ip:~s",["Sip_phone already registered"])}),
	    State3
    end;
handle_instruction(unregister_ip,_Params,#phone_state{registered=Registered} = State,From) ->
    case Registered of
	true->
	    {noreply,State2} = handle_button_register(State),
	    eaa_common:instruction_res(From,unregister_ip,ok),
	    State2;
	_-> 
	    eaa_common:instruction_res(From,unregister_ip, {error,lf:sformat("Faulty unregister_ip:~s",["Sip_phone not registered"])}),
	    State
    end;
handle_instruction(check_tone_ip,_Params,State,From) ->
    eaa_common:instruction_res(From,check_tone_ip,ok),
    State;
handle_instruction(set_overlap,_Params,State,From) ->
    eaa_common:instruction_res(From,set_overlap,ok),
    State;
handle_instruction(set_faststart,_Params,State,From) ->
    eaa_common:instruction_res(From,set_faststart,ok),
    State;
handle_instruction(lift_handset_ip,_Params,
		   #phone_state{dts_status = Dts_status,
				gdr_state=GdrState} = State,From) ->
    {Hook, _R, _L, _D, {_T,_La,_M}} = Dts_status,
    State3 = 
	case Hook of
	    replace->    
		Dts_status2 = led(11,double,Dts_status,GdrState),
		{noreply, State2} = handle_button_offhook(State),
		eaa_common:instruction_res(From,lift_handset_ip,ok),
		State2#phone_state{dts_status = Dts_status2};
	    _-> 
		eaa_common:instruction_res(From,lift_handset_ip,{error,"handset is already lifted"}),
		State
	end,
    State3;
handle_instruction(replace_handset_ip,_Params,State,From) ->
    {noreply, State2} = handle_button_onhook(State),
    eaa_common:instruction_res(From,replace_handset_ip,ok),
    State2;
handle_instruction(dial_ip,[Buttons|_],State,From) -> % Buttons, the only element?
    {noreply, State2} = handle_info({From,{gdr_button,Buttons}},State),
    eaa_common:instruction_res(From,dial_ip,ok),
    State2;
handle_instruction(call_ip,[Buttons|_],State,From) -> % Buttons, the only element?
    {noreply, State1} = handle_info({From,{gdr_button,Buttons}},State),
    {noreply, State2} = handle_button_offhook(State1),
    eaa_common:instruction_res(From,call_ip,ok),
    State2;
handle_instruction(send_digit_ip,[Digits],State,From) ->
    {noreply, State1} = handle_info({From,{gdr_button,Digits}},State),
    eaa_common:instruction_res(From,send_digit_ip,ok),
    State1;
handle_instruction(answer_ip,_Params,State,From) ->
    {noreply,State2} = handle_button_offhook(State),
    eaa_common:instruction_res(From,answer_ip,ok),
    State2;
handle_instruction(check_waptone_ip,[Tone],State,From) ->
    check_waptone_ip(Tone, State, From),
    State;
handle_instruction(check_ring_ip,[Ring],State,From) ->
    check_ring_ip(Ring, State, From),
    State;
handle_instruction(check_lang_ip,[Lang],State,From) ->
    check_lang_ip(Lang, State, From),
    State;

handle_instruction(press_ip_button,[Button|_Params],State,From) ->
    {noreply, State2} = handle_info({From,{gdr_button,list_to_atom(Button)}},State),
    eaa_common:instruction_res(From,press_ip_button,ok),
    State2;
handle_instruction(press_ip_button_msg,[Button|_Params],State,From) ->
    {noreply, State2} = handle_info({From,{gdr_button,list_to_atom(Button)}},State),
    eaa_common:instruction_res(From,press_ip_button_msg,ok),
    State2;

handle_instruction(recv_rtp, [], IP_State,From) ->
    recv_rtp(IP_State,From),
    IP_State;
handle_instruction(recv_rtp, [PT], IP_State,From) ->
    recv_rtp(PT, IP_State,From),
    IP_State;
handle_instruction(not_recv_rtp, [], IP_State,From) ->
    not_recv_rtp(IP_State,From),
    IP_State;
handle_instruction(not_recv_rtp, [PT], IP_State,From) ->
    not_recv_rtp(PT, IP_State,From),
    IP_State;

handle_instruction(Cmd,_Params,State,From) ->
    eaa_common:not_supported(From,instruction,Cmd),
    State.

%% -----------------------------------------------------------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%instruction(clear_codec_list, [], IP_State) ->
%    clear_codec_list(IP_State);
%instruction(add_codec, [Codec], IP_State) ->
%    add_codec(Codec, IP_State);
%instruction(send_dtmf_ip, [DTMF], IP_State) ->
%    send_dtmf_ip(DTMF, IP_State);
%instruction(recv_dtmf_ip, [DTMF], IP_State) ->
%    recv_dtmf_ip(DTMF, IP_State);
%instruction(set_faststart, [Boolean], IP_State) ->
%    set_faststart(Boolean, IP_State);
%instruction(set_version, [Version], IP_State) ->
%    set_version(Version, IP_State);
%instruction(set_overlap, [Boolean], IP_State) ->
%    set_overlap(Boolean, IP_State);



%% -----------------------------------------------------------------------------

%% =============================================================================
%% Functions drawing a GS phone.
%% =============================================================================

%display_phone_draw() ->				%
%    S = gs:start(),
%    Win = gs:create(window, S,             [{title,"IP-Client"},{width,600},{height,450}]),
%    gs:create(label, Win,                  [{x,000},{y,000},{width,50},{height,25},{label,{text, "Display"}}]),
%    Display1 = gs:create(label, Win,        [{x,000},{y,020},{width,800},{height,20},{label, {text, ""}}]),
%    Display2 = gs:create(label, Win,        [{x,000},{y,040},{width,800},{height,20},{label, {text, ""}}]),
%    Display3 = gs:create(label, Win,        [{x,000},{y,060},{width,800},{height,20},{label, {text, ""}}]),
%    Display4 = gs:create(label, Win,        [{x,000},{y,080},{width,800},{height,20},{label, {text, ""}}]),
%    Display5 = gs:create(label, Win,        [{x,000},{y,100},{width,800},{height,20},{label, {text, ""}}]),
%    _Display6 = gs:create(label, Win,        [{x,000},{y,120},{width,800},{height,20},{label, {text, ""}}]),
%    _Display7 = gs:create(label, Win,        [{x,000},{y,140},{width,800},{height,20},{label, {text, ""}}]),

%    Button_F1 = gs:create(button, Win,      [{x,010},{y,155},{width,030},{height,25},{label, {text, "f1"}}]),
%    Button_F2 = gs:create(button, Win,      [{x,110},{y,155},{width,030},{height,25},{label, {text, "f2"}}]),
%    Button_F3 = gs:create(button, Win,      [{x,220},{y,155},{width,030},{height,25},{label, {text, "f3"}}]),
%    Button_F4 = gs:create(button, Win,      [{x,330},{y,155},{width,030},{height,25},{label, {text, "f4"}}]),
%    Button_1 = gs:create(button, Win,      [{x,010},{y,265},{width,030},{height,25},{label, {text, "1"}}]),
%    Button_2 = gs:create(button, Win,      [{x,050},{y,265},{width,030},{height,25},{label, {text, "2"}}]),
%    Button_3 = gs:create(button, Win,      [{x,090},{y,265},{width,030},{height,25},{label, {text, "3"}}]),
%    Button_4 = gs:create(button, Win,      [{x,010},{y,285},{width,030},{height,25},{label, {text, "4"}}]),
%    Button_5 = gs:create(button, Win,      [{x,050},{y,285},{width,030},{height,25},{label, {text, "5"}}]),
%    Button_6 = gs:create(button, Win,      [{x,090},{y,285},{width,030},{height,25},{label, {text, "6"}}]),
%    Button_7 = gs:create(button, Win,      [{x,010},{y,315},{width,030},{height,25},{label, {text, "7"}}]),
%    Button_8 = gs:create(button, Win,      [{x,050},{y,315},{width,030},{height,25},{label, {text, "8"}}]),
%    Button_9 = gs:create(button, Win,      [{x,090},{y,315},{width,030},{height,25},{label, {text, "9"}}]),
%    Button_star = gs:create(button, Win,   [{x,010},{y,345},{width,030},{height,25},{label, {text, "*"}}]),
%    Button_0 = gs:create(button, Win,      [{x,050},{y,345},{width,030},{height,25},{label, {text, "0"}}]),
%    Button_hash = gs:create(button, Win,   [{x,090},{y,345},{width,030},{height,25},{label, {text, "#"}}]),
%    Button_hook = gs:create(button, Win,   [{x,140},{y,155},{width,055},{height,25},{label, {text, "offhook"}}]),
%    RingType = gs:create(button, Win,      [{x,140},{y,195},{width,055},{height,25},{label, {text, "no_ring"}}]),
%    Button_reg = gs:create(button, Win,    [{x,220},{y,195},{width,075},{height,25},{label, {text, "register"}}]),
%    AutoAnswer = gs:checkbutton(Win,       [{x,220},{y,235},{width,150},{height,25},{label, {text, "Auto Answer"}},{group,a}]),
%    Button_quit = gs:create(button, Win,   [{x,140},{y,235},{width,055},{height,25},{label, {text, "quit"}}]),
%    Button_option = gs:create(button, Win, [{x,140},{y,235},{width,055},{height,25},{label, {text, "options"}}]),
%    Button_speaker = gs:create(button, Win, [{x,010},{y,405},{width,050},{height,25},{label, {text, "speak"}}]),
%    Button_clear = gs:create(button, Win,   [{x,070},{y,405},{width,050},{height,25},{label, {text, "clear"}}]),
%    Button_rtp = gs:create(button, Win,    [{x,130},{y,405},{width,050},{height,25},{label, {text, "RTP"}}]),

%    state=gs:create(label, Win,          [{x,200},{y,405},{width,100},{height,25},{label,{text, "idle"}}]),

%    gs:config(Win, {map, true}), 
%   {S, #gs_ids{win = Win,
%		display1 = Display1,
%		display2 = Display2,
%		display3 = Display3,
%		display4 = Display4,
%		display5 = Display5,
%		hook = Button_hook,
%		quit = Button_quit,
%		ring_type = RingType,
%		state=State,
%		options = Button_option,
%		rtpinfo = Button_rtp,
%		register = Button_reg,
%		auto_answer = AutoAnswer,
%		button_speaker = Button_speaker,
%		button_clear = Button_clear,
%		button_f1 = Button_F1,
%		button_f2 = Button_F2,
%		button_f3 = Button_F3,
%		button_f4 = Button_F4,
%		button_1 = Button_1,
%		button_2 = Button_2,
%		button_3 = Button_3,
%		button_4 = Button_4,
%		button_5 = Button_5,
%		button_6 = Button_6,
%		button_7 = Button_7,
%		button_8 = Button_8,
%		button_9 = Button_9,
%		button_star  = Button_star,
%		button_0  = Button_0,
%		button_hash  = Button_hash}}.
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%%
%% -----------------------------------------------------------------------------

check_waptone_ip(Tone, #phone_state{dts_status = Dts_status} = _State, From) ->
    case h323_phone:check_tone(list_to_atom(Tone), Dts_status) of
	ok ->
	    eaa_common:instruction_res(From,check_waptone_ip,ok);
	{not_ready, Reason} ->
	    eaa_common:instruction_res(From,check_waptone_ip,{not_ready, Reason});
	{reply, Reason} ->
	    eaa_common:instruction_res(From,check_waptone_ip,{reply, Reason});
	{error, Reason} ->
	    eaa_common:instruction_res(From,check_waptone_ip, {error,lf:sformat("Faulty check_waptone_ip:~s",[Reason])})
    end.    

check_ring_ip(Ring, #phone_state{dts_status = Dts_status} = State, From) ->
    case check_ring(list_to_atom(Ring), Dts_status) of
	ok ->
	    eaa_common:instruction_res(From,check_ring_ip,ok);	
	{not_ready, Reason} ->
	    eaa_common:instruction_res(From,check_ring_ip,{not_ready, Reason});
	{reply, Reason} ->
	    eaa_common:instruction_res(From,check_ring_ip, {error,lf:sformat("Faulty check_ring_ip:~s",[Reason])}),
	    State;
	{error, Reason} ->
	    eaa_common:instruction_res(From,check_ring_ip, {error,lf:sformat("Faulty check_ring_ip, Reason:~s",[Reason])})
    end.
check_ring(ExpRing,Dts_status)->
    {_H, Ring, _L, _D, {_T,_La,_M}} = Dts_status,
    case check_ringstate(ExpRing,Ring) of
        ok -> ok;
	{not_ready,Reason} -> {not_ready,Reason};
	{error,Reason} -> {error,Reason}
    end.
check_ringstate(Ring,Ring) -> ok;
check_ringstate(any_ring,no_ring) -> {error,"no ring found"};
check_ringstate(any_ring,_S) -> ok;
check_ringstate(external_ring,internal_ring) -> {error,"got internal_ring"};
check_ringstate(internal_ring,external_ring) -> {error,"got external_ring"};
check_ringstate(no_ring,Ring) -> {error,"no ring expected got " ++ atom_to_list(Ring)};
check_ringstate(_Expected,Actual) -> {not_ready,"Ring tones differ got " ++ atom_to_list(Actual)}.


check_lang_ip(Lang, #phone_state{dts_status = Dts_status} = _State, From) ->
    case h323_phone:check_lang(list_to_atom(Lang), Dts_status) of
	ok ->
	    eaa_common:instruction_res(From,check_lang_ip,ok);
	{reply, Reason} ->
	    eaa_common:instruction_res(From,check_lang_ip,{not_ready, Reason});
	{error, Reason} ->
	    eaa_common:instruction_res(From,check_lang_ip, {error,lf:sformat("Faulty check_lang_ip:~s",[Reason])})
    end.    

recv_rtp(#phone_state{gdsx_info = GDSX_info} = _IP_State,From) ->
    case GDSX_info#gdsx_info.payload_type of
	{PayloadType,_Timeslot} ->
	    RTPdata = case GDSX_info#gdsx_info.data of
			  undefined->[];
			  D->D
		      end,
		      
	    case GDSX_info#gdsx_info.packet_type of
		undefined ->
		    eaa_common:instruction_res(From,recv_rtp,{ok,
				     lf:sformat("    RTP packet with pt:~w ~s ~n~w",
						[PayloadType,
						 string:strip(rtp:decode_pt(
								PayloadType),right,16#0A),
						 RTPdata])}); % ASCII \n
		{PacketType} ->
		    eaa_common:instruction_res(From,recv_rtp,{ok,
				     lf:sformat("    RTP packet with pt:~w ~s ~n~w ~n" ++ 
						"    RCTP packet with pt:~w ~s",
					 	[PayloadType, string:strip(
								rtp:decode_pt(PayloadType),right,16#0A),
						 RTPdata,
						 PacketType, string:strip(
							       rtcp:decode_pt(PacketType),right,16#0A)])})
	    end;
	_ ->
	    eaa_common:instruction_res(From,recv_rtp,{error,"No RTP packet received"})
    end.

recv_rtp(Exp_PT, #phone_state{gdsx_info = GDSX_info} = _IP_State,From) ->
  
    {Recv_PT,Timeslot} =
	case GDSX_info#gdsx_info.payload_type of
	    undefined -> {undefined,undefined};
	    {R,T} -> {R,T};
	    _ -> {undefined,undefined}
	end,

    case {Recv_PT,Timeslot} of
	{undefined,undefined} ->
	    eaa_common:instruction_res(From,recv_rtp,{error,"No RTP packet received"});
	{Exp_PT,udpport} when Exp_PT =<127 ->
	    eaa_common:instruction_res(From,recv_rtp,{ok, lf:sformat("    RTP packet with pt:~w ~s~n    on udpport",
					    [Exp_PT, decode_pt(Exp_PT)])});
	{Exp_PT,TS} when Exp_PT =<127 ->
	    eaa_common:instruction_res(From,recv_rtp,{ok, lf:sformat("    RTP packet with pt:~w ~s~n    on timeslot ~w",
					    [Exp_PT, decode_pt(Exp_PT), TS])});
	Other ->
	    eaa_common:instruction_res(From,recv_rtp,{error,lf:sformat("Expecting pt:~w (~s), got pt:~w (~s)",
					      [Exp_PT,decode_pt(Exp_PT),Other,decode_pt(Other)])})
    end.

not_recv_rtp(#phone_state{gdsx_info = GDSX_info} = _IP_State,From) ->
    case GDSX_info#gdsx_info.payload_type of
	{PayloadType,_Timeslot} ->
	    eaa_common:instruction_res(From,not_recv_rtp,{error,
			     lf:sformat("    RTP packet with pt:~w ~s",
					[PayloadType,
					 string:strip(rtp:decode_pt(
							PayloadType),right,16#0A)])}); % ASCII \n
	_ ->
	    eaa_common:instruction_res(From,not_recv_rtp,{ok,"No RTP packet received"})
    end.

not_recv_rtp(Exp_PT, #phone_state{gdsx_info = GDSX_info} = _IP_State,From) ->
  
    {Recv_PT,_Timeslot} = case GDSX_info#gdsx_info.payload_type of
			     undefined->{undefined,undefined};
			     {R,T}->{R,T};
			     _->{undefined,undefined}
			 end,
    case Recv_PT of
	undefined ->
	    eaa_common:instruction_res(From,not_recv_rtp,{ok,"No RTP packet received"});
	Exp_PT when Exp_PT =<127 ->
	    eaa_common:instruction_res(From,not_recv_rtp,{error, lf:sformat("    RTP packet with pt:~w ~s",
					    [Exp_PT, decode_pt(Exp_PT)])});
	Other ->
	    eaa_common:instruction_res(From,not_recv_rtp,{ok,lf:sformat("Expecting pt:~w (~s), got pt:~w (~s)",
					      [Exp_PT,decode_pt(Exp_PT),Other,decode_pt(Other)])})
    end.

decode_pt(PayloadType) ->
    string:strip(rtp:decode_pt(PayloadType),right,16#0A).    % ASCII value \n


%quick() ->
%    l(),
%    sip_port =
%	case init:get_argument(sip_port) of
%	    {ok, [[_sip_port]]} ->
%		_sip_port_string = integer_to_list(_sip_port);
%		?DEBUG_PRINT("Found argument sip_port:~s~n",[_sip_port_string]),
%	    _ ->
%		?DEFAULT_sip_port
%	end,
%    Filename =
%	case init:get_argument(setup) of
%	    {ok, [[_Filename]]} ->
%		?DEBUG_PRINT("Found argument setup:~s~n",[_Filename]),
%		_Filename;
%	    _ ->
%		"h323_phone_options.ini"
%	end,
%    h323_phone_gdr:start_link([{sip_port,sip_port},{filename,Filename}]).

l() ->
%    {ok, PhoneGdr1} = sip_phone:start_link([{sw_version,"HWver-R2B ApplicRev-R4C BootRev-R3C"},
%					    {client_ipaddress,"130.100.182.126"},
%					    {sip_port, 5060},
%					    {user_e164,"41932"},
%					    {wsp_port,2948},
%					    {gatekeeper_host,"130.100.10.72"}
%					   ]),
    {ok, PhoneGdr1} = sip_phone:start_link([{sw_version,"HWver-R2B ApplicRev-R4C BootRev-R3C"},
					    {client_ipaddress,"130.100.182.126"},
					    {sip_port, 5060},
					    {own_phoneno,"41931"},
					    {wsp_port,2949},
					    {gatekeeper_host,"130.100.10.72"}
					   ]),
    Phone1 = gen_server:call(PhoneGdr1, start_gs, ?GENSERVER_TIMER),
    timer:sleep(infinity),
    {PhoneGdr1, Phone1}.

string_to_tuple(ID) when list(ID) ->
    IPdec = larit:hexList2decList(ID),
    IP_List = larit:numList2intList(IPdec),
    list_to_tuple(IP_List).

date(now)->
    Date = tuple_to_list(date()),
    [Year,Month,Day] = Date,
    Month_string=month(Month),
    Date_string=lf:sformat("~2w",[Day]) ++ " " ++ Month_string ++ " " ++ lf:sformat("~4w",[Year]),
    Time = tuple_to_list(time()),
    [H,M,_S] = Time,
    Timestring = case lf:sformat("~2w:~2w",[H,M]) of
		     [$ ,H2,$:,$ ,M2]->[$0,H2,$:,$0,M2];
		     [H1,H2,$:,$ ,M2]->[H1,H2,$:,$0,M2];
		     [$ ,H2,$:,M1,M2]->[$0,H2,$:,M1,M2];
		     [H1,H2,$:,M1,M2]->[H1,H2,$:,M1,M2]
		 end,
    Timestring ++ "  " ++ Date_string.

month(1)->
    "Jan";
month(2)->
    "Feb";
month(3)->
    "Mar";
month(4)->
    "Apr";
month(5)->
    "May";
month(6)->
    "Jun";
month(7)->
    "Jul";
month(8)->
    "Aug";
month(9)->
    "Sep";
month(10)->
    "Oct";
month(11)->
    "Nov";
month(12)->
    "Dec".

close_RTP_port(RTP_Pid,Socket) when port(Socket) ->
    RTP_Pid ! close,
    gen_udp:close(Socket);
close_RTP_port(_,_) ->
    true.

list_to_string(IPAddress) ->
    lf:sformat("~w.~w.~w.~w",IPAddress).

format_string(Direction,Data)->
    StringStrip=string:strip(Data,both,$\n),
    Strings = string:tokens(StringStrip,"\r"),
    format_string(Direction,Data,Strings,[]).
format_string(Direction,Data,[String|Rest],Result)->
    NewRes=eaa_common:wrap_line(Direction,String),
    format_string(Direction,Data,Rest,Result++NewRes);
format_string(_Direction,_Data,[],Result)->
    Result.

date_loop(Parent)->
    gen_server:cast(Parent, date_loop),
    timer:sleep(60000),%1min
    ?MODULE:date_loop(Parent).

register_loop(#phone_state{gdr_state=GdrState,
			   socket=Socket,
			   options = Options} = State) ->
    timer:sleep(600000),%10min
    {Message,Options1} = sip_stack:sip_message(register,Options),
    send_host(Socket,Options1,[Message],GdrState),
    ?MODULE:register_loop(State).

get_button_string(Button)->
    Button_string = 
    	case Button of
	    kacc1->?Button_kacc1;
	    kacc2->?Button_kacc2;
	    kinq->?Button_kinq;
	    ktrans->?Button_ktrans;
	    kspeak->?Button_speaker;
	    f1->?Button_F1;
	    f2->?Button_F2;
	    f3->?Button_F3;
	    f4->?Button_F4;
	    35->"simulateKeyPush #";
	    42->"simulateKeyPush *";
	    48->?Button_Digit0;
	    49->?Button_Digit1;
	    50->?Button_Digit2;
	    51->?Button_Digit3;
	    52->?Button_Digit4;
	    53->?Button_Digit5;
	    54->?Button_Digit6;
	    55->?Button_Digit7;
	    56->?Button_Digit8;
	    57->?Button_Digit9;
	    menu->?Button_menu;
	    kprog->?Button_kprog;
	    k1->?Button_k1;
	    k2->?Button_k2;
	    k3->?Button_k3;
	    k13->?Button_k13;
	    k14->?Button_k14;
	    k15->?Button_k15;
	    k16->?Button_k16;
	    k17->?Button_k17;
	    k18->?Button_k18;
	    k19->?Button_k19;
    	    k20->?Button_k20;
	    k21->?Button_k21;
	    k22->?Button_k22;
	    k23->?Button_k23;
	    _But->undefined
        end,
    Button_string2 = string:substr(Button_string,17,1).

%%----------------------------E-N-D---O-F---F-I-L-E---------------------------%%

