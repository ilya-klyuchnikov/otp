%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2005-2025. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%%
%%----------------------------------------------------------------------
%% Purpose: Encode V3 Megaco/H.248 text messages from internal form
%%----------------------------------------------------------------------

%% -define(d(F,A), io:format("~w:" ++ F ++ "~n", [?MODULE|A])).
	       
-define(META_ENC(Type, Item), Item) .
%% -define(META_ENC(Type, Item), megaco_meta_package:encode(text, Type, Item)).
%% -define(META_DEC(Type, Item), megaco_meta_package:decode(text, Type, Item)).

enc_MegacoMessage(Val) ->
    State = ?INIT_INDENT,
    enc_MegacoMessage(Val, State).

enc_MegacoMessage(#'MegacoMessage'{authHeader = asn1_NOVALUE, 
				   mess       = Mess}, State) ->
    [
     ?LWSP,
     enc_Message(Mess, State)
    ];
enc_MegacoMessage(#'MegacoMessage'{authHeader = Auth, 
				   mess       = Mess}, State) ->
    [
     ?LWSP,
     enc_AuthenticationHeader(Auth, State),
     enc_Message(Mess, State)
    ].

%% Note that encoding the transaction this way
%% make the message look a bit strange.
enc_Transaction(Val) ->
    State = ?INIT_INDENT,
    enc_Transaction(Val, State).

%% Note that encoding the action request's this way
%% make the message look a bit strange.
enc_ActionRequests(Val) ->
    State = ?INIT_INDENT,
    enc_TransactionRequest_actions(Val, State).

%% Note that encoding the action request this way
%% make the message look a bit strange.
enc_ActionRequest(Val) ->
    State = ?INIT_INDENT,
    enc_ActionRequest(Val, State).

enc_CommandRequest(Val) ->
    State = ?INIT_INDENT,
    enc_CommandRequest(Val, State).

enc_ActionReply(Val) ->
    State = ?INIT_INDENT,
    enc_ActionReply(Val, State).

enc_AuthenticationHeader(asn1_NOVALUE, _State) ->
    [];
enc_AuthenticationHeader(Val, State)
  when is_record(Val, 'AuthenticationHeader') ->
    [
     ?AuthToken,
     ?EQUAL,
     enc_SecurityParmIndex(Val#'AuthenticationHeader'.secParmIndex, State),
     ?COLON,
     enc_SequenceNum(Val#'AuthenticationHeader'.seqNum, State),
     ?COLON,
     enc_AuthData(Val#'AuthenticationHeader'.ad, State),
     ?SEP_INDENT(State)
    ].

enc_SecurityParmIndex({'SecurityParmIndex',Val}, State) ->
    enc_SecurityParmIndex(Val, State);
enc_SecurityParmIndex(Val, State) ->
    [
     "0x",
     enc_HEXDIG(Val, State, 8, 8)
    ].

enc_SequenceNum({'SequenceNum',Val}, State) ->
    enc_SequenceNum(Val, State);
enc_SequenceNum(Val, State) ->
    [
     "0x",
     enc_HEXDIG(Val, State, 8, 8)
    ].

enc_AuthData({'AuthData',Val}, State) ->
    enc_AuthData(Val, State);
enc_AuthData(Val, State) ->
    [
     "0x",
     enc_HEXDIG(Val, State, 24, 64)  %% OTP-4710
    ].

enc_Message(Val, State)
  when is_record(Val, 'Message') ->
    [
     ?MegacopToken,
     ?SLASH,
     enc_version(Val#'Message'.version, State),
     ?SEP,
     enc_MId(Val#'Message'.mId, State),
     ?SEP_INDENT(State),
     enc_Message_messageBody(Val#'Message'.messageBody, State)
    ].

enc_version(Val, State) when is_integer(Val) and (Val >= 0) ->
    enc_DIGIT(Val, State, 0, 99).

enc_Message_messageBody({'Message_messageBody',Val}, State) ->
    enc_Message_messageBody(Val, State);
enc_Message_messageBody({Tag, Val}, State) ->
    case Tag of
	messageError ->
	    enc_ErrorDescriptor(Val, State);
	transactions ->
	    enc_Message_messageBody_transactions(Val, State);
	_ ->
	    error({invalid_messageBody_tag, Tag})
    end.

enc_Message_messageBody_transactions({'Message_messageBody_transactions',Val}, 
				     State) ->
    enc_Message_messageBody_transactions(Val, State);
enc_Message_messageBody_transactions(Val, State)
  when is_list(Val) and (Val /= []) ->
    [enc_Transaction(T, State) || T <- Val].

enc_MId({'MId',Val}, State) ->
    enc_MId(Val, State);
enc_MId({Tag, Val}, State) ->
    case Tag of
	ip4Address ->
	    enc_IP4Address(Val, State);
	ip6Address ->
	    enc_IP6Address(Val, State);
	domainName ->
	    enc_DomainName(Val, State);
	deviceName ->
	    enc_PathName(Val, State);
	mtpAddress ->
	    enc_mtpAddress(Val, State);
	_ ->
	    error({invalid_MId_tag, Tag})
    end.

enc_mtpAddress(Val, State) ->
    [
     ?MtpToken,
     ?LBRKT,
     enc_OCTET_STRING(Val, State, 2, 4),
     ?RBRKT
    ].

enc_DomainName(#'DomainName'{portNumber = asn1_NOVALUE,
			     name       = Name}, State) ->
    [
     $<,
     %% BUGBUG: (ALPHA / DIGIT) *63(ALPHA / DIGIT / "-" / ".")
     enc_STRING(Name, State, 1, 64),
     $>
    ];
enc_DomainName(#'DomainName'{portNumber = PortNumber,
			     name       = Name}, State) ->
    [
     $<,
     %% BUGBUG: (ALPHA / DIGIT) *63(ALPHA / DIGIT / "-" / ".")
     enc_STRING(Name, State, 1, 64),
     $>,
     $:,
     enc_portNumber(PortNumber, State)
    ].

enc_IP4Address(#'IP4Address'{portNumber = asn1_NOVALUE,
			     address    = [A1, A2, A3, A4]}, State) ->
    [
     $[,
     enc_V4hex(A1, State),
     ?DOT,
     enc_V4hex(A2, State),
     ?DOT,
     enc_V4hex(A3, State),
     ?DOT,
     enc_V4hex(A4, State),    
     $]
    ];
enc_IP4Address(#'IP4Address'{portNumber = PortNumber,
			     address    = [A1, A2, A3, A4]}, State) ->
    [
     $[,
     enc_V4hex(A1, State),
     ?DOT,
     enc_V4hex(A2, State),
     ?DOT,
     enc_V4hex(A3, State),
     ?DOT,
     enc_V4hex(A4, State),    
     $],
     $:,
     enc_portNumber(PortNumber, State)
    ].    

enc_V4hex(Val, State) ->
    enc_DIGIT(Val, State, 0, 255).

enc_IP6Address(#'IP6Address'{portNumber = asn1_NOVALUE,
			     address    = Addr}, State) 
  when is_list(Addr) andalso (length(Addr) == 16) ->
    [
     $[,
     enc_IP6Address_address(Addr, State),
     $]
    ];
enc_IP6Address(#'IP6Address'{portNumber = PortNumber,
			     address    = Addr}, State) 
  when is_list(Addr) andalso (length(Addr) == 16) ->
    [
     $[,
     enc_IP6Address_address(Addr, State),
     $],
     $:,
     enc_portNumber(PortNumber, State)
    ].

enc_IP6Address_address([0, 0|Addr], State) ->
    enc_IP6Address_address2(Addr, 1, false, true, State);
enc_IP6Address_address(Addr, State) ->
    enc_IP6Address_address2(Addr, 0, false, false, State).

enc_IP6Address_address2([0,0], 0, _Padding, _First, _State) ->
    [$0];
enc_IP6Address_address2([0,0], PadN, false, true, _State) when PadN > 0 ->
    [$:, $:]; % Padding from the beginning (all zero's)
enc_IP6Address_address2([0,0], PadN, false, false, _State) when PadN > 0 ->
    [$:]; % Padding in the middle or end
enc_IP6Address_address2([0,0], _, true, _First, _State) ->
    [$0];
enc_IP6Address_address2([N1,N2], 0, _Padding, _First, State) ->
    [enc_hex4([N1, N2], State)];
enc_IP6Address_address2([N1,N2], 1, _Padding, _First, State) ->
    [$0, $:, enc_hex4([N1, N2], State)];
enc_IP6Address_address2([N1,N2], PadN, false, true, State) when PadN > 1 ->
    [$:, $:, enc_hex4([N1, N2], State)];
enc_IP6Address_address2([N1,N2], PadN, false, false, State) when PadN > 1 ->
    [$:, enc_hex4([N1, N2], State)];
enc_IP6Address_address2([N1,N2], _PadN, true, _First, State) ->
    [enc_hex4([N1, N2], State)];
enc_IP6Address_address2([0, 0|Ns], PadN, false, First, State) ->
    enc_IP6Address_address2(Ns, PadN+1, false, First, State);
enc_IP6Address_address2([0, 0|Ns], _PadN, true, _First, State) ->
    [
     $0,
     $:,
     enc_IP6Address_address2(Ns, 0, true, false, State)
    ];
enc_IP6Address_address2([N1, N2|Ns], 0, Padded, _First, State) ->
    [
     enc_hex4([N1, N2], State),
     $:,
     enc_IP6Address_address2(Ns, 0, Padded, false, State)
    ];
enc_IP6Address_address2([N1, N2|Ns], 1, Padded, _First, State) ->
    [
     $0,
     $:,
     enc_hex4([N1, N2], State),
     $:,
     enc_IP6Address_address2(Ns, 0, Padded, false, State)
    ];
enc_IP6Address_address2([N1, N2|Ns], PadN, false, true, State) when PadN > 1 ->
    %% Padding from the beginning
    [
     $:,
     $:,
     enc_hex4([N1, N2], State),
     $:,
     enc_IP6Address_address2(Ns, 0, true, false, State)
    ];
enc_IP6Address_address2([N1, N2|Ns], PadN, false, false, State) 
  when PadN > 1 ->
    [
     $:,  %% The other ':' has already added
     enc_hex4([N1, N2], State),
     $:,
     enc_IP6Address_address2(Ns, 0, true, false, State)
    ];
enc_IP6Address_address2([N1, N2|Ns], _PadN, true, _First, State) ->
    [
     enc_hex4([N1, N2], State),
     $:,
     enc_IP6Address_address2(Ns, 0, true, false, State)
    ].


enc_hex4([0,0], _State) ->
    $0;
enc_hex4([0,N], _State) ->
    hex(N);
enc_hex4([N1, N2], _State) when N2 =< 15 ->
    [hex(N1), $0, hex(N2)];
enc_hex4([N1, N2], _State) ->
    [hex(N1), hex(N2)].

enc_PathName({'PathName',Val}, State) ->
    enc_PathName(Val, State);
enc_PathName(Val, State) ->
    %% BUGBUG: ["*"] NAME *("/" / "*"/ ALPHA / DIGIT /"_" / "$" ) 
    %% BUGBUG: ["@" pathDomainName ]
    enc_STRING(Val, State, 1, 64).

enc_Transaction(Bin, _State) when is_binary(Bin) ->
    [Bin]; %% Already encoded...
enc_Transaction({'Transaction',Val}, State) ->
    enc_Transaction(Val, State);
enc_Transaction({Tag, Val}, State) ->
    case Tag of
	transactionRequest ->
	    enc_TransactionRequest(Val, State);
	transactionPending ->
	    enc_TransactionPending(Val, State);
	transactionReply ->
	    enc_TransactionReply(Val, State);
	transactionResponseAck ->
	    enc_TransactionResponseAck(Val, State);
 	segmentReply ->
 	    enc_SegmentReply(Val, State);
	_ ->
	    error({invalid_Transaction_tag, Tag})
    end.

enc_TransactionResponseAck([Mand], State) ->
    [
     ?ResponseAckToken,
     ?LBRKT_INDENT(State),
     [enc_TransactionAck(Mand, State)],
     ?RBRKT_INDENT(State)
    ];
enc_TransactionResponseAck([Mand | Opt], State) ->
    [
     ?ResponseAckToken,
     ?LBRKT_INDENT(State),
     [enc_TransactionAck(Mand, State) |
      [[?COMMA_INDENT(State), enc_TransactionAck(Val, State)] || Val <- Opt]],
     ?RBRKT_INDENT(State)
    ].
    
enc_TransactionAck(Val, State)
  when is_record(Val, 'TransactionAck') ->
    [
     enc_TransactionId(Val#'TransactionAck'.firstAck, ?INC_INDENT(State)),
     case Val#'TransactionAck'.lastAck of
	 asn1_NOVALUE ->
	     [];
	 LastAck ->
	     ["-",enc_TransactionId(LastAck, State)]
     end
    ].

enc_TransactionId({'TransactionId',Val}, State) ->
    enc_TransactionId(Val, State);
enc_TransactionId(Val, State) ->
    enc_UINT32(Val, State).

enc_TransactionRequest(#'TransactionRequest'{transactionId = Tid,
					     actions       = Acts}, State) ->
    [
     ?TransToken,
     ?EQUAL,
     enc_TransactionId(Tid, State),
     ?LBRKT_INDENT(State),
     enc_TransactionRequest_actions(Acts, ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ];
enc_TransactionRequest(Bin, _State) when is_binary(Bin) ->
    [Bin].

enc_TransactionRequest_actions(Bin, _State) when is_binary(Bin) ->
    [Bin]; %% Already encoded...
enc_TransactionRequest_actions({'TransactionRequest_actions',Val}, State) ->
    enc_TransactionRequest_actions(Val, State);
enc_TransactionRequest_actions([Mand], State) ->
    [enc_ActionRequest(Mand, State)];
enc_TransactionRequest_actions([Mand | Opt], State) ->
    [enc_ActionRequest(Mand, State) |
     [[?COMMA_INDENT(State), enc_ActionRequest(Val, State)] || Val <- Opt]].

enc_TransactionPending(#'TransactionPending'{transactionId = Tid}, State) ->
    [?PendingToken,
     ?EQUAL,
     enc_TransactionId(Tid, State),
     ?LBRKT_INDENT(State),
     ?RBRKT_INDENT(State)
    ];
enc_TransactionPending(Bin, _State) when is_binary(Bin) ->
    [Bin].

enc_TransactionReply(#'TransactionReply'{transactionId        = Tid,
					 immAckRequired       = asn1_NOVALUE,
					 transactionResult    = Res, 
 					 segmentNumber        = asn1_NOVALUE,
 					 segmentationComplete = asn1_NOVALUE
					}, 
		     State) ->
    [
     ?ReplyToken,
     ?EQUAL,
     enc_TransactionId(Tid, State),
     ?LBRKT_INDENT(State),
     enc_TransactionReply_transactionResult(Res, ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ];
enc_TransactionReply(#'TransactionReply'{transactionId        = Tid,
					 immAckRequired       = Req,
					 transactionResult    = Res, 
 					 segmentNumber        = asn1_NOVALUE,
 					 segmentationComplete = asn1_NOVALUE
					}, 
		     State) ->
    [
     ?ReplyToken,
     ?EQUAL,
     enc_TransactionId(Tid, State),
     ?LBRKT_INDENT(State),
     enc_immAckRequired(Req, State),
     enc_TransactionReply_transactionResult(Res, ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ];
enc_TransactionReply(#'TransactionReply'{transactionId        = Tid,
					 immAckRequired       = Req,
					 transactionResult    = Res, 
 					 segmentNumber        = SegNo,
 					 segmentationComplete = asn1_NOVALUE
					}, 
		     State) ->
    [
     ?ReplyToken,
     ?EQUAL,
     enc_TransactionId(Tid, State),
     ?SLASH,
     enc_SegmentNumber(SegNo, State),
     ?LBRKT_INDENT(State),
     enc_immAckRequired(Req, State),
     enc_TransactionReply_transactionResult(Res, ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ];
enc_TransactionReply(#'TransactionReply'{transactionId        = Tid,
					 immAckRequired       = Req,
					 transactionResult    = Res, 
 					 segmentNumber        = SegNo,
 					 segmentationComplete = 'NULL'
					}, 
		     State) ->
    [
     ?ReplyToken,
     ?EQUAL,
     enc_TransactionId(Tid, State),
     ?SLASH,
     enc_SegmentNumber(SegNo, State),
     ?SLASH,
     ?SegmentationCompleteToken,
     ?LBRKT_INDENT(State),
     enc_immAckRequired(Req, State),
     enc_TransactionReply_transactionResult(Res, ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ];
enc_TransactionReply(Bin, _State) when is_binary(Bin) ->
    [Bin].

enc_SegmentReply({'SegmentReply', SR}, State) ->
    enc_SegmentReply(SR, State);
enc_SegmentReply(#'SegmentReply'{transactionId        = TID,
				 segmentNumber        = SegNo,
				 segmentationComplete = asn1_NOVALUE
				}, State) ->  
    [
     ?MessageSegmentToken,
     ?EQUAL,
     enc_TransactionId(TID, State),
     ?SLASH,
     enc_SegmentNumber(SegNo, State)
    ];
enc_SegmentReply(#'SegmentReply'{transactionId        = TID,
				 segmentNumber        = SegNo,
				 segmentationComplete = 'NULL'
				}, State) ->  
    [
     ?MessageSegmentToken,
     ?EQUAL,
     enc_TransactionId(TID, State),
     ?SLASH,
     enc_SegmentNumber(SegNo, State),
     ?SLASH,
     ?SegmentationCompleteToken
    ];
enc_SegmentReply(Bin, _State) when is_binary(Bin) ->
    [Bin].

enc_SegmentNumber({'SegmentNumber', SN}, State) ->
    enc_SegmentNumber(SN, State);
enc_SegmentNumber(SN, State) ->
    enc_UINT16(SN, State).
    
enc_immAckRequired(Val, _State) ->
    case Val of
	asn1_NOVALUE -> 
	    [];
	'NULL'       -> 
	    [?ImmAckRequiredToken, ?COMMA_INDENT(?INC_INDENT(_State))]
    end.

enc_TransactionReply_transactionResult({'TransactionReply_transactionResult',
					Val}, State) ->
    enc_TransactionReply_transactionResult(Val, State);
enc_TransactionReply_transactionResult({Tag, Val}, State) ->
    case Tag of
	transactionError ->
	    enc_ErrorDescriptor(Val, State);
	actionReplies ->
	    enc_TransactionReply_transactionResult_actionReplies(Val, State);
	_ ->
	    error({invalid_TransactionReply_transactionResult_tag, Tag})
     end.

enc_TransactionReply_transactionResult_actionReplies({'TransactionReply_transactionResult_actionReplies',Val}, State) ->
    enc_TransactionReply_transactionResult_actionReplies(Val, State);
enc_TransactionReply_transactionResult_actionReplies([Mand], State) ->
    [enc_ActionReply(Mand, State)];
enc_TransactionReply_transactionResult_actionReplies([Mand | Opt], State) ->
    [enc_ActionReply(Mand, State),
     [[?COMMA_INDENT(State), enc_ActionReply(Val, State)] || Val <- Opt]].

enc_ErrorDescriptor(#'ErrorDescriptor'{errorText = asn1_NOVALUE,
				       errorCode = Code}, State) ->
    [
     ?ErrorToken,
     ?EQUAL,
     enc_ErrorCode(Code, State),
     ?LBRKT,
     ?RBRKT
    ];
enc_ErrorDescriptor(#'ErrorDescriptor'{errorText = Text,
				       errorCode = Code}, State) ->
    [
     ?ErrorToken,
     ?EQUAL,
     enc_ErrorCode(Code, State),
     ?LBRKT,
     enc_ErrorText(Text, State),
     ?RBRKT
    ].

enc_ErrorCode({'ErrorCode',Val}, State)->
    enc_ErrorCode(Val, State);
enc_ErrorCode(Val, State) ->
    enc_DIGIT(Val, State, 0, 999).

enc_ErrorText({'ErrorText',Val}, State) ->
    enc_ErrorText(Val, State);
enc_ErrorText(Val, State)  ->
    enc_QUOTED_STRING(Val, State).

enc_ContextID({'ContextID',Val}, State) ->
    enc_ContextID(Val, State);
enc_ContextID(Val, State) ->
    case Val of
	?megaco_all_context_id    -> $*;
	?megaco_null_context_id   -> $-;
	?megaco_choose_context_id -> $$;
	Int when is_integer(Int) -> enc_UINT32(Int, State)
    end.

enc_ActionRequest(Bin, _State) when is_binary(Bin) ->
    [Bin]; %% Already encoded...
enc_ActionRequest(#'ActionRequest'{contextId           = CID,
				   contextRequest      = asn1_NOVALUE,
				   contextAttrAuditReq = asn1_NOVALUE,
				   commandRequests     = CmdReqs}, State) ->
    [
     ?CtxToken,
     ?EQUAL,
     enc_ContextID(CID, State),
     ?LBRKT_INDENT(State),
     enc_list([{CmdReqs, fun enc_CommandRequest/2}],
	      ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ];
enc_ActionRequest(#'ActionRequest'{contextId           = CID,
				   contextRequest      = CtxReq,
				   contextAttrAuditReq = asn1_NOVALUE,
				   commandRequests     = CmdReqs}, State) ->
    [
     ?CtxToken,
     ?EQUAL,
     enc_ContextID(CID, State),
     ?LBRKT_INDENT(State),
     enc_list([{[CtxReq], fun enc_ContextRequest/2},
	       {CmdReqs,  fun enc_CommandRequest/2}],
	      ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ];
enc_ActionRequest(#'ActionRequest'{contextId           = CID,
				   contextRequest      = CtxReq,
				   contextAttrAuditReq = CtxAAR,
				   commandRequests     = CmdReqs}, State) ->
    [
     ?CtxToken,
     ?EQUAL,
     enc_ContextID(CID, State),
     ?LBRKT_INDENT(State),
     enc_list([{[CtxReq],  fun enc_ContextRequest/2},
	       {[CtxAAR],  fun enc_ContextAttrAuditRequest/2},
	       {CmdReqs,   fun enc_CommandRequest/2}],
	      ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

enc_ActionReply(Bin, _State) when is_binary(Bin) ->
    [Bin]; % Already encoded...
%% OTP-5085
enc_ActionReply(#'ActionReply'{contextId       = Id,
			       errorDescriptor = asn1_NOVALUE,
			       contextReply    = asn1_NOVALUE,
			       commandReply    = []}, 
		State) ->
%%     d("enc_ActionReply -> entry with"
%%       "~n   Id: ~p", [Id]),
    [
     ?CtxToken,
     ?EQUAL,
     enc_ContextID(Id, State)
    ];
enc_ActionReply(#'ActionReply'{contextId       = Id,
			       errorDescriptor = ED,
			       contextReply    = CtxRep,
			       commandReply    = CmdRep}, 
		State) ->
%%     d("enc_ActionReply -> entry with"
%%       "~n   Id:     ~p"
%%       "~n   ED:     ~p"
%%       "~n   CtxRep: ~p"
%%       "~n   CmdRep: ~p", [Id, ED, CtxRep, CmdRep]),
    [
     ?CtxToken,
     ?EQUAL,
     enc_ContextID(Id, State),
     ?LBRKT_INDENT(State),
     do_enc_ActionReply(ED, CtxRep, CmdRep, State),
     ?RBRKT_INDENT(State)
    ].

do_enc_ActionReply(asn1_NOVALUE, CtxRep, [], State) 
  when CtxRep =/= asn1_NOVALUE ->
    [
     enc_ContextRequest(CtxRep, ?INC_INDENT(State))
    ];
do_enc_ActionReply(asn1_NOVALUE, CtxRep, CmdRep, State) 
  when (CtxRep =/= asn1_NOVALUE) and (CmdRep =/= []) ->
    [
     enc_ContextRequest(CtxRep, ?INC_INDENT(State)),
     ?COMMA_INDENT(?INC_INDENT(State)), 
     enc_list([{CmdRep, fun enc_CommandReply/2}],
	      ?INC_INDENT(State))
    ];
do_enc_ActionReply(asn1_NOVALUE, asn1_NOVALUE, CmdRep, State) 
  when CmdRep =/= [] ->
    [
     enc_list([{CmdRep, fun enc_CommandReply/2}],
	      ?INC_INDENT(State))
    ];
do_enc_ActionReply(ED, CtxRep, [], State) 
  when (ED =/= asn1_NOVALUE) and (CtxRep =/= asn1_NOVALUE) ->
    [
     enc_ContextRequest(CtxRep, ?INC_INDENT(State)),
     ?COMMA_INDENT(?INC_INDENT(State)), 
     enc_list([{[ED], fun enc_ErrorDescriptor/2}], % Indention cosmetics
 	      ?INC_INDENT(State))
    ];
do_enc_ActionReply(ED, asn1_NOVALUE, CmdRep, State) 
  when (ED =/= asn1_NOVALUE) and (CmdRep =/= []) ->
    [
     enc_list([{CmdRep, fun enc_CommandReply/2},
	       {[ED],   fun enc_ErrorDescriptor/2}], % Indention cosmetics
 	      ?INC_INDENT(State))
    ];
do_enc_ActionReply(ED, CtxRep, CmdRep, State) 
  when (ED     =/= asn1_NOVALUE) and
       (CtxRep =/= asn1_NOVALUE) and 
       (CmdRep =/= []) ->
    [
     enc_ContextRequest(CtxRep, ?INC_INDENT(State)),
     ?COMMA_INDENT(?INC_INDENT(State)), 
     enc_list([{CmdRep, fun enc_CommandReply/2},
	       {[ED],   fun enc_ErrorDescriptor/2}], % Indention cosmetics
 	      ?INC_INDENT(State))
    ];
do_enc_ActionReply(ED, asn1_NOVALUE, [], State) 
  when ED =/= asn1_NOVALUE ->
    [
     enc_ErrorDescriptor(ED, ?INC_INDENT(State))
    ].


enc_ContextRequest_priority(asn1_NOVALUE, _State) ->
    {[], dummy};
enc_ContextRequest_priority(Val, _State) ->
    {[Val], fun(X,S) -> [?PriorityToken,?EQUAL,enc_UINT16(X, S)] end}.

enc_ContextRequest_emergency(asn1_NOVALUE, _State) ->
    {[], dummy};
enc_ContextRequest_emergency(true, _State) ->
    {[?EmergencyToken], fun(Elem, _) -> Elem end};
enc_ContextRequest_emergency(false, _State) ->
    {[?EmergencyOffToken], fun(Elem, _) -> Elem end}.

enc_ContextRequest_topologyReq(asn1_NOVALUE, _State) ->
    {[], dummy};
enc_ContextRequest_topologyReq({'ContextRequest_topologyReq', 
				asn1_NOVALUE}, _State) ->
    {[], dummy};
enc_ContextRequest_topologyReq({'ContextRequest_topologyReq', 
				List}, _State) ->
    {List, fun enc_TopologyRequest/2};
enc_ContextRequest_topologyReq(List, _State) ->
    {[List], fun enc_TopologyRequest/2}.

enc_ContextRequest_iepscallind(asn1_NOVALUE, _State) ->
    {[], dummy};
enc_ContextRequest_iepscallind(Bool, _State) ->
    {[Bool], fun enc_iepsValue/2}.

enc_ContextRequest_contextProp(asn1_NOVALUE, _State) ->
    {[], dummy};
enc_ContextRequest_contextProp([], _State) ->
    {[], dummy};
enc_ContextRequest_contextProp(Props, _State) ->
    {[Props], fun(Elem, S) -> enc_contextAttrDescriptor(Elem, contextProps, S) end}.

enc_ContextRequest_contextList(asn1_NOVALUE, _State) ->
    {[], dummy};
enc_ContextRequest_contextList([], _State) ->
    {[], dummy};
enc_ContextRequest_contextList(Props, _State) ->
    {[Props], fun(Elem, S) -> 
		      enc_contextAttrDescriptor(Elem, contextList, S) 
	      end}.

enc_contextAttrDescriptor([Mand|Opt], contextProps, State) ->
    [
     ?ContextAttrToken,
     ?LBRKT_INDENT(State),
     [enc_PropertyParm(Mand, State) | 
      [[?COMMA_INDENT(State), enc_PropertyParm(Val, State)] || Val <- Opt]],
     ?RBRKT_INDENT(State)
    ];
enc_contextAttrDescriptor(CtxIdList, contextList, State) ->
    [
     ?ContextAttrToken,
     ?LBRKT_INDENT(State),
     enc_contextIdList(CtxIdList, ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

enc_contextIdList([Mand|Opt], State) ->
    State2 = ?INC_INDENT(State),
    [
     ?ContextListToken,
     ?EQUAL, 
     ?LBRKT_INDENT(State),
     [enc_ContextID(Mand, State2) |
      [[?COMMA_INDENT(State2), enc_ContextID(Val, State2)] || Val <- Opt]],
     ?RBRKT_INDENT(State)
    ].

enc_ContextRequest(asn1_NOVALUE, _State) ->
    [];
enc_ContextRequest(#'ContextRequest'{priority    = asn1_NOVALUE, 
				     emergency   = asn1_NOVALUE,
				     topologyReq = asn1_NOVALUE,
				     iepscallind = asn1_NOVALUE,
				     contextProp = asn1_NOVALUE,
				     contextList = asn1_NOVALUE}, _State) ->
    [];
enc_ContextRequest(#'ContextRequest'{priority    = asn1_NOVALUE, 
				     emergency   = asn1_NOVALUE,
				     topologyReq = [],
				     iepscallind = asn1_NOVALUE,
				     contextProp = [],
				     contextList = []}, _State) ->
    [];
enc_ContextRequest(#'ContextRequest'{priority    = Prio, 
				     emergency   = Em,
				     topologyReq = TR,
				     iepscallind = Ieps,
				     contextProp = CP,
				     contextList = CL}, State) ->
    [
     enc_list([enc_ContextRequest_priority(Prio, State), 
	       enc_ContextRequest_emergency(Em, State), 
	       enc_ContextRequest_topologyReq(TR, State), 
	       enc_ContextRequest_iepscallind(Ieps, State), 
	       enc_ContextRequest_contextProp(CP, State), 
	       enc_ContextRequest_contextList(CL, State)],
	      State)
    ].


%% -- contextAudit --
%% contextAudit = ContextAuditToken LBRKT 
%%                  (contextAuditProperties *(COMMA contextAuditProperties)) /
%%                  indAudcontextAttrDesscriptor 
%%                RBRKT
%% contextAuditProperties = 
%%                (TopologyToken / EmergencyToken / PriorityToken /
%%                 IEPSToken / pkgdName / contextAuditSelect)
%% contextAuditSelect = 
%%                priority / emergencyValue / iepsValue /
%%                contextAttrDescriptor / auditSelectLogic
%% indAudcontextAttrDesscriptor = 
%%                ContextAttrToken LBRKT 
%%                  (contextAuditProperties *(COMMA contextAuditProperties)) 
%%                RBRKT
%% 
%% This could actually either be 
%% a) a list of contextAuditProperties: 
%%    contextAuditProperties *(COMMA contextAuditProperties)
%% b) a indAudcontextAttrDescriptor
%% But since b) actually has the same content as a) with 
%% the extra ContextAttrToken, there does not seem to be any 
%% reason for using it.
enc_ContextAttrAuditRequest(asn1_NOVALUE, _State) ->
    [];
enc_ContextAttrAuditRequest(
  #'ContextAttrAuditRequest'{topology          = asn1_NOVALUE,
			     emergency         = asn1_NOVALUE,
			     priority          = asn1_NOVALUE,
			     iepscallind       = asn1_NOVALUE,
			     contextPropAud    = asn1_NOVALUE,
			     selectpriority    = asn1_NOVALUE,
			     selectemergency   = asn1_NOVALUE,
			     selectiepscallind = asn1_NOVALUE,
			     selectLogic       = asn1_NOVALUE}, _State) ->
    [];
enc_ContextAttrAuditRequest(
  #'ContextAttrAuditRequest'{topology          = asn1_NOVALUE,
			     emergency         = asn1_NOVALUE,
			     priority          = asn1_NOVALUE,
			     iepscallind       = asn1_NOVALUE,
			     contextPropAud    = [],
			     selectpriority    = asn1_NOVALUE,
			     selectemergency   = asn1_NOVALUE,
			     selectiepscallind = asn1_NOVALUE,
			     selectLogic       = asn1_NOVALUE}, _State) ->
    [];
enc_ContextAttrAuditRequest(
  #'ContextAttrAuditRequest'{topology          = Top,
			     emergency         = Em,
			     priority          = Prio,
			     iepscallind       = Ieps,
			     contextPropAud    = CPA,
			     selectpriority    = SPrio,
			     selectemergency   = SEm,
			     selectiepscallind = SIeps,
			     selectLogic       = SL}, State) ->
    [
     ?ContextAuditToken,
     ?LBRKT_INDENT(State),
     enc_list([{[Top],  fun('NULL', _) -> ?TopologyToken end},
	       {[Em],   fun('NULL', _) -> ?EmergencyToken end},
	       {[Prio], fun('NULL', _) -> ?PriorityToken end},
	       {[Ieps], fun('NULL', _) -> ?IEPSToken end},
	       {CPA,    fun enc_IndAudPropertyParm/2},
	       enc_ContextAttrAuditRequest_selectpriority(SPrio, State), 
	       enc_ContextAttrAuditRequest_selectemergency(SEm, State), 
	       enc_ContextAttrAuditRequest_selectiepscallind(SIeps, State), 
	       enc_ContextAttrAuditRequest_selectLogic(SL, State)],
	      ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

enc_ContextAttrAuditRequest_selectpriority(asn1_NOVALUE, _State) ->
    {[], dummy};
enc_ContextAttrAuditRequest_selectpriority(SPrio, _State) ->
    {[SPrio], fun(X,S) -> [?PriorityToken,?EQUAL,enc_UINT16(X, S)] end}.

enc_ContextAttrAuditRequest_selectemergency(asn1_NOVALUE, _State) ->  
    {[], dummy};    
enc_ContextAttrAuditRequest_selectemergency(SEm, _State) ->
    {[SEm], fun(X,S) -> enc_emergencyValue(X, S) end}.
    
enc_ContextAttrAuditRequest_selectiepscallind(asn1_NOVALUE, _State) ->  
    {[], dummy};    
enc_ContextAttrAuditRequest_selectiepscallind(SEm, _State) ->
    {[SEm], fun(X,S) -> enc_iepsValue(X, S) end}.
    
enc_ContextAttrAuditRequest_selectLogic(asn1_NOVALUE, _State) ->
    {[], dummy};
enc_ContextAttrAuditRequest_selectLogic({andAUDITSelect, 'NULL'}, _State) ->
    {[], dummy}; % This is default so, there is no reason to add it
enc_ContextAttrAuditRequest_selectLogic({orAUDITSelect, 'NULL'}, _State) ->
    {[?OrAUDITselectToken], fun(Elem, _) -> Elem end}.

enc_emergencyValue(true, _) ->
    [?EmergencyValueToken,?EQUAL,?EmergencyToken];
enc_emergencyValue(_, _) ->
    [?EmergencyValueToken,?EQUAL,?EmergencyOffToken].
    

%% enc_IndAudContextAttrDescriptor(
%%   #'ContextAttrAuditRequest'{topology          = Top,
%% 			     emergency         = Em,
%% 			     priority          = Prio,
%% 			     iepscallind       = Ieps,
%% 			     contextPropAud    = CPA,
%% 			     selectpriority    = SelPrio,
%% 			     selectemergency   = SelEm,
%% 			     selectiepscallind = SelIeps,
%% 			     selectLogic       = SelLog}, State) ->
%%     [
%%      ?ContextAttrToken,
%%      ?LBRKT_INDENT(State),
%%      enc_list([{[Top],  fun('NULL', _) -> ?TopologyToken end},
%% 	       {[Em],   fun('NULL', _) -> ?EmergencyToken end},
%% 	       {[Prio], fun('NULL', _) -> ?PriorityToken end},
%% 	       {[Ieps], fun('NULL', _) -> ?IEPSToken end},
%% 	       {CPA,    fun enc_IndAudPropertyParm/2}],
%% 	      ?INC_INDENT(State)),
%%      ?RBRKT_INDENT(State)
%%     ].

enc_CommandRequest(#'CommandRequest'{optional       = asn1_NOVALUE,
				     wildcardReturn = asn1_NOVALUE,
				     command        = Cmd}, State) ->
    [
     enc_Command(Cmd, State)
    ];
enc_CommandRequest(#'CommandRequest'{optional       = 'NULL',
				     wildcardReturn = asn1_NOVALUE,
				     command        = Cmd}, State) ->
    [
     "O-",
     enc_Command(Cmd, State)
    ]; 
enc_CommandRequest(#'CommandRequest'{optional       = asn1_NOVALUE,
				     wildcardReturn = 'NULL',
				     command        = Cmd}, State) ->
    [
     "W-",
     enc_Command(Cmd, State)
    ]; 
enc_CommandRequest(#'CommandRequest'{optional       = 'NULL',
				     wildcardReturn = 'NULL',
				     command        = Cmd}, State) ->
    [
     "O-",
     "W-",
     enc_Command(Cmd, State)
    ]. 

enc_Command({'Command',Val}, State) ->
    enc_Command(Val, State);
enc_Command({Tag, Val}, State) ->
%%     d("enc_Command -> entry with"
%%       "~n   Tag: ~p"
%%       "~n   Val: ~p", [Tag, Val]),
    case Tag of
	addReq ->
	    [?AddToken, enc_AmmRequest(Val, State)];
	moveReq ->
	    [?MoveToken, enc_AmmRequest(Val, State)];
	modReq ->
	    [?ModifyToken, enc_AmmRequest(Val, State)];
	subtractReq ->
	    [?SubtractToken, enc_SubtractRequest(Val, State)];
	auditCapRequest ->
	    [?AuditCapToken, enc_AuditRequest(Val, State)];
	auditValueRequest ->
	    [?AuditValueToken, enc_AuditRequest(Val, State)];
	notifyReq ->
	    [?NotifyToken, enc_NotifyRequest(Val, State)];
	serviceChangeReq ->
	    [?ServiceChangeToken, enc_ServiceChangeRequest(Val, State)];
	_ ->
	    error({invalid_Command_tag, Tag})
    end.

enc_CommandReply({'CommandReply',Val}, State) ->
    enc_CommandReply(Val, State);
enc_CommandReply({Tag, Val}, State) ->
%%     d("enc_CommandReply -> entry with"
%%       "~n   Tag: ~p"
%%       "~n   Val: ~p", [Tag, Val]),
    case Tag of
	addReply ->
	    [?AddToken, enc_AmmsReply(Val, State)];
	moveReply ->
	    [?MoveToken, enc_AmmsReply(Val, State)];
	modReply ->
	    [?ModifyToken, enc_AmmsReply(Val, State)];
	subtractReply ->
	    [?SubtractToken, enc_AmmsReply(Val, State)];
	auditCapReply ->
	    [?AuditCapToken, enc_AuditReply(Val, State)];
	auditValueReply ->
	    [?AuditValueToken, enc_AuditReply(Val, State)];
	notifyReply ->
	    [?NotifyToken, enc_NotifyReply(Val, State)];
	serviceChangeReply ->
	    [?ServiceChangeToken, enc_ServiceChangeReply(Val, State)];
	_ ->
	    error({invalid_CommandReply_tag, Tag})
     end.

enc_TopologyRequest(Val, State)
  when is_list(Val) ->
    [
     ?TopologyToken,
     ?LBRKT_INDENT(State),
     enc_list([{Val, fun enc_TopologyRequest1/2}], ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

enc_TopologyRequest1(
  #'TopologyRequest'{terminationFrom            = From,
		     terminationTo              = To,
		     topologyDirection          = TD,
 		     streamID                   = asn1_NOVALUE, % OPTIONAL
 		     topologyDirectionExtension = asn1_NOVALUE  % OPTIONAL
		    }, State) ->
    [
     enc_TerminationID(From, State),
     ?COMMA_INDENT(State),
     enc_TerminationID(To, State),
     ?COMMA_INDENT(State),
     enc_TopologyDirection(TD, State)
    ];
enc_TopologyRequest1(
  #'TopologyRequest'{terminationFrom            = From,
		     terminationTo              = To,
		     topologyDirection          = TD,
 		     streamID                   = SID,            % OPTIONAL
 		     topologyDirectionExtension = asn1_NOVALUE},  % OPTIONAL
  State) when (SID =/= asn1_NOVALUE) ->
    [
     enc_TerminationID(From, State),
     ?COMMA_INDENT(State),
     enc_TerminationID(To, State),
     ?COMMA_INDENT(State),
     enc_TopologyDirection(TD, State),
     ?COMMA_INDENT(State),
     enc_StreamID(SID, State)
    ];
enc_TopologyRequest1(
  #'TopologyRequest'{terminationFrom            = From,
		     terminationTo              = To,
		     topologyDirection          = TD,
 		     streamID                   = asn1_NOVALUE, % OPTIONAL
 		     topologyDirectionExtension = TDE},         % OPTIONAL
  State) when (TDE =/= asn1_NOVALUE) ->
    [
     enc_TerminationID(From, State),
     ?COMMA_INDENT(State),
     enc_TerminationID(To, State),
     ?COMMA_INDENT(State),
     enc_TopologyDirection(TD, State),
     ?COMMA_INDENT(State),
     enc_TopologyDirectionExtension(TDE, State)
    ];
enc_TopologyRequest1(
  #'TopologyRequest'{terminationFrom            = From,
		     terminationTo              = To,
		     topologyDirection          = TD,
 		     streamID                   = SID,   % OPTIONAL
 		     topologyDirectionExtension = TDE},  % OPTIONAL
  State) when (SID =/= asn1_NOVALUE) and (TDE =/= asn1_NOVALUE) ->
    [
     enc_TerminationID(From, State),
     ?COMMA_INDENT(State),
     enc_TerminationID(To, State),
     ?COMMA_INDENT(State),
     enc_TopologyDirection(TD, State),
     ?COMMA_INDENT(State),
     enc_StreamID(SID, State),
     ?COMMA_INDENT(State),
     enc_TopologyDirectionExtension(TDE, State)
    ].

enc_TopologyDirection(bothway, _State) ->
    ?BothwayToken;
enc_TopologyDirection(isolate, _State) -> 
    ?IsolateToken;
enc_TopologyDirection(oneway, _State) ->  
    ?OnewayToken.

enc_TopologyDirectionExtension(onewayexternal, _State) ->  
    ?OnewayExternalToken;
enc_TopologyDirectionExtension(onewayboth, _State) ->  
    ?OnewayBothToken.

enc_iepsValue(Val, _State) ->
    [
     ?IEPSToken,
     ?EQUAL,
     case Val of
	 false -> ?OffToken;
	 true  -> ?OnToken
     end
    ].



enc_AmmRequest(#'AmmRequest'{terminationID = TIDs,
			     descriptors   = Ds}, State) ->
    [
     %% Assume that Token is added elsewhere
     ?EQUAL,
     enc_termIDList(TIDs, State),
     enc_opt_brackets(
       enc_list([{Ds, fun enc_ammDescriptor/2}], ?INC_INDENT(State)),
       State)
    ].

enc_ammDescriptor({Tag, Desc}, State) ->
    case Tag of
	mediaDescriptor       -> enc_MediaDescriptor(Desc, State);
        modemDescriptor       -> enc_ModemDescriptor(Desc, State);      
        muxDescriptor         -> enc_MuxDescriptor(Desc, State);   
        eventsDescriptor      -> enc_EventsDescriptor(Desc, State);      
        eventBufferDescriptor -> enc_EventBufferDescriptor(Desc, State); 
        signalsDescriptor     -> enc_SignalsDescriptor(Desc, State);    
        digitMapDescriptor    -> enc_DigitMapDescriptor(Desc, State);    
        auditDescriptor       -> enc_AuditDescriptor(Desc, State);
        statisticsDescriptor  -> enc_StatisticsDescriptor(Desc, State);
	_ ->
	    error({invalid_ammDescriptor_tag, Tag})
    end;
enc_ammDescriptor(Tag, State) when is_atom(Tag) ->
    enc_ammDescriptor({Tag, []}, State).

enc_AmmsReply(#'AmmsReply'{terminationID    = TIDs, 
			   terminationAudit = asn1_NOVALUE}, State) ->
    [
     ?EQUAL,
     enc_termIDList(TIDs, State)
    ];
enc_AmmsReply(#'AmmsReply'{terminationID    = TIDs, 
			   terminationAudit = []}, State) ->
    [
     ?EQUAL,
     enc_termIDList(TIDs, State)
    ];
enc_AmmsReply(#'AmmsReply'{terminationID    = TIDs, 
			   terminationAudit = Res}, State) ->
    [
     ?EQUAL,
     enc_termIDList(TIDs, State),
     case lists:flatten(enc_TerminationAudit(Res, ?INC_INDENT(State))) of
	 [] ->
	     [];
	 L ->
	     [
	      ?LBRKT_INDENT(State), 
	      L,
	      ?RBRKT_INDENT(State)
	     ]
     end
    ].

enc_SubtractRequest(#'SubtractRequest'{terminationID   = TIDs,
				       auditDescriptor = asn1_NOVALUE}, 
		    State) ->
    [
     %% Assume that Token is added elsewhere
     ?EQUAL,
     enc_termIDList(TIDs, State)
    ];
enc_SubtractRequest(#'SubtractRequest'{terminationID   = TIDs,
				       auditDescriptor = AD}, 
		    State) ->
    [
     %% Assume that Token is added elsewhere
     ?EQUAL,
     enc_termIDList(TIDs, State),
     ?LBRKT_INDENT(State),
     enc_AuditDescriptor(AD, ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].    

enc_AuditRequest(#'AuditRequest'{terminationID     = TID,
				 auditDescriptor   = asn1_NOVALUE,
				 terminationIDList = asn1_NOVALUE}, State) ->
    [
     %% Assume that Token is added elsewhere
     ?EQUAL,
     enc_termIDList([TID], State)
    ];
enc_AuditRequest(#'AuditRequest'{terminationID     = TID,
				 auditDescriptor   = asn1_NOVALUE,
				 terminationIDList = [TID|_] = TIDList}, 
		 State) ->
    [
     %% Assume that Token is added elsewhere
     ?EQUAL,
     enc_termIDList(TIDList, State)
    ];
enc_AuditRequest(#'AuditRequest'{terminationID     = TID,
				 auditDescriptor   = asn1_NOVALUE,
				 terminationIDList = TIDList}, 
		 _State) ->
    error({invalid_terminationID, TID, TIDList});
enc_AuditRequest(#'AuditRequest'{terminationID     = TID,
				 auditDescriptor   = AD,
				 terminationIDList = asn1_NOVALUE}, State) ->
    [
     %% Assume that Token is added elsewhere
     ?EQUAL,
     enc_termIDList([TID], State),
     ?LBRKT_INDENT(State),
     enc_AuditDescriptor(AD, ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ];
enc_AuditRequest(#'AuditRequest'{terminationID     = TID,
				 auditDescriptor   = AD,
				 terminationIDList = [TID|_] = TIDList}, 
		 State) ->
    [
     %% Assume that Token is added elsewhere
     ?EQUAL,
     enc_termIDList(TIDList, State),
     ?LBRKT_INDENT(State),
     enc_AuditDescriptor(AD, ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ];
enc_AuditRequest(#'AuditRequest'{terminationID     = TID,
				 auditDescriptor   = _AD,
				 terminationIDList = TIDList}, 
		 _State) ->
    error({invalid_terminationID, TID, TIDList}).

%% auditReply           = (AuditValueToken / AuditCapToken ) 
%% 			  ( contextTerminationAudit  / auditOther)
%% auditOther           = EQUAL TerminationID LBRKT 
%% 			  terminationAudit RBRKT
%% terminationAudit     = auditReturnParameter *(COMMA auditReturnParameter) 
%% 
%% contextTerminationAudit = EQUAL CtxToken ( terminationIDList / 
%% 			                      LBRKT errorDescriptor RBRKT )
enc_AuditReply({Tag, Val}, State) ->
%%     d("enc_AuditReply -> entry with"
%%       "~n   Tag: ~p"
%%       "~n   Val: ~p", [Tag, Val]),
    case Tag of
	contextAuditResult ->
	    [
	     ?EQUAL,
	     ?CtxToken,
	     enc_TerminationIDList(Val, State)
	    ];
	error ->
	    [
	     ?EQUAL,
	     ?CtxToken,
	     ?LBRKT_INDENT(State),
	     enc_ErrorDescriptor(Val, ?INC_INDENT(State)),
	     ?RBRKT_INDENT(State)
	    ]; 
	auditResult when is_record(Val, 'AuditResult') ->
	    enc_auditOther(Val, State);
	auditResult ->
	    error({invalid_auditResult, Val});
	auditResultTermList ->
	    enc_TermListAuditResult(Val, State);
	_ ->
	    error({invalid_AuditReply_tag, Tag})
    end.

%% This is actually the same as AuditResult with the exception
%% that instead of terminationID we have terminationIDList.
enc_TermListAuditResult(
  #'TermListAuditResult'{terminationIDList      = TIDList,
			 terminationAuditResult = asn1_NOVALUE}, State) ->
%%     d("enc_TermListAuditResult -> entry with"
%%       "~n   TIDList: ~p", [TIDList]),
    [
     ?EQUAL,
     enc_termIDList(TIDList, State)
    ];
enc_TermListAuditResult(
  #'TermListAuditResult'{terminationIDList      = TIDList,
			 terminationAuditResult = []}, State) ->
%%     d("enc_TermListAuditResult -> entry with"
%%       "~n   TIDList: ~p", [TIDList]),
    [
     ?EQUAL,
     enc_termIDList(TIDList, State)
    ];
enc_TermListAuditResult(
  #'TermListAuditResult'{terminationIDList      = TIDList,
			 terminationAuditResult = TAR}, State) ->
%%     d("enc_TermListAuditResult -> entry with"
%%       "~n   TIDList: ~p"
%%       "~n   TAR:     ~p", [TIDList, TAR]),
    [
     ?EQUAL,
     enc_termIDList(TIDList, State),
     case lists:flatten(enc_TerminationAudit(TAR, ?INC_INDENT(State))) of
	 [] ->
	     [];
	 L ->
	     [
	      ?LBRKT_INDENT(State), 
	      L,
	      ?RBRKT_INDENT(State)
	     ]
     end
    ].

enc_auditOther(#'AuditResult'{terminationID          = TID,
			      terminationAuditResult = asn1_NOVALUE}, State) ->
    [
     ?EQUAL,
     enc_termIDList([TID], State)
    ];
enc_auditOther(#'AuditResult'{terminationID          = TID,
			      terminationAuditResult = []}, State) ->
    [
     ?EQUAL,
     enc_termIDList([TID], State)
    ];
enc_auditOther(#'AuditResult'{terminationID          = TID,
			      terminationAuditResult = Res}, State) ->
    [
     ?EQUAL,
     enc_termIDList([TID], State),
     case lists:flatten(enc_TerminationAudit(Res, ?INC_INDENT(State))) of
	 [] ->
	     [];
	 L ->
	     [
	      ?LBRKT_INDENT(State), 
	      L,
	      ?RBRKT_INDENT(State)
	     ]
     end
    ].

    
enc_AuditDescriptor(#'AuditDescriptor'{auditToken = asn1_NOVALUE,
				       auditPropertyToken = asn1_NOVALUE}, 
		    _State) ->
%     d("enc_AuditDescriptor(asn1_NOVALUE) -> entry"),
    [
     ?AuditToken,
     [?LBRKT, ?RBRKT]
    ];
enc_AuditDescriptor(#'AuditDescriptor'{auditToken = [],
				       auditPropertyToken = asn1_NOVALUE}, 
		    _State) ->
%     d("enc_AuditDescriptor([]) -> entry"),
    [
     ?AuditToken,
     [?LBRKT, ?RBRKT]
    ];
enc_AuditDescriptor(#'AuditDescriptor'{auditToken = List,
				       auditPropertyToken = asn1_NOVALUE}, 
		    State) ->
    [
     ?AuditToken,
     [
      ?LBRKT_INDENT(State),
      enc_list([{List, fun enc_auditItem/2}], ?INC_INDENT(State)),
      ?RBRKT_INDENT(State)
     ]
    ];
%% - v2 -
enc_AuditDescriptor(#'AuditDescriptor'{auditToken = asn1_NOVALUE,
				       auditPropertyToken = Prop}, 
		    State) ->
    [
     ?AuditToken,
     [
      ?LBRKT_INDENT(State),
      enc_auditPropertyToken(Prop, ?INC_INDENT(State)),
      ?RBRKT_INDENT(State)
     ]
    ];
enc_AuditDescriptor(#'AuditDescriptor'{auditToken = List,
				       auditPropertyToken = Prop}, 
		    State) ->
    [
     ?AuditToken,
     [
      ?LBRKT_INDENT(State),
      enc_list([{List, fun enc_auditItem/2}], ?INC_INDENT(State)),
      ?COMMA_INDENT(State), 
      enc_auditPropertyToken(Prop, ?INC_INDENT(State)),  % v2
      ?RBRKT_INDENT(State)
     ]
    ].

enc_auditItem(signalsToken, _State) ->
    ?SignalsToken;
enc_auditItem(eventBufferToken, _State) -> 
    ?EventBufferToken;
enc_auditItem(eventsToken, _State) ->
    ?EventsToken;
enc_auditItem(Val, State) ->
    enc_auditReturnItem(Val, State).


enc_auditReturnItem(muxToken, _State) ->
    ?MuxToken;
enc_auditReturnItem(modemToken, _State) ->
    ?ModemToken;
enc_auditReturnItem(mediaToken, _State) ->
    ?MediaToken;
enc_auditReturnItem(digitMapToken, _State) ->
    ?DigitMapToken;
enc_auditReturnItem(statsToken, _State) ->
    ?StatsToken;
enc_auditReturnItem(observedEventsToken, _State) ->
    ?ObservedEventsToken;
enc_auditReturnItem(packagesToken, _State) ->
    ?PackagesToken.


%% - v2 begin -

enc_auditPropertyToken([], _State) ->
    [];
enc_auditPropertyToken([Param | Params], State) ->
    [enc_IndAudauditReturnParameter(Param, State),
     [[?COMMA_INDENT(State), 
       enc_IndAudauditReturnParameter(P, State)] || P <- Params]].


enc_IndAudauditReturnParameter({Tag, Val}, State) ->
    case Tag of
	indAudMediaDescriptor ->
	    enc_IndAudMediaDescriptor(Val, State);
	indAudEventsDescriptor ->
	    enc_IndAudEventsDescriptor(Val, State);
	indAudSignalsDescriptor ->
	    enc_IndAudSignalsDescriptor(Val, State);
	indAudDigitMapDescriptor ->
	    enc_IndAudDigitMapDescriptor(Val, State); 
	indAudEventBufferDescriptor ->
	    enc_IndAudEventBufferDescriptor(Val, State);
	indAudStatisticsDescriptor ->
	    enc_IndAudStatisticsDescriptor(Val, State);
	indAudPackagesDescriptor ->
	    enc_IndAudPackagesDescriptor(Val, State);
	_ ->
	    error({invalid_IndAudauditReturnParameter_tag, Tag})
    end.

enc_IndAudMediaDescriptor(
  #'IndAudMediaDescriptor'{termStateDescr = asn1_NOVALUE,
			   streams        = Streams}, State) ->
    [
     ?MediaToken,
     ?LBRKT_INDENT(State),
     enc_IndAudMediaDescriptor_streams(Streams, ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ];
enc_IndAudMediaDescriptor(
  #'IndAudMediaDescriptor'{termStateDescr = TSD,
			   streams        = asn1_NOVALUE}, State) ->
    [
     ?MediaToken,
     ?LBRKT_INDENT(State),
     enc_IndAudTerminationStateDescriptor(TSD, ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ];
enc_IndAudMediaDescriptor(
  #'IndAudMediaDescriptor'{termStateDescr = TSD,
			   streams        = Streams}, State) ->
    [
     ?MediaToken,
     ?LBRKT_INDENT(State),
     enc_IndAudTerminationStateDescriptor(TSD, ?INC_INDENT(State)),
     ?COMMA_INDENT(State), 
     enc_IndAudMediaDescriptor_streams(Streams, ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

enc_IndAudMediaDescriptor_streams({oneStream, Val}, State) ->
    enc_IndAudStreamParms(Val, State);
enc_IndAudMediaDescriptor_streams({multiStream, Val}, State) ->
    enc_IndAudMediaDescriptor_multiStream(Val, State);
enc_IndAudMediaDescriptor_streams({Tag, _Val}, _State) ->
    error({invalid_IndAudMediaDescriptor_streams_tag, Tag}).

enc_IndAudTerminationStateDescriptor(
  #'IndAudTerminationStateDescriptor'{propertyParms      = [],
				      eventBufferControl = asn1_NOVALUE,
				      serviceState       = 'NULL',
				      serviceStateSel    = asn1_NOVALUE}, 
  _State) ->
    [
     ?TerminationStateToken,
     ?LBRKT_INDENT(_State),
     ?ServiceStatesToken,
     ?RBRKT_INDENT(_State)
    ];
enc_IndAudTerminationStateDescriptor(
  #'IndAudTerminationStateDescriptor'{propertyParms      = [],
				      eventBufferControl = asn1_NOVALUE,
				      serviceState       = asn1_NOVALUE,
				      serviceStateSel    = SSS}, 
  State) when SSS =/= asn1_NOVALUE ->
    [
     ?TerminationStateToken,
     ?LBRKT_INDENT(State),
     enc_serviceState(SSS, State), 
     ?RBRKT_INDENT(State)
    ];
enc_IndAudTerminationStateDescriptor(
  #'IndAudTerminationStateDescriptor'{propertyParms      = [],
				      eventBufferControl = 'NULL',
				      serviceState       = asn1_NOVALUE,
				      serviceStateSel    = asn1_NOVALUE}, 
  _State) ->
    [
     ?TerminationStateToken,
     ?LBRKT_INDENT(_State),
     ?BufferToken,
     ?RBRKT_INDENT(_State)
    ];
enc_IndAudTerminationStateDescriptor(
  #'IndAudTerminationStateDescriptor'{propertyParms      = [Parms],
				      eventBufferControl = asn1_NOVALUE,
				      serviceState       = asn1_NOVALUE,
				      serviceStateSel    = asn1_NOVALUE}, 
  State) ->
    #'IndAudPropertyParm'{name = Name} = Parms,
    [
     ?TerminationStateToken,
     ?LBRKT_INDENT(State),
     enc_PkgdName(Name, State),
     ?RBRKT_INDENT(State)
    ].

enc_IndAudStreamParms(
  #'IndAudStreamParms'{localControlDescriptor = LCD,
		       localDescriptor        = LD, 
		       remoteDescriptor       = RD, 
		       statisticsDescriptor   = SD}, State) ->
    [
     enc_list([{[LCD], fun enc_IndAudLocalControlDescriptor/2},
	       {[LD],  fun enc_remoteDescriptor/2},
	       {[RD],  fun enc_localDescriptor/2},
	       {[SD],  fun enc_IndAudStatisticsDescriptor/2}], 
	      ?INC_INDENT(State))
    ].

enc_IndAudLocalControlDescriptor(
  #'IndAudLocalControlDescriptor'{streamMode    = SM,
				  reserveValue  = RV,
				  reserveGroup  = RG,
				  propertyParms = PP,
				  streamModeSel = asn1_NOVALUE}, State) ->
    [
     ?LocalControlToken,
     ?LBRKT_INDENT(State),
     enc_list([{[SM], fun('NULL', _) -> ?ModeToken end},
	       {[RV], fun('NULL', _) -> ?ReservedValueToken end},
	       {[RG], fun('NULL', _) -> ?ReservedGroupToken end},
	       {PP,   fun enc_IndAudPropertyParm/2}],
	      ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ];
enc_IndAudLocalControlDescriptor(
  #'IndAudLocalControlDescriptor'{streamMode    = asn1_NOVALUE,
				  reserveValue  = RV,
				  reserveGroup  = RG,
				  propertyParms = PP,
				  streamModeSel = SMS}, State) ->
    [
     ?LocalControlToken,
     ?LBRKT_INDENT(State),
     enc_list([{[RV],  fun('NULL', _) -> ?ReservedValueToken end},
	       {[RG],  fun('NULL', _) -> ?ReservedGroupToken end},
	       { PP,   fun enc_IndAudPropertyParm/2},
	       {[SMS], fun enc_StreamMode/2}], 
	      ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

enc_IndAudPropertyParm(#'IndAudPropertyParm'{name = Name, propertyParms = PP}, 
		       State) ->
    [
     enc_list([{[Name], fun enc_PkgdName/2},
	       {[PP],   fun enc_PropertyParm/2}], State)
    ].

enc_IndAudMediaDescriptor_multiStream(Val, State) when is_list(Val) ->
    [
     enc_list([{Val, fun enc_IndAudStreamDescriptor/2}], State)
    ];
enc_IndAudMediaDescriptor_multiStream(Val, _State) ->
    error({invalid_IndAudMediaDescriptor_multiStream, Val}).

enc_IndAudStreamDescriptor(#'IndAudStreamDescriptor'{streamID    = SID,
						     streamParms = Parms}, 
			   State) ->
    [
     ?StreamToken,
     ?EQUAL,
     enc_StreamID(SID, State),
     ?LBRKT_INDENT(State),
     enc_IndAudStreamParms(Parms, ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].
    
enc_IndAudEventBufferDescriptor(
  #'IndAudEventBufferDescriptor'{eventName = EvName,
				 streamID  = ID}, State) ->
    [
     ?EventBufferToken,
     ?LBRKT_INDENT(State),
     enc_PkgdName(EvName, State),
     enc_IndAudEventBufferDescriptor_eventSpec(ID, ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

enc_IndAudEventBufferDescriptor_eventSpec(asn1_NOVALUE, _State) ->
    [
    ];
enc_IndAudEventBufferDescriptor_eventSpec({eventParameterName, ParamName}, 
					  State) ->
    [
     ?LBRKT_INDENT(State),
     enc_Name(ParamName, State),
     ?RBRKT_INDENT(State)
    ];
enc_IndAudEventBufferDescriptor_eventSpec({eventStream, ID}, State) ->
    [
     ?LBRKT_INDENT(State),
     enc_eventStream(ID, ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ];
enc_IndAudEventBufferDescriptor_eventSpec(ID, State) ->
    [
     ?LBRKT_INDENT(State),
     enc_eventStream(ID, ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

enc_IndAudEventsDescriptor(
  #'IndAudEventsDescriptor'{requestID = asn1_NOVALUE,
			    pkgdName  = Name,
			    streamID  = asn1_NOVALUE}, State) ->
    [
     ?EventsToken,
     ?LBRKT_INDENT(State),
     enc_PkgdName(Name, State), 
     ?RBRKT_INDENT(State)
    ];
enc_IndAudEventsDescriptor(
  #'IndAudEventsDescriptor'{requestID = RID,
			    pkgdName  = Name,
			    streamID  = asn1_NOVALUE}, State) ->
    [
     ?EventsToken,
     ?EQUAL,
     enc_RequestID(RID, State),
     ?LBRKT_INDENT(State),
     enc_PkgdName(Name, State), 
     ?RBRKT_INDENT(State)
    ].


enc_IndAudSignalsDescriptor(asn1_NOVALUE, _State) ->
    [
     ?SignalsToken,
     ?LBRKT_INDENT(_State),
     ?RBRKT_INDENT(_State)
    ];
enc_IndAudSignalsDescriptor(Val, State) ->
    [
     ?SignalsToken,
     ?LBRKT_INDENT(State),
     enc_IndAudSignalsDescriptor_value(Val, ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

enc_IndAudSignalsDescriptor_value({signal, Val}, State) ->
    enc_IndAudSignal(Val, State);
enc_IndAudSignalsDescriptor_value({seqSigList, Val}, State) ->
    enc_IndAudSeqSigList(Val, State).
    
enc_IndAudSignal(#'IndAudSignal'{signalName      = SignalName,
				 streamID        = asn1_NOVALUE,
				 signalRequestID = asn1_NOVALUE}, State) ->
    [
     enc_SignalName(SignalName, State)
    ];
enc_IndAudSignal(#'IndAudSignal'{signalName      = SignalName,
				 streamID        = SID,
				 signalRequestID = SRID}, State) ->
    [
     enc_SignalName(SignalName, State),
     ?LBRKT_INDENT(State),
     enc_list([{[SID],  fun enc_StreamID/2},
	       {[SRID], fun enc_sigRequestID/2}],
	      State),
     ?RBRKT_INDENT(State)     
    ].

enc_IndAudSeqSigList(#'IndAudSeqSigList'{id         = ID,
					 signalList = asn1_NOVALUE}, 
		     State) ->
    [
     ?SignalListToken,
     ?EQUAL,
     enc_UINT16(ID, State)
    ];
enc_IndAudSeqSigList(#'IndAudSeqSigList'{id         = ID,
					 signalList = SL}, 
		     State) ->
    [
     ?SignalListToken,
     ?EQUAL,
     enc_UINT16(ID, State),
     ?LBRKT_INDENT(State),
     enc_IndAudSignal(SL, ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

enc_IndAudDigitMapDescriptor(#'IndAudDigitMapDescriptor'{digitMapName = Name}, 
			     State) ->
    [
     ?DigitMapToken,
     ?EQUAL,
     enc_DigitMapName(Name, State)
    ].

enc_IndAudStatisticsDescriptor(#'IndAudStatisticsDescriptor'{statName = Name}, 
			       State) ->
    [
     ?StatsToken,
     ?LBRKT_INDENT(State),
     enc_PkgdName(Name, State),     
     ?RBRKT_INDENT(State)    
    ].


enc_IndAudPackagesDescriptor(#'IndAudPackagesDescriptor'{packageName    = N,
							 packageVersion = V}, 
			     State) ->
    [
     ?PackagesToken,
     ?LBRKT_INDENT(State),
     enc_Name(N, State),
     "-",
     enc_UINT16(V, State),
     ?RBRKT_INDENT(State) 
    ].


%% - v2 end -

    
enc_TerminationAudit({'TerminationAudit', Val}, State) ->
    enc_TerminationAudit(Val, State);
enc_TerminationAudit([Mand | Opt], State) ->
    [enc_AuditReturnParameter(Mand, State),
     [[?COMMA_INDENT(State), enc_AuditReturnParameter(Val, State)] || Val <- Opt]].

enc_AuditReturnParameter({'AuditReturnParameter',Val}, State) ->
    enc_AuditReturnParameter(Val, State);
enc_AuditReturnParameter({Tag, Val}, State) ->
%%     d("enc_AuditReturnParameter -> entry with"
%%       "~n   Tag: ~p"
%%       "~n   Val: ~p", [Tag, Val]),
    case Tag of
	mediaDescriptor ->
	    enc_MediaDescriptor(Val, State);
	modemDescriptor ->
	    enc_ModemDescriptor(Val, State);
	muxDescriptor ->
	    enc_MuxDescriptor(Val, State);
	eventsDescriptor ->
	    enc_EventsDescriptor(Val, State);
	signalsDescriptor ->
	    enc_SignalsDescriptor(Val, State);
	digitMapDescriptor ->
	    enc_DigitMapDescriptor(Val, State);
	observedEventsDescriptor ->
	    enc_ObservedEventsDescriptor(Val, State);
	eventBufferDescriptor ->
	    enc_EventBufferDescriptor(Val, State);
	statisticsDescriptor ->
	    enc_StatisticsDescriptor(Val, State);
	packagesDescriptor ->
	    enc_PackagesDescriptor(Val, State);
	errorDescriptor ->
	    enc_ErrorDescriptor(Val, State);
        emptyDescriptors ->
            enc_EmptyDescriptors(Val, State);
	_ ->
	    error({invalid_AuditReturnParameter_tag, Tag})
    end.

enc_EmptyDescriptors(#'AuditDescriptor'{auditToken = asn1_NOVALUE}, _State) ->
    [];
enc_EmptyDescriptors(#'AuditDescriptor'{auditToken = []}, _State) ->
    [];
enc_EmptyDescriptors(#'AuditDescriptor'{auditToken = List}, State) ->
%%     d("enc_AuditReturnParameter -> entry with"
%%       "~n   List: ~p", [List]),
    enc_list([{List, fun enc_auditReturnItem/2}], State).


enc_NotifyRequest(#'NotifyRequest'{terminationID            = TIDs,
				   observedEventsDescriptor = OED,
				   errorDescriptor          = asn1_NOVALUE}, 
		  State) ->
    [
     %% Assume that Token is added elsewhere
     ?EQUAL,
     enc_termIDList(TIDs, State),
     ?LBRKT_INDENT(State),
     enc_ObservedEventsDescriptor(OED, ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ];
enc_NotifyRequest(#'NotifyRequest'{terminationID            = TIDs,
				   observedEventsDescriptor = OED,
				   errorDescriptor          = ED}, State) ->
    [
     %% Assume that Token is added elsewhere
     ?EQUAL,
     enc_termIDList(TIDs, State),
     ?LBRKT_INDENT(State),
     enc_ObservedEventsDescriptor(OED, ?INC_INDENT(State)),
     ?COMMA,
     enc_ErrorDescriptor(ED, ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

enc_NotifyReply(#'NotifyReply'{terminationID   = TIDs,
			       errorDescriptor = asn1_NOVALUE}, State) ->
    [
     %% Assume that Token is added elsewhere
     ?EQUAL,
     enc_termIDList(TIDs, State)
    ];
enc_NotifyReply(#'NotifyReply'{terminationID   = TIDs,
			       errorDescriptor = ED}, State) ->
    [
     %% Assume that Token is added elsewhere
     ?EQUAL,
     enc_termIDList(TIDs, State),
     ?LBRKT_INDENT(State),
     enc_ErrorDescriptor(ED, ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

enc_ObservedEventsDescriptor(
  #'ObservedEventsDescriptor'{requestId        = RID,
			      observedEventLst = OEL}, State) ->
    [
     ?ObservedEventsToken,
     ?EQUAL,
     enc_RequestID(RID, State),
     ?LBRKT_INDENT(State),
     enc_observedEvents(OEL, ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

enc_observedEvents([Mand | Opt], State) ->
    [enc_ObservedEvent(Mand, State),
     [[?COMMA_INDENT(State), enc_ObservedEvent(Val, State)] || Val <- Opt]].

%% ;time per event, because it might be buffered
%% observedEvent        = [ TimeStamp LWSP COLON] LWSP 
%% 			  pkgdName [ LBRKT observedEventParameter
%% 			  *(COMMA observedEventParameter) RBRKT ]
%% 
%% ;at-most-once eventStream, every eventParameterName at most once
%% observedEventParameter = eventStream / eventOther
enc_ObservedEvent(#'ObservedEvent'{eventName    = EN,
				   streamID     = SID,
				   eventParList = EPL,
				   timeNotation = asn1_NOVALUE}, State) ->
    [
     ?LWSP,
     enc_EventName(EN, State),
     enc_opt_brackets(
       enc_list([{[SID], fun enc_eventStream/2},
		 { EPL,  fun enc_eventOther/2}],
		?INC_INDENT(State)),
       State)
    ];
enc_ObservedEvent(#'ObservedEvent'{eventName    = EN,
				   streamID     = SID,
				   eventParList = EPL,
				   timeNotation = TN}, State) ->
    [
     enc_TimeNotation(TN, State),
     ?LWSP,
     ?COLON,
     ?LWSP,
     enc_EventName(EN, State),
     enc_opt_brackets(
       enc_list([{[SID], fun enc_eventStream/2},
		 {EPL,   fun enc_eventOther/2}],
		?INC_INDENT(State)),
       State)
    ].

enc_EventName({'EventName', Val}, State) ->
    enc_EventName(Val, State);
enc_EventName(Val, State) ->
    PkgdName = ?META_ENC(event, Val),
    enc_PkgdName(PkgdName, State).

enc_eventStream(Val, State) ->
    [
     ?StreamToken,
     ?EQUAL,
     enc_StreamID(Val, State)
    ].

%% The value is already encoded
enc_eventOther(#megaco_event_parameter{name  = Name,
				       value = Value}, State) 
  when is_list(Value) ->
    [
     enc_Name(Name, State),
     ?EqualToken,
     Value
    ];
%% Special treatment of the ds parameter of the dd/ce event
enc_eventOther(#'EventParameter'{eventParameterName = "ds" = Name,
				 value              = [DigitString],
				 extraInfo          = asn1_NOVALUE}, State) ->
    [
     enc_Name(Name, State),
     ?EqualToken,
     enc_DigitString(DigitString, State)
    ];
enc_eventOther(#'EventParameter'{eventParameterName = Name,
				 value              = Value,
				 extraInfo          = Extra}, State) ->
    [
     enc_Name(Name, State),
     enc_propertyParmValues(Value, Extra, State)
    ].

enc_ServiceChangeRequest(
  #'ServiceChangeRequest'{terminationID      = TIDs,
			  serviceChangeParms = Parms}, State) ->
    [
     %% Assume that Token is added elsewhere
     ?EQUAL,
     enc_termIDList(TIDs, State),
     ?LBRKT_INDENT(State),
     enc_ServiceChangeParm(Parms, ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

%% serviceChangeReply   = ServiceChangeToken EQUAL TerminationID
%% 			  [LBRKT (errorDescriptor / 
%% 			  serviceChangeReplyDescriptor) RBRKT]
%% serviceChangeReplyDescriptor = ServicesToken LBRKT
%% 			  servChgReplyParm *(COMMA servChgReplyParm) RBRKT
%% 
%% ;at-most-once. Version is REQUIRED on first ServiceChange response
%% servChgReplyParm     = (serviceChangeAddress / serviceChangeMgcId /
%% 			  serviceChangeProfile / serviceChangeVersion )
enc_ServiceChangeReply(
  #'ServiceChangeReply'{terminationID       = TIDs,
			serviceChangeResult = Res}, State) ->
    [
     %% Assume that Token is added elsewhere
     ?EQUAL,
     enc_termIDList(TIDs, State),
     enc_ServiceChangeResult(Res, State)
     ].

enc_ServiceChangeResult({'ServiceChangeResult', Val}, State) ->
    enc_ServiceChangeResult(Val, State);
enc_ServiceChangeResult({errorDescriptor, Val}, State) ->
    [
     ?LBRKT_INDENT(State),
     enc_ErrorDescriptor(Val, ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ];
enc_ServiceChangeResult({serviceChangeResParms, Val}, State) ->
    case enc_ServiceChangeResParm(Val, ?INC_INDENT(?INC_INDENT(State))) of
	[] ->
	    [];
	ResParms ->
	    [
	     ?LBRKT_INDENT(State),
	     ?ServicesToken,
	     fun(_S) ->
		     [
		      ?LBRKT_INDENT(_S),
		      ResParms,
		      ?RBRKT_INDENT(_S)
		     ]
	     end(?INC_INDENT(State)),
	     ?RBRKT_INDENT(State)
	    ]
    end;
enc_ServiceChangeResult({Tag, _}, _State) ->
    error({invalid_ServiceChangeResult_tag, Tag}).

%% Required length of termination ID list is 1
%% enc_TerminationIDList1({'TerminationIDList',Val}, State) ->
%%     enc_TerminationIDList1(Val, State);
%% enc_TerminationIDList1([Singleton], State) ->
%%     enc_TerminationID(Singleton, State).

%% No required length of termination ID list
enc_TerminationIDList({'TerminationIDList',Val}, State) ->
    enc_TerminationIDList(Val, State);
enc_TerminationIDList([TID], State) ->
    [
     ?LBRKT_INDENT(State),
     enc_TerminationID(TID, State), 
     ?RBRKT_INDENT(State)
    ];
enc_TerminationIDList(TIDs, State) ->
    [
     ?LBRKT_INDENT(State),
     enc_list([{TIDs, fun enc_TerminationID/2}], State),
     ?RBRKT_INDENT(State)
    ].

enc_termIDList({'TerminationIDList',Val}, State) ->
    enc_termIDList(Val, State);
enc_termIDList([Singleton], State) ->
    enc_TerminationID(Singleton, State);
enc_termIDList(TidList, State) 
  when is_list(TidList) and (length(TidList) > 1) ->
%%     d("enc_termIDList -> entry with"
%%       "~n   TidList: ~p", [TidList]),
    State2 = ?INC_INDENT(State),
    [
     ?LSBRKT_INDENT(State),
     enc_list([{TidList, fun enc_TerminationID/2}], State2),
     ?RSBRKT_INDENT(State)
    ].

%% TerminationID        = "ROOT" / pathNAME / "$" / "*"
%% ; Total length of pathNAME must not exceed 64 chars.
%% pathNAME             = ["*"] NAME *("/" / "*"/ ALPHA / DIGIT /"_" / "$" ) 
%% 			  ["@" pathDomainName ]
enc_TerminationID(Tid, State)
  when is_record(Tid, megaco_term_id) ->
    List = [{Tid#megaco_term_id.id, fun enc_tid_component/2 }],
    enc_list(List, State, fun(_S) -> ?SLASH end, false).    

enc_tid_component(Component, State) when is_list(Component) ->
    [enc_tid_sub_component(Sub, State) || Sub <- Component];
enc_tid_component(Invalid, _State) ->
    error({invalid_id_list_component, Invalid}).

enc_tid_sub_component(all = _Sub, _State) ->
    ?megaco_all;
enc_tid_sub_component(choose = _Sub, _State) ->
    ?megaco_choose;
enc_tid_sub_component(Char, _State) when is_integer(Char) ->
    Char;
enc_tid_sub_component(Invalid, _State) ->
    error({invalid_id_list_sub_component, Invalid}).

%% enc_tid_sub_component(Sub, _State) ->
%%     case Sub of
%% 	all    -> ?megaco_all;
%% 	choose -> ?megaco_choose;
%% 	Char when is_integer(Char) -> Char
%%     end.

%% mediaDescriptor      = MediaToken LBRKT mediaParm *(COMMA mediaParm) RBRKT
%% ; at-most-once per item
%% ; and either streamParm or streamDescriptor but not both
%% mediaParm            = (streamParm / streamDescriptor / 
%% 			   terminationStateDescriptor)
%% ; at-most-once
%% streamParm           = ( localDescriptor / remoteDescriptor / 
%% 			   localControlDescriptor )
%% streamDescriptor     = StreamToken EQUAL StreamID LBRKT streamParm 
%% 			  *(COMMA streamParm) RBRKT
enc_MediaDescriptor(#'MediaDescriptor'{termStateDescr = TSD,
				       streams        = Streams}, State) ->
    [
     ?MediaToken,
     ?LBRKT_INDENT(State),
     enc_list([{[TSD], fun enc_TerminationStateDescriptor/2} |
	       decompose_streams(Streams)],
	      ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

decompose_streams(asn1_NOVALUE) ->
    [];
decompose_streams({'MediaDescriptor_streams',Val}) ->
    decompose_streams(Val);
decompose_streams({Tag, Val}) ->
    case Tag of
	oneStream ->
	    decompose_StreamParms(Val);
	multiStream ->
	    [{Val, fun enc_StreamDescriptor/2}];
	_ ->
	    error({invalid_streams_tag, Tag})
    end.

decompose_StreamParms(Val)
  when is_record(Val, 'StreamParms') ->
    [
     {[Val#'StreamParms'.localControlDescriptor],
      fun enc_LocalControlDescriptor/2},
     {[Val#'StreamParms'.localDescriptor],
      fun enc_localDescriptor/2},
     {[Val#'StreamParms'.remoteDescriptor],
      fun enc_remoteDescriptor/2},
     {[Val#'StreamParms'.statisticsDescriptor],
      fun enc_StatisticsDescriptor/2}
    ].

enc_StreamDescriptor(Val, State) 
    when is_record(Val, 'StreamDescriptor') ->
    [
     ?StreamToken,
     ?EQUAL,
     enc_StreamID(Val#'StreamDescriptor'.streamID, State),
     ?LBRKT_INDENT(State),
     enc_list(decompose_StreamParms(Val#'StreamDescriptor'.streamParms),
	      ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

%% localControlDescriptor = LocalControlToken LBRKT localParm 
%% 			    *(COMMA localParm) RBRKT
%% 
%% ; at-most-once per item
%% localParm            = ( streamMode / propertyParm /
%%                          reservedValueMode  / reservedGroupMode ) 
%% reservedValueMode       = ReservedValueToken EQUAL ( "ON" / "OFF" ) 
%% reservedGroupMode       = ReservedGroupToken EQUAL ( "ON" / "OFF" ) 
%% 
%% reservedMode	     = ReservedToken EQUAL ( "ON" / "OFF" )
%% 
%% streamMode           = ModeToken EQUAL streamModes
enc_LocalControlDescriptor(
  #'LocalControlDescriptor'{streamMode    = asn1_NOVALUE, 
			    reserveValue  = asn1_NOVALUE, 
			    reserveGroup  = asn1_NOVALUE, 
			    propertyParms = []}, _State) ->
    error({invalid_LocalControlDescriptor, empty});
enc_LocalControlDescriptor(
  #'LocalControlDescriptor'{streamMode    = SM, 
			    reserveValue  = RV, 
			    reserveGroup  = RG, 
			    propertyParms = PPs}, State) ->
    [
     ?LocalControlToken,
     ?LBRKT_INDENT(State),
     enc_list([{[SM], fun enc_StreamMode/2},
	       {[RG], fun enc_reservedGroupMode/2},
	       {[RV], fun enc_reservedValueMode/2},
	       {PPs,  fun enc_PropertyParm/2}], ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

enc_reservedGroupMode(Val, _State) ->
    [
     ?ReservedGroupToken,
     ?EQUAL,
     case Val of
	 false -> ?OffToken;
	 true  -> ?OnToken
     end
    ].

enc_reservedValueMode(Val, _State) ->
    [
     ?ReservedValueToken,
     ?EQUAL,
     case Val of
	 false -> ?OffToken;
	 true  -> ?OnToken
     end
    ].

enc_StreamMode({'StreamMode',Val}, State) ->
    enc_StreamMode(Val, State);
enc_StreamMode(Val, _State) ->
    [
     ?ModeToken,
     ?EQUAL,
     case Val of
	 sendOnly -> ?SendonlyToken;
	 recvOnly -> ?RecvonlyToken;
	 sendRecv -> ?SendrecvToken;
	 inactive -> ?InactiveToken;
	 loopBack -> ?LoopbackToken
     end
    ].

enc_Name({'Name',Val}, State) ->
    enc_Name(Val, State);
enc_Name(Val, State) ->
    %% BUGBUG: NAME = ALPHA *63(ALPHA / DIGIT / "_" )
    enc_STRING(Val, State, 1, 64).

enc_PkgdName({'PkgdName', Val}, State) ->
    enc_PkgdName(Val, State);
enc_PkgdName(Val, _State) ->
    %% BUGBUG:  pkgdName =  (NAME / "*")  SLASH  (ItemID / "*" )
    %% enc_OCTET_STRING(Val, _State, 1, 64).
    if 
	is_list(Val) ->
	    Length = length(Val),
	    if
		(Length >= 1) ->
		    if
			(Length =< 64) ->
			    Val;
			true ->
			    error({pkgdName_toolong, Length, 64})
		    end;
		true ->
		    error({pkgdName_tooshort, Length, 1})
	    end;
	true ->
	    error({invalid_PkgdName, Val})
    end.

enc_localDescriptor(Val, State) 
  when is_record(Val, 'LocalRemoteDescriptor') ->
    [
     ?LocalToken,
     ?LBRKT,
     enc_LocalRemoteDescriptor(Val, State),
     ?RBRKT_INDENT(State)
    ].

enc_remoteDescriptor(Val, State) 
  when is_record(Val, 'LocalRemoteDescriptor') ->
    [
     ?RemoteToken,
     ?LBRKT,
     enc_LocalRemoteDescriptor(Val, State),
     ?RBRKT_INDENT(State)
    ].

%% When text encoding the protocol, the descriptors consist of session
%% descriptions as defined in SDP (RFC2327), except that the "s=", "t="
%% and "o=" lines are optional. When multiple session descriptions are
%% provided in one descriptor, the "v=" lines are required as delimiters;
%% otherwise they are optional.  Implementations shall accept session
%% descriptions that are fully conformant to RFC2327. When binary
%% encoding the protocol the descriptor consists of groups of properties
%% (tag-value pairs) as specified in Annex C.  Each such group may
%% contain the parameters of a session description.
enc_LocalRemoteDescriptor(Val, State)
  when is_record(Val, 'LocalRemoteDescriptor') ->
    case Val#'LocalRemoteDescriptor'.propGrps of
	[] ->
	    [];
	[OptV | MandV] ->
	    [?LfToken,
	     enc_PropertyGroup(OptV, opt_v, State) |
	     [enc_PropertyGroup(M, mand_v, State) || M <- MandV]]
    end.

enc_PropertyGroup({'PropertyGroup',Val}, RequiresV, State) ->
    enc_PropertyGroup(Val, RequiresV, State);
enc_PropertyGroup([H | _T] = List, mand_v, State) 
  when is_record(H, 'PropertyParm') and (H#'PropertyParm'.name == "v") ->
    enc_PropertyGroup(List, opt_v, State);
enc_PropertyGroup(PG, opt_v, State) ->
    [
     [[enc_PropertyGroupParm(PP, State), ?CrToken, ?LfToken] || PP <- PG]
    ].

enc_PropertyGroupParm(Val, State)
  when is_record(Val, 'PropertyParm') ->
    [OctetString] = Val#'PropertyParm'.value,
    [
     enc_PkgdName(Val#'PropertyParm'.name, State),
     ?EqualToken,
     enc_OCTET_STRING(OctetString, State, 0, infinity)
    ].

%% propertyParm         = pkgdName parmValue
%% parmValue            = (EQUAL alternativeValue/ INEQUAL VALUE)
%% alternativeValue     = ( VALUE / LSBRKT VALUE *(COMMA VALUE) RSBRKT  / 
%% 			  LSBRKT VALUE DOT DOT VALUE RSBRKT )
enc_PropertyParm(Val, State)
  when is_record(Val, 'PropertyParm') ->
    PkgdName = ?META_ENC(property, Val#'PropertyParm'.name),
    [
     enc_PkgdName(PkgdName, State),
     enc_propertyParmValues(Val#'PropertyParm'.value,
			    Val#'PropertyParm'.extraInfo,
			    State)
    ].
     
enc_propertyParmValues([Single], asn1_NOVALUE, State) ->
    [
     ?EqualToken,
     enc_Value(Single, State)
    ];
enc_propertyParmValues([Single], {relation, Rel}, State) ->
    case Rel of
	greaterThan -> [$>, enc_Value(Single, State)];
	smallerThan -> [$<, enc_Value(Single, State)];
	unequalTo   -> [$#, enc_Value(Single, State)]
    end;
enc_propertyParmValues([Low, High], {range, true}, State)->
    %% Exact two values
    [
     ?EQUAL,
     ?LSBRKT,
     enc_Value(Low, State),
     ?COLON,
     enc_Value(High, State),
     ?RSBRKT
    ];
enc_propertyParmValues(Values, {sublist, true}, State)->
    %% sublist (i.e. A AND B AND ...)
    [
     ?EQUAL,
     ?LSBRKT_INDENT(State),
     enc_list([{Values, fun enc_Value/2}], ?INC_INDENT(State)),
     ?RSBRKT_INDENT(State)
    ];
enc_propertyParmValues(Values, {sublist, false}, State) ->
    %% alternatives (i.e. A OR B OR ...)
    [
     ?EQUAL,
     ?LBRKT_INDENT(State),
     enc_list([{Values, fun enc_Value/2}], ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ];
enc_propertyParmValues(V, EI, _State) ->
    error({invalid_property_parm_values, V, EI}).

enc_TerminationStateDescriptor(Val, State)
  when is_record(Val, 'TerminationStateDescriptor') ->
    [
     ?TerminationStateToken,
     ?LBRKT_INDENT(State),
     enc_list([{Val#'TerminationStateDescriptor'.propertyParms,
		fun enc_PropertyParm/2},
	       {[Val#'TerminationStateDescriptor'.eventBufferControl],
		fun enc_eventBufferControl/2},
	       {[Val#'TerminationStateDescriptor'.serviceState],
		fun enc_serviceState/2}],
	      ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

enc_eventBufferControl(Val, _State) ->
    [

     ?BufferToken,
     ?EQUAL,  
     case Val of
	 off      -> ?OffToken;
	 lockStep -> ?LockStepToken
    end
    ].
    
enc_serviceState({'ServiceState',Val}, State) ->
    enc_serviceState(Val, State);
enc_serviceState(Val, _State) ->
    [
     ?ServiceStatesToken,
     ?EQUAL,
     case Val of
	 test     -> ?TestToken;
	 outOfSvc -> ?OutOfSvcToken;
	 inSvc    -> ?InSvcToken
     end
    ].

enc_MuxDescriptor(Val, State)
  when is_record(Val, 'MuxDescriptor') ->
    [
     ?MuxToken,
     ?EQUAL,
     enc_MuxType(Val#'MuxDescriptor'.muxType, State),
     enc_TerminationIDList(Val#'MuxDescriptor'.termList, State)
    ].

enc_MuxType({'MuxType',Val}, State) ->
    enc_MuxType(Val, State);
enc_MuxType(Val, _State) ->
    case Val of
	h221  -> ?H221Token;
	h223  -> ?H223Token;
	h226  -> ?H226Token;
	v76   -> ?V76Token;
	%% extensionParameter
	nx64k -> ?Nx64kToken  % v2
    end.

enc_StreamID({'StreamID',Val}, State) ->
    enc_StreamID(Val, State);
enc_StreamID(Val, State) ->
    enc_UINT16(Val, State).

enc_EventsDescriptor(#'EventsDescriptor'{requestID = asn1_NOVALUE,
					 eventList = []}, _State) ->
    [
     ?EventsToken
    ];
enc_EventsDescriptor(#'EventsDescriptor'{requestID = RID,
					 eventList = Evs}, State) 
  when (RID =/= asn1_NOVALUE) and (Evs =/= []) ->
    [
     ?EventsToken,
     ?EQUAL,
     enc_RequestID(RID, State),
     ?LBRKT_INDENT(State),
     enc_list([{Evs, fun enc_RequestedEvent/2}], ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ];
enc_EventsDescriptor(#'EventsDescriptor'{requestID = RID,
					 eventList = Evs}, _State) ->
    error({invalid_EventsDescriptor, RID, Evs}).

enc_RequestedEvent(#'RequestedEvent'{pkgdName    = N,
				     streamID    = asn1_NOVALUE,
				     eventAction = asn1_NOVALUE,
				     evParList   = []}, State) ->
    PkgdName = ?META_ENC(event, N),
    [
     enc_PkgdName(PkgdName, State)
    ];
enc_RequestedEvent(#'RequestedEvent'{pkgdName    = N,
				     streamID    = SID,
				     eventAction = EA,
				     evParList   = EPL}, State) ->
    PkgdName = ?META_ENC(event, N),
    [
     enc_PkgdName(PkgdName, State),
     ?LBRKT_INDENT(State),
     enc_list([{[SID], fun enc_eventStream/2},
	       {EPL,   fun enc_eventOther/2} |
	       decompose_requestedActions(EA)],
	      ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

decompose_requestedActions(asn1_NOVALUE) ->
    [];
decompose_requestedActions(
  #'RequestedActions'{keepActive            = asn1_NOVALUE,
		      eventDM               = asn1_NOVALUE,
		      secondEvent           = asn1_NOVALUE,
		      signalsDescriptor     = asn1_NOVALUE,
		      notifyBehaviour       = asn1_NOVALUE,
		      resetEventsDescriptor = asn1_NOVALUE}) ->
    [];

%% 
%% This in the ABNF: 
%% at-most-once each of 
%%       - KeepActiveToken
%%       - notifyBehaviour
%%       - eventDM
%%       - ResetEventsDescriptor
%%       - eventStream
%% at most one of either embedWithSig or embedNoSig but not both
%% KeepActiveToken and embedWithSig must not both be present
%% 

%% embedWithSig
decompose_requestedActions(#'RequestedActions'{keepActive            = KA,
					       eventDM               = EDM,
					       secondEvent           = SE,
					       signalsDescriptor     = SD,
					       notifyBehaviour       = NB,
					       resetEventsDescriptor = RED}) 
  when (KA =/= true) and ((SD =/= asn1_NOVALUE) and (SD =/= [])) ->
    [
     {[EDM],      fun enc_EventDM/2},
     {[{SE, SD}], fun enc_embedWithSig/2},
     {[NB],       fun enc_notifyBehaviour/2},
     {[RED],      fun('NULL', _) -> ?ResetEventsDescriptorToken end}
    ];

%% embedNoSig
decompose_requestedActions(#'RequestedActions'{keepActive            = KA,
					       eventDM               = EDM,
					       secondEvent           = SE,
					       signalsDescriptor     = SD,
					       notifyBehaviour       = NB,
					       resetEventsDescriptor = RED}) 
  when (SD == asn1_NOVALUE) or (SD == []) ->
    [
     {[KA],  fun enc_keepActive/2},
     {[EDM], fun enc_EventDM/2},
     {[SE],  fun enc_embedNoSig/2},
     {[NB],  fun enc_notifyBehaviour/2},
     {[RED], fun('NULL', _) -> ?ResetEventsDescriptorToken end}
    ];

%% Fallback, if everything else failes....
decompose_requestedActions(#'RequestedActions'{keepActive            = KA,
					       eventDM               = EDM,
					       secondEvent           = SE,
					       signalsDescriptor     = SD,
					       notifyBehaviour       = NB,
					       resetEventsDescriptor = RED}) ->
    [
     {[KA],       fun enc_keepActive/2},
     {[EDM],      fun enc_EventDM/2},
     {[{SE, SD}], fun enc_embedWithSig/2},
     {[NB],       fun enc_notifyBehaviour/2},
     {[RED],      fun('NULL', _) -> ?ResetEventsDescriptorToken end}
    ].


enc_embedNoSig(#'SecondEventsDescriptor'{requestID = RID,
					 eventList = Evs}, State) ->
    [
     ?EmbedToken,
     ?LBRKT_INDENT(State),
     enc_embedFirst(RID, Evs, ?INC_INDENT(State)), 
     ?RBRKT_INDENT(State)
    ].

enc_embedWithSig({asn1_NOVALUE, SD}, State) ->
    [
     ?EmbedToken,
     ?LBRKT_INDENT(State),
     enc_SignalsDescriptor(SD, ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ];
enc_embedWithSig({#'SecondEventsDescriptor'{requestID = RID,
					    eventList = Evs}, SD}, State) ->
    [
     ?EmbedToken,
     ?LBRKT_INDENT(State),
     enc_SignalsDescriptor(SD, ?INC_INDENT(State)),
     ?COMMA_INDENT(?INC_INDENT(State)), 
     enc_embedFirst(RID, Evs, ?INC_INDENT(State)), 
     ?RBRKT_INDENT(State)
    ].

enc_keepActive(Val, _State) ->
    case Val of
	true -> [?KeepActiveToken];
	false -> []
    end.
    
enc_EventDM({'EventDM',Val}, State) ->
    enc_EventDM(Val, State);
enc_EventDM({Tag, Val}, State) ->
    case Tag of
	digitMapName ->
	    [
	     ?DigitMapToken,
	     ?EQUAL,
	     enc_DigitMapName(Val, State)
	    ];
	digitMapValue ->
	    [
	     ?DigitMapToken,
	     ?LBRKT_INDENT(State),
	     enc_DigitMapValue(Val, ?INC_INDENT(State)),
	     ?RBRKT_INDENT(State)
	    ];
	_ ->
	    error({invalid_EventDM_tag, Tag})
    end.


enc_embedFirst(RID, Evs, State)
  when (RID =/= asn1_NOVALUE) and (is_list(Evs) and (Evs =/= [])) ->
    %%     d("enc_embedFirst -> entry with"
    %%       "~n   RID: ~p"
    %%       "~n   Evs: ~p", [RID, Evs]),
    [
     ?EventsToken,
     ?EQUAL,
     enc_RequestID(RID, State),
     ?LBRKT_INDENT(State),
     enc_list([{Evs, fun enc_SecondRequestedEvent/2}], ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ];
enc_embedFirst(_RID, _Evs, _State) ->
    %%     d("enc_embedFirst -> entry"),
    [
     ?EventsToken
    ].

enc_notifyBehaviour({notifyImmediate, 'NULL'}, _State) ->
    [?NotifyImmediateToken];
enc_notifyBehaviour({notifyRegulated, Val}, State) ->
    enc_RegulatedEmbeddedDescriptor(Val, State);
enc_notifyBehaviour({neverNotify, 'NULL'}, _State) ->
    [?NeverNotifyToken];
enc_notifyBehaviour({Tag, Val}, _State) ->
    error({invalid_notifyBehaviour, Tag, Val}).

enc_RegulatedEmbeddedDescriptor(
  #'RegulatedEmbeddedDescriptor'{secondEvent       = asn1_NOVALUE,
				 signalsDescriptor = asn1_NOVALUE}, _State) ->
    [
     ?NotifyRegulatedToken
    ];
enc_RegulatedEmbeddedDescriptor(
  #'RegulatedEmbeddedDescriptor'{secondEvent       = SE,
				 signalsDescriptor = asn1_NOVALUE}, State) ->
    [
     ?NotifyRegulatedToken,
     ?LBRKT_INDENT(State),
     enc_embedNoSig(SE, ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ];
enc_RegulatedEmbeddedDescriptor(
  #'RegulatedEmbeddedDescriptor'{secondEvent       = SE,
				 signalsDescriptor = SD}, State) ->
    [
     ?NotifyRegulatedToken,
     ?LBRKT_INDENT(State),
     enc_embedWithSig({SE, SD}, ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ];
enc_RegulatedEmbeddedDescriptor(Val, _State) ->
    error({invalid_RegulatedEmbeddedDescriptor, Val}).

enc_SecondRequestedEvent(#'SecondRequestedEvent'{pkgdName    = N,
						 streamID    = asn1_NOVALUE,
						 eventAction = asn1_NOVALUE,
						 evParList   = []}, State) ->
    PkgdName = ?META_ENC(event, N),
    [
     enc_PkgdName(PkgdName, State)
    ];
enc_SecondRequestedEvent(#'SecondRequestedEvent'{pkgdName    = N,
						 streamID    = SID,
						 eventAction = EA,
						 evParList   = EPL}, State) ->
    PkgdName = ?META_ENC(event, N),
    [
     enc_PkgdName(PkgdName, State),
     ?LBRKT_INDENT(State),
     enc_list([{[SID], fun enc_eventStream/2},
	       {EPL,   fun enc_eventOther/2} |
	       decompose_secondRequestedActions(EA)],
	      ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

%% 
%% This in the ABNF: 
%% at-most-once each of 
%%       - KeepActiveToken
%%       - notifyBehaviour
%%       - eventDM
%%       - ResetEventsDescriptor
%%       - eventStream
%% KeepActiveToken and embedWithSig must not both be present
%% 
decompose_secondRequestedActions(asn1_NOVALUE) ->
    [];
decompose_secondRequestedActions(
  #'SecondRequestedActions'{keepActive            = asn1_NOVALUE,
			    eventDM               = asn1_NOVALUE,
			    signalsDescriptor     = asn1_NOVALUE,
			    notifyBehaviour       = asn1_NOVALUE,
			    resetEventsDescriptor = asn1_NOVALUE}) ->
    [];

decompose_secondRequestedActions(
  #'SecondRequestedActions'{keepActive            = KA,
			    eventDM               = EDM,
			    signalsDescriptor     = SD,
			    notifyBehaviour       = NB,
			    resetEventsDescriptor = RED}) 
  when (KA =/= true) and ((SD =/= asn1_NOVALUE) and (SD =/= [])) ->
    [
     {[EDM], fun enc_EventDM/2},
     {[SD],  fun enc_embedSig/2},
     {[NB],  fun enc_notifyBehaviour/2},
     {[RED], fun('NULL', _) -> ?ResetEventsDescriptorToken end}
    ];
decompose_secondRequestedActions(
  #'SecondRequestedActions'{keepActive            = KA,
			    eventDM               = EDM,
			    signalsDescriptor     = SD,
			    notifyBehaviour       = NB,
			    resetEventsDescriptor = RED}) 
  when (SD == asn1_NOVALUE) or (SD == []) ->
    [
     {[KA],  fun enc_keepActive/2},
     {[EDM], fun enc_EventDM/2},
     {[NB],  fun enc_notifyBehaviour/2},
     {[RED], fun('NULL', _) -> ?ResetEventsDescriptorToken end}
    ];
decompose_secondRequestedActions(SRA) ->
    error({invalid_SecondRequestedActions, SRA}).

enc_embedSig(Val, State) ->
    [
     ?EmbedToken,
     ?LBRKT_INDENT(State),
     enc_SignalsDescriptor(Val, ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].
    
enc_EventBufferDescriptor({'EventBufferDescriptor',Val}, State) ->
    enc_EventBufferDescriptor(Val, State);
enc_EventBufferDescriptor([], _State) ->
    [
     ?EventBufferToken
    ];
enc_EventBufferDescriptor(EvSpecs, State) 
  when is_list(EvSpecs) andalso (length(EvSpecs) >= 1) ->
    [
     ?EventBufferToken,
     ?LBRKT_INDENT(State),
     enc_eventSpecs(EvSpecs, ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)   
    ];
enc_EventBufferDescriptor(EvSpecs, _State) ->
    error({bad_eventSpecs, EvSpecs}).

enc_eventSpecs([Mand | Opt], State) ->
    [enc_eventSpec(Mand, State),
     [[?COMMA_INDENT(State), enc_eventSpec(Val, State)] || Val <- Opt]].

enc_eventSpec(#'EventSpec'{eventName    = N,
			   streamID     = asn1_NOVALUE,
			   eventParList = []}, State) ->
    [
     enc_EventName(N, State)
    ];
enc_eventSpec(#'EventSpec'{eventName    = N,
			   streamID     = SID,
			   eventParList = EPL}, State) ->
    [
     enc_EventName(N, State),
     ?LBRKT_INDENT(State),
     enc_list([{[SID], fun enc_eventStream/2}, {EPL, fun enc_eventOther/2}],
	      ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

enc_SignalsDescriptor({'SignalsDescriptor',Val}, State) ->
    enc_SignalsDescriptor(Val, State);
enc_SignalsDescriptor([], _State) ->
    [
     ?SignalsToken
    ];
enc_SignalsDescriptor(SigRequests, State) when is_list(SigRequests) ->
    [
     ?SignalsToken,
     ?LBRKT_INDENT(State),
     enc_list([{SigRequests, fun enc_SignalRequest/2}], ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

enc_SignalRequest({'SignalRequest',Val}, State) ->
    enc_SignalRequest(Val, State);
enc_SignalRequest({Tag, Val}, State) ->
    case Tag of
	signal ->
	    enc_Signal(Val, State);
	seqSigList ->
	    enc_SeqSigList(Val, State);
	_ ->
	    error({invalid_SignalRequest_tag, Tag})
    end.


enc_SeqSigList(Val, State)
  when is_record(Val, 'SeqSigList') ->
    [
     ?SignalListToken,
     ?EQUAL,
     enc_UINT16(Val#'SeqSigList'.id, State),
     ?LBRKT_INDENT(State),
     enc_list([{Val#'SeqSigList'.signalList, fun enc_Signal/2}],
	      ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

enc_Signal(#'Signal'{signalName       = SN, 
		     streamID         = SID, 
		     sigType          = ST, 
		     duration         = Du, 
		     notifyCompletion = NC, 
		     keepActive       = KA, 
		     sigParList       = SPL, 
		     direction        = Di, 
		     requestID        = RID,
		     intersigDelay    = ISD}, State) ->
    [
     enc_SignalName(SN, State),
     enc_opt_brackets(
       enc_list([{[SID], fun enc_sigStream/2},
		 {[ST],  fun enc_sigSignalType/2},
		 {[Du],  fun enc_sigDuration/2},
		 {[NC],  fun enc_notifyCompletion/2},
		 {[KA],  fun enc_keepActive/2},
		 {SPL,   fun enc_sigOther/2},
		 {[Di],  fun enc_SignalDirection/2},
		 {[RID], fun enc_sigRequestID/2},
		 {[ISD], fun enc_sigIntsigDelay/2}],
		?INC_INDENT(State)),
      State)
    ].

enc_sigStream(Val, State) ->
    [
     ?StreamToken,
     ?EQUAL, 
     enc_StreamID(Val, State)
    ].

enc_sigSignalType(Val, State) ->
    [
     ?SignalTypeToken,
     ?EQUAL,
     enc_SignalType(Val, State)
    ].

enc_sigDuration(Val, State) ->
    [
     ?DurationToken,
     ?EQUAL,
     enc_UINT16(Val, State)
    ].

enc_notifyCompletion(List, State) when is_list(List) ->
    [
     ?NotifyCompletionToken,
     ?EQUAL,
     ?LBRKT_INDENT(State),
     enc_list([{List, fun enc_notifyCompletionItem/2}], ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

enc_notifyCompletionItem(Val, _State) ->
    case Val of
	onTimeOut                   -> ?TimeOutToken;
        onInterruptByEvent          -> ?InterruptByEventToken;
        onInterruptByNewSignalDescr -> ?InterruptByNewSignalsDescrToken;
        otherReason                 -> ?OtherReasonToken;
        onIteration                 -> ?IterationToken
    end.

enc_SignalType({'SignalType',Val}, State) ->
    enc_SignalType(Val, State);
enc_SignalType(Val, _State) ->
    case Val of
	brief ->   ?BriefToken;
	onOff ->   ?OnOffToken;
	timeOut -> ?TimeOutToken
    end.

enc_SignalName({'SignalName',Val}, State)->
    enc_SignalName(Val, State);
enc_SignalName(Val, State) ->
    PkgdName = ?META_ENC(signal, Val),
    enc_PkgdName(PkgdName, State).

enc_sigOther(Val, State)
  when is_record(Val, 'SigParameter') ->
    [
     enc_Name(Val#'SigParameter'.sigParameterName, State),
     enc_propertyParmValues(Val#'SigParameter'.value,
			    Val#'SigParameter'.extraInfo,
			    State)
    ].

enc_SignalDirection({'SignalDirection', Val}, State) ->
    enc_SignalDirection(Val, State);
enc_SignalDirection(Val, _State) ->
    [
     ?DirectionToken,
     ?EQUAL,
     case Val of
	 internal -> ?InternalToken;
	 external -> ?ExternalToken;
	 both     -> ?BothToken
     end
    ].

enc_sigRequestID(Val, State) ->
    [
     ?RequestIDToken,
     ?EQUAL,
     enc_RequestID(Val, State)
    ].

enc_RequestID({'RequestID',Val}, State) ->
    enc_RequestID(Val, State);
enc_RequestID(Val, _State) when Val == ?megaco_all_request_id ->
    "*";
enc_RequestID(Val, State) ->
    enc_UINT32(Val, State).

enc_sigIntsigDelay(Val, State) ->
    [
     ?IntsigDelayToken,
     ?EQUAL,
     enc_UINT16(Val, State)
    ].

enc_ModemDescriptor(MD, _State) ->
    error({deprecated, MD}).

%% Corr1:
%% As of corr 1 ModemDescriptor has been deprecated.
%% 7.1.2: ...shall not be included as part of a transmitted content and,
%%        if received, shall either be ignored or processed at the option
%%        of the implementation. ...
%% enc_ModemDescriptor(#'ModemDescriptor'{mtl = [Val],
%% 				       mpl = [],
%% 				       nonStandardData = asn1_NOVALUE},
%% 		    State) ->
%%     [
%%      ?ModemToken,
%%      ?EQUAL,
%%      enc_ModemType(Val, State)
%%     ];
%% enc_ModemDescriptor(Val, State)
%%   when record(Val, 'ModemDescriptor') ->
%%     [
%%      ?ModemToken,
%%      ?LSBRKT,
%%      enc_list([{Val#'ModemDescriptor'.mtl, fun enc_ModemType/2}], State),
%%      ?RSBRKT,
%%      enc_opt_brackets(
%%        enc_list([{Val#'ModemDescriptor'.mpl, fun enc_PropertyParm/2}],
%% 		?INC_INDENT(State)),
%%        State)
%%      %% BUGBUG: Is PropertyParm == NAME parmValue?
%%     ].

%% enc_ModemDescriptor(Val, State)
%%   when record(Val, 'ModemDescriptor') ->
%%     [
%%      ?ModemToken,
%%      %% BUGBUG: Does never generate: EQUAL modemType
%%      ?LSBRKT,
%%      enc_list([{Val#'ModemDescriptor'.mtl, fun enc_ModemType/2}], State),
%%      ?RSBRKT,
%%      enc_opt_brackets(
%%        enc_list([{Val#'ModemDescriptor'.mpl, fun enc_PropertyParm/2}],
%% 		?INC_INDENT(State)),
%%        State)
%%      %% BUGBUG: Is PropertyParm == NAME parmValue?
%%     ].

%% Corr1: See ModemDescriptor above
%% enc_ModemType({'ModemType',Val}, State)->
%%     enc_ModemType(Val, State);
%% enc_ModemType(Val, _State) ->
%%     %% BUGBUG: Does not handle extensionParameter
%%     case Val of
%%         v18    	  -> ?V18Token;
%%         v22    	  -> ?V22Token;
%%         v22bis 	  -> ?V22bisToken;
%%         v32    	  -> ?V32Token;
%%         v32bis 	  -> ?V32bisToken;
%%         v34    	  -> ?V34Token;
%%         v90    	  -> ?V90Token;
%%         v91    	  -> ?V91Token;
%%         synchISDN -> ?SynchISDNToken
%%     end.

enc_DigitMapDescriptor(#'DigitMapDescriptor'{digitMapName  = asn1_NOVALUE,
					     digitMapValue = Value}, 
		       State) 
  when (Value =/= asn1_NOVALUE) ->
    [
     ?DigitMapToken,
     ?EQUAL,
     ?LBRKT_INDENT(State),
     enc_DigitMapValue(Value, ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ];
enc_DigitMapDescriptor(#'DigitMapDescriptor'{digitMapName  = Name, 
					     digitMapValue = asn1_NOVALUE}, 
		       State) 
  when (Name =/= asn1_NOVALUE) ->
    [
     ?DigitMapToken,
     ?EQUAL,
     enc_DigitMapName(Name, State)
    ];
enc_DigitMapDescriptor(#'DigitMapDescriptor'{digitMapName  = Name,
					     digitMapValue = Value}, 
		       State) 
  when (Name =/= asn1_NOVALUE) andalso (Value =/= asn1_NOVALUE) ->
    [
     ?DigitMapToken,
     ?EQUAL,
     enc_DigitMapName(Name, State),
     ?LBRKT_INDENT(State),
     enc_DigitMapValue(Value, ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ];
enc_DigitMapDescriptor(BadVal, _State) ->
    error({invalid_DigitMapDescriptor, BadVal}).

enc_DigitMapName({'DigitMapName',Val}, State) ->
    enc_DigitMapName(Val, State);
enc_DigitMapName(Val, State) ->
    enc_Name(Val, State).

enc_DigitMapValue(Val, State)
  when is_record(Val, 'DigitMapValue') ->
    [
     enc_timer(Val#'DigitMapValue'.startTimer,     $T, State),
     enc_timer(Val#'DigitMapValue'.shortTimer,     $S, State),
     enc_timer(Val#'DigitMapValue'.longTimer,      $L, State),
     enc_timer(Val#'DigitMapValue'.durationTimer,  $Z, State),
     %% BUGBUG: digitMapBody not handled at all
     enc_STRING(Val#'DigitMapValue'.digitMapBody, State, 0, infinity)
    ].

enc_timer(asn1_NOVALUE, _Prefix, _State) ->
    [];
enc_timer(Timer, Prefix, State) ->
    [
     Prefix,
     ?COLON,
     enc_DIGIT(Timer, State, 0, 99),
     ?COMMA_INDENT(State)
    ].

enc_ServiceChangeParm(Val, State)
  when is_record(Val, 'ServiceChangeParm') ->
    [
     ?ServicesToken,
     ?LBRKT_INDENT(State),
     enc_list([{[Val#'ServiceChangeParm'.serviceChangeMethod],
		fun enc_ServiceChangeMethod/2},
	       {[Val#'ServiceChangeParm'.serviceChangeAddress],
		fun enc_ServiceChangeAddress/2},
	       {[Val#'ServiceChangeParm'.serviceChangeVersion],
		fun enc_serviceChangeVersion/2},
	       {[Val#'ServiceChangeParm'.serviceChangeProfile],
		fun enc_ServiceChangeProfile/2},
	       {[{reason, Val#'ServiceChangeParm'.serviceChangeReason}],
		fun enc_serviceChangeReason/2},
	       {[Val#'ServiceChangeParm'.serviceChangeDelay],
		fun enc_serviceChangeDelay/2},
	       {[Val#'ServiceChangeParm'.serviceChangeMgcId],
		fun enc_serviceChangeMgcId/2},
	       {[Val#'ServiceChangeParm'.timeStamp],
		fun enc_TimeNotation/2},
	       {Val#'ServiceChangeParm'.serviceChangeInfo,
		fun enc_AuditDescriptor/2},
	       {[Val#'ServiceChangeParm'.serviceChangeIncompleteFlag],
		fun('NULL', _) -> ?ServiceChangeIncompleteToken end}],
	      ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].


enc_ServiceChangeMethod({'ServiceChangeMethod',Val}, State) ->
    enc_ServiceChangeMethod(Val, State);
enc_ServiceChangeMethod(Val, _State) ->
    [
     ?MethodToken,
     ?EQUAL,
     case Val of
	 failover      -> ?FailoverToken;
	 forced        -> ?ForcedToken;
	 graceful      -> ?GracefulToken;
	 restart       -> ?RestartToken;
	 disconnected  -> ?DisconnectedToken;
	 handOff       -> ?HandOffToken;
	 _ ->
	     error({invalid_ServiceChangeMethod, Val})
			 
     end
    ].

enc_ServiceChangeAddress({'ServiceChangeAddress',Val}, State) ->
    enc_ServiceChangeAddress(Val, State);
enc_ServiceChangeAddress({Tag, Val}, State) ->
    [
     ?ServiceChangeAddressToken,
     ?EQUAL,
     case Tag of
	 portNumber ->
	     enc_portNumber(Val, State);
	 ip4Address ->
	     enc_IP4Address(Val, State);
	 ip6Address ->
	     enc_IP6Address(Val, State);
	 domainName ->
	     enc_DomainName(Val, State);
	 deviceName ->
	     enc_PathName(Val, State);
	 mtpAddress ->
	     enc_mtpAddress(Val, State);
	 _ ->
	     error({invalid_ServiceChangeAddress_tag, Tag})
     end
    ].

enc_serviceChangeVersion(Val, State) ->
    [
     ?VersionToken,
     ?EQUAL,
     enc_version(Val, State)
    ].

enc_ServiceChangeProfile(#'ServiceChangeProfile'{profileName = Name,
						 version     = Version}, 
			 State) ->
    [
     ?ProfileToken,
     ?EQUAL,
     enc_Name(Name, State),
     ?SLASH,
     enc_version(Version, State)
    ].

enc_serviceChangeReason({reason, Val}, State) ->
    case Val of
	asn1_NOVALUE ->
	    [];
	[List] when is_list(List) ->
	    [
	     ?ReasonToken,
	     ?EQUAL,
	     enc_QUOTED_STRING(List,State) % OTP-4632 enc_Value(List, State)
	    ]
    end.

enc_serviceChangeDelay(Val, State) ->
    [
     ?DelayToken,
     ?EQUAL,
     enc_UINT32(Val, State)
    ].

enc_serviceChangeMgcId(Val, State) ->
    [
     ?MgcIdToken,
     ?EQUAL,
     enc_MId(Val, State)
    ].

enc_portNumber(Val, State) when is_integer(Val) and (Val >= 0) ->
    enc_UINT16(Val, State).
     
enc_ServiceChangeResParm(Val, State)
  when is_record(Val, 'ServiceChangeResParm') ->
    enc_list([{[Val#'ServiceChangeResParm'.serviceChangeAddress],
	       fun enc_ServiceChangeAddress/2},
	      {[Val#'ServiceChangeResParm'.serviceChangeVersion],
	       fun enc_serviceChangeVersion/2},
	      {[Val#'ServiceChangeResParm'.serviceChangeProfile],
	       fun enc_ServiceChangeProfile/2},
	      {[Val#'ServiceChangeResParm'.serviceChangeMgcId],
	       fun enc_serviceChangeMgcId/2},
	      {[Val#'ServiceChangeResParm'.timeStamp],
	       fun enc_TimeNotation/2}],
	     State).

enc_PackagesDescriptor({'PackagesDescriptor',Val}, State) ->
    enc_PackagesDescriptor(Val, State);
enc_PackagesDescriptor(Val, State) ->
    [
     ?PackagesToken,
     ?LBRKT_INDENT(State),
     enc_list([{Val, fun enc_PackagesItem/2}], ?INC_INDENT(State)),  
     ?RBRKT_INDENT(State)    
    ].

enc_PackagesItem(Val, State)
  when is_record(Val, 'PackagesItem') ->
    PkgdName = ?META_ENC(package, Val#'PackagesItem'.packageName),
    [
     enc_Name(PkgdName, State),
     "-",
     enc_UINT16(Val#'PackagesItem'.packageVersion, State)
    ].

enc_StatisticsDescriptor({'StatisticsDescriptor',Val}, State) ->
    enc_StatisticsDescriptor(Val, State);
enc_StatisticsDescriptor(List, State)
  when is_list(List) andalso (List =/= []) ->
    [
     ?StatsToken,
     ?LBRKT_INDENT(State),
     enc_list([{List, fun enc_StatisticsParameter/2}], ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ];
enc_StatisticsDescriptor([], _State) ->
    [
     ?StatsToken
    ].


enc_StatisticsParameter(Val, State)
  when is_record(Val, 'StatisticsParameter') ->
    PkgdName = ?META_ENC(statistics, Val#'StatisticsParameter'.statName),
    case Val#'StatisticsParameter'.statValue of
	asn1_NOVALUE ->
	    [
	     enc_PkgdName(PkgdName, State)
	    ];
	[StatVal] when is_list(StatVal) ->
	    [
	     enc_PkgdName(PkgdName, State),
	     ?EQUAL,
	     enc_Value(StatVal, State)
	    ]
    end.

enc_TimeNotation(Val, State)
  when is_record(Val, 'TimeNotation') ->
    [
     enc_STRING(Val#'TimeNotation'.date, State, 8, 8), % "yyyymmdd"
     "T",
     enc_STRING(Val#'TimeNotation'.time, State, 8, 8)  % "hhmmssss"
    ].

%% BUGBUG: Does not verify that string must contain at least one char
%% BUGBUG: This violation of the is required in order to comply with
%% BUGBUG: the dd/ce ds parameter that may possibly be empty.
enc_Value({'Value',Val}, State) ->
    enc_Value(Val, State);
enc_Value(String, _State) ->
    case quoted_string_count(String, 0, true, false) of
	{_, 0, _} ->
	    [?DQUOTE, String, ?DQUOTE];
	{false, _, _} ->
	    [?DQUOTE, String, ?DQUOTE];
	{true, _, _} ->
	    [String]
    end.
 
quoted_string_count([?DoubleQuoteToken | T], 0 = Count, _IsSafe, _MaybeQuoted) ->
    %% Already a quoted string. Make sure it ends
    quoted_string_count(T, Count + 1, true, true);
quoted_string_count([?DoubleQuoteToken], Count, IsSafe, true = MaybeQuoted) ->
    %% An explicitly quoted string
    {IsSafe, Count, MaybeQuoted};
quoted_string_count([H | T], Count, IsSafe, MaybeQuoted) ->
    case ?classify_char(H) of
	safe_char_upper -> quoted_string_count(T, Count + 1, IsSafe, MaybeQuoted);
	safe_char       -> quoted_string_count(T, Count + 1, IsSafe, MaybeQuoted);
	rest_char       -> quoted_string_count(T, Count + 1, false, MaybeQuoted);
	white_space     -> quoted_string_count(T, Count + 1, false, MaybeQuoted);
	_               -> error({illegal_char, H})
    end;
quoted_string_count([], _Count, _IsSafe, true = _MaybeQuoted) ->
    error({illegal_char, ?DoubleQuoteToken});
quoted_string_count([], Count, IsSafe, MaybeQuoted) ->
    {IsSafe, Count, MaybeQuoted}.

enc_DigitString(String, _State) when is_list(String) ->
    [?DQUOTE, String, ?DQUOTE].

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Encode an octet string, escape } by \ if necessary 
enc_OCTET_STRING(List, State, Min, Max) ->
    do_enc_OCTET_STRING(List, State, Min, Max, 0).
    
do_enc_OCTET_STRING([H | T], State, Min, Max, Count) ->
    case H of
	$} ->
	    [$\\, H | do_enc_OCTET_STRING(T, State, Min, Max, Count + 1)];
	_ ->
	    [H | do_enc_OCTET_STRING(T, State, Min, Max, Count + 1)]
    end;
do_enc_OCTET_STRING([], _State, Min, Max, Count) ->
    verify_count(Count, Min, Max),
    [].

enc_QUOTED_STRING(String, _State) when is_list(String) ->
    case quoted_string_count(String, 0, true, false) of
	{_IsSafe, Count, false = _QuotedString} ->
	    verify_count(Count, 1, infinity),
	    [?DQUOTE, String, ?DQUOTE];
	{_IsSafe, Count, true = _QuotedString} ->
	    verify_count(Count, 3, infinity), % quotes not included in the count
	    [String]
    end.

%% The internal format of hex digits is a list of octets
%% Min and Max means #hexDigits
%% Leading zeros are prepended in order to fulfill Min
enc_HEXDIG(Octets, State, Min, Max) when is_list(Octets) ->
    do_enc_HEXDIG(Octets, State, Min, Max, 0, []).

do_enc_HEXDIG([Octet | Rest], State, Min, Max, Count, Acc) 
  when (Octet >= 0) and (Octet =< 255)  ->
    Hex = hex(Octet), % OTP-4921
    if
	Octet =< 15 ->
	    Acc2 = [[$0|Hex]|Acc],  % OTP-4921
	    do_enc_HEXDIG(Rest, State, Min, Max, Count + 2, ["0" | Acc2]);
	true -> 
	    Acc2 = [Hex|Acc], % OTP-4921    
	    do_enc_HEXDIG(Rest, State, Min, Max, Count + 2, Acc2)
    end;
do_enc_HEXDIG([], State, Min, Max, Count, Acc)
  when is_integer(Min) and (Count < Min) ->
    do_enc_HEXDIG([0], State, Min, Max, Count, Acc);
do_enc_HEXDIG([], _State, Min, Max, Count, Acc) -> %% OTP-4710
    verify_count(Count, Min, Max),
    lists:reverse(Acc).

enc_DIGIT(Val, State, Min, Max) ->
    enc_integer(Val, State, Min, Max).

enc_STRING(String, _State, Min, Max) when is_list(String) ->
    verify_count(length(String), Min, Max),
    String.

enc_UINT16(Val, State) ->
    enc_integer(Val, State, 0, 65535).

enc_UINT32(Val, State) ->
    enc_integer(Val, State, 0, 4294967295).

enc_integer(Val, _State, Min, Max) ->
    verify_count(Val, Min, Max),
    integer_to_list(Val).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Encodes a list of elements with separator tokens between
%% the elements. Optional asn1_NOVALUE values are ignored.

enc_list(List, State) ->
    enc_list(List, State, fun(_S) -> ?COMMA_INDENT(_S) end, false).

-dialyzer({nowarn_function, enc_list/4}). % Future compat
enc_list([], _State, _SepEncoder, _NeedsSep) ->
    [];
enc_list([{Elems, ElemEncoder} | Tail], State, SepEncoder, NeedsSep) ->
    case do_enc_list(Elems, State, ElemEncoder, SepEncoder, NeedsSep) of
	[] ->
	    enc_list(Tail, State, SepEncoder, NeedsSep);
	List ->
	    [List,
	     enc_list(Tail, State, SepEncoder, true)]
    end;
enc_list(A, B, C, D) ->
    error({invalid_list, A, B, C, D}).

do_enc_list(asn1_NOVALUE, _State, _ElemEncoder, _SepEncoder, _NeedsSep) ->
    [];
do_enc_list([], _State, _ElemEncoder, _SepEncoder, _NeedsSep) ->
    [];
do_enc_list([asn1_NOVALUE | T], State, ElemEncoder, SepEncoder, NeedsSep) ->
    do_enc_list(T, State, ElemEncoder, SepEncoder, NeedsSep);
do_enc_list([H | T], State, ElemEncoder, SepEncoder, NeedsSep)
  when is_function(ElemEncoder) and is_function(SepEncoder) ->
    case ElemEncoder(H, State) of
	[] ->
	    do_enc_list(T, State, ElemEncoder, SepEncoder, NeedsSep);
	List when NeedsSep == true ->
	    [SepEncoder(State),
	     List, do_enc_list(T, State, ElemEncoder, SepEncoder, true)];
	List when NeedsSep == false ->
	    [List,
	     do_enc_list(T, State, ElemEncoder, SepEncoder, true)]
    end.

%% Add brackets if list is non-empty
enc_opt_brackets([], _State) ->
    [];
enc_opt_brackets(List, _State) when is_list(List) ->
    [?LBRKT_INDENT(_State), List, ?RBRKT_INDENT(_State)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Int -> list of hex chars
hex(Int) ->
    hexi(get_lo_bits(Int, 4), []).

hexi({0, Lo}, Ack) ->
    [hex4(Lo) | Ack];
hexi({Hi, Lo} , Ack) ->
    hexi(get_lo_bits(Hi, 4), [hex4(Lo) | Ack]).

hex4(Int) when Int < 10 ->
    Int + $0;
hex4(Int) ->
    ($A - 10) + Int.

get_lo_bits(Int, Size) ->
    Lo = Int band ones_mask(Size),
    Hi = Int bsr Size,
    {Hi, Lo}.

ones_mask(Ones) ->
    (1 bsl Ones) - 1.

%% Verify that Count is within the range of Min and Max
verify_count(Count, Min, Max) ->
    if
	is_integer(Count) ->
	    if
		is_integer(Min) andalso (Count >= Min) ->
		    if
			is_integer(Max) andalso (Count =< Max) ->
			    Count;
			Max =:= infinity ->
			    Count;
			true ->
			    error({count_too_large, Count, Max})
		    end;
		true ->
		    error({count_too_small, Count, Min})
	    end;
	true ->
	    error({count_not_an_integer, Count})
    end.




%% -------------------------------------------------------------------

%% d(F) ->
%%     d(F,[]).
%% d(F, A) ->
%%     d(get(dbg), F, A).

%% d(true, F, A) ->
%%     io:format("~p:" ++ F ++"~n", [?MODULE | A]);
%% d(_, _, _) ->
%%     ok.


