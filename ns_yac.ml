exception Error

type token = 
  | TRUE
  | SUB
  | SEMI
  | RPAREN
  | REGEX of (
# 21 "ns_yac.mly"
       (string)
# 12 "ns_yac.ml"
)
  | RBRACK
  | OR
  | NT of (
# 19 "ns_yac.mly"
       (string)
# 19 "ns_yac.ml"
)
  | NOT
  | MUL
  | LT
  | LPAREN
  | LE
  | LBRACK
  | INT of (
# 22 "ns_yac.mly"
       (int)
# 30 "ns_yac.ml"
)
  | GT
  | GE
  | FALSE
  | EQUAL
  | EOF
  | DIV
  | COMMA
  | COLONEQUALS
  | BRACK of (
# 20 "ns_yac.mly"
       (string)
# 43 "ns_yac.ml"
)
  | BECOMES
  | AND
  | ADD

and _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  mutable _menhir_token: token;
  mutable _menhir_startp: Lexing.position;
  mutable _menhir_endp: Lexing.position;
  mutable _menhir_shifted: int
}

and _menhir_state = 
  | MenhirState92
  | MenhirState87
  | MenhirState82
  | MenhirState81
  | MenhirState80
  | MenhirState78
  | MenhirState76
  | MenhirState71
  | MenhirState69
  | MenhirState62
  | MenhirState56
  | MenhirState54
  | MenhirState52
  | MenhirState50
  | MenhirState48
  | MenhirState46
  | MenhirState43
  | MenhirState39
  | MenhirState38
  | MenhirState36
  | MenhirState32
  | MenhirState27
  | MenhirState21
  | MenhirState18
  | MenhirState16
  | MenhirState14
  | MenhirState11
  | MenhirState10
  | MenhirState8
  | MenhirState1


# 1 "ns_yac.mly"
  
  open Printf
  open Lexing
  open Ns_types
  open Ns_types.ParsedPCFG

  let is_act = ref false
  let cur_var = ref ""

  let check_var v =
    if not !is_act && !cur_var = "" then cur_var := v else
    if v <> !cur_var then failwith "ERROR(line %d): Non-unary expression"

  module VM = Ns_types.ParsedPCFG.VarMap
  let to_map l = List.fold_left (fun acc (v,e) -> VM.add v e acc) VM.empty l


# 109 "ns_yac.ml"
let _eRR =
  Error

let rec _menhir_goto_list_term_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_term_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv495 * _menhir_state * 'tv_term) * _menhir_state * 'tv_list_term_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv493 * _menhir_state * 'tv_term) * _menhir_state * 'tv_list_term_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : 'tv_list_term_ = 
# 116 "standard.mly"
    ( x :: xs )
# 126 "ns_yac.ml"
         in
        _menhir_goto_list_term_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv494)) : 'freshtv496)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv505 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 134 "ns_yac.ml"
        )) * _menhir_state * 'tv_option_BRACK_) * 'tv_option_INT_) * _menhir_state * 'tv_list_term_) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv503 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 142 "ns_yac.ml"
        )) * _menhir_state * 'tv_option_BRACK_) * 'tv_option_INT_) * _menhir_state * 'tv_list_term_) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv499 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 151 "ns_yac.ml"
            )) * _menhir_state * 'tv_option_BRACK_) * 'tv_option_INT_) * _menhir_state * 'tv_list_term_) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv497 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 158 "ns_yac.ml"
            )) * _menhir_state * 'tv_option_BRACK_) * 'tv_option_INT_) * _menhir_state * 'tv_list_term_) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s, _1), _, _2), _3), _, _5) = _menhir_stack in
            let _v : 'tv_ns_rule = 
# 43 "ns_yac.mly"
                                      ( (_1,_2,_3,_5) )
# 164 "ns_yac.ml"
             in
            _menhir_goto_ns_rule _menhir_env _menhir_stack _menhir_s _v) : 'freshtv498)) : 'freshtv500)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv501 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 174 "ns_yac.ml"
            )) * _menhir_state * 'tv_option_BRACK_) * 'tv_option_INT_) * _menhir_state * 'tv_list_term_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv502)) : 'freshtv504)) : 'freshtv506)
    | _ ->
        _menhir_fail ()

and _menhir_reduce15 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_term_ = 
# 114 "standard.mly"
    ( [] )
# 186 "ns_yac.ml"
     in
    _menhir_goto_list_term_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_nonempty_list_term_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_nonempty_list_term_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv481 * _menhir_state * 'tv_term) * _menhir_state * 'tv_nonempty_list_term_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv479 * _menhir_state * 'tv_term) * _menhir_state * 'tv_nonempty_list_term_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : 'tv_nonempty_list_term_ = 
# 126 "standard.mly"
    ( x :: xs )
# 203 "ns_yac.ml"
         in
        _menhir_goto_nonempty_list_term_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv480)) : 'freshtv482)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv491 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 211 "ns_yac.ml"
        )) * _menhir_state * 'tv_option_BRACK_) * 'tv_option_INT_) * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 215 "ns_yac.ml"
        )) * _menhir_state) * _menhir_state * 'tv_nonempty_list_term_) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv489 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 223 "ns_yac.ml"
        )) * _menhir_state * 'tv_option_BRACK_) * 'tv_option_INT_) * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 227 "ns_yac.ml"
        )) * _menhir_state) * _menhir_state * 'tv_nonempty_list_term_) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv485 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 236 "ns_yac.ml"
            )) * _menhir_state * 'tv_option_BRACK_) * 'tv_option_INT_) * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 240 "ns_yac.ml"
            )) * _menhir_state) * _menhir_state * 'tv_nonempty_list_term_) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv483 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 247 "ns_yac.ml"
            )) * _menhir_state) * _menhir_state * 'tv_nonempty_list_term_) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, _1), _), _, _3) = _menhir_stack in
            let _v : 'tv_term = 
# 49 "ns_yac.mly"
                           ( (Capture _3, Some _1) )
# 253 "ns_yac.ml"
             in
            _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v) : 'freshtv484)) : 'freshtv486)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv487 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 263 "ns_yac.ml"
            )) * _menhir_state * 'tv_option_BRACK_) * 'tv_option_INT_) * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 267 "ns_yac.ml"
            )) * _menhir_state) * _menhir_state * 'tv_nonempty_list_term_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv488)) : 'freshtv490)) : 'freshtv492)
    | _ ->
        _menhir_fail ()

and _menhir_run78 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 21 "ns_yac.mly"
       (string)
# 277 "ns_yac.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv477 * _menhir_state * (
# 21 "ns_yac.mly"
       (string)
# 286 "ns_yac.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | BRACK _v ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | NT _ | REGEX _ | RPAREN | SEMI ->
        _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78) : 'freshtv478)

and _menhir_run80 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 19 "ns_yac.mly"
       (string)
# 302 "ns_yac.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv475 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 311 "ns_yac.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | BRACK _v ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv473 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 322 "ns_yac.ml"
        )) * _menhir_state * 'tv_option_BRACK_) * 'tv_option_INT_) * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 326 "ns_yac.ml"
        )) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState80 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv471 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 335 "ns_yac.ml"
        )) * _menhir_state * 'tv_option_BRACK_) * 'tv_option_INT_) * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 339 "ns_yac.ml"
        )) * _menhir_state) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | NT _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
        | REGEX _v ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81) : 'freshtv472)) : 'freshtv474)
    | NT _ | REGEX _ | RPAREN | SEMI ->
        _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80) : 'freshtv476)

and _menhir_goto_nonempty_list_ns_rule_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_nonempty_list_ns_rule_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState92 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv453 * _menhir_state * 'tv_ns_rule) * _menhir_state * 'tv_nonempty_list_ns_rule_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv451 * _menhir_state * 'tv_ns_rule) * _menhir_state * 'tv_nonempty_list_ns_rule_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : 'tv_nonempty_list_ns_rule_ = 
# 126 "standard.mly"
    ( x :: xs )
# 371 "ns_yac.ml"
         in
        _menhir_goto_nonempty_list_ns_rule_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv452)) : 'freshtv454)
    | MenhirState69 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv469 * _menhir_state * 'tv_nonempty_list_ns_rule_) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv467 * _menhir_state * 'tv_nonempty_list_ns_rule_) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv463 * _menhir_state * 'tv_nonempty_list_ns_rule_) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv461 * _menhir_state * 'tv_nonempty_list_ns_rule_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, spec) = _menhir_stack in
            let _v : (
# 35 "ns_yac.mly"
      (Ns_types.str_spec_t)
# 392 "ns_yac.ml"
            ) = 
# 40 "ns_yac.mly"
                          ( spec )
# 396 "ns_yac.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv459) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 35 "ns_yac.mly"
      (Ns_types.str_spec_t)
# 404 "ns_yac.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv457) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 35 "ns_yac.mly"
      (Ns_types.str_spec_t)
# 412 "ns_yac.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv455) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_1 : (
# 35 "ns_yac.mly"
      (Ns_types.str_spec_t)
# 420 "ns_yac.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv456)) : 'freshtv458)) : 'freshtv460)) : 'freshtv462)) : 'freshtv464)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv465 * _menhir_state * 'tv_nonempty_list_ns_rule_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv466)) : 'freshtv468)) : 'freshtv470)
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_INT_ : _menhir_env -> 'ttv_tail -> 'tv_option_INT_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv449 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 440 "ns_yac.ml"
    )) * _menhir_state * 'tv_option_BRACK_) * 'tv_option_INT_) = Obj.magic _menhir_stack in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv447 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 448 "ns_yac.ml"
    )) * _menhir_state * 'tv_option_BRACK_) * 'tv_option_INT_) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | BECOMES ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv443 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 457 "ns_yac.ml"
        )) * _menhir_state * 'tv_option_BRACK_) * 'tv_option_INT_) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv441 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 464 "ns_yac.ml"
        )) * _menhir_state * 'tv_option_BRACK_) * 'tv_option_INT_) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | NT _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
        | REGEX _v ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
        | SEMI ->
            _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76) : 'freshtv442)) : 'freshtv444)
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv445 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 485 "ns_yac.ml"
        )) * _menhir_state * 'tv_option_BRACK_) * 'tv_option_INT_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv446)) : 'freshtv448)) : 'freshtv450)

and _menhir_error73 : _menhir_env -> ('ttv_tail * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 493 "ns_yac.ml"
)) * _menhir_state * 'tv_option_BRACK_ -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run46 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_a_exp -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv439 * _menhir_state * 'tv_a_exp) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | NT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46) : 'freshtv440)

and _menhir_run48 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_a_exp -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv437 * _menhir_state * 'tv_a_exp) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | NT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48) : 'freshtv438)

and _menhir_run50 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_a_exp -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv435 * _menhir_state * 'tv_a_exp) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | NT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50) : 'freshtv436)

and _menhir_run52 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_a_exp -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv433 * _menhir_state * 'tv_a_exp) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | NT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52) : 'freshtv434)

and _menhir_run54 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_a_exp -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv431 * _menhir_state * 'tv_a_exp) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | NT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54) : 'freshtv432)

and _menhir_goto_separated_nonempty_list_SEMI_act_chunk_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMI_act_chunk_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv425) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMI_act_chunk_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv423) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (x : 'tv_separated_nonempty_list_SEMI_act_chunk_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_SEMI_act_chunk__ = 
# 59 "standard.mly"
    ( x )
# 604 "ns_yac.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMI_act_chunk__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv424)) : 'freshtv426)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv429) * _menhir_state * 'tv_act_chunk) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMI_act_chunk_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv427 * _menhir_state * 'tv_act_chunk) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (xs : 'tv_separated_nonempty_list_SEMI_act_chunk_) = _v in
        ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_SEMI_act_chunk_ = 
# 146 "standard.mly"
    ( x :: xs )
# 620 "ns_yac.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMI_act_chunk_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv428)) : 'freshtv430)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_a_exp_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_a_exp_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv417 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 634 "ns_yac.ml"
        )) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_a_exp_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv415) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (x : 'tv_separated_nonempty_list_COMMA_a_exp_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_a_exp__ = 
# 59 "standard.mly"
    ( x )
# 645 "ns_yac.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_a_exp__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv416)) : 'freshtv418)
    | MenhirState27 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv421 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 653 "ns_yac.ml"
        )) * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_a_exp_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv419 * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (xs : 'tv_separated_nonempty_list_COMMA_a_exp_) = _v in
        ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_a_exp_ = 
# 146 "standard.mly"
    ( x :: xs )
# 665 "ns_yac.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_a_exp_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv420)) : 'freshtv422)
    | _ ->
        _menhir_fail ()

and _menhir_run14 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_a_exp -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv413 * _menhir_state * 'tv_a_exp) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | NT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14) : 'freshtv414)

and _menhir_run20 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * 'tv_a_exp -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv411 * _menhir_state) * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
    ((let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
    let _v : 'tv_a_exp = 
# 69 "ns_yac.mly"
                        ( _2 )
# 698 "ns_yac.ml"
     in
    _menhir_goto_a_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv412)

and _menhir_run16 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_a_exp -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv409 * _menhir_state * 'tv_a_exp) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | NT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16) : 'freshtv410)

and _menhir_run18 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_a_exp -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv407 * _menhir_state * 'tv_a_exp) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | NT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18) : 'freshtv408)

and _menhir_run21 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_a_exp -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv405 * _menhir_state * 'tv_a_exp) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | NT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21) : 'freshtv406)

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_separated_nonempty_list_SEMI_pred_chunk_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMI_pred_chunk_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv399) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMI_pred_chunk_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv397) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (x : 'tv_separated_nonempty_list_SEMI_pred_chunk_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_SEMI_pred_chunk__ = 
# 59 "standard.mly"
    ( x )
# 776 "ns_yac.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMI_pred_chunk__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv398)) : 'freshtv400)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv403) * _menhir_state * 'tv_pred_chunk) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMI_pred_chunk_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv401 * _menhir_state * 'tv_pred_chunk) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (xs : 'tv_separated_nonempty_list_SEMI_pred_chunk_) = _v in
        ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_SEMI_pred_chunk_ = 
# 146 "standard.mly"
    ( x :: xs )
# 792 "ns_yac.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMI_pred_chunk_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv402)) : 'freshtv404)
    | _ ->
        _menhir_fail ()

and _menhir_run43 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_p_exp -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv395 * _menhir_state * 'tv_p_exp) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | FALSE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | LPAREN ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | NOT ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | NT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | TRUE ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43) : 'freshtv396)

and _menhir_run56 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_p_exp -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv393 * _menhir_state * 'tv_p_exp) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | FALSE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | LPAREN ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | NOT ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | NT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | TRUE ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56) : 'freshtv394)

and _menhir_goto_term : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_term -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState82 | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv387 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        ((if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82
        else
          let _tok = _menhir_env._menhir_token in
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv385 * _menhir_state * 'tv_term) = _menhir_stack in
          let (_tok : token) = _tok in
          ((match _tok with
          | NT _v ->
              _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
          | REGEX _v ->
              _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
          | RPAREN ->
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : 'freshtv383 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
              ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
              let _v : 'tv_nonempty_list_term_ = 
# 124 "standard.mly"
    ( [ x ] )
# 872 "ns_yac.ml"
               in
              _menhir_goto_nonempty_list_term_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv384)
          | _ ->
              assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82) : 'freshtv386)) : 'freshtv388)
    | MenhirState87 | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv391 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        ((if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87
        else
          let _tok = _menhir_env._menhir_token in
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv389 * _menhir_state * 'tv_term) = _menhir_stack in
          let (_tok : token) = _tok in
          ((match _tok with
          | NT _v ->
              _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
          | REGEX _v ->
              _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
          | SEMI ->
              _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack) MenhirState87
          | _ ->
              assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87) : 'freshtv390)) : 'freshtv392)
    | _ ->
        _menhir_fail ()

and _menhir_goto_ns_rule : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_ns_rule -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv381 * _menhir_state * 'tv_ns_rule) = Obj.magic _menhir_stack in
    ((if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
      _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92
    else
      let _tok = _menhir_env._menhir_token in
      let (_menhir_env : _menhir_env) = _menhir_env in
      let (_menhir_stack : 'freshtv379 * _menhir_state * 'tv_ns_rule) = _menhir_stack in
      let (_tok : token) = _tok in
      ((match _tok with
      | NT _v ->
          _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
      | EOF ->
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : 'freshtv377 * _menhir_state * 'tv_ns_rule) = Obj.magic _menhir_stack in
          ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
          let _v : 'tv_nonempty_list_ns_rule_ = 
# 124 "standard.mly"
    ( [ x ] )
# 925 "ns_yac.ml"
           in
          _menhir_goto_nonempty_list_ns_rule_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv378)
      | _ ->
          assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
          _menhir_env._menhir_shifted <- (-1);
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92) : 'freshtv380)) : 'freshtv382)

and _menhir_goto_option_BRACK_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_option_BRACK_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv367 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 942 "ns_yac.ml"
        )) * _menhir_state * 'tv_option_BRACK_) = Obj.magic _menhir_stack in
        ((if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_error73 _menhir_env (Obj.magic _menhir_stack)
        else
          let _tok = _menhir_env._menhir_token in
          let (_menhir_env : _menhir_env) = _menhir_env in
          let (_menhir_stack : ('freshtv365 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 952 "ns_yac.ml"
          )) * _menhir_state * 'tv_option_BRACK_) = _menhir_stack in
          let (_tok : token) = _tok in
          ((match _tok with
          | INT _v ->
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : ('freshtv361 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 961 "ns_yac.ml"
              )) * _menhir_state * 'tv_option_BRACK_) = Obj.magic _menhir_stack in
              let (_v : (
# 22 "ns_yac.mly"
       (int)
# 966 "ns_yac.ml"
              )) = _v in
              ((let _ = _menhir_discard _menhir_env in
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : 'freshtv359) = Obj.magic _menhir_stack in
              let (x : (
# 22 "ns_yac.mly"
       (int)
# 974 "ns_yac.ml"
              )) = _v in
              ((let _v : 'tv_option_INT_ = 
# 31 "standard.mly"
    ( Some x )
# 979 "ns_yac.ml"
               in
              _menhir_goto_option_INT_ _menhir_env _menhir_stack _v) : 'freshtv360)) : 'freshtv362)
          | BECOMES ->
              let (_menhir_env : _menhir_env) = _menhir_env in
              let (_menhir_stack : 'freshtv363) = Obj.magic _menhir_stack in
              ((let _v : 'tv_option_INT_ = 
# 29 "standard.mly"
    ( None )
# 988 "ns_yac.ml"
               in
              _menhir_goto_option_INT_ _menhir_env _menhir_stack _v) : 'freshtv364)
          | _ ->
              assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
              _menhir_env._menhir_shifted <- (-1);
              _menhir_error73 _menhir_env (Obj.magic _menhir_stack)) : 'freshtv366)) : 'freshtv368)
    | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv371 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 1000 "ns_yac.ml"
        )) * _menhir_state * 'tv_option_BRACK_) * 'tv_option_INT_) * _menhir_state * (
# 21 "ns_yac.mly"
       (string)
# 1004 "ns_yac.ml"
        )) * _menhir_state * 'tv_option_BRACK_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv369 * _menhir_state * (
# 21 "ns_yac.mly"
       (string)
# 1010 "ns_yac.ml"
        )) * _menhir_state * 'tv_option_BRACK_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _1), _, _2) = _menhir_stack in
        let _v : 'tv_term = 
# 47 "ns_yac.mly"
                 ( (Regex _1, _2) )
# 1016 "ns_yac.ml"
         in
        _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v) : 'freshtv370)) : 'freshtv372)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv375 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 1024 "ns_yac.ml"
        )) * _menhir_state * 'tv_option_BRACK_) * 'tv_option_INT_) * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 1028 "ns_yac.ml"
        )) * _menhir_state * 'tv_option_BRACK_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv373 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 1034 "ns_yac.ml"
        )) * _menhir_state * 'tv_option_BRACK_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _1), _, _2) = _menhir_stack in
        let _v : 'tv_term = 
# 48 "ns_yac.mly"
              ( (Nt _1, _2) )
# 1040 "ns_yac.ml"
         in
        _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v) : 'freshtv374)) : 'freshtv376)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_a_exp__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_a_exp__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv357 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 1053 "ns_yac.ml"
    )) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_a_exp__) = Obj.magic _menhir_stack in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv355 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 1061 "ns_yac.ml"
    )) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_a_exp__) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv351 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 1070 "ns_yac.ml"
        )) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_a_exp__) = Obj.magic _menhir_stack in
        ((let _ = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv349 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 1077 "ns_yac.ml"
        )) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_a_exp__) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, var), _, xs0) = _menhir_stack in
        let _v : 'tv_a_exp = let args =
          let xs = xs0 in
          
# 135 "standard.mly"
    ( xs )
# 1085 "ns_yac.ml"
          
        in
        
# 66 "ns_yac.mly"
                                                           ( Function(var, args) )
# 1091 "ns_yac.ml"
         in
        _menhir_goto_a_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv350)) : 'freshtv352)
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv353 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 1101 "ns_yac.ml"
        )) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_a_exp__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv354)) : 'freshtv356)) : 'freshtv358)

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv347 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | NT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11) : 'freshtv348)

and _menhir_goto_a_exp : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_a_exp -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv229 * _menhir_state) * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv227 * _menhir_state) * _menhir_state * 'tv_a_exp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | ADD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv225 * _menhir_state) * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv226)) : 'freshtv228)) : 'freshtv230)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv237 * _menhir_state * 'tv_a_exp) * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv235 * _menhir_state * 'tv_a_exp) * _menhir_state * 'tv_a_exp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DIV ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | COMMA | EQUAL | GE | GT | LE | LT | OR | RBRACK | RPAREN | SEMI | SUB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv231 * _menhir_state * 'tv_a_exp) * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _v : 'tv_a_exp = 
# 64 "ns_yac.mly"
                    ( Sub(_1,_3) )
# 1175 "ns_yac.ml"
             in
            _menhir_goto_a_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv232)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv233 * _menhir_state * 'tv_a_exp) * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv234)) : 'freshtv236)) : 'freshtv238)
    | MenhirState16 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv241 * _menhir_state * 'tv_a_exp) * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv239 * _menhir_state * 'tv_a_exp) * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
        let _v : 'tv_a_exp = 
# 63 "ns_yac.mly"
                    ( Multiply(_1,_3) )
# 1194 "ns_yac.ml"
         in
        _menhir_goto_a_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv240)) : 'freshtv242)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv245 * _menhir_state * 'tv_a_exp) * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv243 * _menhir_state * 'tv_a_exp) * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
        let _v : 'tv_a_exp = 
# 65 "ns_yac.mly"
                    ( Divide(_1,_3) )
# 1206 "ns_yac.ml"
         in
        _menhir_goto_a_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv244)) : 'freshtv246)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv253 * _menhir_state * 'tv_a_exp) * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv251 * _menhir_state * 'tv_a_exp) * _menhir_state * 'tv_a_exp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DIV ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | COMMA | EQUAL | GE | GT | LE | LT | OR | RBRACK | RPAREN | SEMI | SUB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv247 * _menhir_state * 'tv_a_exp) * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _v : 'tv_a_exp = 
# 62 "ns_yac.mly"
                    ( Plus(_1,_3) )
# 1229 "ns_yac.ml"
             in
            _menhir_goto_a_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv248)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv249 * _menhir_state * 'tv_a_exp) * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv250)) : 'freshtv252)) : 'freshtv254)
    | MenhirState27 | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv265 * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv263 * _menhir_state * 'tv_a_exp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | ADD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv257 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 1255 "ns_yac.ml"
            )) * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv255 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 1262 "ns_yac.ml"
            )) * _menhir_state * 'tv_a_exp) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | INT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
            | LPAREN ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | NT _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
            | _ ->
                assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27) : 'freshtv256)) : 'freshtv258)
        | DIV ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv259 * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_COMMA_a_exp_ = 
# 144 "standard.mly"
    ( [ x ] )
# 1289 "ns_yac.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_a_exp_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv260)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv261 * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv262)) : 'freshtv264)) : 'freshtv266)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv293) * _menhir_state * 'tv_assign) * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv291) * _menhir_state * 'tv_assign) * _menhir_state * 'tv_a_exp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | ADD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | RBRACK | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv287) = Obj.magic _menhir_stack in
            ((let _v : 'tv_end_act = 
# 56 "ns_yac.mly"
          ( cur_var := ""; is_act:=false; )
# 1322 "ns_yac.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv285) = _menhir_stack in
            let (_v : 'tv_end_act) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv283) * _menhir_state * 'tv_assign) * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
            let (_v : 'tv_end_act) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv281 * _menhir_state * 'tv_assign) * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
            let (_ : 'tv_end_act) = _v in
            ((let ((_menhir_stack, _menhir_s, _1), _, _2) = _menhir_stack in
            let _v : 'tv_act_chunk = 
# 54 "ns_yac.mly"
                                ( _1, _2 )
# 1337 "ns_yac.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv279) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_act_chunk) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv277 * _menhir_state * 'tv_act_chunk) = Obj.magic _menhir_stack in
            ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv275 * _menhir_state * 'tv_act_chunk) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | SEMI ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv269) * _menhir_state * 'tv_act_chunk) = Obj.magic _menhir_stack in
                ((let _tok = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv267) * _menhir_state * 'tv_act_chunk) = _menhir_stack in
                let (_tok : token) = _tok in
                ((match _tok with
                | NT _v ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
                | _ ->
                    assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32) : 'freshtv268)) : 'freshtv270)
            | RBRACK ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv271 * _menhir_state * 'tv_act_chunk) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
                let _v : 'tv_separated_nonempty_list_SEMI_act_chunk_ = 
# 144 "standard.mly"
    ( [ x ] )
# 1373 "ns_yac.ml"
                 in
                _menhir_goto_separated_nonempty_list_SEMI_act_chunk_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv272)
            | _ ->
                assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv273 * _menhir_state * 'tv_act_chunk) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv274)) : 'freshtv276)) : 'freshtv278)) : 'freshtv280)) : 'freshtv282)) : 'freshtv284)) : 'freshtv286)) : 'freshtv288)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv289) * _menhir_state * 'tv_assign) * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv290)) : 'freshtv292)) : 'freshtv294)
    | MenhirState36 | MenhirState62 | MenhirState38 | MenhirState56 | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv299 * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv297 * _menhir_state * 'tv_a_exp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | ADD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv295 * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv296)) : 'freshtv298)) : 'freshtv300)
    | MenhirState46 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv307 * _menhir_state * 'tv_a_exp) * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv305 * _menhir_state * 'tv_a_exp) * _menhir_state * 'tv_a_exp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | ADD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | AND | OR | RBRACK | RPAREN | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv301 * _menhir_state * 'tv_a_exp) * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _v : 'tv_p_exp = 
# 73 "ns_yac.mly"
                   ( Lessthan (_1,_3) )
# 1448 "ns_yac.ml"
             in
            _menhir_goto_p_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv302)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv303 * _menhir_state * 'tv_a_exp) * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv304)) : 'freshtv306)) : 'freshtv308)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv315 * _menhir_state * 'tv_a_exp) * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv313 * _menhir_state * 'tv_a_exp) * _menhir_state * 'tv_a_exp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | ADD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | AND | OR | RBRACK | RPAREN | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv309 * _menhir_state * 'tv_a_exp) * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _v : 'tv_p_exp = 
# 75 "ns_yac.mly"
                   ( LessthanEq (_1,_3) )
# 1482 "ns_yac.ml"
             in
            _menhir_goto_p_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv310)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv311 * _menhir_state * 'tv_a_exp) * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv312)) : 'freshtv314)) : 'freshtv316)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv323 * _menhir_state * 'tv_a_exp) * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv321 * _menhir_state * 'tv_a_exp) * _menhir_state * 'tv_a_exp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | ADD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | AND | OR | RBRACK | RPAREN | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv317 * _menhir_state * 'tv_a_exp) * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _v : 'tv_p_exp = 
# 74 "ns_yac.mly"
                   ( Greaterthan (_1,_3) )
# 1516 "ns_yac.ml"
             in
            _menhir_goto_p_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv318)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv319 * _menhir_state * 'tv_a_exp) * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv320)) : 'freshtv322)) : 'freshtv324)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv331 * _menhir_state * 'tv_a_exp) * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv329 * _menhir_state * 'tv_a_exp) * _menhir_state * 'tv_a_exp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | ADD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | AND | OR | RBRACK | RPAREN | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv325 * _menhir_state * 'tv_a_exp) * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _v : 'tv_p_exp = 
# 76 "ns_yac.mly"
                   ( GreaterthanEq (_1,_3) )
# 1550 "ns_yac.ml"
             in
            _menhir_goto_p_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv326)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv327 * _menhir_state * 'tv_a_exp) * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv328)) : 'freshtv330)) : 'freshtv332)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv339 * _menhir_state * 'tv_a_exp) * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv337 * _menhir_state * 'tv_a_exp) * _menhir_state * 'tv_a_exp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | ADD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | AND | OR | RBRACK | RPAREN | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv333 * _menhir_state * 'tv_a_exp) * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _v : 'tv_p_exp = 
# 72 "ns_yac.mly"
                      ( Equal (_1,_3) )
# 1584 "ns_yac.ml"
             in
            _menhir_goto_p_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv334)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv335 * _menhir_state * 'tv_a_exp) * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv336)) : 'freshtv338)) : 'freshtv340)
    | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv345 * _menhir_state) * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv343 * _menhir_state) * _menhir_state * 'tv_a_exp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | ADD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv341 * _menhir_state) * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv342)) : 'freshtv344)) : 'freshtv346)
    | _ ->
        _menhir_fail ()

and _menhir_goto_p_exp : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_p_exp -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv185 * _menhir_state) * _menhir_state * 'tv_p_exp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv183 * _menhir_state) * _menhir_state * 'tv_p_exp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | AND ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv179 * _menhir_state) * _menhir_state * 'tv_p_exp) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv177 * _menhir_state) * _menhir_state * 'tv_p_exp) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
            let _v : 'tv_p_exp = 
# 82 "ns_yac.mly"
                        ( _2 )
# 1660 "ns_yac.ml"
             in
            _menhir_goto_p_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv178)) : 'freshtv180)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv181 * _menhir_state) * _menhir_state * 'tv_p_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv182)) : 'freshtv184)) : 'freshtv186)
    | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv189 * _menhir_state * 'tv_p_exp) * _menhir_state * 'tv_p_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv187 * _menhir_state * 'tv_p_exp) * _menhir_state * 'tv_p_exp) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
        let _v : 'tv_p_exp = 
# 79 "ns_yac.mly"
                   ( Or (_1,_3) )
# 1679 "ns_yac.ml"
         in
        _menhir_goto_p_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv188)) : 'freshtv190)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv193 * _menhir_state * 'tv_p_exp) * _menhir_state * 'tv_p_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv191 * _menhir_state * 'tv_p_exp) * _menhir_state * 'tv_p_exp) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
        let _v : 'tv_p_exp = 
# 78 "ns_yac.mly"
                    ( And (_1,_3) )
# 1691 "ns_yac.ml"
         in
        _menhir_goto_p_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv192)) : 'freshtv194)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv201 * _menhir_state) * _menhir_state * 'tv_p_exp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv199 * _menhir_state) * _menhir_state * 'tv_p_exp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | AND ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | RBRACK | RPAREN | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv195 * _menhir_state) * _menhir_state * 'tv_p_exp) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
            let _v : 'tv_p_exp = 
# 77 "ns_yac.mly"
              ( Not _2 )
# 1714 "ns_yac.ml"
             in
            _menhir_goto_p_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv196)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv197 * _menhir_state) * _menhir_state * 'tv_p_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv198)) : 'freshtv200)) : 'freshtv202)
    | MenhirState36 | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv223 * _menhir_state * 'tv_p_exp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv221 * _menhir_state * 'tv_p_exp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | AND ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | RBRACK | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv217 * _menhir_state * 'tv_p_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
            let _v : 'tv_pred_chunk = 
# 59 "ns_yac.mly"
                   ( let r = (!cur_var, _1) in cur_var := ""; r )
# 1744 "ns_yac.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv215) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_pred_chunk) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv213 * _menhir_state * 'tv_pred_chunk) = Obj.magic _menhir_stack in
            ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv211 * _menhir_state * 'tv_pred_chunk) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | SEMI ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv205) * _menhir_state * 'tv_pred_chunk) = Obj.magic _menhir_stack in
                ((let _tok = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv203) * _menhir_state * 'tv_pred_chunk) = _menhir_stack in
                let (_tok : token) = _tok in
                ((match _tok with
                | FALSE ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | INT _v ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
                | LPAREN ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | NOT ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | NT _v ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
                | TRUE ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | _ ->
                    assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62) : 'freshtv204)) : 'freshtv206)
            | RBRACK ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv207 * _menhir_state * 'tv_pred_chunk) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
                let _v : 'tv_separated_nonempty_list_SEMI_pred_chunk_ = 
# 144 "standard.mly"
    ( [ x ] )
# 1790 "ns_yac.ml"
                 in
                _menhir_goto_separated_nonempty_list_SEMI_pred_chunk_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv208)
            | _ ->
                assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv209 * _menhir_state * 'tv_pred_chunk) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv210)) : 'freshtv212)) : 'freshtv214)) : 'freshtv216)) : 'freshtv218)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv219 * _menhir_state * 'tv_p_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv220)) : 'freshtv222)) : 'freshtv224)
    | _ ->
        _menhir_fail ()

and _menhir_run77 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv175) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos__1_ in
    let _v : 'tv_term = 
# 50 "ns_yac.mly"
          ( printf "ERROR(line %d): couldn't parse term\n%!" (_startpos.pos_lnum); exit 1 )
# 1821 "ns_yac.ml"
     in
    _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v) : 'freshtv176)

and _menhir_run70 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv173) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos__1_ in
    let _v : 'tv_ns_rule = 
# 44 "ns_yac.mly"
          ( printf "ERROR(line %d): couldn't parse rule\n%!" (_startpos.pos_lnum); exit 1 )
# 1836 "ns_yac.ml"
     in
    _menhir_goto_ns_rule _menhir_env _menhir_stack _menhir_s _v) : 'freshtv174)

and _menhir_reduce29 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_option_BRACK_ = 
# 29 "standard.mly"
    ( None )
# 1845 "ns_yac.ml"
     in
    _menhir_goto_option_BRACK_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run72 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 20 "ns_yac.mly"
       (string)
# 1852 "ns_yac.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv171) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (x : (
# 20 "ns_yac.mly"
       (string)
# 1862 "ns_yac.ml"
    )) = _v in
    ((let _v : 'tv_option_BRACK_ = 
# 31 "standard.mly"
    ( Some x )
# 1867 "ns_yac.ml"
     in
    _menhir_goto_option_BRACK_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv172)

and _menhir_goto_loption_separated_nonempty_list_SEMI_act_chunk__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_SEMI_act_chunk__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv169) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMI_act_chunk__) = Obj.magic _menhir_stack in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv167) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMI_act_chunk__) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | RBRACK ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv163) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMI_act_chunk__) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv161) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMI_act_chunk__) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv157) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMI_act_chunk__) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv155) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMI_act_chunk__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, xs0) = _menhir_stack in
            let _v : (
# 36 "ns_yac.mly"
      (Ns_types.ParsedPCFG.actions)
# 1899 "ns_yac.ml"
            ) = let act =
              let xs = xs0 in
              
# 135 "standard.mly"
    ( xs )
# 1905 "ns_yac.ml"
              
            in
            
# 53 "ns_yac.mly"
                                                          ( to_map act )
# 1911 "ns_yac.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv153) = _menhir_stack in
            let (_v : (
# 36 "ns_yac.mly"
      (Ns_types.ParsedPCFG.actions)
# 1918 "ns_yac.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv151) = Obj.magic _menhir_stack in
            let (_v : (
# 36 "ns_yac.mly"
      (Ns_types.ParsedPCFG.actions)
# 1925 "ns_yac.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv149) = Obj.magic _menhir_stack in
            let (_1 : (
# 36 "ns_yac.mly"
      (Ns_types.ParsedPCFG.actions)
# 1932 "ns_yac.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv150)) : 'freshtv152)) : 'freshtv154)) : 'freshtv156)) : 'freshtv158)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv159) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMI_act_chunk__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv160)) : 'freshtv162)) : 'freshtv164)
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv165) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMI_act_chunk__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv166)) : 'freshtv168)) : 'freshtv170)

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 19 "ns_yac.mly"
       (string)
# 1953 "ns_yac.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv147 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 1962 "ns_yac.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | COLONEQUALS ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv143) * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 1971 "ns_yac.ml"
        )) = Obj.magic _menhir_stack in
        ((let _ = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv141 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 1978 "ns_yac.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : 'tv_assign = 
# 55 "ns_yac.mly"
                         ( cur_var := _1; is_act:=true; _1 )
# 1984 "ns_yac.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv139) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_assign) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv137 * _menhir_state * 'tv_assign) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv135 * _menhir_state * 'tv_assign) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | INT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
        | LPAREN ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | NT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8) : 'freshtv136)) : 'freshtv138)) : 'freshtv140)) : 'freshtv142)) : 'freshtv144)
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv145 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 2016 "ns_yac.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv146)) : 'freshtv148)

and _menhir_goto_loption_separated_nonempty_list_SEMI_pred_chunk__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_SEMI_pred_chunk__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv133) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMI_pred_chunk__) = Obj.magic _menhir_stack in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv131) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMI_pred_chunk__) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | RBRACK ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv127) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMI_pred_chunk__) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv125) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMI_pred_chunk__) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv121) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMI_pred_chunk__) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv119) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMI_pred_chunk__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, xs0) = _menhir_stack in
            let _v : (
# 37 "ns_yac.mly"
      (Ns_types.ParsedPCFG.predicates)
# 2049 "ns_yac.ml"
            ) = let pred =
              let xs = xs0 in
              
# 135 "standard.mly"
    ( xs )
# 2055 "ns_yac.ml"
              
            in
            
# 58 "ns_yac.mly"
                                                              ( to_map pred )
# 2061 "ns_yac.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv117) = _menhir_stack in
            let (_v : (
# 37 "ns_yac.mly"
      (Ns_types.ParsedPCFG.predicates)
# 2068 "ns_yac.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv115) = Obj.magic _menhir_stack in
            let (_v : (
# 37 "ns_yac.mly"
      (Ns_types.ParsedPCFG.predicates)
# 2075 "ns_yac.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv113) = Obj.magic _menhir_stack in
            let (_1 : (
# 37 "ns_yac.mly"
      (Ns_types.ParsedPCFG.predicates)
# 2082 "ns_yac.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv114)) : 'freshtv116)) : 'freshtv118)) : 'freshtv120)) : 'freshtv122)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv123) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMI_pred_chunk__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv124)) : 'freshtv126)) : 'freshtv128)
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv129) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMI_pred_chunk__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)) : 'freshtv132)) : 'freshtv134)

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv111) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_p_exp = 
# 80 "ns_yac.mly"
         ( Const true )
# 2109 "ns_yac.ml"
     in
    _menhir_goto_p_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv112)

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 19 "ns_yac.mly"
       (string)
# 2116 "ns_yac.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv109 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 2125 "ns_yac.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv103 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 2134 "ns_yac.ml"
        )) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv101 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 2141 "ns_yac.ml"
        )) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | INT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
        | LPAREN ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | NT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv99) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState10 in
            ((let _v : 'tv_loption_separated_nonempty_list_COMMA_a_exp__ = 
# 57 "standard.mly"
    ( [] )
# 2158 "ns_yac.ml"
             in
            _menhir_goto_loption_separated_nonempty_list_COMMA_a_exp__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv100)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10) : 'freshtv102)) : 'freshtv104)
    | ADD | AND | COMMA | DIV | EQUAL | GE | GT | LE | LT | MUL | OR | RBRACK | RPAREN | SEMI | SUB ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv105 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 2170 "ns_yac.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : 'tv_a_exp = 
# 68 "ns_yac.mly"
       ( check_var _1; Variable )
# 2176 "ns_yac.ml"
         in
        _menhir_goto_a_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv106)
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv107 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 2186 "ns_yac.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)) : 'freshtv110)

and _menhir_run38 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv97 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | FALSE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | LPAREN ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | NOT ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | NT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | TRUE ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38) : 'freshtv98)

and _menhir_run39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv95 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | FALSE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | LPAREN ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | NOT ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | NT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | TRUE ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39) : 'freshtv96)

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 22 "ns_yac.mly"
       (int)
# 2244 "ns_yac.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv93) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_1 : (
# 22 "ns_yac.mly"
       (int)
# 2254 "ns_yac.ml"
    )) = _v in
    ((let _v : 'tv_a_exp = 
# 67 "ns_yac.mly"
        ( Constant _1 )
# 2259 "ns_yac.ml"
     in
    _menhir_goto_a_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv94)

and _menhir_run40 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv91) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_p_exp = 
# 81 "ns_yac.mly"
          ( Const false )
# 2272 "ns_yac.ml"
     in
    _menhir_goto_p_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv92)

and _menhir_discard : _menhir_env -> token =
  fun _menhir_env ->
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = _menhir_env._menhir_lexer lexbuf in
    _menhir_env._menhir_token <- _tok;
    _menhir_env._menhir_startp <- lexbuf.Lexing.lex_start_p;
    _menhir_env._menhir_endp <- lexbuf.Lexing.lex_curr_p;
    let shifted = Pervasives.(+) _menhir_env._menhir_shifted 1 in
    if Pervasives.(>=) shifted 0 then
      _menhir_env._menhir_shifted <- shifted;
    _tok

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState92 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv31 * _menhir_state * 'tv_ns_rule) = Obj.magic _menhir_stack in
        (_menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp : 'freshtv32)
    | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv33 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        (_menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_startp : 'freshtv34)
    | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv35 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        (_menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _menhir_env._menhir_startp : 'freshtv36)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv37 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 2308 "ns_yac.ml"
        )) * _menhir_state * 'tv_option_BRACK_) * 'tv_option_INT_) * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 2312 "ns_yac.ml"
        )) * _menhir_state) = Obj.magic _menhir_stack in
        (_menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_startp : 'freshtv38)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv39 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 2320 "ns_yac.ml"
        )) = Obj.magic _menhir_stack in
        (_menhir_reduce29 _menhir_env (Obj.magic _menhir_stack) MenhirState80 : 'freshtv40)
    | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv41 * _menhir_state * (
# 21 "ns_yac.mly"
       (string)
# 2328 "ns_yac.ml"
        )) = Obj.magic _menhir_stack in
        (_menhir_reduce29 _menhir_env (Obj.magic _menhir_stack) MenhirState78 : 'freshtv42)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv43 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 2336 "ns_yac.ml"
        )) * _menhir_state * 'tv_option_BRACK_) * 'tv_option_INT_) = Obj.magic _menhir_stack in
        (_menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_startp : 'freshtv44)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv45 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 2344 "ns_yac.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState69 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv47) = Obj.magic _menhir_stack in
        (_menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_startp : 'freshtv48)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv49) * _menhir_state * 'tv_pred_chunk) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv51 * _menhir_state * 'tv_p_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv53 * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv55 * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv57 * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv59 * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState46 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv61 * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv63 * _menhir_state * 'tv_p_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv65 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv67 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv69) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv70)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv71) * _menhir_state * 'tv_act_chunk) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState27 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv73 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 2416 "ns_yac.ml"
        )) * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv75 * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv77 * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState16 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv79 * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv81 * _menhir_state * 'tv_a_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv83 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv85 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 2450 "ns_yac.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv87 * _menhir_state * 'tv_assign) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv89) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv90)

and _menhir_run71 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 19 "ns_yac.mly"
       (string)
# 2467 "ns_yac.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv29 * _menhir_state * (
# 19 "ns_yac.mly"
       (string)
# 2476 "ns_yac.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | BRACK _v ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | BECOMES | INT _ ->
        _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71) : 'freshtv30)

and _menhir_init : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> _menhir_env =
  fun lexer lexbuf ->
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_startp = lexbuf.Lexing.lex_start_p;
      _menhir_endp = lexbuf.Lexing.lex_curr_p;
      _menhir_shifted = 4611686018427387903;
      }

and act : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 36 "ns_yac.mly"
      (Ns_types.ParsedPCFG.actions)
# 2504 "ns_yac.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = _menhir_init lexer lexbuf in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv27) = () in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv25) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | LBRACK ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv19) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | NT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
        | RBRACK ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv17) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState1 in
            ((let _v : 'tv_loption_separated_nonempty_list_SEMI_act_chunk__ = 
# 57 "standard.mly"
    ( [] )
# 2533 "ns_yac.ml"
             in
            _menhir_goto_loption_separated_nonempty_list_SEMI_act_chunk__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv18)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1) : 'freshtv20)) : 'freshtv22)
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv24)) : 'freshtv26)) : 'freshtv28))

and pred : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 37 "ns_yac.mly"
      (Ns_types.ParsedPCFG.predicates)
# 2550 "ns_yac.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = _menhir_init lexer lexbuf in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv15) = () in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv13) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | LBRACK ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv9) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv7) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | FALSE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | INT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | LPAREN ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | NOT ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | NT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | TRUE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | RBRACK ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv5) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState36 in
            ((let _v : 'tv_loption_separated_nonempty_list_SEMI_pred_chunk__ = 
# 57 "standard.mly"
    ( [] )
# 2589 "ns_yac.ml"
             in
            _menhir_goto_loption_separated_nonempty_list_SEMI_pred_chunk__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv6)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36) : 'freshtv8)) : 'freshtv10)
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv11) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv12)) : 'freshtv14)) : 'freshtv16))

and spec : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 35 "ns_yac.mly"
      (Ns_types.str_spec_t)
# 2606 "ns_yac.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = _menhir_init lexer lexbuf in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv3) = () in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | NT _v ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69) : 'freshtv2)) : 'freshtv4))




