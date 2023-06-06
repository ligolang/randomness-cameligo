#import "storage.mligo" "Storage"
#import "parameter.mligo" "Parameter"
#import "errors.mligo" "Errors"


type storage = Storage.Types.t
type parameter = Parameter.Types.t
type return = operation list * storage

 // Sender commits its bytes
let commit(p, st : Parameter.Types.commit_param * storage) : operation list * storage =
    let _check_authorized : unit = assert_with_error (Set.mem (Tezos.get_sender ()) st.participants) Errors.not_authorized in
    let _check_amount : unit = assert_with_error (Tezos.get_amount () >= 10mutez) Errors.commit_expects_10_mutez_lock in
    let new_secrets = match Map.find_opt (Tezos.get_sender ()) st.secrets with
    | None -> Map.add (Tezos.get_sender ()) p.secret_action st.secrets
    | Some _x -> (failwith(Errors.bytes_already_committed) : (address, bytes) map)
    in
    let new_locked : (address, tez) map = match Map.find_opt (Tezos.get_sender ()) st.locked_tez with
    | None -> Map.add (Tezos.get_sender ()) (Tezos.get_amount ()) st.locked_tez
    | Some v -> Map.update (Tezos.get_sender ()) (Some(v + Tezos.get_amount ())) st.locked_tez
    in
    (([] : operation list), { st with secrets=new_secrets; locked_tez=new_locked })
    

// Sender reveals its bytes content
let reveal(p, s : Parameter.Types.reveal_param * storage) : operation list * storage =
    let sender_address : address = Tezos.get_sender () in
    let _check_amount : unit = assert_with_error (Tezos.get_amount () = 0mutez) Errors.reveal_expects_0_mutez_lock in
    let _check_authorized : unit = assert_with_error (Set.mem sender_address s.participants) Errors.not_authorized in
    // check all bytes has been received
    let committed = fun (acc, elt : bool * address) : bool -> match Map.find_opt elt s.secrets with
        | None -> acc && false
        | Some _x -> acc && true
    in
    let _all_bytes_committed = Set.fold committed s.participants true in

    let _check_all_bytes: unit = assert_with_error (_all_bytes_committed = true) Errors.missing_bytes in

    let (bytes, secret) = p in
    let sender_bytes : bytes = match Map.find_opt sender_address s.secrets with
    | None -> failwith(Errors.missing_bytes)
    | Some ch -> ch
    in
    // open bytes and stores the bytes content
    let encoded = Crypto.sha512 (Bytes.pack (bytes, secret)) in
    let decoded_payload = 
        if encoded = sender_bytes
        then bytes
        else failwith(Errors.fail_open_bytes_decrypt)
    in
    let new_decoded_payloads = match Map.find_opt sender_address s.decoded_payloads with
    | None -> Map.add (Tezos.get_sender ()) decoded_payload s.decoded_payloads
    | Some _elt -> (failwith(Errors.bytes_already_revealed) : (address, bytes) map)
    in 
    // check all bytes has been revealed
    let revealed = fun (acc, elt : bool * address) : bool -> match Map.find_opt elt new_decoded_payloads with
        | None -> acc && false
        | Some _x -> acc && true
    in
    let new_locked : (address, tez) map = match Map.find_opt (Tezos.get_sender ()) s.locked_tez with
    | None -> failwith(Errors.wrong_user_balance)
    | Some v ->   
        (match v - 10mutez with
        | None -> failwith(Errors.wrong_amount_locked_tez)
        | Some new_val -> Map.update (Tezos.get_sender ()) (Some(new_val)) s.locked_tez)
    in
    let dest_opt : unit contract option = Tezos.get_contract_opt (Tezos.get_sender ()) in
    let destination : unit contract = match dest_opt with
    | None -> failwith(Errors.unknown_user_account)
    | Some ct -> ct
    in
    let op : operation = Tezos.transaction unit 10mutez destination in 

    let all_bytes_revealed = Set.fold revealed s.participants true in
    if all_bytes_revealed = true then
        let (seed, value) = Storage.Utils.build_random_nat(new_decoded_payloads, s.min, s.max, s.last_seed) in
        ([op], { s with decoded_payloads=new_decoded_payloads; locked_tez=new_locked; result_nat=(Some(value)); last_seed=seed })  
    else
        ([op], { s with decoded_payloads=new_decoded_payloads; locked_tez=new_locked })

        
let reset (param, store : Parameter.Types.reset_param * storage) : operation list * storage =
    // clean secrets_bytes and decoded_payloads
    let _check_amount : unit = assert_with_error (Tezos.get_amount () = 0mutez) Errors.reset_expects_0_mutez_lock in
    (([] : operation list), { store with 
        decoded_payloads=(Map.empty : (address, bytes) map); 
        secrets=(Map.empty : (address, bytes) map); 
        result_nat=(None : nat option);
        min=param.min;
        max=param.max 
    })


let main(ep : parameter) (store : storage) : return =
    match ep with 
    | Commit(p) -> commit(p, store)
    | Reveal(p) -> reveal(p, store)
    | Reset(p) -> reset(p, store)
