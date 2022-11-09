module Types = struct
    type commit_param = {
        secret_action : bytes
    }

    type reveal_param = bytes * nat

    type reset_param = {
        min : nat;
        max : nat
    }

    type t = Commit of commit_param | Reveal of reveal_param | Reset of reset_param
end 