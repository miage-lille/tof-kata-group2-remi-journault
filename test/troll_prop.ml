open TOF.Troll
open TOF.Elf
open Generator.Fantasy

let troll_invariance =
  QCheck2.Test.make
    ~count:1000
    ~name:"Troll score should always be >= 0"
    ~print:troll_print
    troll_gen
    (fun troll -> scoring troll >= 0)

let troll_inverse =
   QCheck2.Test.make
     ~count:1000
     ~name:"oops_he_survived should always be inverse of i_got_one"
     troll_elf_gen
     (fun (troll, elf) ->
       i_got_one elf troll |> oops_he_survived elf |> scoring = scoring troll )

let troll_analogy =
  QCheck2.Test.make
    ~count:1000
    ~name:"i_got_one and i_got should be consistent"
    troll_elf_int_gen
    (fun (troll, elf, qty) ->
      i_got qty elf troll
      = ( List.init qty (fun _ -> 1)
      |> List.fold_left (fun cur_troll _ -> i_got_one elf cur_troll) troll ))


let elf_resurrected_idempotence = 
  QCheck2.Test.make
    ~count:1000
    ~name:"Once the elves are resurrected, resurrect them again won't change anything"
    troll_elf_gen
    (fun (troll, elf) ->  
      all_elves_of_a_kind_resurrected elf troll =  let without_resurected = ElvesMap.remove elf troll.kills in
      { name = troll.name; kills = without_resurected }
    )

let kills_incr_score = 
  QCheck2.Test.make
  ~count:1000
  ~name:"Make any kills add at least 1 to score"
  troll_elf_gen
  (fun (troll, elf) ->
    i_got_one elf troll |> scoring > scoring troll
  )

let injection_test = 
  QCheck2.Test.make
  ~count:1000
  ~name:"Troll after killing elf1 must be different after elf2"
  troll_two_elves_gen
  (fun (troll, elf1, elf2) -> 
    QCheck2.assume (not ((==) (value elf1) (value elf2)));
    (i_got_one elf1 troll |> scoring) != (i_got_one elf2 troll |> scoring)
  )

let troll_prop_set =
  (* ADD EACH TEST IN THE LIST *)
  List.map
    QCheck_alcotest.to_alcotest
    [ troll_invariance ; troll_inverse ; troll_analogy; elf_resurrected_idempotence; injection_test  ]