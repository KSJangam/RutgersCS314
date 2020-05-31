open Proj2_types;;
let getStartSymbol (g : grammar) : string =
  (* YOUR CODE GOES HERE *)
(fst g);;

let getNonterminals (g : grammar) : string list =
  (* YOUR CODE GOES HERE *)

let rec gNt l =
match l with
[] -> []
| h::t ->
match h with (a,b) -> a :: gNt(t)
in
gNt (snd g);;

let getInitFirstSets (g : grammar) : symbolMap =
  (* YOUR CODE GOES HERE *)
let rec addNt l =
match l with
[] -> SMap.empty
| h::t
->
let a =SymbolSet.empty in
let b = addNt t in
SMap.add h a b
in addNt (getNonterminals g);;

let getInitFollowSets (g : grammar) : symbolMap =
let rec addNt l g =
match l with
[] -> SMap.empty
| h::t
-> if h = (getStartSymbol g) then
SMap.add h (SymbolSet.singleton "eof") (addNt t g)
else SMap.add h (SymbolSet.empty) (addNt t g)
in addNt (getNonterminals g) g;;


let rec computeFirstSet (first : symbolMap) (symbolSeq : string list) : SymbolSet.t =
match symbolSeq with
[] ->SymbolSet.singleton "eps"
| h::t
-> if SMap.mem h first then
let a = SymbolSet.singleton "eps" in
let b = SMap.find h first in
if SymbolSet.subset a b then
let c = SymbolSet.remove "eps" b in
let d = computeFirstSet first t in
SymbolSet.union c d
else b
else SymbolSet.singleton h;;

let rec recurseFirstSetsHelper first firstFunc rule_list =
match rule_list with
[] -> first
| h::t -> 
match h with (lhs,rhs)
->
let a = SMap.find lhs first in
let b = firstFunc first rhs in
let c = SymbolSet.union a b in
let d = SMap.add lhs c first in
recurseFirstSetsHelper d firstFunc t;;

let recurseFirstSets (g : grammar) (first : symbolMap) firstFunc : symbolMap =
let rule_list = (snd g) in
recurseFirstSetsHelper first firstFunc rule_list;;

let rec getFirstSets (g : grammar) (first : symbolMap) firstFunc : symbolMap =
  (* YOUR CODE GOES HERE *)
let n = recurseFirstSets g first firstFunc in
if SMap.equal (SymbolSet.equal) n first then first
else getFirstSets g n firstFunc;;

let rec updateFollowSet (first : symbolMap) (follow : symbolMap) (nt : string) (symbolSeq : string list) : symbolMap =
  (* YOUR CODE GOES HERE *)
match symbolSeq with
[] -> follow
| h::t ->
if SMap.mem h follow then
if SymbolSet.subset (SymbolSet.singleton "eps") (computeFirstSet first t) then
SMap.add h (SymbolSet.union (SMap.find h follow) (SymbolSet.union (SymbolSet.remove "eps" (computeFirstSet first t)) (SMap.find nt follow))) (updateFollowSet first follow nt t) 
else SMap.add h (SymbolSet.union (SMap.find h follow) (computeFirstSet first t)) (updateFollowSet first follow nt t)
else updateFollowSet first follow nt t;;

let rec recurseFollowSetsHelper first follow followFunc rule_list = 
match rule_list with
[] -> follow
| h::t -> 
match h with (lhs,rhs)
->recurseFollowSetsHelper first (followFunc first follow lhs rhs) followFunc t;;

let recurseFollowSets (g : grammar) (first : symbolMap) (follow : symbolMap) followFunc : symbolMap =
let rule_list = (snd g) in
recurseFollowSetsHelper first follow followFunc rule_list;;

let rec getFollowSets (g : grammar) (first : symbolMap) (follow : symbolMap) followFunc : symbolMap =
let n = recurseFollowSets g first follow followFunc in
if SMap.equal (SymbolSet.equal) n follow then follow
else getFollowSets g first n followFunc;;

let getPredictSets (g : grammar) (first : symbolMap) (follow : symbolMap) firstFunc : ((string * string list) * SymbolSet.t) list = 
List.map
(fun rule -> match rule with (lhs,rhs) ->
let firstRhs = firstFunc first rhs in
let b = SMap.find lhs follow in
match rhs with [] -> (rule, b)
| h::t ->
if SymbolSet.mem "eps" firstRhs then
let a = SymbolSet.remove "eps" firstRhs in
let c = SymbolSet.union a b in
(rule,c)
else (rule, firstRhs)
)(snd g);;

let rec findRule predict lhs token = 
match predict with
[]->["error"]
| h::t ->
match h with ((lhs2, rhs), predictSet) ->
if lhs = lhs2 && SymbolSet.mem token predictSet then rhs
else findRule t lhs token;;

let rec tryDeriveHelper predict sentence inputStr = 
match sentence with
[] -> (match inputStr with [] -> true | ["eof"] -> true | _ -> false)
| h::t -> match inputStr with
[]->
let rh = findRule predict h "eof" in
let rhs = rh @ t in 
tryDeriveHelper predict rhs ["eof"]
| u::v -> 
if h = u then tryDeriveHelper predict t v
else 
let rhs = findRule predict h u in
match rhs with
["error"] -> false
|[]->tryDeriveHelper predict t inputStr
| a::b -> 
let sen = rhs @ t in
tryDeriveHelper predict sen inputStr
;;

let tryDerive (g : grammar) (inputStr : string list) : bool =
let first = getFirstSets g (getInitFirstSets g) computeFirstSet in
let follow = getFollowSets g first (getInitFollowSets g) updateFollowSet in
let predict = getPredictSets g first follow computeFirstSet in
tryDeriveHelper predict [(fst g)] inputStr ;;

let tryDeriveTree (g : grammar) (inputStr : string list) : parseTree =
  (* YOUR CODE GOES HERE *)
Terminal "empty";;

let genParser g = tryDerive g;;
let genTreeParser g = tryDeriveTree g;;
