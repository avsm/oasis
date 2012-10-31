(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2012, Anil Madhavapeddy                                      *)
(* Copyright (C) 2008-2010, OCamlCore SARL                                    *)
(*                                                                            *)
(* This library is free software; you can redistribute it and/or modify it    *)
(* under the terms of the GNU Lesser General Public License as published by   *)
(* the Free Software Foundation; either version 2.1 of the License, or (at    *)
(* your option) any later version, with the OCaml static compilation          *)
(* exception.                                                                 *)
(*                                                                            *)
(* This library is distributed in the hope that it will be useful, but        *)
(* WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY *)
(* or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more         *)
(* details.                                                                   *)
(*                                                                            *)
(* You should have received a copy of the GNU Lesser General Public License   *)
(* along with this library; if not, write to the Free Software Foundation,    *)
(* Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA              *)
(******************************************************************************)

open Ocamlbuild_plugin

module Util = struct
  let split s ch =
    let x = ref [] in
    let rec go s =
      let pos = String.index s ch in
      x := (String.before s pos)::!x;
      go (String.after s (pos + 1))
    in
    try
      go s
    with Not_found -> !x

    let split_nl s = split s '\n'
    let run_and_read x = List.hd (split_nl (Ocamlbuild_pack.My_unix.run_and_read x))
end

module Ns3 = struct
  (** Link to a standalone Ns3 microkernel *)
  let cc_ns3_link bc tags arg out env =
    (* XXX check ocamlfind path here *)
    let ns3lib = Util.run_and_read "ocamlfind query mirage" in
    let camllib = "-L" ^ (Util.run_and_read "ocamlfind query bigarray") in
    let main_obj = Px (ns3lib / "main.o") in
    let ns_obj = Px (ns3lib / "ns_stubs.o") in
    let clock_obj = Px (ns3lib / "clock_stubs.o") in
    let ocamllib = match bc with |true -> "ocamlbc" |false -> "ocaml" in
    let ld = getenv ~default:"ld" "LD" in
    let dl_libs = 
      [A"-L/usr/lib/"; A"-L/usr/local/lib/"; A(camllib); A"-ldl"; A"-lns3-applications";
      A"-lns3-core"; A"-lns3-internet"; 
      A"-lns3-network"; A"-lns3-point-to-point";
      A"-lns3-tap-bridge"; A"-lunix"; A"-lasmrun";
      A"/home/cr409/.opam/3.12.1+mirage-ns3-direct/lib/ocaml/libcamlrun_shared.so";
      A"/home/cr409/.opam/3.12.1+mirage-ns3-direct/lib/stublibs/dlllwt-unix_stubs.so";
      A"/home/cr409/.opam/3.12.1+mirage-ns3-direct/lib/mirage/oS.a";
      A"/home/cr409/.opam/3.12.1+mirage-ns3-direct/lib/stublibs/dllcstruct_stubs.so";
      A"/home/cr409/.opam/3.12.1+mirage-ns3-direct/lib/mirage/libns3run.a";
      A"-lbigarray"; A"-lcamlstr";] in 
    let ldlibs = List.map (fun x -> Px (ns3lib / ("lib" ^ x ^ ".a")))
      [ "ns3run";] in
    Cmd (S ( A ld :: dl_libs @ [ T(tags++"link"++"ns3"); main_obj; clock_obj; P arg ] 
      @ ldlibs @ [A"-o"; Px out]))

  let cc_ns3_bc_link tags arg out env = cc_ns3_link true tags arg out env
  let cc_ns3_nc_link tags arg out env = cc_ns3_link false tags arg out env

  let rules () =
    let cc_link_c_implem ?tag fn c o env build =
      let c = env c and o = env o in
      fn (tags_of_pathname c++"implem"+++tag) c o env
    in
    rule "final link: %.nobj.o -> %.ns3" ~prod:"%(file).ns3" ~dep:"%(file).nobj.o"
      (cc_link_c_implem cc_ns3_nc_link "%(file).nobj.o" "%(file).ns3")
end

let dispatch =
  function
    | After_rules ->
        Ns3.rules ()       
    | _ -> 
        ()
