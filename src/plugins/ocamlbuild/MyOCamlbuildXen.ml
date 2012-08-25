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

module Xen = struct
  (** Link to a standalone Xen microkernel *)
  let cc_xen_link bc tags arg out env =
    (* XXX check ocamlfind path here *)
    let xenlib = Util.run_and_read "ocamlfind query mirage" in
    let jmp_obj = Px (xenlib / "longjmp.o") in
    let head_obj = Px (xenlib / "x86_64.o") in
    let ocamllib = match bc with |true -> "ocamlbc" |false -> "ocaml" in
    let ld = getenv ~default:"ld" "LD" in
    let ldlibs = List.map (fun x -> Px (xenlib / ("lib" ^ x ^ ".a")))
      [ocamllib; "xen"; "xencaml"; "diet"; "m"] in
    Cmd (S ( A ld :: [ T(tags++"link"++"xen");
      A"-d"; A"-nostdlib"; A"-m"; A"elf_x86_64"; A"-T";
      Px (xenlib / "mirage-x86_64.lds");  head_obj; P arg ]
      @ ldlibs @ [jmp_obj; A"-o"; Px out]))

  let cc_xen_bc_link tags arg out env = cc_xen_link true tags arg out env
  let cc_xen_nc_link tags arg out env = cc_xen_link false tags arg out env

  (* Rewrite sections for Xen LDS layout *)
  let xen_objcopy dst src env builder =
    let dst = env dst in
    let src = env src in
    let cmd = ["objcopy";"--rename-section";".bss=.mlbss";"--rename-section";
      ".data=.mldata";"--rename-section";".rodata=.mlrodata";
      "--rename-section";".text=.mltext"] in
    let cmds = List.map (fun x -> A x) cmd in
    Cmd (S (cmds @ [Px src; Px dst]))

  let rules () =
    let cc_link_c_implem ?tag fn c o env build =
      let c = env c and o = env o in
      fn (tags_of_pathname c++"implem"+++tag) c o env
    in
    rule "final link: %.nobj.o -> %.xen" ~prod:"%(file).xen" ~dep:"%(file).nobj.o"
      (cc_link_c_implem cc_xen_nc_link "%(file).nobj.o" "%(file).xen")

end

let dispatch =
  function
    | After_rules ->
        Xen.rules ()       
    | _ -> 
        ()
