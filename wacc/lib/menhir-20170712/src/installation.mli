(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

(* This module defines a number of installation settings.
   Its source code is generated by the main [Makefile]. *)

(* The directory where Menhir's standard library, [standard.mly],
   is installed. *)

val libdir: string

(* Whether MenhirLib was installed via [ocamlfind] or (manually)
   in the above directory. *)

val ocamlfind: bool

