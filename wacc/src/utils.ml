let array_rev_iter
      (a: 'a array)
      (f: 'a -> unit) =
  let i = ref ((Array.length a) - 1) in
  while !i >= 0 do
    f Array.(i);
    decr i
  done
