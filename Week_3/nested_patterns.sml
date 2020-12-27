
exception LengthOfListNotMatch

fun zip(x) =
  case x of
       ([], [], []) => []
     | (hd1::tl1, hd2::tl2, hd3::tl3) => (hd1, hd2, hd3)::zip(tl1, tl2, tl3)
     | _ => raise LengthOfListNotMatch


fun unzip(x) =
  case x of
       [] => ([], [], [])
     | (a, b, c)::tl => let val (l1, l2, l3) = unzip tl
                        in
                          (a::l1, b::l2, c::l3)
                        end


fun len xs =
  case xs of
       [] => 0
     | _::xs' => 1 + len xs'

