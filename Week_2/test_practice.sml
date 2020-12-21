
use "practice.sml";

val test_alternate = alternate [1, 2, 3, 4] = ~2

val test_min = min [1, 2, 3, 4, 5, 0] = SOME(0)

val test_max = max [1, 2, 3, 4, 5, 0] = SOME(5)

val test_min_max = min_max [1, 2, 3, 4, 5, 0] = (0, 5)
