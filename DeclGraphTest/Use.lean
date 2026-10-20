import DeclGraphTest.Basic

namespace Fixture

theorem quad (n : Nat) : double (double n) = n * 2 * 2 := by
  rw [double_mul, double_mul]

/-- Compiles to an auxiliary `match_1`, which must never become a node. -/
def pred : Nat → Nat
  | 0 => 0
  | k + 1 => k

theorem pred_double : pred (double 1) = 1 := rfl

end Fixture
