namespace Fixture

structure Point where
  x : Nat
  y : Nat

def double (n : Nat) : Nat := n + n

private theorem double_eq (n : Nat) : double n = 2 * n := by
  unfold double; rw [Nat.two_mul]

/-- The node's line is the `theorem` line, not the docstring's. -/
@[simp] theorem double_mul (n : Nat) : double n = n * 2 := by
  rw [double_eq, Nat.mul_comm]

/-- A definition whose body uses a theorem: folded into the edges of its users. -/
def twice (n : Nat) : { m // m = 2 * n } := ⟨double n, double_eq n⟩

theorem twice_spec (n : Nat) : (twice n).val = 2 * n := (twice n).property

theorem x_mk (a b : Nat) : (Point.mk a b).x = a := rfl

/-- Its field `le` is a theorem to Lean, but folded as a field. -/
structure Small (n : Nat) : Prop where
  le : n ≤ 10

theorem small_double (h : Small n) : double n ≤ 20 := by
  rw [double_eq]; have := h.le; omega

end Fixture
