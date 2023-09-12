use std::collections::HashSet;

use derive_more::*;

fn main() {
	println!("Hello, world!");
}

type I = u8;

#[repr(transparent)]
#[derive(Debug, Clone, From, Hash, Eq, PartialEq, PartialOrd, Ord)]
struct V<T>(Vec<T>);

impl<T> V<T> {
	fn get(&self, i: I) -> Option<&T> {
		let u: usize = i.into();
		self.0.get(u)
	}

	fn get_mut(&mut self, i: I) -> Option<&mut T> {
		let u: usize = i.into();
		self.0.get_mut(u)
	}
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
enum Law {
	AndEliminationLeft(I),
	AndEliminationRight(I),
	AndIntroduction(I, I),
	AssumptionIntroduction(&'static Statement),
	Contradiction(I, I),
	DoubleNotIntroduction(I),
	DoubleNotElimination(I),
	ImplicationElimination(I, I),
	ImplicationIntroduction(I),
	Lem(I),
	ModusTollens(I, I),
	NotIntroduction(I),
	NotElimination(I, I),
	NotBottom,
	OrEliminationLeft(I, I),
	OrEliminationRight(I, I),
	OrIntroduction(I, I),
	Premise(&'static Statement),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
enum Statement {
	And(&'static Statement, &'static Statement),
	Or(&'static Statement, &'static Statement),
	Implies(&'static Statement, &'static Statement),
	Not(&'static Statement),
	Variable(u8),
	Bottom,
	AssumptionBlock(&'static Statement, V<(&'static Statement, Law)>),
}

static mut STATEMENTS: HashSet<&'static Statement> = HashSet::new();


fn get_or_insert(s: Statement) -> &'static Statement {
	// Safety: Not thread safe, fine otherwise
	unsafe {
		if !STATEMENTS.contains(&s) {
			STATEMENTS.insert(Box::leak(Box::new(s)));
		}
		STATEMENTS
			.get(&s)
			.expect("But I literally just inserted it?")
	}
}

fn check_statement(ss: V<&'static Statement>, law: Law) -> Option<&'static Statement> {
	match law {
		Law::AndEliminationLeft(x) => {
			let x_ = ss.get(x)?;
			match x_ {
				Statement::And(l, _) => Some(*l),
				_ => None,
			}
		}
		Law::AndEliminationRight(x) => {
			let x_ = ss.get(x)?;
			match x_ {
				Statement::And(_, r) => Some(*r),
				_ => None,
			}
		}
		Law::AndIntroduction(x, y) => {
			let x_ = ss.get(x)?;
			let y_ = ss.get(y)?;
			let and = Statement::And(x_, y_);
			Some(get_or_insert(and))
		}
		Law::DoubleNotIntroduction(x) => {
			let x_ = ss.get(x)?;
			let not_x = get_or_insert(Statement::Not(x_));
			let not_not_x = get_or_insert(Statement::Not(not_x));
			Some(not_not_x)
		}
		Law::DoubleNotElimination(x) => {
			let x_ = ss.get(x)?;
			match x_ {
				Statement::Not(inner) => match **inner {
					Statement::Not(result) => Some(result),
					_ => None,
				},
				_ => None,
			}
		}
		Law::OrEliminationLeft(l, r) => {
			let l_ = ss.get(l)?;
			let r_ = ss.get(r)?;
			match (l_, r_) {
				(Statement::Or(l, _), l_) if l == l_ => Some(*l),
				(l_, Statement::Or(_, l)) if l == l_ => Some(*l),
				_ => None,
			}
		}
		Law::OrEliminationRight(l, r) => {
			let l_ = ss.get(l)?;
			let r_ = ss.get(r)?;
			match (l_, r_) {
				(Statement::Or(_, r), r_) if r == r_ => Some(*r),
				(r_, Statement::Or(_, r)) if r == r_ => Some(*r),
				_ => None,
			}
		}
		Law::Contradiction(l, r) => {
			let l_ = ss.get(l)?;
			let r_ = ss.get(r)?;
			match (l_, r_) {
				(x_, Statement::Not(y_)) if x_ == y_ => Some(get_or_insert(Statement::Bottom)),
				(Statement::Not(x_), y_) if x_ == y_ => Some(get_or_insert(Statement::Bottom)),
				_ => None,
			}
		}
		Law::Lem(l) => {
			let l_ = ss.get(l)?;
			let not = get_or_insert(Statement::Not(l_));
			let or = get_or_insert(Statement::Or(l_, not));
			Some(or)
		}
		Law::Premise(s) => Some(s),
		Law::ImplicationIntroduction(_) => {
			// Handle this case as needed
			None
		}
		Law::ImplicationElimination(l, r) => {
			let l_ = ss.get(l)?;
			let r_ = ss.get(r)?;
			match (l_, r_) {
				(x, Statement::Implies(x_, y)) if x == x_ => Some(*y),
				(Statement::Implies(x_, y), x) if x == x_ => Some(*y),
				_ => None,
			}
		}
		Law::ModusTollens(l, r) => {
			let l_ = ss.get(l)?;
			let r_ = ss.get(r)?;
			match (l_, r_) {
				(Statement::Implies(x, y), Statement::Not(y_)) if y == y_=> {
					let not = get_or_insert(Statement::Not(x));
					Some(not)
				}
				(Statement::Not(y_), Statement::Implies(x, y)) if y == y_=> {
					let not = get_or_insert(Statement::Not(x));
					Some(not)
				}
				_ => None,
			}
		}
		Law::NotBottom => {
			let b = get_or_insert(Statement::Bottom);
			let not = get_or_insert(Statement::Not(b));
			Some(not)
		}
		Law::NotIntroduction(l) => {
			let l_ = ss.get(l)?;
			match l_ {
				Statement::Implies(x, Statement::Bottom) => {
					let not = get_or_insert(Statement::Not(x));
					Some(not)
				}
				_ => None,
			}
		}
		_ => None,
	}
}
