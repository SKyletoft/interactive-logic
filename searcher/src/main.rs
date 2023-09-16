#![allow(dead_code)]

use std::collections::{BinaryHeap, HashSet};

use cached::proc_macro::cached;
use derive_more::*;
use itertools::Itertools;
use once_cell::sync::{Lazy, OnceCell};
use parking_lot::Mutex;

fn main() {
	let a = get_or_insert(Statement::Variable(b'a'));
	let b = get_or_insert(Statement::Variable(b'b'));
	let c = get_or_insert(Statement::Variable(b'c'));
	let d = get_or_insert(Statement::Variable(b'd'));
	let e = get_or_insert(Statement::Variable(b'e'));
	let f = get_or_insert(Statement::Variable(b'f'));
	let g = get_or_insert(Statement::Variable(b'g'));
	let h = get_or_insert(Statement::Variable(b'h'));
	let i = get_or_insert(Statement::Variable(b'i'));

	let and_1 = get_or_insert(Statement::And(a, b));
	let and_2 = get_or_insert(Statement::And(and_1, c));
	let and_3 = get_or_insert(Statement::And(and_2, d));
	let and_4 = get_or_insert(Statement::And(and_3, e));
	let and_5 = get_or_insert(Statement::And(and_4, f));
	let and_6 = get_or_insert(Statement::And(and_5, g));
	let and_7 = get_or_insert(Statement::And(and_6, h));
	let and_8 = get_or_insert(Statement::And(and_7, i));

	let premise = V(vec![and_8]);

	let proof = bfs(Statement::Variable(b'a'), premise);
	dbg!(proof);
}

type I = u8;

#[repr(transparent)]
#[derive(Debug, Clone, From, Hash, Eq, PartialEq, PartialOrd, Ord, Deref, DerefMut)]
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
	AssumptionBlock(&'static Statement, V<&'static Statement>),
}

static STATEMENTS: Lazy<Mutex<HashSet<&'static Statement>>> = Lazy::new(|| {
	let s = [Box::leak(Box::new(Statement::Bottom))]
		.into_iter()
		.map(|x| x as &'static _) // Because apparently we can't implicitly cast &mut T to &T now?
		.collect::<HashSet<&'static _>>();
	Mutex::new(s)
});

fn get_or_insert(s: Statement) -> &'static Statement {
	let mut statements = STATEMENTS.lock();
	if !statements.contains(&s) {
		statements.insert(Box::leak(Box::new(s.clone())));
	}
	statements
		.get(&s)
		.expect("But I literally just inserted it?")
}

fn check_statement(ss: &V<&'static Statement>, law: Law) -> Option<&'static Statement> {
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
				Statement::Not(Statement::Not(result)) => Some(result),
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
				(Statement::Implies(x, y), Statement::Not(y_)) if y == y_ => {
					let not = get_or_insert(Statement::Not(x));
					Some(not)
				}
				(Statement::Not(y_), Statement::Implies(x, y)) if y == y_ => {
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

fn applicable_rules(ss: &V<&'static Statement>) -> Vec<V<&'static Statement>> {
	use Law::*;

	assert!(ss.len() <= 255);
	let numbers = 0..(ss.len() as u8);

	let single_rules = [
		AndEliminationLeft,
		AndEliminationRight,
		DoubleNotIntroduction,
		DoubleNotElimination,
		ImplicationIntroduction,
		Lem,
		NotIntroduction,
	]
	.into_iter()
	.cartesian_product(numbers.clone())
	.map(|(r, n)| r(n));
	let pair_rules = [
		AndIntroduction,
		Contradiction,
		NotElimination,
		ImplicationElimination,
		OrEliminationLeft,
		OrEliminationRight,
		ModusTollens,
	]
	.into_iter()
	.cartesian_product(numbers.clone())
	.cartesian_product(numbers)
	.map(|((r, n), m)| r(n, m));

	single_rules
		.chain(pair_rules)
		.filter_map(|l| check_statement(ss, l))
		.filter_map(|s| {
			(!ss.contains(&s)).then(|| {
				let mut ss = ss.clone();
				ss.push(s);
				ss
			})
		})
		.collect()
}

#[derive(Eq, PartialEq, Clone, Hash)]
struct StatementWrapper(V<&'static Statement>);

impl PartialOrd for StatementWrapper {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		let l_last = (*self.0.last().unwrap()).clone();
		let l_sim = similarity(l_last);
		let r_last = (*other.0.last().unwrap()).clone();
		let r_sim = similarity(r_last);

		l_sim.partial_cmp(&r_sim)
	}
}

impl Ord for StatementWrapper {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		self.partial_cmp(other).unwrap_or(std::cmp::Ordering::Equal)
	}
}

fn bfs(done: Statement, premises: V<&'static Statement>) -> V<&'static Statement> {
	let done_r = &done;
	let done_rr = Some(&done_r);

	SIMILARITY_COMPARISON.get_or_init(|| done.clone());
	SIMILARITY_COMPARISON_SET.get_or_init(|| substatements(done.clone()));

	let mut queue = BinaryHeap::new();
	queue.push(StatementWrapper(premises));
	let mut visited = HashSet::new();

	while let Some(head) = queue.pop() {
		if head.0.last() == done_rr {
			return head.0;
		}
		visited.insert(head.clone());

		for state in applicable_rules(&head.0).into_iter().map(StatementWrapper) {
			if visited.contains(&state) {
				continue;
			}
			queue.push(state);
		}
	}

	panic!("No solution")
}

#[cached]
fn substatements(s: Statement) -> HashSet<&'static Statement> {
	let mut set = HashSet::new();
	set.insert(get_or_insert(s.clone()));
	match s {
		Statement::And(l, r) | Statement::Or(l, r) | Statement::Implies(l, r) => {
			let l_set = substatements(l.clone());
			let r_set = substatements(r.clone());

			for s_ in l_set
				.into_iter()
				.chain(r_set.into_iter())
				.map(|x| get_or_insert(x.clone()))
			{
				set.insert(s_);
			}
		}
		Statement::Not(n) => {
			set.insert(n);
		}
		_ => {}
	}

	set
}

static SIMILARITY_COMPARISON: OnceCell<Statement> = OnceCell::new();
static SIMILARITY_COMPARISON_SET: OnceCell<HashSet<&'static Statement>> = OnceCell::new();

#[cached]
fn similarity(lhs: Statement) -> f32 {
	let l_set = substatements(lhs);
	let r_set = SIMILARITY_COMPARISON_SET.get().unwrap();

	let all = l_set.union(r_set);
	let shared = l_set.intersection(r_set);

	(shared.count() as f32) / (all.count() as f32)
}
