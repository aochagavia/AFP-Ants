use std::mem;
use std::num::Wrapping;

/// A random number generator
///
/// We could use a Rust library, but we want to be able to replicate the same
/// simulations as the Haskell simulator
pub struct Rng {
    stream: Box<Iterator<Item=usize>>,
}

impl Rng {
    pub fn new(seed: usize) -> Rng {
        fn f(x: usize) -> usize {
            (Wrapping(x) * Wrapping(22_695_477) + Wrapping(1)).0
        }

        fn g(x: usize) -> usize {
            (x / 65536) % 16384
        }

        Rng {
            stream: Box::new(iterate(seed, f).skip(4).map(g))
        }
    }

    pub fn random_int(&mut self, n: usize) -> usize {
        self.stream.next().unwrap() % n
    }
}

/// An iterator similar to Haskell's `iterate` function
pub struct Iterate<T: Copy, F: Fn(T) -> T> {
    state: T,
    f: F
}

impl<T: Copy, F: Fn(T) -> T> Iterator for Iterate<T, F> {
    type Item=T;
    fn next(&mut self) -> Option<Self::Item> {
        let new_state = (self.f)(self.state);
        Some(mem::replace(&mut self.state, new_state))
    }
}

fn iterate<T: Copy, F: Fn(T) -> T>(seed: T, f: F) -> Iterate<T, F> {
    Iterate { state: seed, f: f }
}

#[test]
// Ensure that our Rng is similar to the one implemented in Haskell
fn test_random() {
    let seed = 12345;
    let expected_results = [7193,2932,10386,5575,100,15976,430,9740,9449,1636,11030,9848,13965,16051,14483,6708,5184,15931,7014,461];
    let real_results: Vec<_> = Rng::new(seed).stream.take(20).collect();
    assert_eq!(real_results, expected_results);
}
