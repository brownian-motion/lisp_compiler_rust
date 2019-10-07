use std::marker::PhantomData;
use std::ops::{Index, IndexMut};

pub struct MemoryArena<T>(Vec<T>);

#[derive(Copy, Debug)]
pub struct ArenaIdx<T> {
    index: usize,
    node_type: PhantomData<T>, // just denotes that this is used to index a memory arena of the given type and lifetime
}

impl<T> Clone for ArenaIdx<T> {
    fn clone(&self) -> Self {
        ArenaIdx {
            index: self.index,
            node_type: PhantomData,
        }
    }
}

impl<T> MemoryArena<T> {
    pub fn add(&mut self, elem: T) -> ArenaIdx<T> {
        let new_index = self.0.len();
        self.0.push(elem);
        ArenaIdx {
            index: new_index,
            node_type: PhantomData,
        }
    }

    pub fn new() -> MemoryArena<T> {
        MemoryArena(Vec::new())
    }
}

impl<T> Index<ArenaIdx<T>> for MemoryArena<T> {
    type Output = T;

    fn index(&self, node_id: ArenaIdx<T>) -> &Self::Output {
        &self[&node_id]
    }
}

impl<T> Index<&ArenaIdx<T>> for MemoryArena<T> {
    type Output = T;

    fn index(&self, node_id: &ArenaIdx<T>) -> &Self::Output {
        &self.0[node_id.index]
    }
}

impl<T> IndexMut<ArenaIdx<T>> for MemoryArena<T> {
    fn index_mut(&mut self, node_id: ArenaIdx<T>) -> &mut Self::Output {
        &mut self[&node_id]
    }
}

impl<T> IndexMut<&ArenaIdx<T>> for MemoryArena<T> {
    fn index_mut(&mut self, node_id: &ArenaIdx<T>) -> &mut Self::Output {
        &mut self.0[node_id.index]
    }
}
